####################################################################################################
#'La idea de este script es revisar alternativas de pronostico de la trm utilizando MCO y variables 
#'de mercado. 
#'La idea es, utilizando datos de desde marzo de 2012, hacer un backtest de la proyeccion de la trm
#'para el anho 2018.
#'###################################################################################################
####### ----------- Limpiar ambiente ------- ###### 
rm(list = ls())
Fecha_inicial = as.Date("2010-01-1") 
Fecha_final = as.Date("2018-12-31")
##### -------- librerias ------- ###### 
librerias <- c("dplyr","tidyr","xts",'ggplot2','scales','matrixStats','readxl','openxlsx','httr','lubridate','forecast')
set.seed(123)

###### ----- Instalacion paquetes ------ ####
if(length(setdiff(librerias, rownames(installed.packages()))) > 0){
  install.packages(setdiff(librerias, rownames(installed.packages())))}
invisible(sapply(librerias, require, character.only = TRUE,quietly = TRUE))

######## ------- funcion para graficar simulaciones ------- #######
Graficar_simulaciones  = function(mat,title,hist=NULL,plotear=F,quitarCeros=0,leyenda_x,leyenda_y,Fecha_inicial = "2011-09-01"){
  ## INPUTS
  ## 1. mat: matriz con las simulaciones por columnas
  ## 2. hist: vector con la historia de la variable simulada
  ## 3. quitarCeros: numero de ceros a quitar
  ## 4. plot: Si se desea plotear o no
  mat=t(mat)
  medias = colMeans(mat)
  data = data.frame(media = medias)
  upper_seq = seq(70,95,by = 5)/100
  
  
  int=t(apply(mat,2,quantile,probs=upper_seq))
  data=cbind(data,t(apply(mat,2,quantile,probs=c(1-upper_seq,upper_seq))))
  colnames(data) = c("media","min70","min75","min80","min85","min90","min95","max70","max75","max80","max85","max90","max95")
  if(!is.null(hist)){
    hist=matrix(rep(hist,ncol(data)),ncol = ncol(data))
    colnames(hist)=colnames(data)
    data=rbind(hist,data)
    data=data/10^quitarCeros    
  }
  data = cbind(seq(as.Date(Fecha_inicial),length.out = length(rownames(data)),by="month"),data)
  colnames(data) = c("Fecha",colnames(data)[-1])
  plot1 = ggplot(data=data)+  
    geom_ribbon(aes(x=Fecha,ymin=min95,ymax=max95),alpha=0.3,fill="#4d4f44") +
    geom_ribbon(aes(x=Fecha,ymin=min90,ymax=max90),alpha=0.4,fill="#0e4d2c") +
    geom_ribbon(aes(x=Fecha,ymin=min85,ymax=max85),alpha=0.5,fill="#00665e") +
    geom_ribbon(aes(x=Fecha,ymin=min80,ymax=max80),alpha=0.6,fill="#223063") +
    geom_ribbon(aes(x=Fecha,ymin=min75,ymax=max75),alpha=0.7,fill="#98ad3a") +
    geom_line(aes(x=Fecha,y=min95),col="#0e4d2c",size=1.5,alpha =0.5) + 
    geom_line(aes(x=Fecha,y=max95),col="#0e4d2c",size=1.5,alpha =0.5) + 
    geom_line(aes(x=Fecha,y=media),col="#b9bd2e",size=1.5) + 
    theme(legend.position="none") + labs(title = title)+
    theme_bw()+ theme(legend.background = element_rect(fill="white", size=2.5, linetype="solid"),legend.title = element_text(colour="black", size=12, face="bold"),axis.text.x=element_text(angle=0, hjust=1),legend.text = element_text(colour="black", size=15, face="bold"),panel.grid.major = element_blank(), panel.border = element_blank(), plot.title = element_text(hjust = 1),axis.text=element_text(size=25,face="bold"),axis.title=element_text(size=25,face="bold"),legend.position = "top")+labs(title=title)+labs(x = leyenda_x, y = leyenda_y) +scale_x_date(date_breaks = "14 month", date_labels =  "%b %y")+scale_y_continuous(labels = comma)
  if(plotear){print(plot1)}
  return(plot1)
  
}

####### ----------- Cargar datos ----- ####### 
##### --------- Empleo ------ ########
Empleo_colombia = as.data.frame(read_excel('Empleo_COL.xlsx'))
###### ------ eliminamos todas las filas que tengan cero en todo ----- ####### 
Empleo_colombia = Empleo_colombia[apply(Empleo_colombia,1,function(x){!all(is.na(x))} ),] 
###### ------ eliminamos todas las filas que tengan cero en todo ----- ####### 
Empleo_colombia = Empleo_colombia[!is.na(Empleo_colombia[,2]),]
###### ------ Tomamos todo lo númerico----- ####### 
Empleo_colombia = Empleo_colombia[c(grep(pattern = "[0-9]",x = Empleo_colombia[,2])),] ### --- Sólo tomamos los datos reportados --- ### 
Empleo_colombia = Empleo_colombia[,c(1,2)]
### --- Organizamos los datos --- ### 
colnames(Empleo_colombia) = c("Date","Tasa_de_Empleo")
rownames(Empleo_colombia) = NULL
##### ----- Arreglamos la fecha --- ####3
Empleo_colombia$Date =  as.Date(paste0(Empleo_colombia$Date,"-1")) 
##### -------- Numerica ---- ####3
Empleo_colombia$Tasa_de_Empleo = as.numeric(Empleo_colombia$Tasa_de_Empleo)
######## --------- Organizar de mayor a menor ----- ###### 
Empleo_colombia = Empleo_colombia[c(order(Empleo_colombia$Date,decreasing = FALSE)),] 
rownames(Empleo_colombia) = NULL
### --- TS --- ### 
empleo_serie_tiempo = ts(Empleo_colombia[,2],start = c(year(Empleo_colombia$Date[1]),month(Empleo_colombia$Date[1])),end = c(year(Fecha_final),month(Fecha_final)),frequency = 12)
## --- Se eliminan los datos anteriores a la Fecha_inicial --- ### 
Empleo_colombia_t = xts(Empleo_colombia[,2],order.by = as.Date(Empleo_colombia$Date))
### --- Condicionalmos a la Fecha final --- ###
Empleo_colombia_t = Empleo_colombia_t[time(Empleo_colombia_t)<=Fecha_final]
Empleo_colombia_t = Empleo_colombia_t[time(Empleo_colombia_t)>=Fecha_inicial]
### --- Trimestralización de los datos --- ### 
Empleo_colombia_ts = apply.quarterly(Empleo_colombia_t,function(x) mean(x))
Empleo_ts = ts(as.numeric(Empleo_colombia_ts),start = c(2010,1), frequency = 4)

##### --------- Cambio de la serie de tiempo de Empleo ------ ########
plot(Empleo_ts)

##### --------- Cambio de la TASA ICC ------ ########
ICC = read.csv('ICC.csv')
##### --------- Cambio del IPP ------ ########
IPP = read.csv('IPP.csv')

########### ------ Transformar en una serie de tiempo -------- #########
Periodo_inicial = c(year(Fecha_inicial),quarter(Fecha_inicial))
Periodo_Final = c(year(Fecha_final),quarter(Fecha_final))
##### --------- ICC ------ #######
ICC_ts = ts(as.numeric(ICC$ICC),start= Periodo_inicial, end = Periodo_Final, frequency = 4)
plot(ICC_ts)
##### --------- IPP ------ ########
IPP_ts = ts(as.numeric(IPP$IPP),start= Periodo_inicial, end = Periodo_Final, frequency = 4) 
plot(IPP_ts)

############ ----------------- Reporte Grupo exito ------- ###########
Exito = as.data.frame(read.csv('Datos_exito.csv'))
Ingresos_exito_log_ts = ts(log(as.numeric(as.numeric(Exito[,"iexito"]))),start = c(year(Fecha_inicial),quarter(Fecha_inicial)) ,end = c(year(Fecha_final),quarter(Fecha_final)), frequency = 4) 
##### --------- Regresión 1: Logaritmo de los ingresos de Éxito Trimestrales --------- #####
Reg_ingresos_exito_ts = tslm(Ingresos_exito_log_ts~Empleo_ts+ICC_ts+IPP_ts)
###### ---- Estadisticas descriptivas ----- #### 
summary(Reg_ingresos_exito_ts)
####### -------- Nueva regresion ------- ###### 
Reg_ingresos_exito_ts = tslm(Ingresos_exito_log_ts~Empleo_ts)

########### ---------------- Proyeccion ------------- ############3 
###### ------ Cargar simulaciones ---- #### 
Empleo_colombia_simulado = readRDS('/Volumes/GoogleDrive/My Drive/Uniandes Series de tiempo/Clase 1/Codigo/Simulaciones_empleo.Rds')
numeroSim = 1000
Proy_log_ingresos_exito_ts = sapply(1:numeroSim,function(x) forecast(Reg_ingresos_exito_ts,h = 62,newdata = data.frame(Empleo_ts=Empleo_colombia_simulado[,x]))$mean)
### --- Destraformación del logaritmo --- ###
Proy_ingresos_exito = exp(Proy_log_ingresos_exito_ts)

########## ------- Resultados ------ #########3 
grafico3 = Graficar_simulaciones(hist=Exito[,"iexito"],(Proy_ingresos_exito), title = "Proyección de los ingresos",leyenda_x = "",leyenda_y = "")

