###### --------------------- Ejercicio 1: Componentes principales ----------------- ############ 
####### ----------- Limpiar ambiente ------- ###### 
rm(list = ls())
##### --------- Path: automatico  --------- #####
path <<- gsub(rstudioapi::getActiveDocumentContext()$path,pattern = "/Ejercicio 1.+",replacement = "")
setwd(paste0(path,'/Ejercicio 1'))
##### -------- librerias ------- ###### 
librerias <- c("readxl","ggplot2","scales",'timeSeries','timeSeries','forecast','rugarch','fBasics','zoo','moments','tseries','fPortfolio','data.table','kernlab','abind')

###### ----- Instalacion liberarias ------ ####
if(length(setdiff(librerias, rownames(installed.packages()))) > 0){
  install.packages(setdiff(librerias, rownames(installed.packages())))}
invisible(sapply(librerias, require, character.only = TRUE,quietly = TRUE))


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
    geom_ribbon(aes(x=Fecha,ymin=min95,ymax=max95),alpha=0.3,fill="#67001f") +
    geom_ribbon(aes(x=Fecha,ymin=min90,ymax=max90),alpha=0.4,fill="#b2182b") +
    geom_ribbon(aes(x=Fecha,ymin=min85,ymax=max85),alpha=0.5,fill="#d6604d") +
    geom_ribbon(aes(x=Fecha,ymin=min80,ymax=max80),alpha=0.6,fill="#878787") +
    geom_ribbon(aes(x=Fecha,ymin=min75,ymax=max75),alpha=0.7,fill="#4d4d4d") +
    geom_line(aes(x=Fecha,y=min95),col="#4575b4",size=1.5,alpha =0.5) +
    geom_line(aes(x=Fecha,y=max95),col="#4575b4",size=1.5,alpha =0.5) +
    geom_line(aes(x=Fecha,y=media),col="#4575b4",size=1.5) +
    theme(legend.position="none") + labs(title = title)+
    theme_bw()+ theme(legend.background = element_rect(fill="white", size=2.5, linetype="solid"),legend.title = element_text(colour="black", size=12, face="bold"),axis.text.x=element_text(angle=0, hjust=1),legend.text = element_text(colour="black", size=15, face="bold"),panel.grid.major = element_blank(), panel.border = element_blank(), plot.title = element_text(hjust = 1),axis.text=element_text(size=25,face="bold"),axis.title=element_text(size=25,face="bold"),legend.position = "top")+labs(title=title)+labs(x = leyenda_x, y = leyenda_y) +scale_x_date(date_breaks = "14 month", date_labels =  "%b %y")+scale_y_continuous(labels = comma)
  if(plotear){print(plot1)}
  return(plot1)
  
}

############## -------------- 1 Cargar informacion TESCOP y TESIBR --------------- #################
TESCOP = as.data.frame(read_excel('Datos/TESCOP.xlsx'))
rownames(TESCOP) = as.Date(TESCOP[,1])
TESCOP = TESCOP[,-1]

IBR = as.data.frame(read_excel('Datos/IBR.xlsx'))
rownames(IBR) = as.Date(IBR[,1])
IBR = IBR[,-1]

Consolidado = cbind(TESCOP,IBR[rownames(TESCOP),])
Consolidado = na.locf(Consolidado)
############ -------- Semanalizar: Cogiendo ---------- ##########
## Vamos a generar la serie semanal ##
DatosSem=seq(as.Date(rownames(Consolidado)[1]),as.Date(tail(rownames(Consolidado),1)),by = "week")
## En caso de festivos, tomamos el dia mas cercano.
DatosSem=sapply(DatosSem,function(x){which.min(abs(as.Date(rownames(Consolidado))-x))})
######## --------- Consolidar datos -------- ######## 
Consolidado = Consolidado[DatosSem,]

###### ---------- Retornos ------- ###### 
Retornos_historicos = apply(Consolidado, 2, function(x){returns(x, method='simple')})[-1,]


### ----- Asignarme cero retornos atipicos ---- ###### 
Retornos_historicos[which(!is.finite(Retornos_historicos))] = 0
#### ---- Almacenar media ---- #### 
Media_serie_retornos = colMeans(Retornos_historicos)


############## -------------- 2. Simular con componente principales 1000 simulaciones --------------- #################
Componentes = princomp(Retornos_historicos)

### --- Estadisticas descriptivas: Componentes principales --- ###
summary(Componentes)
### --- Varianza Acumulada --- ###
Varianza_Acumulada = cumsum(Componentes$sdev^2 / sum(Componentes$sdev^2))


### --- Se toma el componente 1 --- ###
Componente_significativo = Componentes$scores[,1]
Nivel_confianza = 0.87
Significativo = grep(names(Varianza_Acumulada),pattern = paste0("^",names(head(Varianza_Acumulada [Varianza_Acumulada > Nivel_confianza],1)),"$"))

####### ----------- Componente 1 -------- ######## 
### --- Se toma el componente 1 --- ###
Componente_significativo_1 = Componentes$scores[,1]
######## ------ Modelo ARMA sobre el componente 1 ------- #######
Modelo_ARMA_1 = auto.arima(Componente_significativo_1)
###### ---------- Verificar residuales ----- ###### 
### -- Verificar que los residuales sean ruido blanco (Box Pierce)
### -- H0: Los datos se distribuyen de forma independiente.
### -- H1: Los datos no se distribuyen de forma independiente.
### -- Si P-valor > 0.05, no se rechaza h0 la serie se comporta como Ruido Blanco 
Box.test(Modelo_ARMA_1$residuals)

####### ----------- Componente 2 -------- ######## 
Componente_significativo_2 = Componentes$scores[,2]
######## ------ Modelo ARMA sobre el componente 1 ------- #######
Modelo_ARMA_2 = auto.arima(Componente_significativo_2)
## ----- Prueba residuales ---- ### 
Box.test(Modelo_ARMA_2$residuals)

####### ----------- Componente 3 -------- ######## 
Componente_significativo_3 = Componentes$scores[,3]
######## ------ Modelo ARMA sobre el componente 1 ------- #######
Modelo_ARMA_3 = auto.arima(Componente_significativo_3)
## ----- Prueba residuales ---- ### 
Box.test(Modelo_ARMA_2$residuals)


#### ------ Devolver retornos a niveles ---- ####
Precios_final = tail(Consolidado,1)
###### --------- Para 1 simulacion --------- #######
Simulacion_individual = function(Horizonte_pronostico,Componentes,Modelo_ARMA_1,Modelo_ARMA_2,Modelo_ARMA_3,Media_serie_retornos,Precios_final){
  ######## ----------- Simular componentes ----------- ######## 
  Simular_componente_1 = simulate(object = Modelo_ARMA_1,nsim = Horizonte_pronostico)
  Simular_componente_2 = simulate(object = Modelo_ARMA_2,nsim = Horizonte_pronostico)
  Simular_componente_3 = simulate(object = Modelo_ARMA_3,nsim = Horizonte_pronostico)
  
  ###### ------- Juntar simulacion --------- #####33 
  Componentes_simulados = cbind(Simular_componente_1,Simular_componente_2,Simular_componente_3)
  
  #### ------- Devolver escala y componentes ------ ###
  Simulados = scale(Componentes_simulados%*%t(Componentes$loadings[,c(1:3)]), center = -Media_serie_retornos, scale = F)
  
  #### ------- Devolver retornos ------ ###
  Simulaciones_precios = (1+Simulados)
  Simulaciones_precios = rbind(as.numeric(Precios_final),Simulaciones_precios)
  Resultado = apply((Simulaciones_precios), 2, cumprod)
  return(Resultado)
}

Horizonte_pronostico = 4
nSim = 100
###### --------- Para 1000 simulaciones ----------- ##### 
Simulaciones = replicate(nSim,expr = Simulacion_individual(Horizonte_pronostico = Horizonte_pronostico,
                                                       Componentes = Componentes,Modelo_ARMA_1 = Modelo_ARMA_1,
                                                       Modelo_ARMA_2 =Modelo_ARMA_2,Modelo_ARMA_3 = Modelo_ARMA_3,
                                                       Media_serie_retornos = Media_serie_retornos,
                                                       Precios_final = Precios_final))



dimnames(Simulaciones)[[1]] = as.character(seq(from = as.Date(tail(rownames(Consolidado),1)),by = 'month',length.out = 5))
dimnames(Simulaciones)[[3]] = NULL


##### ----- Graficar para un nodo todas las simulaciones --------- #######
Graficar_simulaciones(mat = Simulaciones[,"TESCOP_1M",],hist = tail(Consolidado[,"TESCOP_1M"],50),title = '',leyenda_x = '',leyenda_y = '')

###### --------- Pendiente de la curva -------- ##### 
plot(as.numeric(tail(Consolidado[,c(grep(colnames(Consolidado),pattern = 'TESCOP'))],1)),type='l')
lines(as.numeric(tail(Simulaciones[-1,c(grep(dimnames(Simulaciones)[[2]],pattern = 'TESCOP')),4],1)),col='red')
lines(as.numeric(tail(Simulaciones[-1,c(grep(dimnames(Simulaciones)[[2]],pattern = 'TESCOP')),60],1)),col='yellow')
lines(as.numeric(tail(Simulaciones[-1,c(grep(dimnames(Simulaciones)[[2]],pattern = 'TESCOP')),100],1)),col='blue')



############## -------------- 2. Indice --------------- #################

####################### ------------------------- TES COP -------------------------- ###########################
Nodos_TESCOP = grep(dimnames(Simulaciones)[[2]],pattern = 'TESCOP')

Simulaciones_TESCOP = Simulaciones[,Nodos_TESCOP,]
colnames(Simulaciones_TESCOP) = gsub(x = colnames(Simulaciones_TESCOP),pattern = 'TESCOP_',replacement ='')
##### ------- Se toman los Meses y se pasan a numero ---- ##### 
colnames(Simulaciones_TESCOP)[grep(colnames(Simulaciones_TESCOP),pattern = 'M')] = as.numeric(gsub(colnames(Simulaciones_TESCOP)[grep(colnames(Simulaciones_TESCOP),pattern = 'M')],pattern = 'M',replacement = ''))/12
##### ------- Se toman los años y se pasan a numero---- ##### 
colnames(Simulaciones_TESCOP)[grep(colnames(Simulaciones_TESCOP),pattern = 'Y')] = as.numeric(gsub(colnames(Simulaciones_TESCOP)[grep(colnames(Simulaciones_TESCOP),pattern = 'Y')],pattern = 'Y',replacement = ''))

######## --------- Interpolacion de nodos 10 años-1 semana ------ ######3 
Interpolacion = sapply(c(1:dim(Simulaciones_TESCOP)[3]), function(z){apply(Simulaciones_TESCOP[,,z], 1, function(z){approx(x = as.numeric(colnames(Simulaciones_TESCOP)),y = as.numeric(z),xout = ((365*10-7)/365))$y})})
Interpolacion = as.data.frame(Interpolacion)
colnames(Interpolacion) = rep('9.98',dim(Interpolacion)[2])
Simulaciones_TESCOP = abind(Simulaciones_TESCOP,Interpolacion,along = 2)
colnames(Simulaciones_TESCOP)[[length(colnames(Simulaciones_TESCOP))]] = '9.98'

######## --------- Interpolacion de nodos 4 años-1 semana ------ ######3 
Interpolacion = sapply(c(1:dim(Simulaciones_TESCOP)[3]), function(z){apply(Simulaciones_TESCOP[,,z], 1, function(z){approx(x = as.numeric(colnames(Simulaciones_TESCOP)),y = as.numeric(z),xout = ((365*4-7)/365))$y})})
Interpolacion = as.data.frame(Interpolacion)
colnames(Interpolacion) = rep('3.98',dim(Interpolacion)[2])
Simulaciones_TESCOP = abind(Simulaciones_TESCOP,Interpolacion,along = 2)
colnames(Simulaciones_TESCOP)[[length(colnames(Simulaciones_TESCOP))]] = '3.98'
######## --------- Interpolacion de nodos 1 años-1 semana ------ ######3 
Interpolacion = sapply(c(1:dim(Simulaciones_TESCOP)[3]), function(z){apply(Simulaciones_TESCOP[,,z], 1, function(z){approx(x = as.numeric(colnames(Simulaciones_TESCOP)),y = as.numeric(z),xout = ((365*1-7)/365))$y})})
Interpolacion = as.data.frame(Interpolacion)
colnames(Interpolacion) = rep('0.98',dim(Interpolacion)[2])
Simulaciones_TESCOP = abind(Simulaciones_TESCOP,Interpolacion,along = 2)
colnames(Simulaciones_TESCOP)[[length(colnames(Simulaciones_TESCOP))]] = '0.98'
### ---------- Organizar de mayor a menor --------- ###### 
Simulaciones_TESCOP = Simulaciones_TESCOP[,order(as.numeric(colnames(Simulaciones_TESCOP))),]
dimnames(Simulaciones_TESCOP)[[3]] = NULL



####################### ------------------------- IBR -------------------------- ###########################
############## -------------- 3: Seleccionar nodos 1,4, y 10 para TEs y IBR--------------- #################
Nodos_IBR = grep(dimnames(Simulaciones)[[2]],pattern = 'IBR')
Simulaciones_IBR = Simulaciones[,Nodos_IBR,]
colnames(Simulaciones_IBR) = gsub(x = colnames(Simulaciones_IBR),pattern = 'IBR_',replacement ='')
##### ------- Se toman los Meses y se pasan a numero ---- ##### 
colnames(Simulaciones_IBR)[grep(colnames(Simulaciones_IBR),pattern = 'M')] = as.numeric(gsub(colnames(Simulaciones_IBR)[grep(colnames(Simulaciones_IBR),pattern = 'M')],pattern = 'M',replacement = ''))/12
##### ------- Se toman los años y se pasan a numero---- ##### 
colnames(Simulaciones_IBR)[grep(colnames(Simulaciones_IBR),pattern = 'Y')] = as.numeric(gsub(colnames(Simulaciones_IBR)[grep(colnames(Simulaciones_IBR),pattern = 'Y')],pattern = 'Y',replacement = ''))

######## --------- Interpolacion de nodos 10 años-1 semana ------ ######3 
Interpolacion = sapply(c(1:dim(Simulaciones_IBR)[3]), function(z){apply(Simulaciones_IBR[,,z], 1, function(z){approx(x = as.numeric(colnames(Simulaciones_IBR)),y = as.numeric(z),xout = ((365*10-7)/365))$y})})
Interpolacion = as.data.frame(Interpolacion)
colnames(Interpolacion) = rep('9.98',dim(Interpolacion)[2])
Simulaciones_IBR = abind(Simulaciones_IBR,Interpolacion,along = 2)
colnames(Simulaciones_IBR)[[length(colnames(Simulaciones_IBR))]] = '9.98'

######## --------- Interpolacion de nodos 4 años-1 semana ------ ######3 
Interpolacion = sapply(c(1:dim(Simulaciones_IBR)[3]), function(z){apply(Simulaciones_IBR[,,z], 1, function(z){approx(x = as.numeric(colnames(Simulaciones_IBR)),y = as.numeric(z),xout = ((365*4-7)/365))$y})})
Interpolacion = as.data.frame(Interpolacion)
colnames(Interpolacion) = rep('3.98',dim(Interpolacion)[2])
Simulaciones_IBR = abind(Simulaciones_IBR,Interpolacion,along = 2)
colnames(Simulaciones_IBR)[[length(colnames(Simulaciones_IBR))]] = '3.98'

######## --------- Interpolacion de nodos 1 años-1 semana ------ ######3 
Interpolacion = sapply(c(1:dim(Simulaciones_IBR)[3]), function(z){apply(Simulaciones_IBR[,,z], 1, function(z){approx(x = as.numeric(colnames(Simulaciones_IBR)),y = as.numeric(z),xout = ((365*1-7)/365))$y})})
Interpolacion = as.data.frame(Interpolacion)
colnames(Interpolacion) = rep('0.98',dim(Interpolacion)[2])
Simulaciones_IBR = abind(Simulaciones_IBR,Interpolacion,along = 2)
colnames(Simulaciones_IBR)[[length(colnames(Simulaciones_IBR))]] = '0.98'
### ---------- Organizar de mayor a menor --------- ###### 
Simulaciones_IBR = Simulaciones_IBR[,order(as.numeric(colnames(Simulaciones_IBR))),]
dimnames(Simulaciones_IBR)[[3]] = NULL



############## -------------- 3: Seleccionar nodos 1,4, y 10 para TEs y IBR--------------- #################
Simulaciones_IBR = Simulaciones_IBR[,c("0.98","1","3.98","4","9.98","10"),]
Simulaciones_TESCOP = Simulaciones_TESCOP[,c("0.98","1","3.98","4","9.98","10"),]
############## -------------- 4: Seleccionar nodos 1,4, y 10 para TEs y IBR--------------- #################


############## ------------------- TESCOP --------------------- #############
####### ------- Rendimientos ------ ###### 
Indices_totales_TESCOP = NULL
for( s in c(1:dim(Simulaciones_TESCOP)[3])){
  Resultados = data.frame('1'=rep(0,dim(Simulaciones_TESCOP)[1]),'4'=rep(0,dim(Simulaciones_TESCOP)[1]),'10'=rep(0,dim(Simulaciones_TESCOP)[1]))
  colnames(Resultados) = c('1','3','5')
  
  Resultados_week = data.frame('1'=rep(0,dim(Simulaciones_TESCOP)[1]),'4'=rep(0,dim(Simulaciones_TESCOP)[1]),'10'=rep(0,dim(Simulaciones_TESCOP)[1]))
  colnames(Resultados_week) = c('1','3','5')
  
  for (k in c(1,3,5)){
    for (j in c(1:dim(Simulaciones_IBR)[1])) {
      ##### ---- TES 1 ---- ###
      Resultados[j,as.character(k)] = 1/(1+(Simulaciones_TESCOP[j,k+1,s]/100))^(as.numeric(names(Simulaciones_TESCOP[j,,s])[k+1]))
      ##### ---- TES 2 ---- ###
      Resultados_week[j,as.character(k)] = 1/(1+(Simulaciones_TESCOP[j,k,s]/100))^(as.numeric(names(Simulaciones_TESCOP[j,,s])[k]))  
    }
  }
  ######### ------- creacion del indice ------ #####
  Indice = Resultados_week[-1,]/Resultados[-nrow(Resultados),]
  
  ###### -------- Cumprod -------- ####
  Indice_final = apply(Indice, 2, function(x){c(1,cumprod(x))})
  colnames(Indice_final) = c('TESCOP_1','TESCOP_4','TESCOP_10')
  
  Indices_totales_TESCOP = abind(Indices_totales_TESCOP,Indice_final,along = 3)
  
}

############## ------------------- IBR --------------------- #############
####### ------- Rendimientos ------ ###### 
Indices_totales_IBR = NULL
for( s in c(1:dim(Simulaciones_IBR)[3])){
  Resultados = data.frame('1'=rep(0,dim(Simulaciones_IBR)[1]),'4'=rep(0,dim(Simulaciones_IBR)[1]),'10'=rep(0,dim(Simulaciones_IBR)[1]))
  colnames(Resultados) = c('1','3','5')
  
  Resultados_week = data.frame('1'=rep(0,dim(Simulaciones_IBR)[1]),'4'=rep(0,dim(Simulaciones_IBR)[1]),'10'=rep(0,dim(Simulaciones_IBR)[1]))
  colnames(Resultados_week) = c('1','3','5')
  
  for (k in c(1,3,5)){
    for (j in c(1:dim(Simulaciones_IBR)[1])) {
      ##### ---- TES 1 ---- ###
      Resultados[j,as.character(k)] = 1/(1+(Simulaciones_IBR[j,k+1,s]/100))^(as.numeric(names(Simulaciones_IBR[j,,s])[k+1]))
      ##### ---- TES 2 ---- ###
      Resultados_week[j,as.character(k)] = 1/(1+(Simulaciones_IBR[j,k,s]/100))^(as.numeric(names(Simulaciones_IBR[j,,s])[k]))  
    }
  }
  ######### ------- creacion del indice ------ #####
  Indice = Resultados_week[-1,]/Resultados[-nrow(Resultados),]
  
  ###### -------- Cumprod -------- ####
  Indice_final = apply(Indice, 2, function(x){c(1,cumprod(x))})
  colnames(Indice_final) = c('IPC_1','IPC_4','IPC_10')
  Indices_totales_IBR = abind(Indices_totales_IBR,Indice_final,along = 3)
  
}




Consolidado_indices = abind(Indices_totales_TESCOP,Indices_totales_IBR,along = 2)

####### ----------- Se toman los datos del ultimo periodo y se vuelve retorno ------- ######### 
Resultados_retornos = Consolidado_indices[5,,]-1
  

############## -------------- 5 Calibrar VARCOV--------------- #################
###### ---------- Sigma -------- #######
Sigma = cov(t(Resultados_retornos))
###### ---------- Media -------- #######
mu = colMeans(t(Resultados_retornos))

############## -------------- 6 Optimizacion de portafolio--------------- #################
X = t(Resultados_retornos)
caja=cbind(rep(0,ncol(X)),rep(1,ncol(X)))

portfolioOptimQP = function(sigma,mu,Rbar,caja="no",A_sects=NULL,lims_sects=NULL){
  ## INPUTS:
  # 1. sigma: matriz var-covar de las variables
  # 2. mu: Retornos de las variables
  # 3. Rbar: es el minimo retorno esperado
  # 4. caja: Hay restricciones de caja? opciones: "no" o una matriz con las restricciones de caja
  # 5. A_sets: matriz con las restricciones por sector ( nRestriccionesSectoriale x nAcciones)
  # 6. lims_sects: Vector con los limites sectoriales correspondientes a A_sects
  ## OUTPUTS:
  # 1. w: vector de pesos
  
  Nvar = length(mu)
  
  if(caja!="no" & is.null(A_sects)){ #Caso con restriccion de caja, sin restricciones sectoriales (Se puede usar LowRankQP)
    #   # Solucion LowRankQP (FUNCIONA BIEN, la condicion Ax es de IGUALDAD) (OJO limite inferior es cero!!!)
    #    Vmat = 2*sigma
    #    Amat = t(cbind(rep(1,Nvar),mu))
    #    dvec = t(0*mu)  
    #    bvec = c(1,Rbar)                   
    #    uvec = caja[,2]       
    #    sol = LowRankQP(Vmat,dvec,Amat,bvec,uvec,method="LU",verbose=FALSE,niter=200)$alpha
    # Solucion kernlab  (FUNCIONA BIEN, no optimiza en los dos extremos de la forntera!!!!!)
    c = t(0*mu)
    H = 2*sigma
    A = rbind(rep(1,Nvar),mu)
    b = c(1,Rbar)
    l=(caja[,1])
    u = (caja[,2])
    r= c(0,0)                                    #verb=1 hace que se muestre la informacion de la convergencia
    sol = try(ipop(c, H, A, b, l, u, r, sigf = 6, maxiter = 1000, margin = 0.005, bound = 10,verb = 0),silent=TRUE)
    if(class(sol)=="try-error"){ sol=rep(NA,Nvar) }
    else{ sol=sol@primal }
    
  }
  if(caja!="no" & !is.null(A_sects)){ #Caso con restriccion de caja, sin restricciones sectoriales (toca usar kernlab)
    # Solucion kernlab  (FUNCIONA BIEN, no optimiza en los dos extremos de la forntera!!!!!)
    c = t(0*mu)
    H = 2*sigma
    A = rbind(rep(1,Nvar),mu,A_sects)
    b = c(1,Rbar,as.numeric(lims_sects[1,]))
    l=(caja[,1])
    u = (caja[,2])
    r= c(0,0,as.numeric(lims_sects[2,]-lims_sects[1,]))                                    #verb=1 hace que se muestre la informacion de la convergencia
    sol = try(ipop(c, H, A, b, l, u, r, sigf = 7, maxiter = 400, margin = 0.05, bound = 10,verb = 0),silent=TRUE)
    if(class(sol)=="try-error"){ sol=rep(NA,Nvar) }
    else{ sol=sol@primal }
  }
  
  return( sol )
}


######## ---------- Grilla de retornos de la frontera ---------- ####### 
Rbar=seq(min(mu),max(mu),length.out=100)
###### --------- Calculo de portafolios ------- #####3 
Portafolios=na.omit(t(sapply(Rbar,portfolioOptimQP,mu=mu,sigma=Sigma,caja=caja)))

