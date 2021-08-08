###### --------------------- Ejercicio 2: Funcion de utilidad ----------------- ############
####### ----------- Limpiar ambiente ------- ######
rm(list = ls())
options(scipen = 10000000)
##### --------- Path: automatico  --------- #####
path <<- gsub(rstudioapi::getActiveDocumentContext()$path,pattern = "/Ejercicio 2.+",replacement = "")
setwd(paste0(path,'/Ejercicio 2'))
##### -------- librerias ------- ######
librerias <- c("readxl","ggplot2","scales",'timeSeries','timeSeries','forecast','rugarch','fBasics','moments','tseries','PortfolioAnalytics','dplyr','zoo','nleqslv')

###### ----- Instalacion liberarias ------ ####
if(length(setdiff(librerias, rownames(installed.packages()))) > 0){
  install.packages(setdiff(librerias, rownames(installed.packages())))}
invisible(sapply(librerias, require, character.only = TRUE,quietly = TRUE))


##### --------- Datos acciones ------ ##### 
Acciones = as.data.frame(read_excel('Colcap.xlsx',sheet = 'Close'))
Acciones= na.locf(Acciones)
## ---- Organizar nombres --- ###
Fechas_originales = Acciones[,'Fecha']
Acciones = Acciones[,-1]
#### ------ Retornos simples ------ #### 
Acciones = apply(Acciones, 2, function(x){returns(x,method='simple')})[-1,]
Acciones = as.data.frame(Acciones) 
rownames(Acciones) = Fechas_originales[-1]
Acciones = Acciones[,c("GRUPOSURA","NUTRESA")]

###### ---------- Calcular frontera: Forma 2 -------- ##########
###### ------- Especificacion del portafolio ------ ###### 
espcartera<-portfolioSpec()
###### ------- Puntos de la frontera ------ ###### 
setNFrontierPoints(espcartera) <- 1000
###### ------- Restricciones: Positivas ------ ###### 
constraints="LongOnly"
###### ------- Construccion de la frontera ------ ###### 
Frontera <- portfolioFrontier(as.timeSeries(Acciones),spec=espcartera,constraints )
er_p_auto = Frontera@portfolio@portfolio$targetReturn[,'mean']
sd_p_auto = Frontera@portfolio@portfolio$targetRisk[,"Sigma"]


UtilidadPyG = function(DistribucionPyG,param){
  #Funcion que mide la bondad de una distribucion de PyG. Funcion de utilidad u(x) = (x+b)^a,
  #donde x es el PyG en DECIMALES, a<1. Esta funci?n se representa con una aversi?n al riesgo decreciente
  #(igual a (1-a)/(x+L)), consistente con lo que se puede esperar en el manejo del libro. b puede ser visto
  #como un nivel de tolerancia a p?rdidas m?ximas. Se devuelve una constante m?nima para valores por debajo.

  if(param==1){
    utilidad <- function(retorno) {
      return(log(retorno))
      }
  }else{
    utilidad <- function(retorno) {
      return(((retorno)^(1-param)-1)/(1-param))
    }
  }

  return(mean(sapply(DistribucionPyG,utilidad)))
}


######## -------- Estimacion del Parametro de Aversion ---------- ##### 
CalcularAversion = function(Equiv0){
  Equiv0 = Equiv0/100
  
  #     #Funcion que calibra la funcion de utilidad anterior a loterias equivalentes segun el nivel de PyG.
  fun_resolver <- function(param,E1){
    #### ----- Loteria 50%*U() + 50% U()
    return(0.5*UtilidadPyG(1,param)+0.5*UtilidadPyG(2,param)-UtilidadPyG(1+E1,param))
  }
  return(nleqslv(ifelse(Equiv0>0,2,0), fun_resolver, E1=Equiv0, control=list(ftol=1e-14))$x)
}



## --- Paso 1: Ajustar el valor en millones --- ### 
ParamAversion<<-CalcularAversion(Equiv0=20)


## --- Paso 2: Modelamos --- ### 
Simulacion=sapply(1:length(er_p_auto),function(x){rnorm(10000,(1+er_p_auto[x])^250-1,sqrt(250)*sd_p_auto[x])})

PortOptim=which.max(apply(1+Simulacion,2,UtilidadPyG,param=ParamAversion))

plot((1+er_p_auto)^250-1,x=sqrt(250)*sd_p_auto,type="l",xlab="Volatilidad",ylab="Retorno")
points(x=sqrt(250)*sd_p_auto[PortOptim],y=(1+er_p_auto[PortOptim])^250-1,col=2,pch=15)

