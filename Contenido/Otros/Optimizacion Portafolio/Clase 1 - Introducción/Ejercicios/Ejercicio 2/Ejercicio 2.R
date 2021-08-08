####### ----------- Limpiar ambiente ------- ###### 
rm(list = ls())
##### --------- Path: automatico  --------- #####
path <<- gsub(rstudioapi::getActiveDocumentContext()$path,pattern = "/Ejercicio 2.+",replacement = "")
setwd(paste0(path,'/Ejercicio 2'))
##### -------- librerias ------- ###### 
librerias <- c("readxl","ggplot2","scales",'timeSeries','timeSeries','forecast','rugarch','fBasics')

###### ----- Instalacion liberarias ------ ####
if(length(setdiff(librerias, rownames(installed.packages()))) > 0){
  install.packages(setdiff(librerias, rownames(installed.packages())))}
invisible(sapply(librerias, require, character.only = TRUE,quietly = TRUE))

####### --------- Etapa 1: Organizción de datos Insumos ---------- ###### 
### Defina Portafolio_acciones y asigne la informacion de Portafolio.xlsx
### Defina Historia_precios y asigne la informacion de Historia.xlsx

###### ----------- Precios acciones ---------- ##### 
Historia_precios = as.data.frame(read_excel('Historia.xlsx'))
## ----- Organice los datos 
Historia_precios = Historia_precios[-c(1,3),-c(1,2)]
## -- Ponga el nombre de las columnas -- ##
colnames(Historia_precios) = Historia_precios[1,]
#### ------ Elimine la primera fila de nombres ------ ###
Historia_precios = Historia_precios[-1,]
#### ----- Todo numerico ----- ####
Historia_precios = as.data.frame(apply(Historia_precios, 2, as.numeric))
#### ----- Arregle el formato de fechas ---- ###### 
Historia_precios$Fecha = as.Date(Historia_precios$Fecha,origin = "1899-12-30")
#### ----- Modificar el nombre de las filas con las fechas ------ ####
rownames(Historia_precios) =  Historia_precios$Fecha
#### ------- Eliminar fila de fecha ------ ####
Historia_precios = Historia_precios[,-c(1)]
Historia_precios = as.matrix(Historia_precios)

############### ------------ Retornos -------------- #############

###------  Simples ------ ##
Retornos_simples = function(Valores){
  ### ------ Retorno simple (Pt-Pt-1)/(Pt-1) ----- ##### 
  Resultado= diff(Valores)/Valores[-length(Valores)]
  return(Resultado)
}

###------  Simples ------ ##
Retornos_log = function(Valores){
  ### 
  ### ------ Retorno simple (Pt-Pt-1)/(Pt-1) ----- ##### 
  Resultado= log(Valores[-1]/Valores[-length(Valores)])
  return(Resultado)
}

### ----------- Retornos simples ---------- ###
Retornos_manuales = apply(Historia_precios,2, Retornos_simples)
### ----------- Retornos Log ---------- ###
Retornos_manuales = apply(Historia_precios,2, Retornos_log)


### --------- Se extraen los retornos ----------- ###
Retornos = as.data.frame(apply(Historia_precios, 2, function(x){returns(x,method='simple')}))[-1,]

### ---------- Graficos ---------- ####
Serie = Retornos[,"Bancolombia"]
hist(Serie)
### --------- Simulada uniforme -------- ######
qqnorm(Serie, pch = 19, col = "gray50")
qqline(Serie)


######## --------- Contraste de hipótesis: Normalidad -------- ##########
# Pruebas para para analizar la normalidad de los datos. 
# hipótesis nula que los datos sí proceden de una distribución normal 
# hipótesis alternativa que no se distribuye nromalmente
# Un valor menor de 0.05 implica que se rechaza la hipotesis nula 

######### ------------- Shapiro ----------- ############
# Este test se emplea para contrastar 
# normalidad cuando el tamaño de la muestra es menor de 50. 
# Para muestras grandes es equivalente al test de kolmogorov-Smirnov.
###### ------------ Normal ---------- ####### 
Simulacion_normal = rnorm(n=1000,mean = 100,sd = 10)
shapiro.test(x = Simulacion_normal)
###### ------------ Uniforme ---------- ####### 
Simulacion_uniforme = runif(n = 1000,min = 0,max = 1)
shapiro.test(x = Simulacion_uniforme)

#######----------- Paso 1: Estimación de Distribuciones ----------- ########
Retornosaux = Retornos
#### --------- Pruebas de distancia de Kolmogorov-Smirnov ------- #######
######### ---------- Se crea una Matriz con 6 columnas (distribuciones)
######### ---------- 10 filas (Acciones)
Testkol=as.data.frame(matrix(0,ncol=5,nrow=ncol(Retornosaux)))
####### ------------ Muestra de entrenamiento/ Muestra Prueba --------- ####### 
porcentajeTrain=0.5
indicesTrain_aux=sample(1:(nrow(Retornosaux)),porcentajeTrain*nrow(Retornosaux))

######### ------------- Separacion entre muestra de entrenamiento y muestra de prueba ------- ########
indicesTest_aux=(1:nrow(Retornosaux))[-indicesTrain_aux] 
rownames(Testkol)=colnames(Retornosaux)
#####---------- Distribuciones a estimar -------------- #######
colnames(Testkol)=c("Normal","Logistica","Uniforme","Hiperbolica","Ganador")

##### ---------- Fin construcción tabla ------ #########
retsAcc = Retornosaux
for (i in 1:ncol(Retornosaux)){
  x=retsAcc[,i]
  #   descdist(c(x),discrete=F)
  
  ####### --------- Ajustar un modelo Arima para definir en un GARCH--------- ##### 
  ARMA=auto.arima(x)
  AR=length(grep("ar",names(ARMA$coef)))
  MA=length(grep("ma",names(ARMA$coef)))
  if(AR+MA==0){
    AR=1
  }
  
  ##### -------- Definir el número de grados de ARCH a partir de los residuales -------- ######
  ## A a partir de los residuales al cuadrado se determina el grado del modelo ARCH.
  #### --------- Auto
  ARCH=auto.arima(ARMA$residuals^2)$coef
  AR_ARCH=length(grep("ar",names(ARCH)))
  MA_ARCH=length(grep("ma",names(ARCH)))
  
  ####### --------- Especificación GARCH ------- ##########
  Especificacion_garch=ugarchspec(mean.model = list(armaOrder = c(AR,MA)),variance.model = list(garchOrder = c(max(1,AR_ARCH),MA_ARCH)), distribution.model = "norm")
  ####### ---------- Ajustar el modelo ------- ######
  Garch_ajustado = ugarchfit(Especificacion_garch, data = x,solver="gosolnp")
  Residuales_garch = residuals(Garch_ajustado)
  xyz=as.numeric(Residuales_garch)
  set.seed(124)
  indicesTrain=NULL
  
  ############# --------- Definicion de entrenamiento/prueba -----#######
  indicesTrain=sample(1:(length(xyz)),0.5*length(xyz))
  indicesTest=(1:length(xyz))[-indicesTrain] 
  
  ################ -------------- Normal: (Prueba+Entrenamiento)/2 --------------#########
  ######### ----------- Sobre la muestra de entrenamiento --------- ######### 
  param_norm=try(unlist(fitdistrplus::fitdist(c(xyz[indicesTrain]),"norm")[1]),silent=T)
  if(class(param_norm)=="try-error"){
    Testkol[i,"Normal"]=Inf
    } else {
    names(param_norm)=gsub("estimate.","",names(param_norm))
    ######## --------- Kolmogorov-Smirnov Tests --------- #########3
    Testkol[i,"Normal"]=ks.test(x=xyz[indicesTest],y = "pnorm",mean = param_norm[1], sd = param_norm[2])[[1]]  
    }
  
  
  ######### ----------- Sobre la muestra de prueba --------- ######### 
  param_norm=try(unlist(fitdistrplus::fitdist(c(xyz[-indicesTrain]),"norm")[1]),silent=T)
  if(class(param_norm)=="try-error"){
    Testkol[i,"Normal"]=Inf
    } else {
    names(param_norm)=gsub("estimate.","",names(param_norm))
    #### -------- Promedio ------ ####
    Testkol[i,"Normal"]=(Testkol[i,"Normal"]+ks.test(xyz[-indicesTest],"pnorm",mean = param_norm[1], sd = param_norm[2])[[1]])/2
    }
  
  
  ################ -------------- Uniforme: (Prueba+Entrenamiento)/2 --------------#########
  ######### ----------- Sobre la muestra de entrenamiento --------- ######### 
  param_unif=try(unlist(fitdistrplus::fitdist(c(x[indicesTrain]),"unif")[1]),silent=T)
  if(class(param_unif)=="try-error"){
    Testkol[i,"Uniforme"] = Inf
    } else {
    names(param_unif)=gsub("estimate.","",names(param_unif))
    Testkol[i,"Uniforme"] = ks.test(xyz[-indicesTrain],"punif",min=param_unif["min"],max=param_unif["max"])[[1]]    
  }
  
  ######### ----------- Sobre la muestra de prueba --------- ######### 
  param_unif=try(unlist(fitdistrplus::fitdist(c(x[-indicesTrain]),"unif")[1]),silent=T)
  if(class(param_unif)=="try-error"){
    Testkol[i,"Uniforme"]=Inf
    } else {
    names(param_unif)=gsub("estimate.","",names(param_unif))
    #### -------- Promedio ------ ####
    Testkol[i,"Uniforme"]=(Testkol[i,"Uniforme"]+ks.test(xyz[indicesTrain],"punif",min=param_unif["min"],max=param_unif["max"])[[1]])/2
  }
  
  
  ################ -------------- Logistica: (Prueba+Entrenamiento)/2 --------------#########
  ######### ----------- Sobre la muestra de entrenamiento --------- ######### 
  param_logistic=try(unlist(fitdistrplus::fitdist(c(xyz[indicesTrain]),"logis")[1]),silent=T)
  if (class(param_logistic)=="try-error"){
    Testkol[i,"Logistica"]= Inf 
    } else {
    names(param_logistic)=gsub("estimate.","",names(param_logistic))
    Testkol[i,"Logistica"]=ks.test(xyz[-indicesTrain],"plogis",location = param_logistic[1], scale = param_logistic[2])[[1]]
  }
  ######### ----------- Sobre la muestra de prueba --------- ######### 
  param_logistic=try(unlist(fitdistrplus::fitdist(c(xyz[-indicesTrain]),"logis")[1]),silent=T)
  if(class(param_logistic)=="try-error"){
    Testkol[i,"Logistica"]=Inf 
    } else {
    names(param_logistic)=gsub("estimate.","",names(param_logistic))
    Testkol[i,"Logistica"]=(Testkol[i,"Logistica"]+ks.test(xyz[indicesTrain],"plogis",location = param_logistic[1], scale = param_logistic[2])[[1]])/2
  }
  
  
  ################ -------------- Hiperbolica: (Prueba+Entrenamiento)/2 --------------#########
  param_hyper=try(unlist(ghFit(c(xyz[indicesTrain]),trace=F,doplot=F)@fit$estimate),silent=T)
  ######### ----------- Sobre la muestra de entrenamiento --------- ######### 
  if(class(param_hyper)=="try-error"){
    Testkol[i,"Hiperbolica"]=Inf 
    } else {
    names(param_hyper)=gsub("estimate.","",names(param_hyper))
    Testkol[i,"Hiperbolica"]=ks.test(xyz[-indicesTrain],"pgh",alpha=param_hyper["alpha"],beta=param_hyper["beta"],delta=param_hyper["delta"],mu=param_hyper["mu"],lambda=param_hyper["lambda"])[[1]]
    }
  
  ######### ----------- Sobre la muestra de prueba --------- ######### 
  param_hyper=try(unlist(ghFit(c(xyz[-indicesTrain]),trace=F,doplot=F)@fit$estimate),silent=T)
  if(class(param_hyper)=="try-error"){
    Testkol[i,"Hiperbolica"]= Inf
    } else {
    names(param_hyper)=gsub("estimate.","",names(param_hyper))
    Testkol[i,"Hiperbolica"]=(Testkol[i,"Hiperbolica"]+ks.test(xyz[indicesTrain],"pgh",alpha=param_hyper["alpha"],beta=param_hyper["beta"],delta=param_hyper["delta"],mu=param_hyper["mu"],lambda=param_hyper["lambda"])[[1]])/2
    }
  
  print(i)
}

###### ----------- Las mejores marginales Independiente -------- ##### 
Testkol[,"Ganador"]=colnames(Testkol)[apply(Testkol[,-ncol(Testkol)],1,which.min)]
# Testkol=Testkol[,-3]
Testkol[,1:5]=round(Testkol[,1:5],4)
Testkol[Testkol==Inf]="No Convergió"

############ -------------- Resultados ----------------- ####### 
Resultados = (list(Mejor=Testkol[,"Ganador"],indicesTrain=indicesTrain,p_value=Testkol[,-ncol(Testkol)]))


