#'En este script se van a revisar, utilizando modelos multivariados tipo VAR y VECM una hipotesis
#'importantes de la economia.
# Existe una relacion importante (posiblemente unidireccional y de corto plazo) entre la inflacion y el desempleo


# Paquetes ------------------------------------------------------------------------------------
rm(list=ls())
if (!require(dplyr))install.packages("dplyr");library(dplyr)
if (!require(tidyr))install.packages("tidyr");library(tidyr)
if (!require(xts))install.packages("xts");library(xts)
if (!require(ggplot2))install.packages("ggplot2");library(ggplot2)
if (!require(scales))install.packages("scales");library(scales)
if (!require(matrixStats))install.packages("matrixStats");library(matrixStats)
if (!require(readxl))install.packages("readxl");library(readxl)
if (!require(openxlsx))install.packages("openxlsx");library(openxlsx)
if (!require(httr))install.packages("httr");library(httr)
if (!require(lubridate))install.packages("lubridate");library(lubridate)
if (!require(forecast))install.packages("forecast");library(forecast)
if (!require(tseries))install.packages("tseries");library(tseries)
if (!require(vars))install.packages("vars");library(vars)
if (!require(tsDyn))install.packages("tsDyn");library(tsDyn)
if (!require(abind))install.packages("abind");library(abind)

path <- gsub("Clase5.R","",rstudioapi::getActiveDocumentContext()$path)
setwd(path)


# Arreglo de datos ----------------------------------------------------------------------------

#se lee los datos
datosModelo <- readRDS('datosCrudos.RDS')
datosModelo <- xts(datosModelo[,-1],order.by = datosModelo[,1])


#se separa train y test
datosPron <- 6
data_train <- head(datosModelo,nrow(datosModelo)-datosPron)
data_test <- tail(datosModelo,datosPron)


# Desarrollo ----------------------------------------------------------------------------------


#revisar estacionariedad
#WTI estacionario
adf.test(data_train[,1], alternative="stationary")
#Brent estacionario
adf.test(data_train[,2], alternative="stationary")

#ninguno de los dos es estacionario, queremos hacer diferencias, se guarda data_test como data_train_niveles
#y se crea uno en diferencias
data_train_niveles <- data_train
data_test_niveles <- data_test
#ae ponen en los niveles de test el ultimo de train solo para tener la diferencia del primer periodo
#luego se quita
data_test <- rbind(tail(data_train,1),data_test)
data_test[,1] <- diff(data_test[,1])
data_test[,2] <- diff(data_test[,2])
data_test <- data_test[complete.cases(data_test),]

data_train[,1] <- diff(data_train[,1])
data_train[,2] <- diff(data_train[,2])
data_train <- data_train[complete.cases(data_train),]

#se repite la prueba
adf.test(data_train[,1], alternative="stationary")
adf.test(data_train[,2], alternative="stationary")
#ambos son estacionarios ahora

#escoger el p de VAR bajo el criterio de Schwarts
## con solo constante
p1 <- VARselect(data_train, type="const")$selection[3]
## con solo constante y controlando por estacionalidad
p2 <- VARselect(data_train, type="const",season=12)$selection[3]
## con tendencia deterministica
p3 <- VARselect(data_train, type="trend")$selection[3]
## con estacionalidad
p4 <- VARselect(data_train, type="trend",season=12)$selection[3]
## con ambas
p5 <- VARselect(data_train, type="both")$selection[3]
## con estacionalidad
p6 <- VARselect(data_train, type="both",season=12)$selection[3]

#se estima cada uno de los modelos y se encuentra el mae fuera de muestra
modelos1 <- lapply(1:6,function(x){
  
  #se identifica cuantos lags tiene este modelo
  lagsVar <- get(paste0('p',x))
  #si es un numero par es porque tiene estacionalidad
  #si es 1 o 2 es const, 3 o 4 es trend, 5 o 6 es ambas
  if(x==1|x==2){
    if(x==1){
      modelo <- VAR(data_train, p=lagsVar, type="const")
    }else{
      modelo <- VAR(data_train, p=lagsVar, season=12,type="const")
    }
  }else if(x==3|x==4){
    if(x==3){
      modelo <- VAR(data_train, p=lagsVar, type="trend")
    }else{
      modelo <- VAR(data_train, p=lagsVar, season=12,type="trend")
    }
  }else{
    if(x==5){
      modelo <- VAR(data_train, p=lagsVar, type="both")
    }else{
      modelo <- VAR(data_train, p=lagsVar,season=12, type="both")
    }
  }
  #se extrae e pronostico y los intervalos de confianza del modelo para cada variable para el 2018
  pronWTI <- cbind(predict(modelo)$fcst$WTI[,1],
                             predict(modelo)$fcst$WTI[,2],
                             predict(modelo)$fcst$WTI[,3])[1:datosPron,]
  pronBrent<- cbind(predict(modelo)$fcst$Brent[,1],
                        predict(modelo)$fcst$Brent[,2],
                        predict(modelo)$fcst$Brent[,3])[1:datosPron,]
  #se guardan en un array
  arrayPronostico <- abind(pronWTI,pronBrent,along=3)
  colnames(arrayPronostico) <- c('media','lim_inf','lim_sup')
  dimnames(arrayPronostico)[[3]] <- c('WTI','Brent')
  #se calcula el mae de cada variable
  maeWTI <- mean(abs(arrayPronostico[,1,1]-data_test[,1]))
  maeBrent<- mean(abs(arrayPronostico[,1,2]-data_test[,2]))
  mae <- c(maeWTI,maeBrent)
  names(mae) <- c('WTI','Brent')
  #se retorna una lista con los 3 objetos
  return(list(modelo=modelo,pronostico=arrayPronostico,mae=mae))
  
})
names(modelos1) <- c('Constante','Constante-Estacionalidad','Tendencia','Tendencia-Estacionalidad',
                     'Ambos','Ambos-Estacionalidad')
#revisar mae
maes1 <- sapply(modelos1,function(x) x[['mae']])
#el que reduce el mae de WTI es el que tiene tendencia
which.min(maes1[1,])
#el que reduce el mae de Brent es el que tiene tendencia y constante
which.min(maes1[2,])
#se verifica cual tiene un promedio de los dos maes mas bajo
which.min(apply(maes1[,c(which.min(maes1[1,]),which.min(maes1[2,]))],2,mean))
#tomamos el mejor modelo despues de estas observaciones
modeloVar <- modelos1[[which.min(maes1[1,])]]
modeloVar <- modelos1[[6]]


#se hace el modelo vecm
modeloVecm <- VECM(data_train_niveles, lag=get(paste0("p",which.min(maes1[1,]))), r=1, estim=c("2OLS"), include=c("trend"),LRinclude=c("const"))
maeWTIVecm <- mean(abs(predict(modeloVecm,n.ahead = datosPron)[,1]-data_test_niveles[,1]))
maeBrentVecm <- mean(abs(predict(modeloVecm,n.ahead = datosPron)[,2]-data_test_niveles[,2]))
#comparando resultados con el var
c(maeWTIVecm,maeBrentVecm)
modeloVar[['mae']]

#son cointegradas?
#son integradas del mismo orden (1), pero hace falta revisar si son cointegradas, test de engels y granger
#al parecer hay una relacion de cointegracion pero solo en cambios
adf.test(lm(data_train[,1]~data_train[,2])$residuals)
adf.test(lm(data_train_niveles[,1]~data_train_niveles[,2])$residuals)

#y si se desestacionaliza antes de hacer el vecm?
data_train1 <- ts(cbind(tslm(ts(data_train_niveles[,1],frequency = 12)~season)$residual,tslm(ts(data_train_niveles[,2],frequency = 12)~season)$residual))
colnames(data_train1) <- colnames(data_train)
#se hace el modelo vecm
modeloVecm <- VECM(data_train1, lag=get(paste0("p",which.min(maes1[1,]))), r=1, estim=c("ML"), include=c("trend"),LRinclude=c("const"))
#se predicen las estaciones
estacionesWTI<- forecast(tslm(ts(data_train_niveles[,1],frequency = 12)~season),h=datosPron)
estacionesBrent <- forecast(tslm(ts(data_train_niveles[,2],frequency = 12)~season),h=datosPron)
#se predice lo restante
estocasticoWTI <- predict(modeloVecm, n.ahead=datosPron)[,1]
estocasticoBrent <- predict(modeloVecm, n.ahead=datosPron)[,2]
#se hace el pronostico final
pronosticoVecmWTI <- as.vector(estacionesWTI$mean) + estocasticoWTI
pronosticoVecmBrent<- as.vector(estacionesBrent$mean) + estocasticoBrent
maeWTI1 <- mean(abs(pronosticoVecmWTI-data_test_niveles[,1]))
maeBrent1 <- mean(abs(pronosticoVecmBrent-data_test_niveles[,2]))
#comparando resultados con el var
c(maeWTI1,maeBrent1)
modeloVar[['mae']]


#impulso respuesta
lagsVar <- get(paste0('p',which.min(maes1[1,])))
plot(irf(modeloVar[["modelo"]],impulse="WTI", n.ahead=12))
plot(irf(modeloVar[["modelo"]],impulse="Brent", n.ahead=12))


