################## Analisis de series de tiempo financieras ##################
# Autor: Germán González
# Clase 3: Parte 1: Autocorrelogramas

####### ----------- Limpiar ambiente ------- ###### 
rm(list = ls())
##### -------- librerias ------- ###### 
librerias <- c("tseries","lubridate","dplyr","forecast")

###### ----- Carga de librerias ------ ####
if(length(setdiff(librerias, rownames(installed.packages()))) > 0){
  install.packages(setdiff(librerias, rownames(installed.packages())))}
invisible(sapply(librerias, require, character.only = TRUE,quietly = TRUE))


path <- gsub('Parte 1.R','',rstudioapi::getActiveDocumentContext()$path)
setwd(path)
# Cargar archivos ---------------------------------------------------------
set.seed(1)
datos <- readRDS('datosTESUVR.Rds')
View(datos)

# Veamos el tipo de dato que tienen nuestras columnas
sapply(datos,class)

# Definimos como serie de tiempo
spread <- xts::xts(datos$Spread,order.by = datos$Fecha)
plot(spread)

set.seed(1)
##### ---- Normal ----- ####
plot(rnorm(1000), type='l')
plot(rnorm(spread), type='l')

adf.test(rnorm(1000))
adf.test(spread)

#correlogramas
acf(spread)
pacf(spread)

#diferencias 
dif1 <- diff(spread)

#quitar NA
dif1 <- dif1[!is.na(dif1)]

# Test de Dickey - Fuller
# H0: hipótesis nula es que la serie tiene raíces unitarias, por tanto, no es estacionaria.
# Ha es que la serie es estacionaria 
# Con un p-valor inferior a 0.05 se rechaza la hipotesis nula se suele rechazar.

adf.test(dif1)
#autocorrelogramas
acf(dif1)
pacf(dif1)






