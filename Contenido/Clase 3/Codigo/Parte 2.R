################## Analisis de series de tiempo financieras ##################
# Autor: Germán González 
# Clase 3: Parte 2: Descomposición series de tiempo

####### ----------- Limpiar ambiente ------- ###### 
rm(list = ls())
##### -------- librerias ------- ###### 
librerias <- c("readxl","forecast","xts","rugarch","timeSeries","ggplot2","astsa","scales","lubridate","car","reshape2","quantmod","xtable","tseries")

###### ----- Carga de librerias ------ ####
if(length(setdiff(librerias, rownames(installed.packages()))) > 0){
  install.packages(setdiff(librerias, rownames(installed.packages())))}
invisible(sapply(librerias, require, character.only = TRUE,quietly = TRUE))

path <- gsub('Parte 2.R','',rstudioapi::getActiveDocumentContext()$path)
setwd(path)

AirPassengers = readRDS(file='./Pasajeros.Rds')
AirPassengers = log(AirPassengers)

# Test de Dickey - Fuller
# H0: hipótesis nula es que la serie tiene raíces unitarias, por tanto, no es estacionaria.
# Ha es que la serie es estacionaria 
# Con un p-valor inferior a 0.05 se rechaza la hipotesis nula se suele rechazar.

set.seed(1)
adf.test(rnorm(1000))
adf.test(AirPassengers)

#### ------ Segun esto es estacionaria ----- ### 
modelo3 = tslm(AirPassengers ~ season+trend)
summary(modelo3)
########################### -------------------- Clase  ------------------------ ###############
### Estime el componente cíclico (si es relevante). Justifique con estadísticas y criterios de información el modelo estimado. 


Construccion_dummys = function(Datos,Year, Month, Grado = 2){
  bi_estac = seasonaldummy(Datos)
  bi_estac = cbind(bi_estac, Dec=apply(bi_estac, MARGIN = 1, function(x){1-sum(x)}))
  if (Grado == 1) {
    tendencia<-cbind(t=1:length(Datos))
  }  else if (Grado == 2) {
    tendencia<-cbind(t=1:length(Datos), t2=(1:length(Datos))^2)
  } else if (Grado == 4) {
    tendencia<-cbind(t=1:length(Datos), t2=(1:length(Datos))^2,t3=(1:length(Datos))^3,t4=(1:length(Datos))^4)
  }
  tend_esta <-ts(cbind(bi_estac,tendencia), start=c(Year, Month), frequency = 12)
  return(tend_esta)
}

####### ----------- Descomposición ------- #########
decompose(AirPassengers)
plot(decompose(AirPassengers))
### ---------------------- Lineal --------------------------- ### 
### --- A pesar de quitar la tendencia y la estacionalidad, la serie sigue siendo estacionaria --- ### 
tsdisplay(modelo3$residuals)
### Prueba de ruido blanco 
Box.test(rnorm(1000))
Box.test(modelo3$residuals)

### --- Es un ARIMA(1,1,1) --- ###
TE_1t = Construccion_dummys(Datos = AirPassengers,Year = 1949,Month = 1,Grado = 1)
#### -------- Se quita la primera para controlar ----- #### 
TE_1t = TE_1t[,-1]
### --- Sin embargo, el autoarima no tiene en cuenta la tendencia y la estacionalidad y difer(encia --- ##
### --- Al incluirle la tendencia y la estacionalidad no existe convergencia, se escoje ARIMA(1,0,1)

#### ------ Evaluar ahora los valores de AR() ---- #####3
Mejor_Modelo_0 = Arima(AirPassengers, order = c(1,0,0), xreg = TE_1t)
Tabla = NULL 
for(i in 0:5) {
  Modelo_p2_0 = Arima(AirPassengers, order = c(i,0,0), xreg = TE_1t, include.mean = F)
  Tabla_t = data.frame(Orden = c(paste0("c(",i,",0,0)")),AIC = c(AIC(Modelo_p2_0)), BIC =c(BIC(Modelo_p2_0))) 
  Tabla = rbind(Tabla,Tabla_t)
}
Tabla
Mejor_modelo = Tabla[which.min(Tabla$AIC),]

#### ------ Evaluar ahora los valores de MA() ---- #####3
Tabla = NULL 
for(i in 0:5) {
  Modelo_p2_0 = Arima(AirPassengers, order = c(0,0,i), xreg = TE_1t, include.mean = F)
  Tabla_t = data.frame(Orden = c(paste0("c(0",",0,",i,")")),AIC = c(AIC(Modelo_p2_0)), BIC =c(BIC(Modelo_p2_0))) 
  Tabla = rbind(Tabla,Tabla_t)
}
Tabla
Mejor_modelo = Tabla[which.min(Tabla$AIC),]

##### ------ Se fija el mejor de los dos ---- ###
### --- Despues del P = 2, los criterios aumentan --- ### 
Tabla = NULL 
for(i in 0:5) {
  Modelo_p2_0 = Arima(AirPassengers, order = c(1,0,i), xreg = TE_1t, include.mean = F)
  Tabla_t = data.frame(Orden = c(paste0("c(1,0,",i,")")),AIC = c(AIC(Modelo_p2_0)), BIC =c(BIC(Modelo_p2_0))) 
  Tabla = rbind(Tabla,Tabla_t)
}
print(Tabla)

Mejor_modelo = Tabla[which.min(Tabla$AIC),]
### --- Mejor modelo con tendencia lineal --- ### 
Mejor_Modelo_0 = Arima(AirPassengers, order = c(1,0,1),xreg = TE_1t)

### Prueba de ruido blanco 
Box.test(rnorm(1000))
Box.test(Mejor_Modelo_0$residuals)

### --- Pruebas de residuales: Mejor_Modelo_0 ---- ###
### ---- Modelo y Residuales  ---- ###  
par(mfrow=c(2,1), mar=c(3,4.5,1,1))
plot(Mejor_Modelo_0$fitted, ylab="Observado vs ajustado")
lines(Mejor_Modelo_0$x, col="tomato")
legend("bottom",c("Observado", "Ajustado"), lwd = c(2,2), col = c("black","tomato"))
#### ------ Objetivo: Ver que los residuales sean  Ruido Blanco  señal aleatoria (proceso estocástico)   ----- ######
#### ------ Ruido blanco es: Su media es 0: 
#### ------ Ruido blanco es: La varianza es constante
#### ------ Ruido blanco es: no está correlacionada: correlación de t-1, t ------- #####

plot(Mejor_Modelo_0$residuals, ylab="Residuales")

# Test de Ljung_Box
set.seed(1)
plot(rnorm(1000), type='l')
Box.test(rnorm(1000),type="Ljung-Box")
Box.test(rnorm(1000),type="Box-Pierce")
### ---- Modelo y Residuales  ---- ###  
Box.test(Mejor_Modelo_0$residuals)


### --------------- Revisión de los errores: ------------ ### 
Tabla_box_test = data.frame(Modelo = "Lineal", Ljung_Box = Box.test(Mejor_Modelo_0$residuals,type="Ljung-Box")$p.value,Box_Pierce = Box.test(Mejor_Modelo_0$residuals,type="Box-Pierce")$p.value)

############# --------------------- Punto 9 ---------------------  #############
### Documente pruebas de normalidad, u otras relevantes para justificar los supuestos de su modelo ### 


jarque.bera.test(rnorm(1000))
# H0: Los datos vienen de una distribución normal. 
# Ha: Los datos no vienen de una distribución normal. 
# Con un p-valor superior a 0.05 no se rechaza la hipotesis nula 

Tabla_resumen_2<-function(x, nombre="variable x"){
  data.frame(Modelo=nombre,
             Media=mean(x, na.rm = T),
             Desviacion=sd(x, na.rm = T),
             Curtosis=kurtosis(x, na.rm = T),
             Skewness=skewness(x, na.rm = T),
             JV_pvalor=jarque.bera.test(x)$p.value,
             L_B=Box.test(x, type = "Ljung-Box")$p.value, row.names = NULL)
}

Tabla_resumen_2(Mejor_Modelo_0$residuals,nombre = "Lineal")

############# ---------------------  Pronostico dentro de la muestra  ---------------------  #############
### ------ Mejor Modelo 1 : Prónostico en la muestra --- ### 

### ------ Lineal : Prónostico en la muestra --- ### 
Mejor_Modelo_0_den<-Arima(window(AirPassengers, end=c(1960,1)), order = c(2,0,0),xreg = window(TE_1t, end=c(1960,1)),include.mean = F)
pron_insample<-forecast(Mejor_Modelo_0_den, h=12, xreg =  window(TE_1t, start=c(1960, 1)))
plot(pron_insample, showgap = F, main="Lineal", ylab="Log valor Colcap",cex.lab=2.5, cex.axis=2.5, cex.main=2.5, cex.sub=2.5)
dev.off()


############## -----------   Pronóstico fuera de la muestra: Lineal ---------------- #################
Horizonte=12
Month =1 
year = 1961
###### -------- construccion de tendencia ------ ###########
tendencia_fuera = ts(cbind(t=(length(AirPassengers)+1):(length(AirPassengers)+Horizonte)),start=c(year,Month), frequency = 12) 
TE_1f<-seasonaldummy(tendencia_fuera)
TE_1f<-cbind(TE_1f, Dec=apply(TE_1f, MARGIN = 1, function(x){1-sum(x)}))
TE_1f<-ts(cbind(TE_1f,tendencia_fuera), start=c(year,Month), frequency = 12)
TE_1f=TE_1f[,-1]
colnames(TE_1f)<-colnames(TE_1t)

Mejor_Modelo_1 = Arima(AirPassengers, order = c(2,0,0), include.mean = F,xreg = TE_1t)
Pronostico_log_final = forecast(Mejor_Modelo_1, h=horizonte, xreg =  TE_1f)
plot(Pronostico_log_final, showgap = F,main="", ylab="Logaritmo (Millones de pesos)",cex.lab=2.5, cex.axis=2.5, cex.main=2.5, cex.sub=2.5)



######################## -------------- simulación de densidad ------- #######################
## Densidad del pronóstico del tercer periodo asumiendo un comportamiento normal
Media = Pronostico_log_final$mean[1]
set.seed(2018)
###### ---- se extrae la desviación ---- ###
innovaciones<-rnorm(10000, 0,sqrt(Mejor_Modelo_0$sigma2))
sim_h1<-Media+innovaciones
IC<-quantile(sim_h1, c(0.025, 0.975))
plot(density(sim_h1), yaxs="i", bty="l", main="",ylab="",cex.lab=2.5, cex.axis=2.5, cex.main=2.5, cex.sub=2.5)
abline(v=IC, col="red", lwd=2, lty=2)
abline(v=7.314743, col="darkblue", lwd=2, lty=1)
dev.off()



