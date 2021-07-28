############# ---------- Clase 2 ------------ #######
####### ----------- Limpiar ambiente ------- ###### 
rm(list = ls())
##### -------- librerias ------- ###### 
librerias <- c("readxl","ggplot2","scales","timeSeries","forecast","fOptions","mFilter","lubridate","quantmod","car")

###### ----- Carga de librerias ------ ####
if(length(setdiff(librerias, rownames(installed.packages()))) > 0){
  install.packages(setdiff(librerias, rownames(installed.packages())))}
invisible(sapply(librerias, require, character.only = TRUE,quietly = TRUE))


### ---------------------------- COLCAP ----------------------------- ### 
Colcap =  as.data.frame(read_excel("Colcap.xlsx",col_types = c("date","numeric","numeric","numeric")))
head(Colcap)
### --- Organizar los datos --- ###
Colcap = Colcap[,c(1:2)]
### ----- Renombrar columnas  ------ ##### 
colnames(Colcap) = c("Fecha","Colcap")
### ----- Asegurarse que la variable de Colcap sea númerica --- #### 
Colcap[,2] = as.numeric(Colcap[,2])
### -----  Convertir en fecha --- #### 
Colcap[,1] = as.Date(Colcap[,1])
################### ------------ Convertir en una serie de tiempo -------------------- ##################
Colcap_ts = xts(Colcap$Colcap,order.by = Colcap$Fecha)
plot(Colcap_ts)

########## ------------------ temporalidades ------------------ ###########
################### ------------ Precios del COLCAP: Semanales -------------------- ##################
Colcap_week = apply.weekly(Colcap_ts,function(x) mean(x))
plot(Colcap_week)

################### ------------ Precios del COLCAP: Mensuales -------------------- ##################
Colcap_month = apply.monthly(Colcap_ts,function(x) mean(x))
plot(Colcap_month)

################### ------------ Precios del COLCAP: Trimestrales -------------------- ###############
Colcap_quarter = apply.quarterly(Colcap_ts,function(x) mean(x))
plot(Colcap_quarter)

############# --------------------- Parte 3: Decidir temporalidad ---------------------  #############
### --- Para decidir la Granuralidad del tiempo a tratar se gráfican todas las temporalidades --- ###

### --- Se puede ver que debido a la alta volatilidad del COLCAP, la frecuencia mensual es un buen ###
### --- Candidato debido a que este captura indirectamente las volatilidades semanales y diarias ---### 
### ---  Pero no ignora, ni altera los posibles componentes de la serie de Tiempo anual  ---- ### 
Grafico_Colcap = ggplot(Colcap) + geom_line(aes(y = Colcap, x = Fecha), size = 1,color="#0040a4") + theme_bw()  


### --- Conceptualmente se puede pensar que el COLCAP es heterocedastico en T --- ### 
Colcap_month_log = log(Colcap_month)
################### ------------ Componentes del COLCAP -------------------- ###############
Colcap_month_log = ts(as.data.frame(Colcap_month)[,1],start = c(year(as.Date(rownames(as.data.frame(Colcap_month))[1])),month(as.Date(rownames(as.data.frame(Colcap_month))[1]))),frequency = 12)

########### ------------------------ Parte 2: Filtro de Hodrick–Prescott ------------------------- ###########
### ------- Prueba con un lambda 0 ------ #####
Filtro_0 = hpfilter(Colcap_month_log,type = "lambda", freq = 0)
plot(Filtro_0)

### ------- Prueba con un lambda 14400 ------ #####
Filtro_14400=hpfilter(Colcap_month_log,type = "lambda", freq = 14400)
plot(Filtro_14400)


########## ------------------- Estimar tendencia: Regresion --------- ################3 
### ------------ Objetivo: Evaluar prueba de hipotesis  ------------ ### 
### ------------ Modelos: Tendencia --------------- ### 
modelo1 = tslm(Colcap_month_log ~ trend)
modelo2 = tslm(Colcap_month_log ~ trend+I(trend^2))
modelo3 = tslm(Colcap_month_log ~ trend+I(trend^2)+I(trend^3))
modelo4 = tslm(Colcap_month_log ~ trend+I(trend^2)+I(trend^3)+I(trend^4))
modelo5 = tslm(Colcap_month_log ~ I(log(trend)))
modelo6 = tslm(Colcap_month_log ~ I(exp(trend)))
### --- Estadisticas descriptivas --- ### 
summary(modelo1)
summary(modelo2)
summary(modelo3)
summary(modelo4)
summary(modelo5)
summary(modelo6)

### ------------ Gráfico Serie Ajustada por Tendencia --------------- ### 
par(mfrow=c(2,3), mar=c(3, 4, 2, 2) )
### ------------ Lineal --------------- ### 
plot(Colcap_month_log, lwd=1, xlab="", main="Lineal", ylab="Log Valor Colcap",cex.lab=1, cex.axis=1, cex.main=1, cex.sub=1)
lines(modelo1$fitted.values, col="#0040a4", lwd=2, lty=2)
### ------------ P2  --------------- ### 
plot(Colcap_month_log, lwd=1, xlab="", main="Polinomio (Grado 2)",ylab="Log Valor Colcap",cex.lab=1, cex.axis=1, cex.main=1, cex.sub=1) 
lines(modelo2$fitted.values, col="#0040a4", lwd=2, lty=1)
### ------------ P3 --------------- ### 
plot(Colcap_month_log, lwd=1, xlab="", main="Polinomio (Grado 3)",ylab="Log Valor Colcap",cex.lab=1, cex.axis=1, cex.main=1, cex.sub=1) 
lines(modelo3$fitted.values, col="#0040a4", lwd=2, lty=1)
### ------------ P4 --------------- ### 
plot(Colcap_month_log, lwd=1, xlab="", main="Polinomio (Grado 4)",ylab="Log Valor Colcap",cex.lab=1, cex.axis=1, cex.main=1, cex.sub=1) 
lines(modelo4$fitted.values, col="#0040a4", lwd=2, lty=1)
### ------------ log --------------- ### 
plot(Colcap_month_log, lwd=1, xlab="", main="Logarítmico",ylab="Log Valor Colcap",cex.lab=1, cex.axis=1, cex.main=1, cex.sub=1) 
lines(modelo5$fitted.values, col="#0040a4", lwd=2, lty=1)
### ------------ Exp --------------- ### 
plot(Colcap_month_log, lwd=1, xlab="", main="Exponencial",ylab="Log Valor Colcap",cex.lab=1, cex.axis=1, cex.main=1, cex.sub=1) 
lines(modelo6$fitted.values, col="#0040a4", lwd=2, lty=1)
#### --- limpiar grafico ---- #
dev.off()

### ------------ Gráfico Residuales Ajustados por Tendencia -------------- ### 
par(mfrow=c(2,3), mar=c(3, 4, 2, 2) )
plot(modelo1$residuals, lwd=2, xlab="", main="Lineal",ylab="Valor Colcap")
abline(h=0, col="#0040a4", lty=2)
plot(modelo2$residuals, lwd=2, xlab="", main="Polinomio (Grado 2)",ylab="Valor Colcap")
abline(h=0, col="#0040a4", lty=2)
plot(modelo3$residuals, lwd=2, xlab="", main="Polinomio (Grado 3)",ylab="Valor Colcap")
abline(h=0, col="#0040a4", lty=2)
plot(modelo4$residuals, lwd=2, xlab="", main="Polinomio (Grado 4)",ylab="Valor Colcap")
abline(h=0, col="#0040a4", lty=2)
plot(modelo5$residuals, lwd=2, xlab="", main="Logarítmico",ylab="Valor Colcap")
abline(h=0, col="#0040a4", lty=2)
plot(modelo6$residuals, lwd=2, xlab="", main="Exponencial",ylab="Valor Colcap")
abline(h=0, col="#0040a4", lty=2)

##### ------ Evaluacion de tendencia con criterios de informacion ------- #####
Modelo = modelo1
AIC(Modelo)
BIC=BIC(Modelo)
R2_ajustado=summary(Modelo)$adj.r.squared
Box_Pierce=Box.test(Modelo$residuals, type = "Box-Pierce")$p.value
Ljung_Box=Box.test(Modelo$residuals, type = "Ljung-Box")$p.value
#### =-- Pvalor: Prueba F -- ###
round(pf(q=summary(Modelo)$fstatistic[1], df1=summary(Modelo)$fstatistic[2], df2=summary(Modelo)$fstatistic[3], lower.tail=FALSE),4)


#### ------ Generalizar en una función ---- #### 
Tabla_resumen<-function(Modelo,Titulo){
  data.frame(Modelo=Titulo,
             AIC=AIC(Modelo),
             BIC=BIC(Modelo),
             R2_ajustado=summary(Modelo)$adj.r.squared,
             Box_Pierce=Box.test(Modelo$residuals, type = "Box-Pierce")$p.value,
             Ljung_Box=Box.test(Modelo$residuals, type = "Ljung-Box")$p.value, row.names = NULL,
             P_valor_f = round(pf(q=summary(Modelo)$fstatistic[1], df1=summary(Modelo)$fstatistic[2], df2=summary(Modelo)$fstatistic[3], lower.tail=FALSE),4)
  )
  
}

### --- Criterios de información sin Tendencia --- ### 
Tabla_tendencia = rbind(Tabla_resumen(modelo1,Titulo = "Lineal"),
               Tabla_resumen(modelo2,Titulo = "Polinomio (Grado 2)"),
               Tabla_resumen(modelo3,Titulo = "Polinomio (Grado 3)"),
               Tabla_resumen(modelo4,Titulo = "Polinomio (Grado 4)"),
               Tabla_resumen(modelo5,Titulo = "Logaritmico"),
               Tabla_resumen(modelo6,Titulo = "Exponencial"))




### ------------ Modelos: Season --------------- ### 
modelo1_season = tslm(Colcap_month_log ~ trend+season)
modelo2_season = tslm(Colcap_month_log ~ trend+I(trend^2)+season)
modelo3_season = tslm(Colcap_month_log ~ trend+I(trend^2)+I(trend^3)+season)
modelo4_season = tslm(Colcap_month_log ~ trend+I(trend^2)+I(trend^3)+I(trend^4)+season)
modelo5_season = tslm(Colcap_month_log ~ I(log(trend))+season)
modelo6_season = tslm(Colcap_month_log ~ I(exp(trend))+season)

### --- Estadisticas descriptivas --- ### 
summary(modelo1_season)
summary(modelo2_season)
summary(modelo3_season)
summary(modelo4_season)
summary(modelo5_season)
summary(modelo6_season)

### ------------ Gráfico Serie Ajustada por Tendencia --------------- ### 
par(mfrow=c(2,3), mar=c(3, 4, 2, 2) )
### ------------ Lineal --------------- ### 
plot(Colcap_month_log, lwd=1, xlab="", main="Lineal", ylab="Log Valor Colcap",cex.lab=1, cex.axis=1, cex.main=1, cex.sub=1)
lines(modelo1_season$fitted.values, col="#0040a4", lwd=2, lty=2)
### ------------ P2  --------------- ### 
plot(Colcap_month_log, lwd=1, xlab="", main="Polinomio (Grado 2)",ylab="Log Valor Colcap",cex.lab=1, cex.axis=1, cex.main=1, cex.sub=1) 
lines(modelo2_season$fitted.values, col="#0040a4", lwd=2, lty=1)
### ------------ P3 --------------- ### 
plot(Colcap_month_log, lwd=1, xlab="", main="Polinomio (Grado 3)",ylab="Log Valor Colcap",cex.lab=1, cex.axis=1, cex.main=1, cex.sub=1) 
lines(modelo3_season$fitted.values, col="#0040a4", lwd=2, lty=1)
### ------------ P4 --------------- ### 
plot(Colcap_month_log, lwd=1, xlab="", main="Polinomio (Grado 4)",ylab="Log Valor Colcap",cex.lab=1, cex.axis=1, cex.main=1, cex.sub=1) 
lines(modelo4_season$fitted.values, col="#0040a4", lwd=2, lty=1)
### ------------ log --------------- ### 
plot(Colcap_month_log, lwd=1, xlab="", main="Logarítmico",ylab="Log Valor Colcap",cex.lab=1, cex.axis=1, cex.main=1, cex.sub=1) 
lines(modelo5_season$fitted.values, col="#0040a4", lwd=2, lty=1)
### ------------ Exp --------------- ### 
plot(Colcap_month_log, lwd=1, xlab="", main="Exponencial",ylab="Log Valor Colcap",cex.lab=1, cex.axis=1, cex.main=1, cex.sub=1) 
lines(modelo6_season$fitted.values, col="#0040a4", lwd=2, lty=1)
#### --- limpiar grafico ---- #
dev.off()

### ------------ Gráfico Residuales Ajustados por Tendencia -------------- ### 
par(mfrow=c(2,3), mar=c(3, 4, 2, 2) )
plot(modelo1_season$residuals, lwd=2, xlab="", main="Lineal",ylab="Valor Colcap")
abline(h=0, col="#0040a4", lty=2)
plot(modelo2_season$residuals, lwd=2, xlab="", main="Polinomio (Grado 2)",ylab="Valor Colcap")
abline(h=0, col="#0040a4", lty=2)
plot(modelo3_season$residuals, lwd=2, xlab="", main="Polinomio (Grado 3)",ylab="Valor Colcap")
abline(h=0, col="#0040a4", lty=2)
plot(modelo4_season$residuals, lwd=2, xlab="", main="Polinomio (Grado 4)",ylab="Valor Colcap")
abline(h=0, col="#0040a4", lty=2)
plot(modelo5_season$residuals, lwd=2, xlab="", main="Logarítmico",ylab="Valor Colcap")
abline(h=0, col="#0040a4", lty=2)
plot(modelo6_season$residuals, lwd=2, xlab="", main="Exponencial",ylab="Valor Colcap")
abline(h=0, col="#0040a4", lty=2)

### --- Criterios de información sin esta --- ### 
Tabla_tendencia = rbind(Tabla_resumen(modelo1_season,Titulo = "Lineal"),
                        Tabla_resumen(modelo2_season,Titulo = "Polinomio (Grado 2)"),
                        Tabla_resumen(modelo3_season,Titulo = "Polinomio (Grado 3)"),
                        Tabla_resumen(modelo4_season,Titulo = "Polinomio (Grado 4)"),
                        Tabla_resumen(modelo5_season,Titulo = "Logaritmico"),
                        Tabla_resumen(modelo6_season,Titulo = "Exponencial"))



##### ------ Ajustados ----- #####
plot(Colcap_month_log, lwd=2, xlab="", main="Polinomio (Grado 2)", ylab="Valor Colcap",cex.lab=2.5, cex.axis=2.5, cex.main=2.5, cex.sub=2.5) 
lines(modelo1_season$fitted.values, col="#0040a4", lwd=2, lty=1)





