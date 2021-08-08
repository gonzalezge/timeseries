####### ----------- Limpiar ambiente ------- ###### 
rm(list = ls())
##### --------- Path: automatico  --------- #####
path <<- gsub(rstudioapi::getActiveDocumentContext()$path,pattern = "/Ejercicio 1.+",replacement = "")
setwd(paste0(path,'/Ejercicio 1'))
##### -------- librerias ------- ###### 
librerias <- c("readxl","ggplot2","scales",'timeSeries','timeSeries','forecast','rugarch','fBasics','moments')

###### ----- Instalacion liberarias ------ ####
if(length(setdiff(librerias, rownames(installed.packages()))) > 0){
  install.packages(setdiff(librerias, rownames(installed.packages())))}
invisible(sapply(librerias, require, character.only = TRUE,quietly = TRUE))



### ------ Ejercicio 1: Gráficar distribuciones sacar percentiles, media, varianza, Asimetria y kurtosis.
#### --------- Normal --------- ####### 
####### ---  Miles/(US) gallon ----- ########
ggplot(data = mtcars, aes(x = mpg)) +
  geom_histogram(aes(y = ..density.., fill = ..count..)) +
  scale_fill_gradient(low = "#DCDCDC", high = "#7C7C7C") +
  stat_function(fun = dnorm, colour = "blue",
                args = list(mean = mean(mtcars$mpg),
                            sd = sd(mtcars$mpg))) +
  ggtitle("Histograma + curva normal teórica") +
  theme_bw()


#### ------- Simulacion de datos normal --------- #######
Distribucion_normal = rnorm(mean = 20,n = 1000,sd = 6)
#### ------- Simulacion de datos de una uniforme ------- ########
Distribucion_uniforme = runif(min = 10.4,max = 33.9,n = 1000)
 
#### ---------- Tipo de Gráfico: Distribucion --------- ########
## frecuencia de los valores representados por medio de barras  
## ------ Histograma Distribucion normal ----- ##### 
hist(Distribucion_normal)

## ------ Histograma Distribucion uniforme ----- ##### 
hist(Distribucion_uniforme)

######## ------ Tipo de Gráfico: percentiles plot ----------- ######
# Grafico comparartivo de la distribución observada con los 
# percentiles teóricos de una distribución normal con la misma media y desviación estándar que los datos. 
# Cuanto más se aproximen los datos a una normal, más alineados están los puntos entorno a la recta. 

### --------- Carros -------- #####
qqnorm(mtcars$mpg, pch = 19, col = "gray50")
qqline(mtcars$mpg)

### --------- Simulada normal -------- ######
qqnorm(Distribucion_normal, pch = 19, col = "gray50")
qqline(Distribucion_normal)

### --------- Simulada uniforme -------- ######
qqnorm(Distribucion_uniforme, pch = 19, col = "gray50")
qqline(Distribucion_uniforme)


#####------ Media ----- ####### 
### --------- Carros -------- #####
mean(mtcars$mpg)
### --------- Distribucion_normal -------- #####
mean(Distribucion_normal)
### --------- Simulada uniforme -------- ######
mean(Distribucion_uniforme)

#####------ Varianza ----- ####### 
######## --------- Construir una funcion ------- ###### 
varianza <- function (x){
  Numerador = sum((x-mean(x))^2) 
  Denominador = (length(x)-1) 
  Resultado= Numerador/Denominador
  return(Resultado)
}

#####------ Varianza ----- ####### 
### --------- Carros -------- #####
varianza(mtcars$mpg)
### --------- Distribucion_normal -------- #####
varianza(Distribucion_normal)
### --------- Simulada uniforme -------- ######
varianza(Distribucion_uniforme)

### --------- Comprobacion con funcion de R -------- #####
### --------- Carros -------- #####
sd(mtcars$mpg)^2 
### --------- Distribucion_normal -------- #####
sd(Distribucion_normal)^2
### --------- Simulada uniforme -------- ######
sd(Distribucion_uniforme)^2

### --------- Desviacion -------- #####
### --------- Carros -------- #####
sd(mtcars$mpg)
### --------- Distribucion_normal -------- #####
sd(Distribucion_normal)
### --------- Simulada uniforme -------- ######
sd(Distribucion_uniforme)

#### ------ Percentiles ------- ###### 
### --------- Carros -------- #####
quantile(mtcars$mpg)
### --------- Distribucion_normal -------- #####
quantile(Distribucion_normal)
### --------- Simulada uniforme -------- ######
quantile(Distribucion_uniforme)

########## --------- Estadisticas en un solo comando --------- ##########  
### --------- Carros -------- #####
summary(mtcars$mpg)
### --------- Distribucion_normal -------- #####
summary(Distribucion_normal)
### --------- Simulada uniforme -------- ######
summary(Distribucion_uniforme)

####### ------------------- Asimetria -------------- #########
# Permite identificar y describir la manera como los datos tiende
# a reunirse de acuerdo con la frecuencia con que se hallen dentro de la distribución. 
# Permite identificar las características de la distribución de datos sin necesidad de generar el gráfico.
# Valores cercanos a cero implica que la distribución es simétrica. 
### --------- Carros -------- #####
skewness(mtcars$mpg)
### --------- Distribucion_normal -------- #####
skewness(Distribucion_normal)
### --------- Simulada uniforme -------- ######
skewness(Distribucion_uniforme)


####### ------------------- Curtosis - Apuntamiento -------------- #########
# Mayor curtosis implica una mayor concentración de valores de la variable 
# muy cerca de la media de la distribución (pico) y muy lejos de la misma (colas), 
### --------- Carros -------- #####
kurtosis(mtcars$mpg)
### --------- Distribucion_normal -------- #####
kurtosis(Distribucion_normal)
### --------- Simulada uniforme -------- ######
kurtosis(Distribucion_uniforme)
