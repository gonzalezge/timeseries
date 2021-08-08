###### --------------------- Ejercicio 1 ----------------- ############ 
####### ----------- Limpiar ambiente ------- ###### 
rm(list = ls())
options(scipen = 10000000)
##### --------- Path: automatico  --------- #####
path <<- gsub(rstudioapi::getActiveDocumentContext()$path,pattern = "/Ejercicio 2.+",replacement = "")
setwd(paste0(path,'/Ejercicio 2'))
##### -------- librerias ------- ###### 
librerias <- c("readxl","ggplot2","scales",'timeSeries','timeSeries','forecast','rugarch','fBasics','moments','tseries','fPortfolio','dplyr','zoo','plotly')

###### ----- Instalacion liberarias ------ ####
if(length(setdiff(librerias, rownames(installed.packages()))) > 0){
  install.packages(setdiff(librerias, rownames(installed.packages())))}
invisible(sapply(librerias, require, character.only = TRUE,quietly = TRUE))

###### ----------- Cargar datos --------- ####### 
##### ------- Historia Colcap ------ ########
Colcap = as.data.frame(read_excel('Datos.xlsx',sheet = 'Colcap'))
rownames(Colcap) = Colcap[,1]

### ------ Pesos colcap ---- #### 
Pesos_colcap = as.data.frame(read_excel('Datos.xlsx',sheet = 'Pesos'))
Pesos_colcap = t(Pesos_colcap)
colnames(Pesos_colcap) = as.character(Pesos_colcap[1,])
Pesos_colcap = Pesos_colcap[-1,]
Pesos_colcap = as.matrix(Pesos_colcap)

##### --------- Datos acciones ------ ##### 
Acciones = as.data.frame(read_excel('Datos.xlsx',sheet = 'Close'))
Acciones= na.locf(Acciones)
## ---- Organizar nombres --- ###
rownames(Acciones) = Acciones[,'Fecha']
Acciones = Acciones[,-1]
### ---- Separar la base en antes y despues del 2019-08-01
Antes_acciones = Acciones[rownames(Acciones) < '2018-11-01',]
Despues_acciones = Acciones[(rownames(Acciones) >= '2018-11-01') & (rownames(Acciones) < '2019-01-31'),]


## ----- Indice Colcap y Niveles acciones --- ###
Colcap_acciones = cbind(Colcap[rownames(Antes_acciones),"Colcap"],Antes_acciones)
colnames(Colcap_acciones)[1] = 'Colcap'
## --- Retornos  --- ### 
Retornos_colcap= apply(Colcap_acciones, 2, function(x){returns(x,method = 'simple')})[-1,]
### -------- Se resta los retornos del indice ficticio a los retornos de todas las acciones ----- ####
Retornos_colcap=Retornos_colcap-matrix(rep(Retornos_colcap[,1],ncol(Retornos_colcap)),ncol=ncol(Retornos_colcap),byrow = F)
Retornos_colcap=Retornos_colcap[,-1]
Retornos_colcap = Retornos_colcap[,-c(grep(colnames(Retornos_colcap),pattern = 'BCOLOMBIA'))]
Despues_acciones = Despues_acciones[,-c(grep(colnames(Despues_acciones),pattern = 'BCOLOMBIA'))]

### ------ Se quita Bancolombia para que la inversa de la matriz de covarianza este definida ------ #### 

###### ---------- Calcular frontera -------- ##########
###### ------- Especificacion del portafolio ------ ###### 
Especificacion_portafolio_colcap<-portfolioSpec()
###### ------- Puntos de la frontera ------ ###### 
setNFrontierPoints(Especificacion_portafolio_colcap) <- 1000
###### ------- Restricciones: Positivas ------ ###### 
Restricciones_colcap="LongOnly"
###### ------- Construccion de la frontera ------ ###### 
Frontera_colcap <- portfolioFrontier(data = as.timeSeries(Retornos_colcap),spec=Especificacion_portafolio_colcap,constraints=Restricciones_colcap)
### ---- Minima varianza --- ###
Pesos_minima_varianza_colcap <- efficientPortfolio(data = as.timeSeries(Retornos_colcap),spec=Especificacion_portafolio_colcap,constraints=Restricciones_colcap)

##### ------ Grafico ------- ######## 
frontierPlot(Frontera_colcap)
grid()
minvariancePoints(Frontera_colcap, col="blue", pch=19, cex=2)

##### ---- Pesos ----- ####
col <- qualiPalette(ncol(Retornos_colcap), "Dark2")
pdf("Pesos_colcap.pdf", width=26, height=12)
print(weightsPlot(Frontera_colcap, col=col,box=FALSE))
dev.off()

#### ------ Pesos de minima varianza ----- ####
min_var = which.min(Frontera_colcap@portfolio@portfolio$targetRisk)
Portafolio=Frontera_colcap@portfolio@portfolio$weights[min_var,]
plot_ly() %>% add_pie(Portafolio*100,labels=as.character(names(Portafolio))) %>% 
  layout(title = 'Pesos')

#### ------ Pesos Colcap ----- ####
Colcap_pesos=Pesos_colcap
plot_ly() %>% add_pie(as.numeric(Colcap_pesos)*100,labels=as.character(rownames(Pesos_colcap))) %>% 
  layout(title = 'Pesos')


#####------- Organizar pesos igual que el vector ----####
Pesos_colcap = Pesos_colcap[colnames(Acciones),]

### ---- Retornos ---- #### 
Retornos_antes = apply(Antes_acciones, 2, function(x){returns(x,method = 'simple')})[-1,]
### ----- Colcap ficticio --- ###
Resultados = c(1,cumprod(1+c(as.numeric(Pesos_colcap)%*%t(Retornos_antes))))
## ----- Indice ficticio y Niveles acciones --- ###
Resultados = cbind(Resultados,Antes_acciones)
## --- Retornos  --- ### 
Retornos_ficticios = apply(Resultados, 2, function(x){returns(x,method = 'simple')})[-1,]
### -------- Se resta los retornos del indice ficticio a los retornos de todas las acciones ----- ####
Retornos_ficticios = Retornos_ficticios-matrix(rep(Retornos_ficticios[,1],ncol(Retornos_ficticios)),ncol=ncol(Retornos_ficticios),byrow = F)
Retornos_ficticios = Retornos_ficticios[,-1]
Retornos_ficticios = Retornos_ficticios[,-c(grep(colnames(Retornos_ficticios),pattern = 'BCOLOMBIA'))]
### ------ Se quita Bancolombia para que la inversa de la matriz de covarianza este definida ------ #### 

###### ---------- Calcular frontera -------- ##########
###### ------- Especificacion del portafolio ------ ###### 
Especificacion_portafolio<-portfolioSpec()
###### ------- Puntos de la frontera ------ ###### 
setNFrontierPoints(Especificacion_portafolio) <- 100
###### ------- Restricciones: Positivas ------ ###### 
Restricciones="LongOnly"
###### ------- Construccion de la frontera ------ ###### 
Frontera <- portfolioFrontier(data = as.timeSeries(Retornos_ficticios),spec=Especificacion_portafolio,constraints=Restricciones)
### ---- Minima varianza --- ###
Pesos_minima_varianza <- efficientPortfolio(data = as.timeSeries(Retornos_ficticios),spec=Especificacion_portafolio,constraints=Restricciones)

##### ------ Grafico ------- ######## 
frontierPlot(Frontera)
grid()
minvariancePoints(Frontera, col="blue", pch=19, cex=2)

##### ---- Pesos ----- ####
col <- qualiPalette(ncol(Retornos_ficticios), "Dark2")
pdf("Pesos_ficticio.pdf", width=26, height=12)
print(weightsPlot(Frontera, col=col,box=FALSE))
dev.off()

#### ------ Pesos de minima varianza ----- ####
min_var = which.min(Frontera@portfolio@portfolio$targetRisk)
Portafolio=Frontera@portfolio@portfolio$weights[min_var,]
plot_ly() %>% add_pie(Portafolio*100,labels=as.character(names(Portafolio))) %>% 
                      layout(title = 'Pesos')

#### ------ Pesos Colcap ----- ####
Colcap_pesos=Pesos_colcap
plot_ly() %>% add_pie(as.numeric(Colcap_pesos)*100,labels=as.character(names(Pesos_colcap))) %>% 
  layout(title = 'Pesos')


######## ----- Tamano portafolio ---------- #####
Tamano_Portafolio = 100

###### ---------- Se plantea un portafolio con los pesos del Colcap ---------- ####### 
Pesos_min_varianza_colcap = Pesos_minima_varianza_colcap@portfolio@portfolio$weights
Portafolio_colcap = Tamano_Portafolio*Pesos_min_varianza_colcap
Portafolio_colcap = Portafolio_colcap/t(head(Despues_acciones[,c(names(Portafolio_colcap))],1))
Matriz_portafolio_colcap = matrix(rep(Portafolio_colcap[,1],nrow(Despues_acciones)),nrow =nrow(Despues_acciones),byrow = T)
#### ---------- Se saca el valor del portafolio -------- ##### 
Valor_portafolio_colcap = rowSums(as.matrix(Despues_acciones[,])*(Matriz_portafolio_colcap))
###### ---------- Se plantea un portafolio con los pesos del indice ficticio ---------- ####### 
Pesos_min_varianza_ficticio = Pesos_minima_varianza@portfolio@portfolio$weights
Portafolio_ficticio = Tamano_Portafolio*Pesos_min_varianza_ficticio
Portafolio_ficticio = Portafolio_ficticio/t(head(Despues_acciones[,c(names(Portafolio_ficticio))],1))
Matriz_portafolio_ficticio= matrix(rep(Portafolio_ficticio[,1],(nrow(Despues_acciones))),nrow =(nrow(Despues_acciones)),byrow = T)
#### ---------- Se saca el valor del portafolio -------- ##### 
Valor_portafolio_ficticio = rowSums(as.matrix(Despues_acciones[,])*(Matriz_portafolio_ficticio))
##### ----------- Ahora se comparan los tres índices -------- ######3 
Colcap_indice = Colcap[names(Valor_portafolio_ficticio),"Colcap"]
Colcap_indice = (Colcap_indice/Colcap_indice[1])*100

Resultados = cbind(Colcap_indice,as.numeric(Valor_portafolio_colcap),as.numeric(Valor_portafolio_ficticio))
colnames(Resultados) = c('Indice','Portafolio_Colcap','Portafolio_ficticio')
rownames(Resultados) = rownames(Despues_acciones)
Resultados = as.data.frame(Resultados)

######### ----------- Grafico resultados -------- #######
plot(Resultados[,"Indice"],type='l')
lines(Resultados[,"Portafolio_Colcap"],type='l',col='blue')
lines(Resultados[,"Portafolio_ficticio"],type='l',col='red')


