###### --------------------- Ejercicio 1: Optimizacion restringida ----------------- ############
####### ----------- Limpiar ambiente ------- ######
rm(list = ls())
options(scipen = 10000000)
##### --------- Path: automatico  --------- #####
path <<- gsub(rstudioapi::getActiveDocumentContext()$path,pattern = "/Ejercicio 1.+",replacement = "")
setwd(paste0(path,'/Ejercicio 1'))
##### -------- librerias ------- ######
librerias <- c("readxl","ggplot2","scales",'timeSeries','timeSeries','forecast','rugarch','fBasics','moments','tseries','PortfolioAnalytics','dplyr','zoo','plotly','PortfolioAnalytics','ROI','ROI.plugin.quadprog','ROI.plugin.glpk')

###### ----- Instalacion liberarias ------ ####
if(length(setdiff(librerias, rownames(installed.packages()))) > 0){
  install.packages(setdiff(librerias, rownames(installed.packages())))}
invisible(sapply(librerias, require, character.only = TRUE,quietly = TRUE))

###### ----------- Cargar datos --------- #######
##### ------- Historia ------ ########
Datos_mercado = as.data.frame(read_excel('Datos.xlsx'))
Fechas_originales = Datos_mercado[,"Fecha"]
Datos_mercado = Datos_mercado[,-1]
#### ----- Volver numerico todo ----- #### 
Datos_mercado = apply(Datos_mercado, 2, as.numeric) 
##### ---- Eliminar Na ------- ######
Datos_mercado = na.locf(Datos_mercado)
#### ------ Retornos simples ------ #### 
Datos_mercado = apply(Datos_mercado, 2, function(x){returns(x,method='simple')})[-1,]
Datos_mercado = as.data.frame(Datos_mercado) 
rownames(Datos_mercado) = Fechas_originales[-1]


############ ------------------ Optimizacion 1:  ----------------- ##########
#### --------- Especificacion del portafolio -------- #######
Especificacion_portafolio <- portfolio.spec(colnames(Datos_mercado))
### ----- Restriccion 1: Solo largo ---- ####
Especificacion_portafolio <- add.constraint(portfolio = Especificacion_portafolio, type= "long_only")
### ----- Restriccion 2: la suma siempre debe ser 1 ---- #### 
Especificacion_portafolio <- add.constraint(portfolio = Especificacion_portafolio, type = "weight_sum", min_sum = 1, max_sum = 1)

###### ------- Objetivo optimizacion -------- ###### 
# Objetivo 1: Maximizar el retorno 
Especificacion_portafolio <- add.objective(portfolio = Especificacion_portafolio,
                           type = "return",
                           name = "mean")

# Objetivo 2: Minimizar el retorno
Especificacion_portafolio <- add.objective(portfolio = Especificacion_portafolio,
                           type = "risk",
                           name = "StdDev")

######### ------------- Optimizacion ----------- ######### 
Resultados_optmizacion <- optimize.portfolio(Datos_mercado, portfolio = Especificacion_portafolio,
                          optimize_method = "random",
                          trace=TRUE)

#### --------- Pesos -------- #####
print(Resultados_optmizacion$weights)

#### --------- Optimizacion -------- #####
Resultados_optmizacion$opt_values

###### --------------- Resultados ------------ #######
chart.RiskReward(Resultados_optmizacion, risk.col = "StdDev", return.col = "mean",
                 chart.assets = TRUE)

#### --------- Pesos -------- #####
print(Resultados_optmizacion$weights)
Pesos_optimos = as.matrix(Resultados_optmizacion$weights)
Retornos = as.matrix(Datos_mercado)
Resultados = Retornos%*%(Pesos_optimos)
#### ----- Resultados de la optimizacion ------ ##### 
Retornos_esperados = round(mean(Resultados),6) 
### Percentil 
Percentil_95 = quantile(Resultados,p=c(0.05))



############ ------------------ Optimizacion 2: ----------------- ######## 
# Objetivo: Encontrar el portafolio de minima varianza sujeto a E[r] = 0.0002 
#### --------- Especificacion del portafolio -------- #######
Especificacion_portafolio <- portfolio.spec(colnames(Datos_mercado))
### ----- Restriccion 1: Solo largo ---- ####
Especificacion_portafolio <- add.constraint(portfolio = Especificacion_portafolio, type= "long_only")
### ----- Restriccion 2: la suma siempre debe ser 1 ---- #### 
Especificacion_portafolio <- add.constraint(portfolio = Especificacion_portafolio, type = "weight_sum", min_sum = 1, max_sum = 1)
### ----- Restriccion 3: Retorno esperado ---- #### 
Especificacion_portafolio <- add.constraint(portfolio=Especificacion_portafolio, type="return", return_target=0.0002)



###### ------- Objetivo optimizacion -------- ###### 
# Objetivo 1: Maximizar el retorno 
Especificacion_portafolio <- add.objective(portfolio = Especificacion_portafolio,
                                           type = "return",
                                           name = "mean")

# Objetivo 1: Minimizar el restorno el retorno 
Especificacion_portafolio <- add.objective(portfolio = Especificacion_portafolio,
                                           type = "risk",
                                           name = "StdDev")

######### ------------- Optimizacion ----------- ######### 
Resultados_optmizacion <- optimize.portfolio(Datos_mercado, portfolio = Especificacion_portafolio,
                                             optimize_method = "random",
                                             trace=TRUE)

#### --------- Optimizacion -------- #####
Resultados_optmizacion$opt_values

###### --------------- Resultados ------------ #######
chart.RiskReward(Resultados_optmizacion, risk.col = "StdDev", return.col = "mean",
                 chart.assets = TRUE)
#### --------- Pesos -------- #####
print(Resultados_optmizacion$weights)
Pesos_optimos = as.matrix(Resultados_optmizacion$weights)
Retornos = as.matrix(Datos_mercado)
Resultados = Retornos%*%(Pesos_optimos)
#### ----- Resultados de la optimizacion ------ ##### 
Retornos_esperados = round(mean(Resultados),6) 



############ ------------------ Optimizacion 3: ----------------- ######## 
# Objetivo: La pérdida de cola esperada (ETL)
# también se llama valor condicional en riesgo (CVaR) 
#### --------- Especificacion del portafolio -------- #######
Especificacion_portafolio <- portfolio.spec(colnames(Datos_mercado))
### ----- Restriccion 1: Solo largo ---- ####
Especificacion_portafolio <- add.constraint(portfolio = Especificacion_portafolio, type= "long_only")
### ----- Restriccion 2: la suma siempre debe ser 1 ---- #### 
Especificacion_portafolio <- add.constraint(portfolio = Especificacion_portafolio, type = "weight_sum", min_sum = 1, max_sum = 1)
### ----- Restriccion 3: Retorno esperado ---- #### 
Especificacion_portafolio <- add.objective(portfolio=Especificacion_portafolio, type='risk',name='ETL', arguments=list(p=0.95))

###### ------- Objetivo optimizacion -------- ###### 
# Objetivo 1: Maximizar el retorno 
Especificacion_portafolio <- add.objective(portfolio = Especificacion_portafolio,
                                           type = "return",
                                           name = "mean")

# Objetivo 1: Minimizar el restorno el retorno 
Especificacion_portafolio <- add.objective(portfolio = Especificacion_portafolio,
                                           type = "risk",
                                           name = "StdDev")

######### ------------- Optimizacion ----------- ######### 
Resultados_optmizacion <- optimize.portfolio(Datos_mercado, portfolio = Especificacion_portafolio,
                                             optimize_method = "random",
                                             trace=TRUE)

#### --------- Optimizacion -------- #####
Resultados_optmizacion$opt_values

###### --------------- Resultados ------------ #######
chart.RiskReward(Resultados_optmizacion, risk.col = "StdDev", return.col = "mean",
                 chart.assets = TRUE)
#### --------- Pesos -------- #####
print(Resultados_optmizacion$weights)
Pesos_optimos = as.matrix(Resultados_optmizacion$weights)
Retornos = as.matrix(Datos_mercado)
Resultados = Retornos%*%(Pesos_optimos)
#### ----- Resultados de la optimizacion ------ ##### 
Retornos_esperados = round(mean(Resultados),6) 
### Percentil 
Percentil_optimizado_95 = quantile(Resultados,p=c(0.95))



############ ------------------ Optimizacion 4: ----------------- ######## 
# Objetivo: Encontrar el portafolio de minima varianza sujeto a unos limites de pesos 
# COLSC (10%,20%)
# COLIBR (30%,50%)
# COLTES (10%,20%)
# COLTES UVR (20%,40%)
# COLCAP  (10%,20%)
####### -------- Construccion de pesos ------- ###3
Restriccion_pesos = data.frame(Activos = c("COLSC","COLIBR","COLTES","COLTES UVR","COLCAP"),Inferior= 0,Superior=0)
### ---- Definicion de limite inferior ----- ### 
Restriccion_pesos['Inferior'] = c(0.1,0.3,0.1,0.2,0.1)
### ---- Definicion de limite superior ----- ### 
Restriccion_pesos['Superior'] = c(0.2,0.5,0.2,0.4,0.2)

#### --------- Especificacion del portafolio -------- #######
Especificacion_portafolio <- portfolio.spec(colnames(Datos_mercado))
### ----- Restriccion 1: Solo largo ---- ####
Especificacion_portafolio <- add.constraint(portfolio = Especificacion_portafolio, type= "long_only")
### ----- Restriccion 2: la suma siempre debe ser 1 ---- #### 
Especificacion_portafolio <- add.constraint(portfolio = Especificacion_portafolio, type = "weight_sum", min_sum = 1, max_sum = 1)
### ----- Restriccion 3: Pesos  ---- #### 
Especificacion_portafolio <- add.constraint(portfolio = Especificacion_portafolio, type = "box", min = c(0.1,0.3,0.1,0.2,0.1), max = c(0.2,0.5,0.2,0.4,0.2))


###### ------- Objetivo optimizacion -------- ###### 
# Objetivo 1: Maximizar el retorno 
Especificacion_portafolio <- add.objective(portfolio = Especificacion_portafolio,
                                           type = "return",
                                           name = "mean")

# Objetivo 1: Minimizar el restorno el retorno 
Especificacion_portafolio <- add.objective(portfolio = Especificacion_portafolio,
                                           type = "risk",
                                           name = "StdDev")

######### ------------- Optimizacion ----------- ######### 
Resultados_optmizacion <- optimize.portfolio(Datos_mercado, portfolio = Especificacion_portafolio,
                                             optimize_method = "random",
                                             trace=TRUE)

#### --------- Optimizacion -------- #####
Resultados_optmizacion$opt_values

###### --------------- Resultados ------------ #######
chart.RiskReward(Resultados_optmizacion, risk.col = "StdDev", return.col = "mean",
                 chart.assets = TRUE)
#### --------- Pesos -------- #####
print(Resultados_optmizacion$weights)
Pesos_optimos = as.matrix(Resultados_optmizacion$weights)
Retornos = as.matrix(Datos_mercado)
Resultados = Retornos%*%(Pesos_optimos)
#### ----- Resultados de la optimizacion ------ ##### 
Retornos_esperados = round(mean(Resultados),6) 



############ ------------------ Optimizacion 5: ----------------- ######## 
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

### ------ Especificacion del modelo -------- ###### 
Especificacion_portafolio <- portfolio.spec(colnames(Acciones))

### ----- Restriccion 1: Solo largo ---- ####
Especificacion_portafolio <- add.constraint(portfolio = Especificacion_portafolio, type= "long_only")
### ----- Restriccion 2: la suma siempre debe ser 1 ---- #### 
Especificacion_portafolio <- add.constraint(portfolio = Especificacion_portafolio, type = "weight_sum", min_sum = 1, max_sum = 1)
### ----- Restriccion 3: Grupos --------- #####3
Grupo_1 ='GRUPOARGOS|CONCONCRET|PFCEMARGOS|CELSIA|CLH'
Elementos_grupo_1 = grep(x =colnames(Acciones),pattern = Grupo_1)
Elementos_grupo_2 = grep(x =colnames(Acciones),pattern = paste0(colnames(Acciones)[-grep(x =colnames(Acciones),pattern = Grupo_1)],collapse = '|'))
Especificacion_portafolio <- add.constraint(portfolio = Especificacion_portafolio, type = "group", groups = list(Elementos_grupo_1, Elementos_grupo_2), group_min = 0.4, group_max = 0.6)


###### ------- Objetivo optimizacion -------- ###### 
# Objetivo 1: Maximizar el retorno 
Especificacion_portafolio <- add.objective(portfolio = Especificacion_portafolio,
                                           type = "return",
                                           name = "mean")

# Objetivo 1: Minimizar el restorno el retorno 
Especificacion_portafolio <- add.objective(portfolio = Especificacion_portafolio,
                                           type = "risk",
                                           name = "StdDev")
######### ------------- Optimizacion ----------- ######### 
Resultados_optmizacion <- optimize.portfolio(Acciones, portfolio = Especificacion_portafolio,
                                             optimize_method = "random",
                                             trace=TRUE)

#### --------- Optimizacion -------- #####
####### -------- Pesos del grupo 1 ------- #### 
sum(Resultados_optmizacion$weights[Elementos_grupo_1])
####### -------- Pesos del grupo 2 ------- #### 
sum(Resultados_optmizacion$weights[Elementos_grupo_2])
