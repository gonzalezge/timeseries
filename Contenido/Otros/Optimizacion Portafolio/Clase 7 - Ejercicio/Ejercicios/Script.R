###### --------------------- Ejercicio: Optimizacion de portafolios ----------------- ############
####### ----------- Limpiar ambiente ------- ######
rm(list = ls())
options(scipen = 10000000)
##### --------- Path: automatico  --------- #####
path <<- gsub(rstudioapi::getActiveDocumentContext()$path,pattern = "/Script.+",replacement = "")
setwd(paste0(path))
##### -------- librerias ------- ######
librerias <- c("readxl","ggplot2","scales",'timeSeries','timeSeries','forecast','rugarch','fBasics','moments','tseries','PortfolioAnalytics','dplyr','zoo','nleqslv')

###### ----- Instalacion liberarias ------ ####
if(length(setdiff(librerias, rownames(installed.packages()))) > 0){
  install.packages(setdiff(librerias, rownames(installed.packages())))}
invisible(sapply(librerias, require, character.only = TRUE,quietly = TRUE))

source('Utilities.R',)

######## ---------- Pasos a seguir ----------- ######### 
##### --------- Paso 1: Cargar datos ------ #####

# Aca les dejamos unas funciones que pueden ser util para cada uno de los pasos

##### --------- Paso 1: Cargar datos ------ ##### 
TESCOP = read_excel('Datos/TESCOP.xlsx')
TESCOP= as.data.frame(TESCOP)

############ ---------- Ejemplo de la construcción de un índice entre dos TES -------- ###### 
####### ---------- Funcion TES ----------- ###### 
rownames(TESCOP) = as.Date(TESCOP[,1])
TESCOP = TESCOP[,-1]
colnames(TESCOP) = gsub(x = colnames(TESCOP),pattern = 'TESCOP_',replacement = '')

##### ------- Se toman los Meses y se pasan a numero ---- ##### 
colnames(TESCOP)[grep(colnames(TESCOP),pattern = 'M')] = as.numeric(gsub(colnames(TESCOP)[grep(colnames(TESCOP),pattern = 'M')],pattern = 'M',replacement = ''))/12
##### ------- Se toman los años y se pasan a numero---- ##### 
colnames(TESCOP)[grep(colnames(TESCOP),pattern = 'Y')] = as.numeric(gsub(colnames(TESCOP)[grep(colnames(TESCOP),pattern = 'Y')],pattern = 'Y',replacement = ''))


######## ---- Interpolacion de un nodo de 10 - semana ------ ###
Interpolacion = apply(TESCOP, 1, function(z){approx(x = as.numeric(colnames(TESCOP)),y = as.numeric(z),xout = ((365*10-7)/365))$y})
TESCOP[,'9.98'] = Interpolacion

############ -------- Semanalizar: Cogiendo ---------- ##########
## Vamos a generar la serie semanal ##
DatosSem=seq(as.Date(rownames(TESCOP)[1]),as.Date(tail(rownames(TESCOP),1)),by = "week")
## En caso de festivos, tomamos el dia mas cercano.
DatosSem=sapply(DatosSem,function(x){which.min(abs(as.Date(rownames(TESCOP))-x))})

######## --------- Consolidar DataFrame: Resultados -------- ###### 
TESCOP = TESCOP[c(DatosSem),]
Resultados = data.frame('Semana'=c(1:dim(TESCOP)[1]),'Tasa1'=TESCOP[,'10'],'Tasa2'=TESCOP[,'9.98'],'TES1'=0,'TES2'=0)

####### ------- Rendimientos ------ ###### 
for (j in c(1:dim(Resultados)[1])) {
  ##### ---- TES 1 ---- ###
  Resultados[j,"TES1"] = 1/(1+(Resultados[j,"Tasa1"]/100))^(10)
  ##### ---- TES 2 ---- ###
  Resultados[j,"TES2"] = 1/(1+(Resultados[j,"Tasa2"]/100))^((365*10-7)/365)  
}

TESS = cbind(Resultados[-nrow(Resultados),"TES1"],Resultados[-1,"TES2"])
Resultados$Acum = c(1,cumprod(TESS[,2]/TESS[,1]))



##### --------- Paso 2: Generar los modelos de la prior (mu - sigma) ------ #####

#### -----No Normalidad ----- ####
# Se define una copula N-dimensional y se definen unos grados de libertad inicial (luego se optimizaran)
# Recuerde que las variables de las copulas deben ser iid.
tC.f <- tCopula(dim=ncol(Retornos), df=5, df.fixed=FALSE)
# Por maxima verosimulitud se definen los mejores parametros de la copula
tcF.ml  <- fitCopula(tC.f, pobs(Retornos), method="ml")
# Se toman los coeficientes de la copula estimada
coef.t=summary(tcF.ml)$coefficients[,1]
# Se guarda esa copula con los parametros estimados (la usaremos mas adelante para simular)
tC.f.post <- tCopula(param=coef.t[-length(coef.t)],dim=ncol(Retornos), df=coef.t[length(coef.t)], df.fixed=FALSE)
# Simulamos la copula
pvars<-rCopula(n=nSim,tC.f.post)

##### --------- Paso 3: Generar las restricciones ------ ##### 

# 1. Restricciones de caja por accion
# Ejemplo: Todas las acciones deben estar entre 0 y 100%.
caja=cbind(rep(0,ncol(X)),rep(1,ncol(X)))
rownames(caja)=rownames(X)

# 2. Limites Sectoriales
# Ejemplo: Los ultimos dos activos del portafolio deben sumar entre 0 y 25%.
A_sects=matrix(c(rep(0,ncol(X)-2),1,1),nrow=1)
lims_sects=cbind(0,0.25)


##### --------- Paso 4: Diseñar la metrica de evaluacion ------ ##### 
# Ejemplos:
# 1. Sharpe
SharpeRatio=mean(RetornoHistorico)/sd(RetornoHistorico)
# 2. Rentabilidad total
RentabilidadTotal=cumprod(1+RetornoHistorico)-1
# 3. Volatilidad 30 dias
Volatilidad30D=sapply(30:length(RetornoHistorico),function(x){sd(RetornoHistorico[(x-29):x])})


##### --------- Paso 5: Diseñar la estrategia de inversion (funcion de utilidad) ------ ##### 
ParamAversion<<-CalcularAversion(Equiv0=-0.035)
# Usar esta funcion una vez se simulen los puntos de la frontera.
PortOptim=which.max(apply(1+Simulacion,2,UtilidadPyG,param=ParamAversion))


########### ------------ Black Litterman Opcional ----------- ###########
#### ------- Input:  ----- ###### 
Prior=list(mu=Mu,sigma=Sigma)
horizonte = 182
Sigma=matrizCovHorizonte(prior=Prior,horizonte)
Mu=(1+Mu)^horizonte-1

###### ----------------- Cargar excel de Views --------- ######
ViewsTot = as.data.frame(read_excel('Ejemplo_Analistas.xlsx'))
#### ---- Acotar las columnas ----- #####
Views = ViewsTot[,c("Fecha","Activo","Retorno Esperado","Confianza")]
Views = data.frame(Mercado = Views[,'Activo'],Views_ret = Views[,"Retorno Esperado"],Views_conf = Views[,"Confianza"])
#### ---- Varianza ---- #### 
Views[,"Views_conf"]=Views[,"Views_conf"]^2
Ret_View = Views

###### ---------- Matriz de Pick ---------- ######
# Transformacion matricial de los views: 
Pick=diag(ncol(RetornosMercado))
colnames(Pick)=colnames(RetornosMercado)
rownames(Pick)=colnames(RetornosMercado)
Pick=rbind(Pick[match(Ret_View[,1],colnames(Pick)),])
#### ------- Views ----- ####
Views=BLViews(P=Pick,q=c(Ret_View[,"Views_ret"]), confidence=c(1/(Ret_View[,"Views_conf"])),assetNames=colnames(RetornosMercado))
#### ------ TAU desviavion de mu ------ ##### 
tau = 1/250
MarketPosterior=posteriorEst(views=Views,sigma=Sigma,mu=Mu,tau=tau)    
#### ------ Sigma incorporando views ------ #####
Sigma = MarketPosterior@posteriorCovar
#### ------ Mu incorporando views ------ #####
Mu = MarketPosterior@posteriorMean
densityPlots(MarketPosterior)

##### --------- Paso 7: Diseñar el backtest ------ ##### 


##### --------- Paso 7.1: Definir horizonte de inversion ------ ##### 


PortafolioHistorico=NULL
for(i in 52:(nrow(X)-1)){
  
  # 1. Escoger los datos hasta esa fecha
  
  
  # 2. Estimar mu y sigma
  
  
  # 3. Estirar mu y sigma hasta el horizonte
  
  
  # 4. Generar la frontera eficiente
  
  
  # 5. Escoger el portafolio optimo
  
  
  # 6. Guardar los pesos del portafolio y obtener los retornos de la semana
  PortafolioHistorico=rbind(PortafolioHistorico,PortafolioHoy)
  Retorno=t(PortafolioHoy)%*%RetAcciones[i+1,]
  RetornoHistorico=c(RetornoHistorico,Retorno)
  
}
