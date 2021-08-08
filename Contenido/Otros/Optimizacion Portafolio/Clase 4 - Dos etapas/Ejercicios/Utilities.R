###### --------------------- Ejercicio: Optimizacion de portafolios ----------------- ############
####### ----------- Limpiar ambiente ------- ######
rm(list = ls())
options(scipen = 10000000)
##### --------- Path: automatico  --------- #####
path <<- gsub(rstudioapi::getActiveDocumentContext()$path,pattern = "/Utilities.R",replacement = "")
setwd(paste0(path,'/'))
##### -------- librerias ------- ######
librerias <- c("readxl","ggplot2","scales",'timeSeries','timeSeries','forecast','rugarch','fBasics','moments','tseries','PortfolioAnalytics','dplyr','zoo','nleqslv','usdm','lubridate','xts','forecast','RQuantLib','tseries','urca','nleqslv','uroot','vars','GeneralizedHyperbolic','plyr','fUnitRoots','xtable','mvtnorm','reshape2','ggplot2','grid','fitdistrplus','tsDyn')

###### ----- Instalacion liberarias ------ ####
if(length(setdiff(librerias, rownames(installed.packages()))) > 0){
  install.packages(setdiff(librerias, rownames(installed.packages())))}
invisible(sapply(librerias, require, character.only = TRUE,quietly = TRUE))


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

# Distribuciones conjuntas:

# 1. Estimador de Shrink
shrinkEstimator = function(X,mu,sigma){
  
  retornos = X

  I=diag(ncol(var(X)))
  C=mean(eigen(var(X))$values)*I
  Traza=0
  for (i in 1:nrow(X)){
    Traza=Traza+tr(((t(t(X[i,]))%*%t(X[i,]))-sigma)%*%(t(t(X[i,]))%*%t(X[i,]))-sigma)
  }
  alfa=((Traza/nrow(X)))/(tr(t(sigma-C)%*%(sigma-C))*nrow(X))
  if (alfa>0.9){
    alfa=0.9
  }
  if (alfa<0.1){
    alfa=0.1
  }
  
  alfa=matrix(rep(alfa,ncol(X)*ncol(X)),nrow=ncol(X))
  sigmaS=(1-alfa)*sigma+alfa*C
  
  lambda = eigen(sigmaS)$values
  N = ncol(retornos)
  unos = rep(1,N)
  
  b = (unos%*%solve(sigmaS)%*%t(t(mu)))/(unos%*%solve(sigmaS)%*%t(t(unos)))*unos
  lambda = eigen(sigmaS)$values
  alpha = 1/nrow(retornos)*(N*mean(lambda)-2*lambda[1])/(sum((mu-b)^2))
  ###############################################################################################
  if(alpha>=0.9){ alpha=0.9 }
  if(alpha<=0.1){ alpha=0.1 }
  ###############################################################################################  
  muS = (1-alpha)*mu + alpha*b
  
  return( list(sigma=sigmaS,mu=muS) )
}

# 2. Suavizamiento exponencial
SuavizamientoExponencial = function(X){
  retornos = X
  lambda= 0.086
  TiempoFin=nrow(X)
  
  ##Pesos
  w=matrix((1-lambda)^(seq(TiempoFin-1,0)),ncol=TiempoFin)
  ##Mu
  mu=lambda/(1-(1-lambda)^TiempoFin)*w%*%X
  mu=matrix(mu,ncol=1)
  ##Sigma
  w=matrix((1-lambda)^(seq(TiempoFin-1,0)),ncol=TiempoFin)
  w=matrix(rep(w,ncol(X)), ncol=ncol(X))
  mu=matrix(rep(mu,ncol(t(retornos))),ncol=ncol(t(retornos)))
  sigma=lambda/(1-(1-lambda)^TiempoFin)*(t(w*t((t(retornos)-mu)))%*%t((t(retornos)-mu)))
  mu=mu[,1]
  names(mu)=colnames(X) 
  
  ##
  w=matrix((1-lambda)^(seq(TiempoFin-1,0)),ncol=TiempoFin)
  w=matrix(rep(w,ncol(X)), ncol=ncol(X))
  mu=matrix(rep(mu,ncol(t(retornos))),ncol=ncol(t(retornos)))
  
  sigma=lambda/(1-(1-lambda)^TiempoFin)*(t(w*t((t(retornos)-mu)))%*%t((t(retornos)-mu)))
  
  mu=mu[,1]
  names(mu)=colnames(X) 
  
  return(list(mu=mu,sigma=sigma))
}

# 3. Covarianza y mu historico.
Historico = function(X){
  
  mu=colMeans(X)
  sigma=cov(X)
  
  return(list(mu=mu,sigma=sigma))
}

# 4. VAR
VAR=function(X){
  #Se calibra un modelo VAR multivariado para simular el IPC, el dólar y el Brent de manera integrada
  #para incorporar correlaciones ya que no son independientes entre sí. EL modelo calibrado es un VAR con
  #constante.
  ## Definir modelo
  p     <- 3
  vecm  <- ca.jo(X, type = "eigen", ecdet = "const", K = p)
  data.frame(test = round(summary(vecm)@teststat, 2), summary(vecm)@cval)
  model <- vec2var(vecm, r = 1)
  
  ## Se generan simulaciones del VAR usando la función VAR.sim junto con replicate para crear 1000
  #simulaciones de cada factor de riesgo.
  MatCoefB=cbind(model$deterministic,model$A[[1]], model$A[[2]],model$A[[3]])
  nsim=1000
  HorizonteRiesgo=1
  
  SimVAR=function(B,n=1, lag, starting = NULL,residual){
    return(MatCoefB[,-1]%*%t(t(unlist(c(t(starting)))))+MatCoefB[,1]+t(t(apply(residual,2,sample,size=1))))
  }
  SimsVAR<-replicate(n=nsim,SimVAR(B=MatCoefB,n=HorizonteRiesgo, lag=p, starting = as.data.frame(tail(X,3)),residual=model$resid))
  mu=colMeans(t(SimsVAR[,1,]))
  sigma=cov(t(SimsVAR[,1,]))
  
  return(list(mu=mu,sigma=sigma))
}

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
UtilidadPyG = function(DistribucionPyG,param){
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


CalcularAversion = function(Equiv0){

  #     #Funcion que calibra la funcion de utilidad anterior a loterias equivalentes segun el nivel de PyG.
  fun_resolver <- function(param,E1){
    #### ----- Loteria 50%*U() + 50% U()
    return(0.5*UtilidadPyG(0.9,param)+0.5*UtilidadPyG(1.1,param)-UtilidadPyG(1+E1,param))
  }
  return(nleqslv(ifelse(Equiv0>0,2,0), fun_resolver, E1=Equiv0, control=list(ftol=1e-14))$x)
}

ParamAversion<<-CalcularAversion(Equiv0=-0.035)


# Usar esta funcion una vez se simulen los puntos de la frontera.
PortOptim=which.max(apply(1+Simulacion,2,UtilidadPyG,param=ParamAversion))


##### --------- Paso 6: Diseñar el backtest ------ ##### 



portfolioOptimQP = function(sigma,mu,Rbar,caja="no",A_sects=NULL,lims_sects=NULL){
  ## INPUTS:
  # 1. sigma: matriz var-covar de las variables
  # 2. mu: Retornos de las variables
  # 3. Rbar: es el minimo retorno esperado
  # 4. caja: Hay restricciones de caja? opciones: "no" o una matriz con las restricciones de caja
  # 5. A_sets: matriz con las restricciones por sector ( nRestriccionesSectoriale x nAcciones)
  # 6. lims_sects: Vector con los limites sectoriales correspondientes a A_sects
  ## OUTPUTS:
  # 1. w: vector de pesos
  
  Nvar = length(mu)
  
  if(caja!="no" & is.null(A_sects)){ #Caso con restriccion de caja, sin restricciones sectoriales (Se puede usar LowRankQP)
    #   # Solucion LowRankQP (FUNCIONA BIEN, la condicion Ax es de IGUALDAD) (OJO limite inferior es cero!!!)
    #    Vmat = 2*sigma
    #    Amat = t(cbind(rep(1,Nvar),mu))
    #    dvec = t(0*mu)  
    #    bvec = c(1,Rbar)                   
    #    uvec = caja[,2]       
    #    sol = LowRankQP(Vmat,dvec,Amat,bvec,uvec,method="LU",verbose=FALSE,niter=200)$alpha
    # Solucion kernlab  (FUNCIONA BIEN, no optimiza en los dos extremos de la forntera!!!!!)
    c = t(0*mu)
    H = 2*sigma
    A = rbind(rep(1,Nvar),mu)
    b = c(1,Rbar)
    l=(caja[,1])
    u = (caja[,2])
    r= c(0,0)                                    #verb=1 hace que se muestre la informacion de la convergencia
    sol = try(ipop(c, H, A, b, l, u, r, sigf = 6, maxiter = 1000, margin = 0.005, bound = 10,verb = 0),silent=TRUE)
    if(class(sol)=="try-error"){ sol=rep(NA,Nvar) }
    else{ sol=sol@primal }
    
  }
  if(caja!="no" & !is.null(A_sects)){ #Caso con restriccion de caja, sin restricciones sectoriales (toca usar kernlab)
    # Solucion kernlab  (FUNCIONA BIEN, no optimiza en los dos extremos de la forntera!!!!!)
    c = t(0*mu)
    H = 2*sigma
    A = rbind(rep(1,Nvar),mu,A_sects)
    b = c(1,Rbar,as.numeric(lims_sects[1,]))
    l=(caja[,1])
    u = (caja[,2])
    r= c(0,0,as.numeric(lims_sects[2,]-lims_sects[1,]))                                    #verb=1 hace que se muestre la informacion de la convergencia
    sol = try(ipop(c, H, A, b, l, u, r, sigf = 7, maxiter = 400, margin = 0.05, bound = 10,verb = 0),silent=TRUE)
    if(class(sol)=="try-error"){ sol=rep(NA,Nvar) }
    else{ sol=sol@primal }
  }
  
  return( sol )
}


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



