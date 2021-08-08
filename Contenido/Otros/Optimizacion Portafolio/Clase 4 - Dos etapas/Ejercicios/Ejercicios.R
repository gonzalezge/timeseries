###### --------------------- Ejercicio: Optimizacion de portafolios ----------------- ############
####### ----------- Limpiar ambiente ------- ######
rm(list = ls())
options(scipen = 10000000)
##### --------- Path: automatico  --------- #####
path <<- gsub(rstudioapi::getActiveDocumentContext()$path,pattern = "/Ejercicio 2.+",replacement = "")
setwd(paste0(path,'/Ejercicio 2'))
##### -------- librerias ------- ######
librerias <- c("readxl","ggplot2","scales",'timeSeries','timeSeries','forecast','rugarch','fBasics','moments','tseries','PortfolioAnalytics','dplyr','zoo','nleqslv')

###### ----- Instalacion liberarias ------ ####
if(length(setdiff(librerias, rownames(installed.packages()))) > 0){
  install.packages(setdiff(librerias, rownames(installed.packages())))}
invisible(sapply(librerias, require, character.only = TRUE,quietly = TRUE))


# Aca les dejamos unas funciones que pueden ser util para cada uno de los pasos

##### --------- Paso 1: Cargar datos ------ ##### 



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

# 4. Garch 


##### --------- Paso 3: Generar las restricciones ------ ##### 

# 1. Restricciones de caja por accion
caja=cbind(rep(0,ncol(X)),rep(1,ncol(X)))
rownames(caja)=rownames(X)

# 2. Limites Sectoriales
# Ejemplo: Los ultimos dos activos del portafolio deben sumar entre 0 y 25%.
A_sects=matrix(c(rep(0,ncol(X)-2),1,1),nrow=1)
lims_sects=cbind(0,0.25)


##### --------- Paso 4: Diseñar la metrica de evaluacion ------ ##### 



##### --------- Paso 5: Diseñar la estrategia de inversion (funcion de utilidad) ------ ##### 



##### --------- Paso 6: Diseñar la metrica de evaluacion ------ ##### 



##### --------- Paso 7: Diseñar el backtest ------ ##### 



##### --------- Paso 7.1: Definir horizonte de inversion ------ ##### 


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
