###### --------------------- Ejercicio 1 ----------------- ############ 
####### ----------- Limpiar ambiente ------- ###### 
rm(list = ls())
options(scipen = 10000000)
##### --------- Path: automatico  --------- #####
path <<- gsub(rstudioapi::getActiveDocumentContext()$path,pattern = "/Ejercicio 3.+",replacement = "")
setwd(paste0(path,'/Ejercicio 3'))
##### -------- librerias ------- ###### 
librerias <- c("readxl","ggplot2","scales",'timeSeries','timeSeries','forecast','rugarch','fBasics','moments','tseries','fPortfolio','dplyr','zoo','plotly')

###### ----- Instalacion liberarias ------ ####
if(length(setdiff(librerias, rownames(installed.packages()))) > 0){
  install.packages(setdiff(librerias, rownames(installed.packages())))}
invisible(sapply(librerias, require, character.only = TRUE,quietly = TRUE))

###### ----------- Cargar datos --------- ####### 
##### --------- Datos acciones ------ ##### 
Acciones = as.data.frame(read_excel('Datos.xlsx',sheet = 'Close'))
Acciones= na.locf(Acciones)
## ---- Organizar nombres --- ###
rownames(Acciones) = Acciones[,'Fecha']
Acciones = Acciones[,-1]

## Vamos a generar la serie semanal ##
DatosSem=seq(as.Date(rownames(Acciones))[1],as.Date(rownames(Acciones))[nrow(Acciones)],by = "week")

# En caso de festivos, tomamos el dia mas cercano.
DatosSem=sapply(DatosSem,function(x){which.min(abs(as.Date(rownames(Acciones))-x))})

# Nos quedamos solo con los datos semanales
Acciones=Acciones[DatosSem,]

# Definimos una funcion que aplique el estimador de Shrink con suavizamiento exponencial.
shrinkEstimator = function(X){

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
  Sigma=matrix(0,ncol=ncol(X), nrow=ncol(X))
  w=matrix((1-lambda)^(seq(TiempoFin-1,0)),ncol=TiempoFin)
  w=matrix(rep(w,ncol(X)), ncol=ncol(X))
  mu=matrix(rep(mu,ncol(t(retornos))),ncol=ncol(t(retornos)))
  
  sigma=lambda/(1-(1-lambda)^TiempoFin)*(t(w*t((t(retornos)-mu)))%*%t((t(retornos)-mu)))
  
  mu=mu[,1]
  names(mu)=colnames(X) 
  
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

  return( list(Sigma=sigmaS,mu=muS) )
}

# Generamos la serie de retornos
Retornos=na.omit(apply(Acciones,2,returns))
rownames(Retornos)=rownames(Acciones)[-1]

# Generamos cuatro estrategias (1,2,3,4): minima varianza en 2 y maximo retorno en 2.
# Una con shrink y otra con media y covarianza historica (estimador de max vero).
Retorno1=Retorno2=NULL
Retorno3=Retorno4=NULL


for(i in 52:(nrow(Retornos)-1)){
  
  # Definimos que datos usar con un rolling window.
  Datos=Retornos[(i-51):i,]
  
  # Corremos nuestra funcion de shrink con suavizamiento.
  Covarianza1=shrinkEstimator(Datos)
  
  # Se lo asignamos a un vector de mu y sigma.
  Mu1=Covarianza1$mu
  Cov1=Covarianza1$Sigma
  
  # Calibramos con maxima verosimilitud mu y sigma.
  Mu2=colMeans(Datos)
  Cov2=cov(Datos)
  
  # Generamos una grilla de pesos para las dos acciones
  w=matrix(seq(0,1,0.001),ncol=1)
  w=cbind(w,1-w)
  
  # Encontramos sigma y mu en la frontera.
  Sigma1=apply(w,1,function(x){t(x)%*%Cov1%*%(x)})
  Sigma2=apply(w,1,function(x){t(x)%*%Cov2%*%(x)})
  Mu1=apply(w,1,function(x){t(x)%*%Mu1})
  Mu2=apply(w,1,function(x){t(x)%*%Mu2})
  
  # Tomamos los portafolios de minima varianza y maximo retorno
  Portafolio1=w[which.min(abs(sqrt(Sigma1))),]
  Portafolio2=w[which.min(abs(sqrt(Sigma2))),]
  Portafolio3=w[which.max(Mu1),]
  Portafolio4=w[which.max(Mu2),]
  
  # Evaluamos la rentabilidad del portafolio una semana despues.
  Retorno1=c(Retorno1,sum(Portafolio1*Retornos[i+1,]))
  Retorno2=c(Retorno2,sum(Portafolio2*Retornos[i+1,]))
  Retorno3=c(Retorno3,sum(Portafolio3*Retornos[i+1,]))
  Retorno4=c(Retorno4,sum(Portafolio4*Retornos[i+1,]))
  
}

# Acumulamos los retornos para las estrategias de maxima rentabilidad.
Acum1=cumprod(Retorno3+1)
Acum2=cumprod(Retorno4+1)

plot(Acum1,type="l",ylim=range(c(Acum1,Acum2)))
par(new=T)
plot(Acum2,col=2,type="l",ylim=range(c(Acum1,Acum2)))


# Calculamos volatilidades de 30 dias
Varianza1=sapply(30:length(Retorno1),function(x){sd(Retorno1[(x-29):x])})
Varianza2=sapply(30:length(Retorno2),function(x){sd(Retorno2[(x-29):x])})

plot(Varianza1,type="l",ylim=range(c(Varianza1,Varianza2)))
par(new=T)
plot(Varianza2,col=2,type="l",ylim=range(c(Varianza1,Varianza2)))


