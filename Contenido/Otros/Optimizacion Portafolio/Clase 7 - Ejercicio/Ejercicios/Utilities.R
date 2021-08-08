############ --------- Funciones ----------- ########

##### ------ Horizonte ------- ######
matrizCovHorizonte = function(prior,periodo){
  # Inputs
  # prior: lista con retornos esperados y matriz de varianza covarianza
  # Output:
  # matCov: matriz de varianza covarianza al horizonte escogido en periodo.
  
  mu = prior$mu
  sigma = prior$sigma
  matCov = matrix(0,nrow(sigma),ncol(sigma))
  nAcc = nrow(sigma)
  for(i in 1:nAcc){
    for(j in 1:nAcc){
      matCov[i,j] = (1+mu[i]+mu[j]+sigma[i,j]+mu[i]*mu[j])^periodo - (1+mu[i])^periodo*(1+mu[j])^periodo
    }
  }
  rownames(matCov)=rownames(sigma)
  colnames(matCov)=colnames(sigma)
  
  return(matCov)
}



##### --------- Paso 2: Calibracion MU SIGMA ------ ##### 


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




CalcularMejor=function(Retornosaux,indicesTrain=NULL,j=NULL){
  #######----------- Paso 1: Estimación de Distribuciones ----------- ########
  Retornosaux = Retornos
  #### --------- Pruebas de distancia de Kolmogorov-Smirnov ------- #######
  ######### ---------- Se crea una Matriz con 6 columnas (distribuciones)
  ######### ---------- 10 filas (Acciones)
  Testkol=as.data.frame(matrix(0,ncol=5,nrow=ncol(Retornosaux)))
  ####### ------------ Muestra de entrenamiento/ Muestra Prueba --------- ####### 
  porcentajeTrain=0.5
  indicesTrain_aux=sample(1:(nrow(Retornosaux)),porcentajeTrain*nrow(Retornosaux))
  
  ######### ------------- Separacion entre muestra de entrenamiento y muestra de prueba ------- ########
  indicesTest_aux=(1:nrow(Retornosaux))[-indicesTrain_aux] 
  rownames(Testkol)=colnames(Retornosaux)
  #####---------- Distribuciones a estimar -------------- #######
  colnames(Testkol)=c("Normal","Logistica","Uniforme","Hiperbolica","Ganador")
  
  ##### ---------- Fin construcción tabla ------ #########
  retsAcc = Retornosaux
  for (i in 1:ncol(Retornosaux)){
    x=retsAcc[,i]
    #   descdist(c(x),discrete=F)
    
    ####### --------- Ajustar un modelo Arima para definir en un GARCH--------- ##### 
    ARMA=auto.arima(x)
    AR=length(grep("ar",names(ARMA$coef)))
    MA=length(grep("ma",names(ARMA$coef)))
    if(AR+MA==0){
      AR=1
    }
    
    ##### -------- Definir el número de grados de ARCH a partir de los residuales -------- ######
    ## A a partir de los residuales al cuadrado se determina el grado del modelo ARCH.
    #### --------- Auto
    ARCH=auto.arima(ARMA$residuals^2)$coef
    AR_ARCH=length(grep("ar",names(ARCH)))
    MA_ARCH=length(grep("ma",names(ARCH)))
    
    ####### --------- Especificación GARCH ------- ##########
    Especificacion_garch=ugarchspec(mean.model = list(armaOrder = c(AR,MA)),variance.model = list(garchOrder = c(max(1,AR_ARCH),MA_ARCH)), distribution.model = "norm")
    ####### ---------- Ajustar el modelo ------- ######
    Garch_ajustado = ugarchfit(Especificacion_garch, data = x,solver="gosolnp")
    Residuales_garch = residuals(Garch_ajustado)
    xyz=as.numeric(Residuales_garch)
    set.seed(124)
    indicesTrain=NULL
    
    ############# --------- Definicion de entrenamiento/prueba -----#######
    indicesTrain=sample(1:(length(xyz)),0.5*length(xyz))
    indicesTest=(1:length(xyz))[-indicesTrain] 
    
    ################ -------------- Normal: (Prueba+Entrenamiento)/2 --------------#########
    ######### ----------- Sobre la muestra de entrenamiento --------- ######### 
    param_norm=try(unlist(fitdistrplus::fitdist(c(xyz[indicesTrain]),"norm")[1]),silent=T)
    if(class(param_norm)=="try-error"){
      Testkol[i,"Normal"]=Inf
    } else {
      names(param_norm)=gsub("estimate.","",names(param_norm))
      ######## --------- Kolmogorov-Smirnov Tests --------- #########3
      Testkol[i,"Normal"]=ks.test(x=xyz[indicesTest],y = "pnorm",mean = param_norm[1], sd = param_norm[2])[[1]]  
    }
    
    
    ######### ----------- Sobre la muestra de prueba --------- ######### 
    param_norm=try(unlist(fitdistrplus::fitdist(c(xyz[-indicesTrain]),"norm")[1]),silent=T)
    if(class(param_norm)=="try-error"){
      Testkol[i,"Normal"]=Inf
    } else {
      names(param_norm)=gsub("estimate.","",names(param_norm))
      #### -------- Promedio ------ ####
      Testkol[i,"Normal"]=(Testkol[i,"Normal"]+ks.test(xyz[-indicesTest],"pnorm",mean = param_norm[1], sd = param_norm[2])[[1]])/2
    }
    
    
    ################ -------------- Uniforme: (Prueba+Entrenamiento)/2 --------------#########
    ######### ----------- Sobre la muestra de entrenamiento --------- ######### 
    param_unif=try(unlist(fitdistrplus::fitdist(c(x[indicesTrain]),"unif")[1]),silent=T)
    if(class(param_unif)=="try-error"){
      Testkol[i,"Uniforme"] = Inf
    } else {
      names(param_unif)=gsub("estimate.","",names(param_unif))
      Testkol[i,"Uniforme"] = ks.test(xyz[-indicesTrain],"punif",min=param_unif["min"],max=param_unif["max"])[[1]]    
    }
    
    ######### ----------- Sobre la muestra de prueba --------- ######### 
    param_unif=try(unlist(fitdistrplus::fitdist(c(x[-indicesTrain]),"unif")[1]),silent=T)
    if(class(param_unif)=="try-error"){
      Testkol[i,"Uniforme"]=Inf
    } else {
      names(param_unif)=gsub("estimate.","",names(param_unif))
      #### -------- Promedio ------ ####
      Testkol[i,"Uniforme"]=(Testkol[i,"Uniforme"]+ks.test(xyz[indicesTrain],"punif",min=param_unif["min"],max=param_unif["max"])[[1]])/2
    }
    
    
    ################ -------------- Logistica: (Prueba+Entrenamiento)/2 --------------#########
    ######### ----------- Sobre la muestra de entrenamiento --------- ######### 
    param_logistic=try(unlist(fitdistrplus::fitdist(c(xyz[indicesTrain]),"logis")[1]),silent=T)
    if (class(param_logistic)=="try-error"){
      Testkol[i,"Logistica"]= Inf 
    } else {
      names(param_logistic)=gsub("estimate.","",names(param_logistic))
      Testkol[i,"Logistica"]=ks.test(xyz[-indicesTrain],"plogis",location = param_logistic[1], scale = param_logistic[2])[[1]]
    }
    ######### ----------- Sobre la muestra de prueba --------- ######### 
    param_logistic=try(unlist(fitdistrplus::fitdist(c(xyz[-indicesTrain]),"logis")[1]),silent=T)
    if(class(param_logistic)=="try-error"){
      Testkol[i,"Logistica"]=Inf 
    } else {
      names(param_logistic)=gsub("estimate.","",names(param_logistic))
      Testkol[i,"Logistica"]=(Testkol[i,"Logistica"]+ks.test(xyz[indicesTrain],"plogis",location = param_logistic[1], scale = param_logistic[2])[[1]])/2
    }
    
    
    ################ -------------- Hiperbolica: (Prueba+Entrenamiento)/2 --------------#########
    param_hyper=try(unlist(ghFit(c(xyz[indicesTrain]),trace=F,doplot=F)@fit$estimate),silent=T)
    ######### ----------- Sobre la muestra de entrenamiento --------- ######### 
    if(class(param_hyper)=="try-error"){
      Testkol[i,"Hiperbolica"]=Inf 
    } else {
      names(param_hyper)=gsub("estimate.","",names(param_hyper))
      Testkol[i,"Hiperbolica"]=ks.test(xyz[-indicesTrain],"pgh",alpha=param_hyper["alpha"],beta=param_hyper["beta"],delta=param_hyper["delta"],mu=param_hyper["mu"],lambda=param_hyper["lambda"])[[1]]
    }
    
    ######### ----------- Sobre la muestra de prueba --------- ######### 
    param_hyper=try(unlist(ghFit(c(xyz[-indicesTrain]),trace=F,doplot=F)@fit$estimate),silent=T)
    if(class(param_hyper)=="try-error"){
      Testkol[i,"Hiperbolica"]= Inf
    } else {
      names(param_hyper)=gsub("estimate.","",names(param_hyper))
      Testkol[i,"Hiperbolica"]=(Testkol[i,"Hiperbolica"]+ks.test(xyz[indicesTrain],"pgh",alpha=param_hyper["alpha"],beta=param_hyper["beta"],delta=param_hyper["delta"],mu=param_hyper["mu"],lambda=param_hyper["lambda"])[[1]])/2
    }
    
    print(i)
  }
  
  ###### ----------- Las mejores marginales Independiente -------- ##### 
  Testkol[,"Ganador"]=colnames(Testkol)[apply(Testkol[,-ncol(Testkol)],1,which.min)]
  # Testkol=Testkol[,-3]
  Testkol[,1:5]=round(Testkol[,1:5],4)
  Testkol[Testkol==Inf]="No Convergió"
  
}

