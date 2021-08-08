###### --------------------- Ejercicio 1: ----------------- ############ 
####### ----------- Limpiar ambiente ------- ###### 
rm(list = ls())
##### --------- Path: automatico  --------- #####
path <<- gsub(rstudioapi::getActiveDocumentContext()$path,pattern = "/Ejercicio 1.+",replacement = "")
setwd(paste0(path,'/Ejercicio 1'))
##### -------- librerias ------- ###### 
librerias <- c("readxl","ggplot2","scales",'timeSeries','timeSeries','forecast','rugarch','fBasics','zoo','moments','tseries','fPortfolio','data.table','kernlab','abind','BLCOP','xts')

###### ----- Instalacion liberarias ------ ####
if(length(setdiff(librerias, rownames(installed.packages()))) > 0){
  install.packages(setdiff(librerias, rownames(installed.packages())))}
invisible(sapply(librerias, require, character.only = TRUE,quietly = TRUE))


Graficar_simulaciones  = function(mat,title,hist=NULL,plotear=F,quitarCeros=0,leyenda_x,leyenda_y,Fecha_inicial = "2011-09-01"){
  ## INPUTS
  ## 1. mat: matriz con las simulaciones por columnas
  ## 2. hist: vector con la historia de la variable simulada
  ## 3. quitarCeros: numero de ceros a quitar
  ## 4. plot: Si se desea plotear o no
  mat=t(mat)
  medias = colMeans(mat)
  data = data.frame(media = medias)
  upper_seq = seq(70,95,by = 5)/100
  
  
  int=t(apply(mat,2,quantile,probs=upper_seq))
  data=cbind(data,t(apply(mat,2,quantile,probs=c(1-upper_seq,upper_seq))))
  colnames(data) = c("media","min70","min75","min80","min85","min90","min95","max70","max75","max80","max85","max90","max95")
  if(!is.null(hist)){
    hist=matrix(rep(hist,ncol(data)),ncol = ncol(data))
    colnames(hist)=colnames(data)
    data=rbind(hist,data)
    data=data/10^quitarCeros
  }
  data = cbind(seq(as.Date(Fecha_inicial),length.out = length(rownames(data)),by="month"),data)
  colnames(data) = c("Fecha",colnames(data)[-1])
  plot1 = ggplot(data=data)+
    geom_ribbon(aes(x=Fecha,ymin=min95,ymax=max95),alpha=0.3,fill="#67001f") +
    geom_ribbon(aes(x=Fecha,ymin=min90,ymax=max90),alpha=0.4,fill="#b2182b") +
    geom_ribbon(aes(x=Fecha,ymin=min85,ymax=max85),alpha=0.5,fill="#d6604d") +
    geom_ribbon(aes(x=Fecha,ymin=min80,ymax=max80),alpha=0.6,fill="#878787") +
    geom_ribbon(aes(x=Fecha,ymin=min75,ymax=max75),alpha=0.7,fill="#4d4d4d") +
    geom_line(aes(x=Fecha,y=min95),col="#4575b4",size=1.5,alpha =0.5) +
    geom_line(aes(x=Fecha,y=max95),col="#4575b4",size=1.5,alpha =0.5) +
    geom_line(aes(x=Fecha,y=media),col="#4575b4",size=1.5) +
    theme(legend.position="none") + labs(title = title)+
    theme_bw()+ theme(legend.background = element_rect(fill="white", size=2.5, linetype="solid"),legend.title = element_text(colour="black", size=12, face="bold"),axis.text.x=element_text(angle=0, hjust=1),legend.text = element_text(colour="black", size=15, face="bold"),panel.grid.major = element_blank(), panel.border = element_blank(), plot.title = element_text(hjust = 1),axis.text=element_text(size=25,face="bold"),axis.title=element_text(size=25,face="bold"),legend.position = "top")+labs(title=title)+labs(x = leyenda_x, y = leyenda_y) +scale_x_date(date_breaks = "14 month", date_labels =  "%b %y")+scale_y_continuous(labels = comma)
  if(plotear){print(plot1)}
  return(plot1)
  
}

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

############## -------------- 1 Cargar informacion TESCOP y TESIBR --------------- #################
DatosMercado = as.data.frame(read_excel('Datos/Datos.xlsx'))
rownames(DatosMercado) = DatosMercado[,1]
DatosMercado = DatosMercado[,-1]
#######----- Crear indice IBR ------ #####
tasaBR=t(t(DatosMercado[,c("IBR")]))/100
rownames(tasaBR)=rownames(DatosMercado)
DatosMercado=DatosMercado[,!colnames(DatosMercado)%in%c("IBR")]
tasaBR=(1+tasaBR)^(1/250)-1
# Calcular precio del activo libre de riesgo (arranca en 100, rendimiento igual a tasa BanRep)
nomAccs = colnames(DatosMercado) 
nomActivo="IBR"
DatosMercado = cbind(DatosMercado,rep(100,nrow(DatosMercado))) 
names(DatosMercado)=c(nomAccs,nomActivo)
DatosMercado[,"IBR"]=cumprod(1+tasaBR)
fechasAccs = as.Date(rownames(DatosMercado))
###### ------------ Retornos ------------ #######
Datos=xts(DatosMercado,order.by=(as.Date(rownames(DatosMercado))))
RetornosMercado = apply(Datos, 2, function(x){returns(x, method='simple')})[-1,]

#### ----- Portafolio S&P ficticio: Mix CEMBI 50%  ---- ####
RetornosMercado[,"S&P"]= as.matrix((RetornosMercado[,"CEMBI"]*0.5) + (RetornosMercado[,"S&P"]*0.5))
### --- Eliminarlo --- ####
RetornosMercado=RetornosMercado[,!colnames(RetornosMercado)%in%"CEMBI"]

### ---  2. Suavizamiento exponencial ------ ####
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

############## -------------- Calibrar VARCOV--------------- #################
Resultados_estimacion = SuavizamientoExponencial(X = RetornosMercado)
Mu = Resultados_estimacion$mu
Sigma = Resultados_estimacion$sigma
Prior=list(mu=Mu,sigma=Sigma)

#### ------- Proyeccion ----- ###### 
horizonte = 182
Sigma=matrizCovHorizonte(prior=Prior,horizonte)
Mu=(1+Mu)^horizonte-1

###### ----------------- Views --------- ######
ViewsTot = as.data.frame(read_excel('Datos/Analistas.xlsx'))
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


############## -------------- Optimizacion de portafolio--------------- #################
X = (RetornosMercado)
## ---- Caja de restricciones --- #### 
caja=cbind(rep(0,ncol(X)),rep(0.4,ncol(X)))

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


######## ---------- Grilla de retornos de la frontera ---------- ####### 
Rbar=seq(min(Mu),max(Mu),length.out=100)
###### --------- Calculo de portafolios ------- #####
Portafolios=na.omit(t(sapply(Rbar,portfolioOptimQP,mu=Mu,sigma=Sigma,caja=caja)))

SigmaPort=sqrt(apply(Portafolios,1,function(x){t(x)%*%Sigma%*%(x)}))
MuPort=Portafolios%*%Mu

plot(x=SigmaPort,MuPort,type="l")

