###### --------------------- Ejercicio 1 ----------------- ############ 
####### ----------- Limpiar ambiente ------- ###### 
rm(list = ls())
##### --------- Path: automatico  --------- #####
path <<- gsub(rstudioapi::getActiveDocumentContext()$path,pattern = "/Ejercicio 2.+",replacement = "")
setwd(paste0(path,'/Ejercicio 2'))
##### -------- librerias ------- ###### 
librerias <- c("readxl","ggplot2","scales",'timeSeries','timeSeries','forecast','rugarch','fBasics','moments','tseries','fPortfolio','data.table')

###### ----- Instalacion liberarias ------ ####
if(length(setdiff(librerias, rownames(installed.packages()))) > 0){
  install.packages(setdiff(librerias, rownames(installed.packages())))}
invisible(sapply(librerias, require, character.only = TRUE,quietly = TRUE))


###### ----------- 
cvarOpt = function(rmat, alpha=0.05, rmin, wmin=0, wmax=1, weight.sum=1)
{
  require(Rglpk)
  n = ncol(rmat) # number of assets
  s = nrow(rmat) # number of scenarios i.e. periods
  averet = colMeans(rmat)
  # creat objective vector, constraint matrix, constraint rhs
  Amat = rbind(cbind(rbind(1,averet),matrix(data=0,nrow=2,ncol=s+1)),
               cbind(rmat,diag(s),1))
  objL = c(rep(0,n), rep(-1/(alpha*s), s), -1)
  bvec = c(weight.sum,rmin,rep(0,s))
  # direction vector
  dir.vec = c("==","==",rep(">=",s))
  # bounds on weights
  bounds = list(lower = list(ind = 1:n, val = rep(wmin,n)),
                upper = list(ind = 1:n, val = rep(wmax,n)))
  res = Rglpk_solve_LP(obj=objL, mat=Amat, dir=dir.vec, rhs=bvec,
                       types=rep("C",length(objL)), max=T, bounds=bounds)
  w = as.numeric(res$solution[1:n])
  return(w)
}


Mejor=CalcularMejor(rets_aux,j=j)
indicesTrain=Mejor[["indicesTrain"]]
p_value=Mejor[["p_value"]]
Mejor=Mejor[["Mejor"]]

gC.f <- normalCopula(dim=ncol(rets_aux))
gcF.ml  <- fitCopula(gC.f, pobs(rets_aux), method="ml")

tC.f <- tCopula(dim=ncol(rets_aux), df=5, df.fixed=FALSE)
tcF.ml  <- fitCopula(tC.f, pobs(rets_aux), method="ml")
coef.t=summary(tcF.ml)$coefficients[,1]
tC.f.post <- tCopula(param=coef.t[-length(coef.t)],dim=ncol(rets_aux), df=coef.t[length(coef.t)], df.fixed=FALSE)

Vero_Copulas=rbind(Vero_Copulas,c(summary(gcF.ml)$loglik,summary(tcF.ml)$loglik))

set.seed(123)
pvars<-rCopula(n=nSim,tC.f.post)
rawvars=pvars
var_new=matrix(0,ncol=ncol(pvars),nrow=nrow(pvars))

for (i in (1:ncol(pvars))){
  x=rets_aux[,i]
  set.seed(123)
  
  # Distribucion normal
  
  ARMA=auto.arima(x)$coef
  AR=max(1,length(grep("ar",names(ARMA))))
  MA=length(grep("ma",names(ARMA)))
  
  Errors=arima(x,order = c(AR,0,MA))$residuals
  ARCH=auto.arima(Errors^2)$coef
  AR_ARCH=length(grep("ar",names(ARCH)))
  MA_ARCH=length(grep("ma",names(ARCH)))
  
  if(Mejor[i]=="Normal"){
    GARCH_spec=ugarchspec(mean.model = list(armaOrder = c(max(AR),MA)),variance.model = list(garchOrder = c(max(1,AR_ARCH),MA_ARCH)), distribution.model = "norm")
    if(j==200){
      GARCH_spec=ugarchspec(mean.model = list(armaOrder = c(max(1,AR),MA)),variance.model = list(garchOrder = c(max(1,AR_ARCH),MA_ARCH)), distribution.model = "norm")
    }
    
    garch.fit = ugarchfit(GARCH_spec, data = x,solver="hybrid")
    
    xyz=ugarchsim(garch.fit,n.sim=nDias,n.start=0,m.sim=nrow(rawvars)*100,prereturns = x)
    xyz=apply(fitted(xyz)+1,2,prod)-1
    
    var_new[,i]=quantile(ecdf(xyz),pvars[,i])
  }
  
  if(Mejor[i]=="Logistica"){
    
    GARCH_spec=ugarchspec(mean.model = list(armaOrder = c(max(AR),MA)),variance.model = list(garchOrder = c(max(1,AR_ARCH),MA_ARCH)), distribution.model = "sstd")
    
    garch.fit = ugarchfit(GARCH_spec, data = x,solver="hybrid")
    
    xyz=ugarchsim(garch.fit,n.sim=nDias,n.start=0,m.sim=nrow(rawvars)*100,prereturns = x)
    xyz=apply(fitted(xyz)+1,2,prod)-1
    
    var_new[,i]=quantile(ecdf(xyz),pvars[,i])
  }
  
  if(Mejor[i]=="T-Student"){
    GARCH_spec=ugarchspec(mean.model = list(armaOrder = c(max(AR),MA)),variance.model = list(garchOrder = c(max(1,AR_ARCH),MA_ARCH)), distribution.model = "std")
    
    garch.fit = ugarchfit(GARCH_spec, data = x,solver="hybrid")
    
    xyz=ugarchsim(garch.fit,n.sim=nDias,n.start=0,m.sim=nrow(rawvars)*100,prereturns = x)
    xyz=apply(fitted(xyz)+1,2,prod)-1
    
    var_new[,i]=quantile(ecdf(xyz),pvars[,i])
  }
  
  if(Mejor[i]=="Uniforme"){
    param_unif=try(unlist((fitdistrplus::fitdist(c((1+x)^nDias-1),"unif"))[1]),silent=T)
    names(param_unif)=gsub("estimate.","",names(param_unif))
    var_new[,i]=qunif(pvars[,i],min=param_unif[1],max=param_unif[2])
  }
  
  if(Mejor[i]=="TasaInt"){
    param_unif=try(unlist((fitdistrplus::fitdist(c((1+x)^nDias-1),"unif"))[1]),silent=T)
    names(param_unif)=gsub("estimate.","",names(param_unif))
    var_new[,i]=qunif(pvars[,i],min=rets_aux[nrow(rets_aux),ncol(rets_aux)]-0.001,max=rets_aux[nrow(rets_aux),ncol(rets_aux)]+0.001)
  }
  
  if(Mejor[i]=="Hiperbolica"){
    
    GARCH_spec=ugarchspec(mean.model = list(armaOrder = c(max(1,AR),MA)),variance.model = list(garchOrder = c(max(1,AR_ARCH),MA_ARCH)), distribution.model = "ghyp")
    
    
    garch.fit = ugarchfit(GARCH_spec, data = x,solver="hybrid")
    
    xyz=ugarchsim(garch.fit,n.sim=nDias,n.start=0,m.sim=nrow(rawvars)*100,prereturns = x)
    xyz=apply(fitted(xyz)+1,2,prod)-1
    
    var_new[,i]=quantile(ecdf(xyz),pvars[,i])
    
  }
  
}
colnames(var_new)=colnames(rets_aux)
rmat=var_new
simRetornos=rmat
rets_normales[j-nIniF+1,colnames(rets_aux)][rets_normales[j-nIniF+1,colnames(rets_aux)]>0.25]=max(rets_normales[j-nIniF+1,colnames(rets_aux)][rets_normales[j-nIniF+1,colnames(rets_aux)]<0.25])
simRetornos=simRetornos+matrix(rep(rets_normales[j-nIniF+1,colnames(rets_aux)]-colMeans(simRetornos),nrow(simRetornos)),ncol=ncol(simRetornos),byrow=T)

pos = j-nIniF+1
Mejor_Historico[pos,colnames(rets_aux)]=Mejor
MPrior=simRetornos

rmat=MPosterior


w=matrix(NA,ncol=length(e),nrow=ncol(rmat))
for (i in (1:length(e))){
  w[,i]=cvarOpt(rmat=rmat,rmin=e[i])
}
w=w[,abs(colSums(w)-1)<=0.000001 & colMins(w)>=0]
sigmaFront=NULL
for (i in (1:ncol(w))){
  sigmaFront=cbind(sigmaFront,cvarRisk(rmat, w[,i], alpha = 0.05))
}

RetPort=t(t(w)%*%t(MPosterior))

maximizador_alto=which.max(apply(RetPort,2,UtilidadPyG,a=a_alto))
maximizador_medio=which.max(apply(RetPort,2,UtilidadPyG,a=a_medio))
maximizador_bajo=which.max(apply(RetPort,2,UtilidadPyG,a=a_bajo))

UtMaxAlto=max(c(UtMaxAlto,apply(RetPort,2,UtilidadPyG,a=a_alto)))
UtMaxMedio=max(c(UtMaxMedio,apply(RetPort,2,UtilidadPyG,a=a_medio)))
UtMaxBajo=max(c(UtMaxBajo,apply(RetPort,2,UtilidadPyG,a=a_bajo)))