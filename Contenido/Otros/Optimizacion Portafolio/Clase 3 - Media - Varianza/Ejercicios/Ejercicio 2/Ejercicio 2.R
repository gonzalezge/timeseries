#### OJO!!! POR EL MOMENTO SOLO SE TIENE SOLUCION DE CAJA MAYOR DE CERO


portfolioOptimQP(sigma = Covarianza,mu=)
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
  
  
  if(caja[1]=="no"){
    # Crear matriz de restricciones. (Amat entra transpuesta en quadprog)  
    Amat = cbind(rep(1,Nvar),mu)
    bvec = c(1,Rbar)
    meq = 2
    ## Preparar input para usar quadprog
    dvec = t(0*mu)                     #entra como vector fila en quadprog
    Dmat = sigma*2
    # Resolver problema
    sol = try(solve.QP(Dmat, dvec, Amat, bvec, meq))
    if(class(sol)=="try-error"){ sol=rep(NA,Nvar) }
    else{ sol=sol$solution }              
  }
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

portfolioOptimQP_minrisk = function(sigma,caja="no"){
  ## INPUTS:
  # 1. sigma: matriz var-covar de las variables
  # 2. mu: Retornos de las variables
  # 3. Rbar: es el minimo retorno esperado
  # 4. caja: Hay restricciones de caja? opciones: "no" o una matriz con las restricciones de caja
  ## OUTPUTS:
  # 1. w: vector de pesos
  
  Nvar = ncol(sigma)
  
  if(caja[1]=="no"){
    # Crear matriz de restricciones. (Amat entra transpuesta en quadprog)  
    Amat = cbind(rep(1,Nvar))
    bvec = 1
    meq = 1
    ## Preparar input para usar quadprog
    dvec = t(rep(0,Nvar))                     #entra como vector fila en quadprog
    Dmat = sigma*2    
    # Resolver problema
    sol = solve.QP(Dmat, dvec, Amat, bvec, meq)$solution
    
  }
  else{
    #    Amat = cbind(1,diag(Nvar),-1*diag(Nvar))
    #    bvec = c(1,caja[,1],-caja[,2])
    #    meq = 1
    
    # Solucion LowRankQP
    Vmat = 2*sigma
    Amat = t(cbind(rep(1,Nvar)))
    dvec = t(0*mu)  
    bvec = 1                   
    uvec = caja[,2]       
    sol = LowRankQP(Vmat,dvec,Amat,bvec,uvec,method="LU",verbose=FALSE,niter=200)$alpha    
  }
  
  return( sol )
}
