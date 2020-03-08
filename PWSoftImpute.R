PWSoftImputeALS <- function(X, lambda2, maxIter, e, data3, r, PHI)
{
  
  PhiTINV <- ginv(t(PHI))
  
  m <- X@Dim[1]
  n <- X@Dim[2]
  #Initialize matrices
  V <- matrix(0,n,r)
  U <- matrix(rnorm(m*r),m,r)
  U <- svd(U)$u
  Dsq <- rep(1,r)
  
  for (t in 1:maxIter) 
  {
    print(paste0("Current iteration:  ", t))
    
    U_old <- U
    V_old  <- V
    Dsq_old  <- Dsq
    D <- sqrt(Dsq) 
    
    #C step
    pAB <- P_Omega(t(t(U)*D),t(t(V)*D),data3)
    C_thilde  <- ((t(U)%*%(X-pAB)) + Dsq*t(V)) %*% PhiTINV
    C_thilde   <- C_thilde*(sqrt(Dsq)/(Dsq+lambda2))
    
    
    #update V & D
    SVD1 <- svd( t(C_thilde%*%t(PHI)))
    V <- SVD1$u
    Dsq <- SVD1$d
    D <- sqrt(Dsq)
    U <- U%*%SVD1$v
    pAB <- P_Omega(t(t(U)*D),t(t(V)*D),data3)
    
    #obj=(.5*sum( (xfill-xhat)[!xnas]^2)+lambda*sum(Dsq))/nz
    obj =1
    
    #A step
    A_thilde <- t(V)%*%(t(X)-t(pAB)) + Dsq*t(U)
    A_thilde <- A_thilde*(sqrt(Dsq)/(Dsq+lambda2))
    
    #Update U & D
    SVD2 <- svd(t(A_thilde))
    U    <- SVD2$u
    Dsq  <- SVD2$d
    D    <- sqrt(Dsq)
    V    <- V%*%SVD2$v
    pAB <- P_Omega(t(t(U)*D),t(t(V)*D),data3)
    
    ratio=Frob(U_old,Dsq_old,V_old,U,Dsq,V)
    cat(t, ":", "obj",format(round(obj,5)),"ratio", ratio, "\n")
    
    if(ratio< e || t == maxIter)
    {
      print("Solution found")
      
      U   <- (X-pAB)%*%V + U 
      sU  <-svd(U)
      U   <-sU$u
      Dsq <-sU$d
      V <-V%*%sU$v
      Dsq <-pmax(Dsq-lambda2,0)
      D   <- sqrt(Dsq)
      
      return(list(t(t(U)*D),t(t(V)*D), sum(D>0)))
    }
  }
}

