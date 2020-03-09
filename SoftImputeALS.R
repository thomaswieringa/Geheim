SoftImputeALS <- function(X, lambda2, maxIter, e, training2, r)
{
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
    
    
    if(iter>2){
      BD <- t(t(V)*Dsq)
      
      xfill=suvC(U,BD,irow,pcol)
      xres@x=x@x-xfill
    }else BD=0 
    B=t(t(U)%*%xres)+BD
    if(lambda>0)B=UD(B,Dsq/(Dsq+lambda),m)
    Bsvd=svd(B)
    V=Bsvd$u
    Dsq=Bsvd$d
    U=U%*%Bsvd$v
    
    
    
    
    
    
    
    
    
    
    #A step
    A_thilde <- t(V)%*%(t(X)-t(pAB)) + Dsq*t(U)
    A_thilde <- A_thilde*(sqrt(Dsq)/(Dsq+lambda2))
    
    #Update U & D
    SVD2 <- svd(t(A_thilde))
    U    <- SVD2$u
    Dsq  <- SVD2$d
    D    <- sqrt(Dsq)
    V    <- V%*%SVD2$v
    pAB  <- P_Omega(t(t(U)*D),t(t(V)*D),training2)
    
    
    
    
    #B step
    if(t>2){
      
    }
    pAB <- P_Omega(t(t(U)*D),t(t(V)*D),training2)
    B_thilde  <- (t(U)%*%(X-pAB)) + Dsq*t(V)
    B_thilde   <- B_thilde*(sqrt(Dsq)/(Dsq+lambda2))
    
    
    #update V & D
    SVD1 <- svd(t(B_thilde))
    V <- SVD1$u
    Dsq <- SVD1$d
    D <- sqrt(Dsq)
    U <- U%*%SVD1$v
    pAB <- P_Omega(t(t(U)*D),t(t(V)*D),training2)
    
   
    
    ratio <- Frob(U_old,Dsq_old,V_old,U,Dsq,V)
    cat(t, ":","ratio", ratio, "\n")
    
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

