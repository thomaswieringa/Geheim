SoftImputeALS <- function(X, lambda2, maxIter, e, training2, r)
{
  m <- X@Dim[1]
  n <- X@Dim[2]
  #Initialize matrices
  V=matrix(0,n,r)
  U=matrix(rnorm(m*r),m,r)
  U=svd(U)$u
  Dsq=rep(1,r)
  
  for (t in 1:maxIter) 
  {
    print(paste0("Current iteration:  ", t))
    
    U_old=U
    V_old=V
    Dsq_old=Dsq
    D <- diag(sqrt(Dsq))
    
    #B step
    pAB <- P_Omega(U%*%D,V%*%D,training2)
    B_thilde=(t(U)%*%(X-pAB)) + diag(Dsq)%*%t(V)
    B_thilde=B_thilde*(sqrt(Dsq)/(Dsq+lambda2))
    
    
    #update V & D
    SVD1 <- svd(t(B_thilde))
    V <- SVD1$u
    Dsq <- SVD1$d
    U <- U%*%SVD1$v
    D <- diag(sqrt(Dsq))
    pAB <- P_Omega(U%*%D,V%*%D,training2)
    
    #obj=(.5*sum( (xfill-xhat)[!xnas]^2)+lambda*sum(Dsq))/nz
    obj =1
    
    # V step
    A_thilde <- t(V)%*%(t(X)-t(pAB)) + diag(Dsq)%*%t(U)
    A_thilde <- A_thilde*(sqrt(Dsq)/(Dsq+lambda2))
    SVD2 <- svd(t(A_thilde))
    U    <- SVD2$u
    Dsq  <- SVD2$d
    V    <- V%*%SVD2$v
    pAB <- P_Omega(U%*%diag(Dsq),V%*%diag(Dsq),training2)
    
    ratio=Frob(U_old,Dsq_old,V_old,U,Dsq,V)
    cat(t, ":", "obj",format(round(obj,5)),"ratio", ratio, "\n")
    
    if(ratio< e || t == maxIter)
    {
      print("Solution found")
      
      U <- (X-pAB)%*%V + U 
      sU=svd(U)
      U=sU$u
      Dsq=sU$d
      V=V%*%sU$v
      Dsq=pmax(Dsq-lambda2,0)
      D <- diag(sqrt(Dsq))
      return(list(U%*%D,V%*%D))
    }
  }
}

