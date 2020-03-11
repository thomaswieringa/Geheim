CBSoftImputeALS <- function(X, lambda3, maxIter, e, training2, R,PHI)
{
  
  PhiTINV <- ginv(t(PHI))
  
  set.seed(1)
  m <- X@Dim[1]
  n <- X@Dim[2]
  #Initialize matrices
  V <- matrix(0,n,R)
  U <- matrix(rnorm(m*R),m,R)
  U <- svd(U)$u
  Dsq <- rep(1,R)
  D <- sqrt(Dsq) 
  Xdiff <- X
  
  for (t in 1:maxIter) 
  {
    print(paste0("Current iteration:  ", t))
    
    U_old <- U
    V_old  <- V
    Dsq_old  <- Dsq
    
    #B step
    B <- t(t(V)*D)
    A <- t(t(U)*D)
    Xomega <- POmega(A,B,training2)
    valueVec <- training2$CLICK - Xomega
    Xdiff <- sparseMatrix(i = training2$USERID,j = training2$OFFERID, x=valueVec)
    
    C_thilde <- t(PhiTINV)%*%(t(t(U)%*%Xdiff) + t(t(B)*D))
    C_thilde <- C_thilde%*%diag(Dsq/(Dsq+lambda3))
    
    SVD1 <- svd(PHI%*%C_thilde)
    V <- SVD1$u
    Dsq <- SVD1$d
    D <- sqrt(Dsq)
    U   <- U%*%SVD1$v
    
    #A step
    A <- t(t(U)*D)
    Xomega <- POmega(A, t(t(V)*D) ,training2)
    valueVec <- training2$CLICK - Xomega
    Xdiff <- sparseMatrix(i = training2$USERID,j = training2$OFFERID, x=valueVec)
    
    A_thilde <- Xdiff%*%V + t(t(A)*D)
    A_thilde <- A_thilde%*%diag((Dsq/(Dsq+lambda3)))
    
    #Update U & D
    SVD2 <- svd(A_thilde)
    U    <- SVD2$u
    Dsq  <- SVD2$d
    D    <- sqrt(Dsq)
    V    <- V%*%SVD2$v
    
    ratio <- Frob(U_old,Dsq_old,V_old,U,Dsq,V)
    cat(t, ":","ratio", ratio, "\n")
    
    if(ratio< e || t == maxIter)
    {
      print("Solution found")
      
      A <- t(t(U)*D)
      Xomega <- POmega(A, t(t(V)*D) ,training2)
      valueVec <- training2$CLICK - Xomega
      Xdiff <- sparseMatrix(i = training2$USERID,j = training2$OFFERID, x=valueVec)
      A_thilde <- Xdiff%*%V + t(t(A)*D)
      SVD2 <- svd(A_thilde)
      U    <- SVD2$u
      Dsq  <- SVD2$d
      Dsq  <- pmax(Dsq-lambda3,0)
      D    <- sqrt(Dsq)
      V    <- V%*%SVD2$v
      
      rank <- min(sum(Dsq>0)+1,R)
      
      return(list(t(t(U)*D),t(t(V)*D), rank,C_thilde))
    }
  }
}
