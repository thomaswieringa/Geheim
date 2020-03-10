SoftImputeALS <- function(X, lambda3, maxIter, e, training2, R)
{
  m <- X@Dim[1]
  n <- X@Dim[2]
  #Initialize matrices
  V <- matrix(0,n,R)
  U <- matrix(rnorm(m*r),m,R)
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
    
    if(t>2){
  
      B <- t(t(V)*D)
      A <- t(t(U)*D)
      Xomega <- POmega(A,B,training2)
      Xdiff@x <- X@x - Xomega
      B_thildeT <- (t(U)%*%Xdiff + t(B)*D)
    }
    
    else {
      B_thildeT <- t(U)%*%Xdiff 
    }
    
    B_thildeT <- B_thildeT*(Dsq/(Dsq+lambda3))
    
    SVD1 <- svd(t(B_thildeT))
    V <- SVD1$u
    Dsq <- SVD1$d
    D <- sqrt(Dsq)
    U   <- U%*%SVD1$v
   
    #A step
    A <- t(t(U)*D)
    Xstar <- POmega(A, t(t(V)*D) ,training2)
    Xdiff@x <- X@x - Xstar
   
    A_thildeT <- t(V)%*%t(Xdiff) + D*t(A)
    A_thildeT <- A_thildeT*(Dsq/(Dsq+lambda3))
  
    #Update U & D
    SVD2 <- svd(t(A_thildeT))
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
      Xstar <- POmega(A, t(t(V)*D) ,training2)
      Xdiff@x <- X@x - Xstar
      A_thildeT <- t(V)%*%t(Xdiff) + D*t(A)
      SVD2 <- svd(t(A_thildeT))
      U    <- SVD2$u
      Dsq  <- SVD2$d
      Dsq  <- pmax(Dsq-lambda3,0)
      D    <- sqrt(Dsq)
      V    <- V%*%SVD2$v
      
      rank <- min(sum(Dsq>0)+1,R)
      
      return(list(t(t(U)*D),t(t(V)*D), rank))
    }
  }
}

