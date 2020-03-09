SoftImputeALS <- function(X, lambda2, maxIter, e, training2, r)
{
  m <- X@Dim[1]
  n <- X@Dim[2]
  #Initialize matrices
  V <- matrix(0,n,r)
  U <- matrix(rnorm(m*r),m,r)
  U <- svd(U)$u
  Dsq <- rep(1,r)
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
      Xstar <- POmega(A,B,training2)
      Xdiff@x <- X@x - Xstar
      B_thildeT <- t(U)%*%Xdiff + D*t(B)
    }
    
    else {
      B_thildeT <- t(U)%*%Xdiff 
    }
    
    B_thildeT <- B_thildeT*(sqrt(Dsq)/(Dsq+lambda2))
    
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
    A_thildeT <- A_thildeT*(sqrt(Dsq)/(Dsq+lambda2))
    
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
      Dsq  <- pmax(Dsq-lambda2,0)
      D    <- sqrt(Dsq)
      V    <- V%*%SVD2$v
      
      rank <- min(sum(Dsq>0)+1,r)
      
      return(list(t(t(U)*D),t(t(V)*D), rank))
    }
  }
}

