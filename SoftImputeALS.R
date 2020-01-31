library(ptycho)

SoftImputeALS <- function(X, lambda2, maxIter, e, training, r)
{
  m <- X@Dim[1]
  n <- X@Dim[2]
  #Initialize matrices
  D <- diag(r)
  U <- createOrthogonalX(m,r)
  V <- matrix(0, n, r)
  A <- U%*%D
  B <- V%*%D
  AB <- A%*%t(B)
  
  for (t in 1:maxIter) 
  {
    print(paste0("Current iteration:  ", t))
    start_time <- Sys.time()
    pAB <- P_Omega(AB,training)
    end_time <- Sys.time()
    print(end_time - start_time)
    
    #update B
    t(B_thilde) <- inv(D%*%D+lambda2*diag(r))%*%t(A)%*%(X-pAB)+inv(D%*%D+lambda2*diag(r))%*%D%*%D%*%t(B)
    SVD1 <- irlba(B_thilde%*%D)
    U_thilde <- SVD1$u
    D_thilde%*%D_thilde <- SVD1$d
    V <- U_thilde
    D <- D_thilde
    B <- V%*%D
   
    #Update A
    t(A_thilde) <- inv(D%*%D+lambda2*diag(r))%*%t(B)(t(X)-t(pAB))+inv(D%*%D+lambda2*diag(r))%*%D%*%D%*%t(A)
    SVD2 <- irlba(A_thilde%*%D)
    V_thilde <- SVD2$v
    D_thilde%*%D_thilde <- SVD2$d
    U <- V_thilde
    D <- D_thilde
    A <- U%*%D
  }
}
