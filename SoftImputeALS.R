SoftImputeALS <- function(X, lambda2, maxIter, e, training2, r)
{
  m <- X@Dim[1]
  n <- X@Dim[2]
  #Initialize matrices
  D <- diag(r)
  U <- createOrthogonalX(m,r)
  V <- matrix(0, n, r)
  A <- U%*%D
  B <- V%*%D
  
  for (t in 1:maxIter) 
  {
    print(paste0("Current iteration:  ", t))
    start_time <- Sys.time()
    pAB <- P_Omega(A,B,training2)
    end_time <- Sys.time()
    print(end_time - start_time)
    
    #update B
    B_thilde <- t(inv(D%*%D+lambda2*diag(r))%*%t(A)%*%(X-pAB)+inv(D%*%D+lambda2*diag(r))%*%D%*%D%*%t(B))
    SVD1 <- svd(B_thilde%*%D)
    U_thilde <- SVD1$u
    D_thilde_sq <- diag(SVD1$d)
    D_thilde <- sqrtm(D_thilde_sq)
    V <- U_thilde
    D <- D_thilde
    B_new <- V%*%D
    pAB <- P_Omega(A,B_new,training2)
    
    #Update A
    A_thilde <- t(inv(D%*%D+lambda2*diag(r))%*%t(B_new)%*%(t(X)-t(pAB))+inv(D%*%D+lambda2*diag(r))%*%D%*%D%*%t(A))
    SVD2 <- svd(A_thilde%*%D)
    U_thilde <- SVD2$u
    D_thilde_sq <- diag(SVD2$d)
    D_thilde <- sqrtm(D_thilde_sq)
    U <- U_thilde
    D <- D_thilde
    A_new <- U%*%D
    
    #convergence
    diffA <- norm(A_new-A, type = "F")^2/norm(A, type = "F")^2
    print(paste0("Difference A:     ", diffA))
    diffB <- norm(B_new-B, type = "F")^2/norm(B, type = "F")^2
    print(paste0("Difference B:     ", diffB))
    diff <- diffA+diffB
    
    if(diff< e)
    {
      print("Solution found")
      return(list(A_new,B_new))
    }
    A <- A_new
    B <- B_new
    
  }
}
