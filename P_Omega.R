P_Omega <- function(X, data)
{

  Xinv <- Pinv(X,data)
  X <- X-Xinv
  
  return(X)
}
