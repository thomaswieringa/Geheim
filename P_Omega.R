P_Omega <- function(A, B, data)
{

  Btrans <- t(B)
  ivec <- data$USERID
  jvec <- data$OFFERID
  
  Astar <- A[ivec,]
  Bstar <- Btrans[jvec,]
  
  
  predVec <- rowSums(Astar*Bstar)
  
  #CREATE SPARSE MATRIX
  X <- sparseMatrix(i = ivec,
                    j = jvec,
                    x = predVec)
  
  
  return(X)
}
