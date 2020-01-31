P_Omega <- function(A, B, data)
{

  ivec <- data$USERID
  jvec <- data$OFFERID
  
  Astar <- A[ivec,]
  Bstar <- B[jvec,]
  
  
  predVec <- rowSums(Astar*Bstar)
  
  #CREATE SPARSE MATRIX
  AB <- sparseMatrix(i = ivec,
                     j = jvec,
                     x = predVec)
  
  
  return(AB)
}
