P_Omega <- function(A2, B2, data2)
{
  ivec <- data2$USERID
  jvec <- data2$OFFERID
  Astar <- A2[ivec,]
  Bstar <- B2[jvec,]
  predVec <- rowSums(Astar*Bstar)
  
  #CREATE SPARSE MATRIX
  AB <- sparseMatrix(i = ivec,
                     j = jvec,
                     x = predVec)
  
  
  return(AB)
}

