POmega <- function(A, B, training3)
{
  ivec <- training3$USERID
  jvec <- training3$OFFERID
  
  Astar <- A[ivec,]
  Bstar <- B[jvec,]
  predVec <- rowSums(Astar*Bstar)
  
  return(predVec)
}
