predictions <- function(A, B, data)
{
  ivec <- data$USERID
  jvec <- data$OFFERID
  Astar <- A[ivec,]
  Bstar <- B[jvec,]
  predVec <- rowSums(Astar*Bstar)
  predVec[predVec<0]=0
  predVec[predVec>1]=1
  
  return(predVec)
}
