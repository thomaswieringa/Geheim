predictions <- function(A, B, data)
{
  
  ivec <- data$USERID
  jvec <- data$OFFERID
  
  Astar <- A[ivec,]
  Bstar <- B[jvec,]
  
  
  predVec <- rowSums(Astar*Bstar)
  
  return(predVec)
}
