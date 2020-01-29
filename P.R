P <- function(X,data) 
{
  
  ivec <- data$USERID
  jvec <- data$OFFERID
  
  X[ cbind(ivec,jvec) ] <- integer(nrow(data))
  
  return(X)
}

