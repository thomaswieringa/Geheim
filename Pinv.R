Pinv <- function(X,data) 
  {
  for(i in nrow(data))
  {
    i <- data[i,1] 
    j <- data[i,3]
    X[i,j]=0;
  }
  return(X)
}

