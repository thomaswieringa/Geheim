Pinv <- function(X,data) 
  {
  for(i in 1:nrow(data))
  {
    i <- data[i]$USERID
    j <- data[i]$OFFERID
    X[i,j]=0
  }
  return(X)
}

