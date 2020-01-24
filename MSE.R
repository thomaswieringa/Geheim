MSE <- function(result,testing)
{
  #rescale data
  scaledResult = (result+1)/2
  scaledTesting = testing
  scaledTesting[,4] = (scaledTesting[,4]+1)/2
  
  SSE<-0
  for(i in 1:nrow(testing))
  {
    scaledPrediction <- scaledResult[testing[i,1],testing[i,3]]
    SE <- (scaledTesting[i,4]-scaledPrediction)^2   
    SSE <- SSE+SE
  }
  return(SSE/nrow(testing))
}
