MSE <- function(result,testing,uniqueUser,uniqueOffer)
{
  #rescale data
  scaledResult = (result+1)/2
  scaledTesting = testing
  scaledTesting[,4] = (scaledTesting[,4]+1)/2
  
  SSE<-0
  for(i in 1:nrow(testing))
  {
    prediction   <-0
    UserID <- which(uniqueUser==testing[i,1])
    OfferID <- which(uniqueOffer==testing[i,3])
    
    SE<-0
    
    if(length(UserID)==0 )
    {
      SE <- scaledTesting[i,4]-prediction
    }
    else if(length(OfferID)==0)
    {
      SE <- scaledTesting[i,4]-prediction
    }
    
    else
    {
      scaledPrediction <- scaledResult[UserID,OfferID]
      SE <- (scaledTesting[i,4]-scaledPrediction)^2   
    }
    SSE <- SSE+SE
  }
  return(SSE/nrow(testing))
}
