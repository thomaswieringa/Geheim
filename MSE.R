MSE <- function(result,testing, uniqueUser2,uniqueUser2star,uniqueOffer2,allTrainedUsers,nonTrainedOffers)
{

  #rescale data
  scaledResult      <-  (result+1)/2
  scaledTesting     <-  testing
  scaledTesting[,4] <-  (scaledTesting[,4]+1)/2
  
  setkey(scaledTesting,USERID)

  trainedUsers2 <- intersect(unique(scaledTesting$USERID),uniqueUser2)
  trainedUsers2star <- intersect(unique(scaledTesting$USERID),uniqueUser2star)


  #Split testing into set with user which are trained and users which are not trained
  trainedTest     <- scaledTesting[.(trainedUsers2)]                            #test set of users in train2
  nontrainedTest  <- scaledTesting[.(trainedUsers2star)]                        #test set of users in train2* 
 
  
  #Remove observations with offers not trained for.
  offersTest          <- unique(trainedTest$OFFERID)
  offersTrainedFor    <- intersect(uniqueOffer2,offersTest)
  offersNotTrainedFor <- setdiff(offersTest,offersTrainedFor)
  
  setkey(trainedTest,OFFERID)
  
  validTest     <- trainedTest[.(offersTrainedFor)]          #part of test set with users AND offers in train2  
  nonvalidTest  <- trainedTest[.(offersNotTrainedFor)]       #part of test set where offers are not in training 
  
  #Error for trained part
  #Convert indices to subset of training indices.
  validTest$USERID  <- mapvalues(validTest$USERID,from=uniqueUser2, to=1:length(uniqueUser2))
  validTest$OFFERID <- mapvalues(validTest$OFFERID, from=uniqueOffer2,to=1:length(uniqueOffer2))
  
  predictions <-  result[cbind(validTest$USERID,validTest$OFFERID) ]
  SE1 <- (predictions-validTest$CLICK)^2
 
  #Error for non trained part
  SE2 <- (0-nontrainedTest$CLICK)^2
  SE <- c(SE1,SE2)
  MSE <- mean(SE)
  
  return(MSE)
}
