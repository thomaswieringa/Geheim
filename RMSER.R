RMSE <- function(A,B,testing, uniqueUser2 , uniqueUser2star , uniqueOffer2, clr, Clickratings)
{
  #rescale data
  scaledTesting     <-  testing
  setkey(scaledTesting,USERID)
  
  #UserIDs for users which appear in test as well as in training2
  trainedUsers2 <- intersect(unique(scaledTesting$USERID),uniqueUser2)
  #UserIDs for users which appear in test as well as in training2* ---> users for which we predict zero
  trainedUsers2star <- intersect(unique(scaledTesting$USERID),uniqueUser2star)
  
  
  #Split testing into set with users which are trained for and users which are not trained for
  trainedTest     <- scaledTesting[.(trainedUsers2)]                            #test set of users in train2
  nontrainedTest  <- scaledTesting[.(trainedUsers2star)]                        #test set of users in train2* 
  
  
  #Remove observations with offers not trained for.
  offersTest          <- unique(trainedTest$OFFERID)            #all offers which appear in testing
  offersTrainedFor    <- intersect(uniqueOffer2,offersTest)     #all offers which we have trained for
  offersNotTrainedFor <- setdiff(offersTest,offersTrainedFor)   #all offers which we have NOT trained for
  
  setkey(trainedTest,OFFERID)
  
  validTest     <- trainedTest[.(offersTrainedFor)]          #Part of test-set with Users and Offers where we have trained for.
  nonvalidTest  <- trainedTest[.(offersNotTrainedFor)]       #part of test set where offers are not in training 
  
  #Error for trained part
  #Convert indices to subset of training indices.
  options(warn=-1)
  validTest$USERID  <- mapvalues(validTest$USERID,from=uniqueUser2, to=1:length(uniqueUser2))
  validTest$OFFERID <- mapvalues(validTest$OFFERID, from=uniqueOffer2,to=1:length(uniqueOffer2))
  options(warn=0)
  
  predictions <-  predictions(A,B,validTest)
  AE1 <- (predictions-validTest$CLICK)^2
  AE2 <- (Clickratings[nonvalidTest$USERID]  - nonvalidTest$CLICK)^2
  AE3 <- (Clickratings[nontrainedTest$USERID]- nontrainedTest$CLICK)^2
  AE <- c(AE1,AE2,AE3)
  RMSE <- sqrt(mean(AE))
  
  print(sqrt(mean(AE3)))
  print("maxpredonclick")
  print(max(predictions[validTest$CLICK==1]))
  
  output <- list()
  output[[1]] <- RMSE
  output[[2]] <- sqrt(mean(AE1))
  
  return(output)
}
