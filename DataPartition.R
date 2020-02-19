DataPartition <- function(Clickrates,data,intrain)
{
  
  training    <- as.data.table(data[intrain,])
  testing     <- as.data.table(data[-intrain,])
  
  print(mean(training$CLICK))
  print(mean(testing$CLICK))
  
  print(length(unique(training$USERID)))
  print(length(unique(testing$USERID)))
  
  uniqueUsersTraining <-  unique(training$USERID)
  testingOffers       <-  unique(testing$OFFERID)
  
  #Create data.table index for fast access of data
  setkey(training, USERID)
  setkey(testing, USERID)
  setkey(data, USERID)
  
  print("Trying threshold")
  print(threshold)
  
  selectedUsers    <- uniqueUsersTraining[Clickrates>threshold]    #1 User IDS
  nonselectedUsers <- uniqueUsersTraining[Clickrates<=threshold]   #1 User IDS
  training2        <- training[.(selectedUsers)]
  training2star    <- training[.(nonselectedUsers)]
  
  res <-list()
  res[[1]] <- training2
  res[[2]] <- training2star
  res[[3]] <- testing
  
  return(res)
}

