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

  #DATA ID PREP
  uniqueUser2     <- unique(training2$USERID)
  uniqueUser2star <- unique(training2star$USERID)
  uniqueOffer2    <- unique(training2$OFFERID)
  
  training2$USERID  <- mapvalues(training2$USERID, from=uniqueUser2, to=1:length(uniqueUser2))
  training2$OFFERID <- mapvalues(training2$OFFERID,from=uniqueOffer2,to=1:length(uniqueOffer2))
  
  
  #CREATE SPARSE MATRIX
  X <- sparseMatrix(i = training2$USERID,
                    j = training2$OFFERID,
                    x = training2$CLICK)
  
  
  
  res <-list()
  res[[1]] <- X
  res[[2]] <- training2
  res[[3]] <-   uniqueUser2
  res[[4]] <-  uniqueUser2star
  res[[5]] < - uniqueOffer2
  

  return(res)
}

