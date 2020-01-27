library(Matrix)
library(RGCCA)
library(irlba)
library(scales)
library(caret)
library(plyr)
library(data.table)

#READ DATA
#data <- read.csv("~/Desktop/SUNWEB Data/Observations_Report kopie.csv", sep=";")
data <- read.csv("~/Desktop/Observations_Report kopie.csv", sep=";")
data<- as.data.table(data)

#DATA RESCALE
data[,4]=(data[,4]*2)-1

#DATA ID PREP
uniqueUser <- unique(data$USERID)
uniqueOffer <- unique(data$OFFERID)
data$USERID  <- mapvalues(data$USERID, from=uniqueUser, to=1:length(uniqueUser))
data$OFFERID <- mapvalues(data$OFFERID,from=uniqueOffer,to=1:length(uniqueOffer))
userIDs <- 1:length(uniqueUser)

#DATA PARTITIONING
intrain  <- createDataPartition(y=data$CLICK,p=0.9,list=FALSE)
training <- data[intrain,]
testing  <- data[-intrain,]

#Create data.table index for fast access of data
setkey(training, USERID)
setkey(data, USERID)

#Calculate clickrate for every User in test set
Clickrates <- 0
for(u in 1:length(uniqueUser))
{
  userclicks <- training[.(u)]
  click <- sum(userclicks$CLICK == 1)
  nonclick <- sum(userclicks$CLICK == -1)
  Clickrates[u] <- click/(click+nonclick)
  if(is.na(Clickrates[u]))
  {
    Clickrates[u] <-0
  }
}

#TRAINING CLICK RATES AND REMOVE FROM TESTING
#thresholds  <- c(0.001,0.01,0.1,0.5,0.6,0.7,0.8,0.9)
thresholds <- 0.1
MSEresults <-0
counter <-1
for(threshold in thresholds)
{
  print("Trying threshold")
  print(threshold)
  
  selectedUsers <- userIDs[Clickrates>threshold]
  data2 <- data[.(selectedUsers)]

  print("Amount of observations")
  print(nrow(data2))

  #DATA ID PREP
  uniqueUser2 <- unique(data2$USERID)
  uniqueOffer2 <- unique(data2$OFFERID)
  data2$USERID  <- mapvalues(data2$USERID, from=uniqueUser2, to=1:length(uniqueUser2))
  data2$OFFERID <- mapvalues(data2$OFFERID,from=uniqueOffer2,to=1:length(uniqueOffer2))


  print("Amount of users")
  print(length(uniqueUser2))

  #DATA PARTITIONING
  intrain  <- createDataPartition(y=data2$CLICK,p=0.9,list=FALSE)
  training2 <- data2[intrain,]
  testing2  <- data2[-intrain,]

  #CREATE SPARSE MATRIX
  X <- sparseMatrix(i = training$USERID,
                  j = training$OFFERID,
                  x = training$CLICK)


  maxIter <- 100
  e <- 0.01
  lambda <-exp(-4)
  results<-list()
  count = 1
  for(l in lambda)
{
  result <- SoftImpute(X,l,maxIter,e,data2)
  results[[count]] = result
  print("Found solution")
  count=count+1
}


MSEs <-0
count = 1
for(i in 1:length(results))
{
  MSEs[count] = MSE(results[[i]],testing,uniqueUser2,uniqueOffer2)
  count = count +1
}

MSEresults[counter]=MSEs

counter <- counter+1
}



