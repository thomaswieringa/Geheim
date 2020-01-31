library(Matrix)
library(RGCCA)
library(irlba)
library(scales)
library(caret)
library(plyr)
library(data.table)
library(svd)
library(ptycho)
library(expm)



#READ DATA
#data <- read.csv("~/Documents/SunWeb/data2.csv", sep=";")
#data <- read.csv("~/Desktop/Observations_Report kopie.csv", sep=";")
data<- read.csv("~/Documents/Seminar master/Rscript/Geheim/data2.csv", sep=";")
data<- as.data.table(data)

#DATA RESCALE
data[,4]=(data[,4]*2)-1

#DATA ID PREP
uniqueUser   <- unique(data$USERID)
uniqueOffer  <- unique(data$OFFERID)
data$USERID  <- mapvalues(data$USERID, from=uniqueUser, to=1:length(uniqueUser))
data$OFFERID <- mapvalues(data$OFFERID,from=uniqueOffer,to=1:length(uniqueOffer))
userIDs      <- 1:length(uniqueUser)

#DATA PARTITIONING
data$USERID  <- as.factor(data$USERID)
intrain      <- createDataPartition(data$USERID, p = 0.7, list = F)
data$USERID  <- as.numeric(data$USERID)
training     <- as.data.table(data[intrain,])
testing      <- as.data.table(data[-intrain,])


print(length(unique(training$USERID)))
print(length(unique(testing$USERID)))

uniqueUsersTraining <-  unique(training$USERID)
testingOffers      <- unique(testing$OFFERID)


#Create data.table index for fast access of data
setkey(training, USERID)
setkey(testing, USERID)
setkey(data, USERID)

#Calculate clickrate for every User in training set
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
thresholds <- 0
MSEresults <-list()
counter <-1
for(threshold in thresholds)
{
  print("Trying threshold")
  print(threshold)
  
  selectedUsers    <- uniqueUsersTraining[Clickrates>threshold]    #1 User IDS
  nonselectedUsers <- uniqueUsersTraining[Clickrates<=threshold]   #1 User IDS
  training2        <- training[.(selectedUsers)]
  training2star    <- training[.(nonselectedUsers)]
  
  
  #DATA ID PREP
  uniqueUser2  <- unique(training2$USERID)
  uniqueUser2star <- unique(training2star$USERID)
  uniqueOffer2 <- unique(training2$OFFERID)
  
  training2$USERID  <- mapvalues(training2$USERID, from=uniqueUser2, to=1:length(uniqueUser2))
  training2$OFFERID <- mapvalues(training2$OFFERID,from=uniqueOffer2,to=1:length(uniqueOffer2))
  
  #CREATE SPARSE MATRIX
  X <- sparseMatrix(i = training2$USERID,
                    j = training2$OFFERID,
                    x = training2$CLICK)
  
  maxIter <- 100
  e <- 1
  lambda <-exp(-4:2)
  results<-list()
  count = 1
  for(l in lambda)
  {
    
    result <- SoftImputeALS(X,l,maxIter,e,training2,4)
    
    
    results[[count]] = result
    print("Found solution")
    count=count+1
  }
  
  print("started calculating MSE")
  MSEs <-0
  count = 1
  for(i in 1:length(results))
  {
    MSEs[count] = MSE(results[[i]][[1]],results[[i]][[2]],testing,uniqueUser2,uniqueUser2star,uniqueOffer2)
    count = count +1
  }
  
  print("MSE calculated")
  
  MSEresults[[counter]]=MSEs
  counter <- counter+1
}



