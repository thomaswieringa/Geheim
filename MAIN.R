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
library(pracma)


#READ DATA
#full data thomas
#data <- read.csv("~/Documents/SunWeb/Observations_Report.csv", sep=";")
data <- read.csv("~/Desktop/Observations_Report.csv", sep=";")
#subset thomas
#data  <- read.csv("~/Documents/SunWeb/data2.csv", sep=";")

#LUDO DINGEN
#data <- read.csv("~/Desktop/Observations_Report kopie.csv", sep=";")
#data <- read.csv("~/Documents/Seminar master/Rscript/Geheim/data2.csv", sep=";")
#data <- read.csv("~/Documents/Seminar master/Rscript/Data/Observations_Report.csv", sep=";")


data <- as.data.table(data)



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


print(mean(training$CLICK))
print(mean(testing$CLICK))


print(length(unique(training$USERID)))
print(length(unique(testing$USERID)))

uniqueUsersTraining <-  unique(training$USERID)
testingOffers      <- unique(testing$OFFERID)


#Create data.table index for fast access of data
setkey(training, USERID)
setkey(testing, USERID)
setkey(data, USERID)

#Calculate clickrate for every User in training set
#Calculate them
#Clickrates <- calcClickRates(uniqueUser, training)
#fullset Thomas
Clickrates <- read.csv2("~/Documents/SunWeb/clickrate.csv", header=FALSE, sep="")
#subset Thomas
#Clickrates <- read.csv2("~/Documents/SunWeb/data2cr.csv", header=FALSE, sep="")


#TRAINING CLICK RATES AND REMOVE FROM TESTING
#thresholds  <- c(0.001,0.01,0.1,0.5,0.6,0.7,0.8,0.9)


thresholds <- 0
for(threshold in thresholds)
{
  selectedUsers    <- uniqueUsersTraining[Clickrates>threshold]    #1 User IDS
  nonselectedUsers <- uniqueUsersTraining[Clickrates<=threshold]   #1 User IDS
  training2        <- training[.(selectedUsers)]
  training2star    <- training[.(nonselectedUsers)]

  print(length(unique(training2$USERID)))
  
  #DATA ID PREP
  uniqueUser2     <- unique(training2$USERID)
  uniqueUser2star <- unique(training2star$USERID)
  uniqueOffer2    <- unique(training2$OFFERID)
  
  training2$USERID  <- mapvalues(training2$USERID, from=uniqueUser2, to=1:length(uniqueUser2))
  training2$OFFERID <- mapvalues(training2$OFFERID,from=uniqueOffer2,to=1:length(uniqueOffer2))
  
  lambda <-c(exp(4:0),0)
  for(l in lambda)
  {
    folds <- createFolds(training2$USERID, 5)
    for(fold in folds)
    {
      data.training   <- training2[-fold] 
      data.validation <- training2[fold]
      uniqueUser     <- unique(data.training$USERID)
      uniqueOffer    <- unique(data.training$OFFERID)
      
      data.training$USERID  <- mapvalues(data.training$USERID, from=uniqueUser, to=1:length(uniqueUser))
      data.training$OFFERID <- mapvalues(data.training$OFFERID,from=uniqueOffer,to=1:length(uniqueOffer))
      data.validation$USERID  <- mapvalues(data.validation$USERID, from=uniqueUser, to=1:length(uniqueUser))
      data.validation$OFFERID <- mapvalues(data.validation$OFFERID,from=uniqueOffer,to=1:length(uniqueOffer))
      
      X <- sparseMatrix(i = data.training$USERID, j = data.training$OFFERID, x = data.training$CLICK)
      
      maxIter <- 100
      e <- 0.0001
      r<-20
      result <- SoftImputeALS(X,l,maxIter,e,data.training,r)
      write.csv(result[[1]],file = paste0("A","cr",threshold,"r",r,"l",round(l,2),".csv"),row.names = FALSE,col.names = FALSE)
      write.csv(result[[2]],file = paste0("B","cr",threshold,"r",r,"l",round(l,2),".csv"),row.names = FALSE,col.names = FALSE)
      print("Found solution")
      print("started calculating MAE")
      MAE = MAE(result[[1]],result[[2]],data.validation,uniqueUser2,uniqueUser2star,uniqueOffer2)
      print("MAE calculated")
      print(MAE)
      
      
    }
  }
}


