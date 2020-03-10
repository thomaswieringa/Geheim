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
set.seed(1908)
options(warn=-1)
intrain      <- createDataPartition(data$USERID, p = 0.8, list = F)
options(warn=0)
data$USERID  <- as.numeric(data$USERID)
data2              <- as.data.table(data[intrain,])
finaltesting      <- as.data.table(data[-intrain,])


uniqueUserData  <- unique(data2$USERID)
uniqueOfferData <- unique(data2$OFFERID)    

#MAP OFFER IDS values
data2$OFFERID                  <- mapvalues(data2$OFFERID, from=uniqueOfferData, to=1:length(uniqueOfferData))
finaltesting$OFFERID           <- mapvalues(finaltesting$OFFERID, from=uniqueOfferData, to=1:length(uniqueOfferData))



holdCount <- 1
for(i in 1:5)
{
  #DATA PARTITIONING
  data2$USERID  <- as.factor(data2$USERID)
  set.seed(i*2)
  options(warn=-1)
  intrain      <- createDataPartition(data2$USERID, p = 0.8, list = F)
  options(warn=0)
  data2$USERID  <- as.numeric(data2$USERID)
  training     <- as.data.table(data2[intrain,])
  testing      <- as.data.table(data2[-intrain,])
  
  print(mean(training$CLICK))
  print(mean(testing$CLICK))
  
  print(length(unique(training$USERID)))
  print(length(unique(testing$USERID)))
  
  uniqueUsersTraining <-  unique(training$USERID)
  testingOffers       <-  unique(testing$OFFERID)
  
  #Create data.table index for fast access of data
  setkey(training, USERID)
  setkey(testing, USERID)
  setkey(data2, USERID)
  
  #Calculate clickrate for every User in training set
  Clickrates <- calcClickRates(uniqueUsersTraining, training)
  
  #TRAINING CLICK RATES AND REMOVE FROM TESTING
  #thresholds  <- c(-1,0:19/20)
  threshold  <- 0.8
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
    uniqueUser2     <- unique(training2$USERID)
    uniqueUser2star <- unique(training2star$USERID)
    uniqueOffer2    <- unique(training2$OFFERID)
    
    training2$USERID  <- mapvalues(training2$USERID, from=uniqueUser2, to=1:length(uniqueUser2))
    training2$OFFERID <- mapvalues(training2$OFFERID,from=uniqueOffer2,to=1:length(uniqueOffer2))
    
    #CREATE SPARSE MATRIX
    
    X <- sparseMatrix(i = training2$USERID,
                      j = training2$OFFERID,
                      x = training2$CLICK)
    
    
    maxIter <- 1000
    e <- 0.001
    #lambda <-c(exp(4:0),0)
    lambda <- exp(1)
    r<-20
    results<-list()
    MAEs <-0
    MAEsTrained <-0
    Ranks <- 0
    count = 1
    
  
    for(l in lambda)
    {
      result              <- SoftImputeALS(X,l,maxIter,e,training2,r)
      resultP             <- Ssimpute.als(X,J=r,thresh=e,lambda=l,maxit=maxIter,trace.it = TRUE)
      ABT                 <- result[[1]]%*%t(result[[2]])
      ABTP                <- resultP[[1]]%*%diag(sqrt(resultP[[2]]))%*%t(resultP[[3]]%*%diag(sqrt(resultP[[2]])))
      MAEresultpackage    <-  MAE(resultP[[1]]%*%diag(sqrt(resultP[[2]])),resultP[[3]]%*%diag(sqrt(resultP[[2]])),testing,uniqueUser2,uniqueUser2star,uniqueOffer2)
      MAEresult           <-  MAE(result[[1]],result[[2]],testing,uniqueUser2,uniqueUser2star,uniqueOffer2)
      MAEs[count]         <- MAEresult[[1]]
      MAEsTrained[count]  <- MAEresult[[2]]
      Ranks[count]        <- result[[3]]
      count=count+1
    }
  
    print("MAE calculated")
    write.csv(MAEs,file = paste0("MAEs","cr",threshold,"r",r,"fold",holdCount,".csv"),row.names = FALSE)
    write.csv(MAEsTrained,file = paste0("MAEsonTrained","cr",threshold,"r",r,"fold",holdCount,".csv"),row.names = FALSE)
    write.csv(Ranks,file = paste0("Ranks","cr",threshold,"r",r,"fold",holdCount,".csv"),row.names = FALSE)
    counter <- counter+1
  }
  holdCount <- holdCount + 1
}

#OPNIEUW TRAINEN MET OPTIMALE LAMBDA ZONDER THRESHOLD.
#OPNIEUW TRAINEN MET OPTIMALE LAMBDA EN THRESHOLD.








