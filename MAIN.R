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
#data <- read.csv("~/Desktop/Observations_Report.csv", sep=";")
#subset thomas
data  <- read.csv("~/Documents/SunWeb/data2.csv", sep=";")

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

#create 5 partitions
data$USERID  <- as.factor(data$USERID)
intrain <- createDataPartition(data$USERID, p=0.7, list =F,times =5)
data$USERID  <- as.numeric(data$USERID)


#TRAINING CLICK RATES AND REMOVE FROM TESTING
#thresholds  <- c(0.001,0.01,0.1,0.5,0.6,0.7,0.8,0.9)
thresholds <- 0
MAEresults <-list()
counter <-1
for(threshold in thresholds)
{
  MAEsfoldMatrix <- numeric(0)
  for(fold in 1:size(intrain)[2])
  {
    
    training     <- as.data.table(data[intrain[,fold],])
    testing      <- as.data.table(data[-intrain[,fold],])
    
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
    #Clickrates <- read.csv2("~/Documents/SunWeb/clickrate.csv", header=FALSE, sep="")
    #subset Thomas
    Clickrates <- read.csv2("~/Documents/SunWeb/data2cr.csv", header=FALSE, sep="")
    
    
    
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
    
    maxIter <- 100
    e <- 0.0001
    lambda <-c(exp(4:0),0)
    r<-20
    results<-list()
    MAEs <-0
    count = 1
    for(l in lambda)
    {
      result <- SoftImputeALS(X,l,maxIter,e,training2,r)
      write.csv(result[[1]],file = paste0("A","cr",threshold,"fold",fold,"l",round(l,2),".csv"),row.names = FALSE)
      write.csv(result[[2]],file = paste0("B","cr",threshold,"fold",fold, "l",round(l,2),".csv"),row.names = FALSE)
      print("Found solution")
      MAEs[count] = MAE(result[[1]],result[[2]],testing,uniqueUser2,uniqueUser2star,uniqueOffer2)
      count = count + 1
    }
    
    MAEsfoldMatrix  <- cbind(MAEsfoldMatrix,MAEs)
  }
  
  foldedMAE <- rowMeans(MAEsfoldMatrix)
  MSEresults[[counter]] = foldedMAE
  write.csv(foldedMAE,file = paste0("MAE","cr",threshold,"csv"),row.names = FALSE)
  counter = counter + 1
  
}


