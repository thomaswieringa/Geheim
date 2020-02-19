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

#create 3 partitions
data$USERID  <- as.factor(data$USERID)
intrain <- list()
set.seed(12)
intrain[[1]]   <- createDataPartition(data$USERID, p = 0.7, list =F)
set.seed(5343)
intrain[[2]]   <- createDataPartition(data$USERID, p = 0.7, list =F)
set.seed(6728)
intrain[[3]]   <- createDataPartition(data$USERID, p = 0.7, list =F)
data$USERID  <- as.numeric(data$USERID)


#Calculate clickrate for every User in training set
#Calculate them
#Clickrates <- calcClickRates(uniqueUser, training)
#fullset Thomas
#Clickrates <- read.csv2("~/Documents/SunWeb/clickrate.csv", header=FALSE, sep="")
#subset Thomas
Clickrates <- read.csv2("~/Documents/SunWeb/data2cr.csv", header=FALSE, sep="")

#TRAINING CLICK RATES AND REMOVE FROM TESTING
#thresholds  <- c(-1,0,1:10/20)
thresholds <- 0
MAEresults <-list()
counter <-1
for(threshold in thresholds)
{
  MAEsfoldMatrix        <- numeric(0)
  MAEsOnTrainfoldMatrix <- numeric(0)
  for(fold in 2:2)
  {
    datas <- DataPartition(Clickrates , data, intrain[[fold]])
    maxIter <- 100
    e <- 0.0001
    #lambda <-c(exp(4:0),0)
    lambda <- 0
    r<-20
    results<-list()
    MAEs <-0
    MAEsOnTrain <-0
    count = 1
    for(l in lambda)
    {
      result <- SoftImputeALS(datas[[1]],l,maxIter,e,datas[[2]],r)
      #write.csv(result[[1]],file = paste0("A","cr",threshold,"fold",fold,"l",round(l,2),".csv"),row.names = FALSE)
      #write.csv(result[[2]],file = paste0("B","cr",threshold,"fold",fold, "l",round(l,2),".csv"),row.names = FALSE)
      print("Found solution")
      MAEresult             <- MAE(result[[1]],result[[2]],testing,  datas[[3]], datas[[4]], datas[[5]])
      MAEs[count]        <- MAEresult[[1]]
      MAEsOnTrain[count] <- MAEresult[[2]]
      count = count + 1
    }
    
    MAEsfoldMatrix  <- cbind(MAEsfoldMatrix,MAEs)
    MAEsOnTrainfoldMatrix <- cbind(MAEsOnTrainfoldMatrix,MAEsOnTrain)
    
  }
  
  foldedMAE <- rowMeans(MAEsfoldMatrix)
  foldedMAEonTrain <- rowMeans(MAEsOnTrainfoldMatrix)
  
  write.csv(foldedMAE,        file = paste0("MAE_","TH",threshold,"csv"), row.names = FALSE)
  write.csv(foldedMAEonTrain, file = paste0("MAEontrain_","TH",threshold,"csv"), row.names = FALSE)
  counter = counter + 1
}


