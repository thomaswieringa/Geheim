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
library(MASS)


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

#OFFERS
Offer <- read.csv2("~/Desktop/Offerdetails_extensie.csv")


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

#PHI PREP
Phi <- Offer[,c(2,7:25,28:50)]
Phi <- as.data.table(Phi)
Phi$OFFERID <- mapvalues(Phi$OFFERID,from=uniqueOffer,to=1:length(uniqueOffer))
Phi <- Phi[(Phi$OFFERID <= length(uniqueOffer)),]
Phi <- Phi[!duplicated(Phi[,'OFFERID']),]
Phi$OFFERID <- mapvalues(Phi$OFFERID,from=uniqueOfferData,to=1:length(uniqueOfferData))
Phi <- Phi[(Phi$OFFERID <= length(uniqueOfferData)),]
Phi <- Phi[!duplicated(Phi[,'OFFERID']),]
PHI <- Phi[order(Phi$OFFERID),]



for(i in c(1))
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
  #thresholds  <- c(0:16/20)
  thresholds <- c(0:19/20)
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
    
    
    #PHI PREP
    PHI2 <- PHI
    PHI2$OFFERID <- mapvalues(PHI2$OFFERID,from=uniqueOffer2,to=1:length(uniqueOffer2))
    PHI2 <- PHI2[(PHI2$OFFERID <= length(uniqueOffer2)),]
    PHI2 <- PHI2[!duplicated(PHI2[,'OFFERID']),]
    PHI2 <- PHI2[order(PHI2$OFFERID),]
    PHI2 <- PHI2[,-c(1)] 
    PHI2 <- as.matrix(PHI2)
    
    
    #CREATE SPARSE MATRIX
    
    training2 <- as.data.frame(training2)
    training2 <- training2[!duplicated(training2[c("USERID","OFFERID")]),]
    
    X <- sparseMatrix(i = training2$USERID,
                      j = training2$OFFERID,
                      x = training2$CLICK)
    
    
    maxIter <- 100
    e <- 0.001
    lambda <-c(exp(5:0),0)
    r<-20
    results<-list()
    MAEs <-0
    MAEsNUL <-0
    MAEs_CB <-0
    MAEsTrained <-0
    MAEsTrainedNUL <-0
    MAEsTrained_CB <-0
    Ranks <- 0
    Ranks_CB <- 0
    count = 1
    
    for(l in lambda)
    {
      #SoftImpute
      result              <- SoftImputeALS(X,l,maxIter,e,training2,r)
      MAEresult           <-  RMSE(result[[1]],result[[2]],testing,uniqueUser2,uniqueUser2star,uniqueOffer2,threshold,Clickrates)
      MAEresultnul           <-  RMSEnul(result[[1]],result[[2]],testing,uniqueUser2,uniqueUser2star,uniqueOffer2,threshold,Clickrates)
      
      MAEs[count]         <-  MAEresult[[1]]
      MAEsTrained[count]  <-  MAEresult[[2]]
      MAEsNUL[count]         <-  MAEresultnul[[1]]
      MAEsTrainedNUL[count]  <-  MAEresultnul[[2]]
      Ranks[count]        <-  result[[3]]
      
      #CB SoftImpute
      #result_CB              <- CBSoftImputeALS(X,l,maxIter,e,training2,r,PHI2)
      #MAEresult_CB           <-  MAE(result_CB[[1]],result_CB[[2]],testing,uniqueUser2,uniqueUser2star,uniqueOffer2)
      #MAEs_CB[count]         <-  MAEresult_CB[[1]]
      #MAEsTrained_CB[count]  <-  MAEresult_CB[[2]]
      #Ranks_CB[count]        <-  result_CB[[3]]
      count=count+1
      
    }
    
    print("Finished one lambda set")
    
    #Write for SoftImpute
    write.csv(MAEs,file = paste0("MAEs","cr",threshold,"r",r,"fold",i,".csv"),row.names = FALSE)
    write.csv(MAEsTrained,file = paste0("MAEsonTrained","cr",threshold,"r",r,"fold",i,".csv"),row.names = FALSE)
    write.csv(MAEsNUL,file = paste0("MAEsnul","cr",threshold,"r",r,"fold",i,".csv"),row.names = FALSE)
    write.csv(MAEsTrainedNUL,file = paste0("MAEsonTrainednul","cr",threshold,"r",r,"fold",i,".csv"),row.names = FALSE)
    write.csv(Ranks,file = paste0("Ranks","cr",threshold,"r",r,"fold",i,".csv"),row.names = FALSE)
    
    #Write for CB SoftImpute
    write.csv(MAEs_CB,file = paste0("MAEs_CB","cr",threshold,"r",r,"fold",i,".csv"),row.names = FALSE)
    write.csv(MAEsTrained_CB,file = paste0("MAEsonTrained_CB","cr",threshold,"r",r,"fold",i,".csv"),row.names = FALSE)
    write.csv(Ranks_CB,file = paste0("Ranks_CB","cr",threshold,"r",r,"fold",i,".csv"),row.names = FALSE)
    
    counter <- counter+1
  }
}


#OPNIEUW TRAINEN MET OPTIMALE LAMBDA EN THRESHOLD.
bestThreshCF    <- 0.45
bestLCF         <- exp(1)
bestThreshCBCF  <-  0.45
bestLCBCF       <- 0

setkey(data2, USERID)
uniqueUserData <- unique(data2$USERID)
ClickratesFinal <- calcClickRates(uniqueUserData, data2)

#part for CF

selectedUsers    <- uniqueUserData[ClickratesFinal>bestThreshCF]    #1 User IDS
nonselectedUsers <- uniqueUserData[ClickratesFinal<=bestThreshCF]   #1 User IDS
training2        <- data2[.(selectedUsers)]
training2star    <- data2[.(nonselectedUsers)]


#DATA ID PREP
uniqueUser2     <- unique(training2$USERID)
uniqueUser2star <- unique(training2star$USERID)
uniqueOffer2    <- unique(training2$OFFERID)

training2$USERID  <- mapvalues(training2$USERID, from=uniqueUser2, to=1:length(uniqueUser2))
training2$OFFERID <- mapvalues(training2$OFFERID,from=uniqueOffer2,to=1:length(uniqueOffer2))


#PHI PREP
PHI2 <- PHI
PHI2$OFFERID <- mapvalues(PHI2$OFFERID,from=uniqueOffer2,to=1:length(uniqueOffer2))
PHI2 <- PHI2[(PHI2$OFFERID <= length(uniqueOffer2)),]
PHI2 <- PHI2[!duplicated(PHI2[,'OFFERID']),]
PHI2 <- PHI2[order(PHI2$OFFERID),]
PHI2 <- PHI2[,-c(1)] 
PHI2 <- as.matrix(PHI2)


#CREATE SPARSE MATRIX

training2 <- as.data.frame(training2)
training2 <- training2[!duplicated(training2[c("USERID","OFFERID")]),]

X         <- sparseMatrix(i = training2$USERID,
                          j = training2$OFFERID,
                          x = training2$CLICK)


maxIter <- 100
e <- 0.001
r<-20

result                     <-  SoftImputeALS(X,bestLCF,maxIter,e,training2,r)
FINALMAEresultCF           <-  MAE(result[[1]],result[[2]],finaltesting,uniqueUser2,uniqueUser2star,uniqueOffer2)




#part for CBCF

selectedUsers    <- uniqueUserData[ClickratesFinal>bestThreshCBCF]    #1 User IDS
nonselectedUsers <- uniqueUserData[ClickratesFinal<=bestThreshCBCF]   #1 User IDS
training2        <- data2[.(selectedUsers)]
training2star    <- data2[.(nonselectedUsers)]


#DATA ID PREP
uniqueUser2     <- unique(training2$USERID)
uniqueUser2star <- unique(training2star$USERID)
uniqueOffer2    <- unique(training2$OFFERID)

training2$USERID  <- mapvalues(training2$USERID, from=uniqueUser2, to=1:length(uniqueUser2))
training2$OFFERID <- mapvalues(training2$OFFERID,from=uniqueOffer2,to=1:length(uniqueOffer2))


#PHI PREP
PHI2 <- PHI
PHI2$OFFERID <- mapvalues(PHI2$OFFERID,from=uniqueOffer2,to=1:length(uniqueOffer2))
PHI2 <- PHI2[(PHI2$OFFERID <= length(uniqueOffer2)),]
PHI2 <- PHI2[!duplicated(PHI2[,'OFFERID']),]
PHI2 <- PHI2[order(PHI2$OFFERID),]
PHI2 <- PHI2[,-c(1)] 
PHI2 <- as.matrix(PHI2)


#CREATE SPARSE MATRIX

training2 <- as.data.frame(training2)
training2 <- training2[!duplicated(training2[c("USERID","OFFERID")]),]

X <- sparseMatrix(i = training2$USERID,
                  j = training2$OFFERID,
                  x = training2$CLICK)


maxIter <- 100
e <- 0.001
r<-20


result_CB              <- CBSoftImputeALS(X,bestLCBCF,maxIter,e,training2,r,PHI2)
MAEresult_CB           <-  MAE(result_CB[[1]],result_CB[[2]],finaltesting ,uniqueUser2,uniqueUser2star,uniqueOffer2)


write.csv2(MAEresult_CB,file="PHI.csv",row.names=FALSE)
phiMatrix <- as.matrix(result_CB[[4]])

cosim <- matrix(0,42,42)

row.names(cosim) <- colnames(PHI2)
colnames(cosim) <- colnames(PHI2)

for(i in 1:42)
{
  for(j in 1:42)
  {
    vec1 <- as.vector(phiMatrix[i,])
    vec2 <- as.vector(phiMatrix[j,])
    
    cosim[i,j] <- (vec1%*%vec2)/(Norm(vec1,2)*Norm(vec2,2))
  }
}



