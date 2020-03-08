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


data <- read.csv("~/Desktop/Observations_Report.csv", sep=";")
Offer <- read.csv2("~/Desktop/OfferDetails_Cleaned.csv")

#DATA PARTITIONING
data$USERID  <- as.factor(data$USERID)
set.seed(1908)
options(warn=-1)
intrain      <- createDataPartition(data$USERID, p = 0.01, list = F)
options(warn=0)
data$USERID  <- as.numeric(data$USERID)
data2              <- as.data.table(data[intrain,])
finaltesting      <- as.data.table(data[-intrain,])

Phi <- Offer[,c(3,8:14)]

data3 <- data2

data3 <- as.data.table(data3)

#DATA ID PREP
uniqueUser   <- unique(data3$USERID)
uniqueOffer  <- unique(data3$OFFERID)
data3$USERID  <- mapvalues(data3$USERID, from=uniqueUser, to=1:length(uniqueUser))
data3$OFFERID <- mapvalues(data3$OFFERID,from=uniqueOffer,to=1:length(uniqueOffer))
userIDs      <- 1:length(uniqueUser)

Phi <- as.data.table(Phi)

Phi$OFFERID <- mapvalues(Phi$OFFERID,from=uniqueOffer,to=1:length(uniqueOffer))

Phi <- Phi[(Phi$OFFERID <= 2094),]


PHI <- Phi[order(Phi$OFFERID),]

PHI <- unique(PHI)
PHI <- PHI[,-1] 

PHI <- as.matrix(PHI)

X <- sparseMatrix(i = data3$USERID,
                  j = data3$OFFERID,
                  x = data3$CLICK)


res <- PWSoftImputeALS(X,lambda2 = 1,maxIter = 100,e = 0.001,data3 = data3, r = 20, PHI = as.matrix(PHI))

 