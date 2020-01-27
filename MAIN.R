library(Matrix)
library(RGCCA)
library(irlba)
library(scales)
library(caret)
library(plyr)

#READ DATA
data <- read.csv("~/Desktop/SUNWEB Data/Observations_Report kopie.csv", sep=";")

#DATA RESCALE
data[,4]=(data[,4]*2)-1

#DATA ID PREP
uniqueUser <- unique(data$USERID)
uniqueOffer <- unique(data$OFFERID)
data$USERID  <- mapvalues(data$USERID, from=uniqueUser, to=1:length(uniqueUser))
data$OFFERID <- mapvalues(data$OFFERID,from=uniqueOffer,to=1:length(uniqueOffer))



#DATA PARTITIONING
intrain  <- createDataPartition(y=data$CLICK,p=0.9,list=FALSE)
training <- data[intrain,]
testing  <- data[-intrain,]

#TRAINING CLICK RATES AND REMOVE FROM TESTING
thresholds  <- c(0.001,0.01,0.1,0.5,0.6,0.7,0.8,0.9)
MSEresults <-0
counter <-1


for(threshold in thresholds)
{
  print("Trying first threshold")
Clickrates <- 0
data2  <- data.frame()
for(u in 1:length(uniqueUser))
{
  click <- sum(training[training[,1]==u,4]==1)
  nonclick <- sum(training[training[,1]==u,4]==-1)
  Clickrates[u] <- click/(click+nonclick)
  if(is.na(Clickrates[u]))
  {
    Clickrates[u] <-0
  }

  if(Clickrates[u]>threshold)
  {
    data2 <- rbind(data2, data[data[,1]==u,])
  }
}

print("Clickrates")
print(Clickrates)
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



