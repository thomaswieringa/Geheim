library(Matrix)
library(RGCCA)
library(irlba)
library(scales)
library(caret)


#READ DATA
data <- read.csv("~/Desktop/SUNWEB Data/Observations_Report kopie.csv", sep=";")

#DATA ID PREP
uniqueUser <- unique(data$USERID)
uniqueOffer <- unique(data$OFFERID)
for(j in 1:length(uniqueUser))
{
  data[data[,1]==uniqueUser[j],1]=j
}

for(j in 1:length(uniqueOffer))
{
  data[data[,3]==uniqueOffer[j],3]=j
}

#DATA RESCALE
data[,4]=(data[,4]*2)-1


#DATA PARTITIONING
intrain<-createDataPartition(y=data$CLICK,p=0.9,list=FALSE)
training<-data[intrain,]
testing <- data[-intrain,]

#Create Sparse Matrix
X <- sparseMatrix(i = training$USERID,
                         j = training$OFFERID,
                         x = training$CLICK)

maxIter <- 100
e <- 0.1
lambda <-0:5/2
results<-list()
count = 1
for(l in lambda)
{
  result <- SoftImpute(X,l,maxIter,e)
  results[[count]] = result
  print("Found solution")
  count=count+1
}

MSEs <-0
count = 1
for(i in 1:length(results))
{
  MSEs[count] = MSE(results[[i]],testing)
  count = count +1
}

plot(MSEs)

