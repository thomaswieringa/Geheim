clickrates <- 0:19/20

folds<-2

finalTable <- 0
finalTableOnTrain <- 0
finalRankOnTrain <- 0
for(cr in clickrates){
  data<-0
  data2<-0
  rank <-0
  for(fold in 1:folds){
    
    data <- data+read.csv(file = paste0('~/Desktop/Results4/MAEscr',cr,'r20fold',fold,'.csv'))
    data2 <- data2 + read.csv(file = paste0('~/Desktop/Results4/MAEsonTrainedcr',cr,'r20fold',fold,'.csv'))
    rank <- rank + read.csv(file = paste0('~/Desktop/Results4/Rankscr',cr,'r20fold',fold,'.csv'))
  }
  data<-data/folds
  data2<-data2/folds
  rank <- rank/folds
  finalTable<- cbind(finalTable,data)
  finalTableOnTrain<- cbind(finalTableOnTrain,data2)
  finalRankOnTrain <- cbind(finalRankOnTrain,rank)
}

finalTable[,1] <- NULL
finalTableOnTrain[,1] <- NULL
finalRankOnTrain[,1] <- NULL

xas <- 0:19/20

plot(xas, finalTable[1,], type = "l", col = "black",  xlab = "Threshold", ylab = "MAE", ylim = range(0.0219,0.0230))
lines(xas, finalTable[2,], col = "purple")
lines(xas, finalTable[3,], col = "blue")
lines(xas, finalTable[4,], col = "red")
lines(xas, finalTable[5,], col = "green")
lines(xas, finalTable[6,], col = "orange")
legend(0.65,0.023,c(expression(paste(lambda, " = ", exp(4))), expression(paste(lambda, " = ", exp(3))), expression(paste(lambda, " = ", exp(2))), expression(paste(lambda, " = ", exp(1))), expression(paste(lambda, " = ", 1)), expression(paste(lambda, " = ", 0))), col = c("black", "purple", "blue", "red", "green", "orange"),lty = c(1,1,1,1,1,1))

plot(xas, finalTableOnTrain[1,], type = "l", col = "black",  xlab = "Threshold", ylab = "MAE", ylim = range(0.0219,1))
lines(xas, finalTableOnTrain[2,], col = "purple")
lines(xas, finalTableOnTrain[3,], col = "blue")
lines(xas, finalTableOnTrain[4,], col = "red")
lines(xas, finalTableOnTrain[5,], col = "green")
lines(xas, finalTableOnTrain[6,], col = "orange")
legend(0.3,0.023,c(expression(paste(lambda, " = ", exp(4))), expression(paste(lambda, " = ", exp(3))), expression(paste(lambda, " = ", exp(2))), expression(paste(lambda, " = ", exp(1))), expression(paste(lambda, " = ", 1)), expression(paste(lambda, " = ", 0))), col = c("black", "purple", "blue", "red", "green", "orange"),lty = c(1,1,1,1,1,1))


plot(xas, finalRankOnTrain[1,], type = "l", col = "black",  xlab = "Threshold", ylab = "MAE", ylim = range(0,20))
lines(xas, finalRankOnTrain[2,], col = "purple")
lines(xas, finalRankOnTrain[3,], col = "blue")
lines(xas, finalRankOnTrain[4,], col = "red")
lines(xas, finalRankOnTrain[5,], col = "green")
lines(xas, finalRankOnTrain[6,], col = "orange")
legend(0.5,17,c(expression(paste(lambda, " = ", exp(4))), expression(paste(lambda, " = ", exp(3))), expression(paste(lambda, " = ", exp(2))), expression(paste(lambda, " = ", exp(1))), expression(paste(lambda, " = ", 1)), expression(paste(lambda, " = ", 0))), col = c("black", "purple", "blue", "red", "green", "orange"),lty = c(1,1,1,1,1,1))

