clickrates <- 0:16/20

folds<-1

finalTable <- 0
finalTableOnTrain <- 0
finalRankOnTrain <- 0
for(cr in clickrates){
  data<-0
  data2<-0
  rank <-0
  for(fold in 1:folds){
    
    data <- data+read.csv(file = paste0('~/Desktop/Results7/MAEscr',cr,'r20fold',fold,'.csv'))
    data2 <- data2 + read.csv(file = paste0('~/Desktop/Results7/MAEsonTrainedcr',cr,'r20fold',fold,'.csv'))
    rank <- rank + read.csv(file = paste0('~/Desktop/Results7/Rankscr',cr,'r20fold',fold,'.csv'))
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



finalTableCB <- 0
finalTableOnTrainCB <- 0
finalRankOnTrainCB <- 0
for(cr in clickrates){
  dataCB<-0
  data2CB<-0
  rankCB <-0
  for(fold in 1:folds){
    
    dataCB <- dataCB + read.csv(file = paste0('~/Desktop/Results7/MAEs_CBcr',cr,'r20fold',fold,'.csv'))
    data2CB <- data2CB + read.csv(file = paste0('~/Desktop/Results7/MAEsonTrained_CBcr',cr,'r20fold',fold,'.csv'))
    rankCB <- rankCB + read.csv(file = paste0('~/Desktop/Results7/Ranks_CBcr',cr,'r20fold',fold,'.csv'))
  }
  dataCB<-dataCB/folds
  data2CB<-data2CB/folds
  rankCB <- rankCB/folds
  
  finalTableCB<- cbind(finalTableCB,dataCB)
  finalTableOnTrainCB<- cbind(finalTableOnTrainCB,data2CB)
  finalRankOnTrainCB <- cbind(finalRankOnTrainCB,rankCB)
}

finalTableCB[,1] <- NULL
finalTableOnTrainCB[,1] <- NULL
finalRankOnTrainCB[,1] <- NULL

xas <- 0:16/20

plot(xas, finalTable[1,], type = "l", col = "black",  xlab = "Threshold", ylab = "MAE", ylim = range(0.0222,0.0217))
lines(xas, finalTable[2,], col = "purple")
lines(xas, finalTable[3,], col = "blue")
lines(xas, finalTable[4,], col = "red")
lines(xas, finalTable[5,], col = "green")
lines(xas, finalTable[6,], col = "orange")
lines(xas, finalTable[7,], col = "pink")
legend(0.0,0.0219,c(expression(paste(lambda, " = ", exp(5))), expression(paste(lambda, " = ", exp(4))), expression(paste(lambda, " = ", exp(3))), expression(paste(lambda, " = ", exp(2))), expression(paste(lambda, " = ", exp(1))), expression(paste(lambda, " = ", 1)), expression(paste(lambda, " = ", 0))), col = c("black", "purple", "blue", "red", "green", "orange","pink"),lty = c(1,1,1,1,1,1))

plot(xas, finalTableOnTrain[1,], type = "l", col = "black",  xlab = "Threshold", ylab = "MAE", ylim = range(0.0219,1))
lines(xas, finalTableOnTrain[2,], col = "purple")
lines(xas, finalTableOnTrain[3,], col = "blue")
lines(xas, finalTableOnTrain[4,], col = "red")
lines(xas, finalTableOnTrain[5,], col = "green")
lines(xas, finalTableOnTrain[6,], col = "orange")
lines(xas, finalTableOnTrain[7,], col = "pink")
legend(0.0219,0.5,c(expression(paste(lambda, " = ", exp(5))), expression(paste(lambda, " = ", exp(4))), expression(paste(lambda, " = ", exp(3))), expression(paste(lambda, " = ", exp(2))), expression(paste(lambda, " = ", exp(1))), expression(paste(lambda, " = ", 1)), expression(paste(lambda, " = ", 0))), col = c("black", "purple", "blue", "red", "green", "orange","pink"),lty = c(1,1,1,1,1,1))


plot(xas, finalRankOnTrain[2,], type = "l", col = "black",  xlab = "Threshold", ylab = "Rank", ylim = range(0,20))
lines(xas, finalRankOnTrain[3,], col = "purple")
lines(xas, finalRankOnTrain[4,], col = "blue")
lines(xas, finalRankOnTrain[5,], col = "red")
lines(xas, finalRankOnTrain[6,], col = "green")
lines(xas, finalRankOnTrain[7,], col = "orange")
lines(xas, finalRankOnTrain[8,], col = "pink")
legend(0.6,17,c(expression(paste(lambda, " = ", exp(5))), expression(paste(lambda, " = ", exp(4))), expression(paste(lambda, " = ", exp(3))), expression(paste(lambda, " = ", exp(2))), expression(paste(lambda, " = ", exp(1))), expression(paste(lambda, " = ", 1)), expression(paste(lambda, " = ", 0))), col = c("black", "purple", "blue", "red", "green", "orange","pink"),lty = c(1,1,1,1,1,1))


#CBMF


plot(xas, finalTableCB[1,], type = "l", col = "black",  xlab = "Threshold", ylab = "MAE", ylim = range(0.0222,0.0217))
lines(xas, finalTableCB[2,], col = "purple")
lines(xas, finalTableCB[3,], col = "blue")
lines(xas, finalTableCB[4,], col = "red")
lines(xas, finalTableCB[5,], col = "green")
lines(xas, finalTableCB[6,], col = "orange")
lines(xas, finalTableCB[7,], col = "pink")
legend(0.0,0.0219,c(expression(paste(lambda, " = ", exp(5))), expression(paste(lambda, " = ", exp(4))), expression(paste(lambda, " = ", exp(3))), expression(paste(lambda, " = ", exp(2))), expression(paste(lambda, " = ", exp(1))), expression(paste(lambda, " = ", 1)), expression(paste(lambda, " = ", 0))), col = c("black", "purple", "blue", "red", "green", "orange","pink"),lty = c(1,1,1,1,1,1))

plot(xas, finalTableOnTrainCB[1,], type = "l", col = "black",  xlab = "Threshold", ylab = "MAE", ylim = range(0.0219,1))
lines(xas, finalTableOnTrainCB[2,], col = "purple")
lines(xas, finalTableOnTrainCB[3,], col = "blue")
lines(xas, finalTableOnTrainCB[4,], col = "red")
lines(xas, finalTableOnTrainCB[5,], col = "green")
lines(xas, finalTableOnTrainCB[6,], col = "orange")
lines(xas, finalTableOnTrainCB[7,], col = "pink")
legend(0.0219,0.5,c(expression(paste(lambda, " = ", exp(5))), expression(paste(lambda, " = ", exp(4))), expression(paste(lambda, " = ", exp(3))), expression(paste(lambda, " = ", exp(2))), expression(paste(lambda, " = ", exp(1))), expression(paste(lambda, " = ", 1)), expression(paste(lambda, " = ", 0))), col = c("black", "purple", "blue", "red", "green", "orange","pink"),lty = c(1,1,1,1,1,1))











#Threshold = -1
folds<-2
fulldata<-0
fulldata2<-0
fullrank <-0
cr <- -1

for(fold in 1:folds){
  
  fulldata <- fulldata+read.csv(file = paste0('~/Documents/Seminar master/Rscript/Geheim/MAEscr',cr,'r20fold',fold,'.csv'))
  fulldata2 <- fulldata2 + read.csv(file = paste0('~/Documents/Seminar master/Rscript/Geheim/MAEsonTrainedcr',cr,'r20fold',fold,'.csv'))
  fullrank <- fullrank + read.csv(file = paste0('~/Documents/Seminar master/Rscript/Geheim/Rankscr',cr,'r20fold',fold,'.csv'))
}

fulldatarev <- as.data.frame(Rev(fulldata,margin = 1))
plot(lambda,fulldatarev$`Rev(fulldata, margin = 1)` , type = "l", col = "black",  xlab = expression(lambda), ylab = "MAE", ylim = range(0.0220,0.0263),xlim = range(0,180),lwd = 1.5)
