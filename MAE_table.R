MAE_onfull <- read_excel("~/Documents/Seminar master/MAE_onfull.xlsx")
MAE_onfull <- as.matrix(MAE_onfull)
MAE_onfull <- data.frame(MAE_onfull[,-1], row.names = MAE_onfull[,1])
MAE_onfull <- as.matrix(MAE_onfull)
MAE_onfull <- matrix(as.numeric(MAE_onfull), 6, 11)
MAE_onfull_table <- format(MAE_onfull, scientific=TRUE)

MAE_ontrained <- read_excel("~/Documents/Seminar master/MAE_ontrained.xlsx")
MAE_ontrained <- as.matrix(MAE_ontrained)
MAE_ontrained <- data.frame(MAE_ontrained[,-1], row.names = MAE_ontrained[,1], stringsAsFactors = FALSE)
MAE_ontrained <- as.matrix(MAE_ontrained)
MAE_ontrained <- matrix(as.numeric(MAE_ontrained), 6, 11)
MAE_ontrained_table <- format(MAE_ontrained, scientific=TRUE)

xas <- c(0, 1:10/20)

plot(xas, MAE_onfull[1,], type = "l", col = "black",  xlab = "Threshold", ylab = "MAE", ylim = range(0.0219,0.0230))
lines(xas, MAE_onfull[2,], col = "purple")
lines(xas, MAE_onfull[3,], col = "blue")
lines(xas, MAE_onfull[4,], col = "red")
lines(xas, MAE_onfull[5,], col = "green")
lines(xas, MAE_onfull[6,], col = "orange")
legend(0.3,0.023,c(expression(paste(lambda, " = ", exp(4))), expression(paste(lambda, " = ", exp(3))), expression(paste(lambda, " = ", exp(2))), expression(paste(lambda, " = ", exp(1))), expression(paste(lambda, " = ", 1)), expression(paste(lambda, " = ", 0))), col = c("black", "purple", "blue", "red", "green", "orange"),lty = c(1,1,1,1,1,1))

plot(xas, MAE_ontrained[1,], type = "l", col = "black",  xlab = "Threshold", ylab = "MAE", ylim = range(0.0219,0.0280))
lines(xas, MAE_ontrained[2,], col = "purple")
lines(xas, MAE_ontrained[3,], col = "blue")
lines(xas, MAE_ontrained[4,], col = "red")
lines(xas, MAE_ontrained[5,], col = "green")
lines(xas, MAE_ontrained[6,], col = "orange")
legend(0.3,0.027,c(expression(paste(lambda, " = ", exp(4))), expression(paste(lambda, " = ", exp(3))), expression(paste(lambda, " = ", exp(2))), expression(paste(lambda, " = ", exp(1))), expression(paste(lambda, " = ", 1)), expression(paste(lambda, " = ", 0))), col = c("black", "purple", "blue", "red", "green", "orange"),lty = c(1,1,1,1,1,1))



