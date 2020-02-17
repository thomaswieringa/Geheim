MAE2 <- function(A,B,testing)
{
  
  #predictions <-  (predictions(A,B,validTest)+1)/2
  predictions <-  predictions(A,B,testing)
  AE <- abs(predictions-validTest$CLICK)
  MAE <- mean(AE)
  
  print("maxpredonclick")
  print(max(predictions[validTest$CLICK==1]))
  
  return(MAE)
}
