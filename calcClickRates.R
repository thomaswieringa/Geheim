calcClickRates <- function(uniqueUserTraining, trainingData)
{
  Clickrates <- 0
  count<-1
  for(u in uniqueUserTraining)
  {
    userclicks <- trainingData[.(u)]
    click <- sum(userclicks$CLICK == 1)
    nonclick <- sum(userclicks$CLICK == 0)
    Clickrates[count] <- click/(click+nonclick)
    if(is.na(Clickrates[count]))
    {
      Clickrates[count] <-0
    }
    count <- count+1
  }
  return(Clickrates)
}



