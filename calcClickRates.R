calcClickRates <- function(uniqueUser, training)
{
  Clickrates <- 0
  for(u in 1:length(uniqueUser))
  {
    userclicks <- training[.(u)]
    click <- sum(userclicks$CLICK == 1)
    nonclick <- sum(userclicks$CLICK == 0)
    Clickrates[u] <- click/(click+nonclick)
    if(is.na(Clickrates[u]))
    {
      Clickrates[u] <-0
    }
  }
  return(Clickrates)
}
