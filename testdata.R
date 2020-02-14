testdata <- function(n=200)
{
  #READ DATA
  #small test set
  
  data <- c(101,1,10,0)
  for(i in 1:70)
  {
    for(j in 1:10)
    {
      if(runif(1, min = 0, max = 1) >0.7)
      {
        if(runif(1, min = 0, max = 1) >0.6)
        {
          row <- c(i,1,j,1)
          data <- rbind(data,row)
        }
        else
        {
          row <- c(i,1,j,0)
          data <- rbind(data,row)
        }
      }
    }
  }
  for(i in 71:n)
  {
    for(j in 1:3)
    {
      
      row <- c(i,1,j,1)
      data <- rbind(data,row)
      
    }
    for(j in 2:4*2)
    {
      row <- c(i,1,j,0)
      data <- rbind(data,row)
    }
  }
  
  colnames(data) <- c("USERID", "MAILID", "OFFERID","CLICK")
  
  return(data)
}
