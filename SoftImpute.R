SoftImpute <- function(X,lambda,maxIter = 100,e,training)
{
  #Create Initial Z matrix
  Zold <- matrix(0, nrow=nrow(X), ncol=ncol(X))
  
  #Loop untill maximum number of iterations
  for(iter in 1:maxIter)
  {
    print(paste0("Current iteration:  ", iter))
    
    #Do SVD
    start_time <- Sys.time()
    pinvZ <- Pinv(Zold,training)
    end_time <- Sys.time()
    print(end_time - start_time)
    
    start_time <- Sys.time()
    ewa<-soft.threshold(X+pinvZ)
    SVD<-irlba(X+pinvZ)
    U<- SVD$u
    end_time <- Sys.time()
    print(end_time - start_time)
    
    
    #Subtract Lambda
    D<- SVD$d-lambda
    V<- SVD$v
    
    #Construct 'new' Z.
    Znew <- U%*%diag(D)%*%t(V)
    
    #Calculate difference between old and new Z
    diff <- norm(Znew-Zold, type = "F")^2/norm(Zold, type = "F")^2
    print(paste0("Difference:     ", diff))
    
    if(diff< e)
    {
      print("Solution found")
      return(Znew)
    }
    Zold <- Znew
  }
}

