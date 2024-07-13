## Input
#
## Output
#
## Narrative
#

Snell_Transformation <- function(x) {
  NumCategories <- length(unique(x))
  
  boundaryVec <- rep(NA,NumCategories+1)
  boundaryVec[1] <- -Inf
  boundaryVec[2] <- 0 # arbitrary but valid
  boundaryVec[NumCategories+1] <- Inf  
  
  boundaryDiffVec <- rep(NA,NumCategories-2)
  # start counting backwards
  
  countVec <- table(x)
  cumulativePropVec <- cumsum(countVec) / sum(countVec)
  propVec <- countVec / sum(countVec)
  
  boundaryDiffVec[NumCategories-2] <- log(countVec[NumCategories-1] / ( (countVec[NumCategories-1] + countVec[NumCategories])*cumulativePropVec[NumCategories-1] - countVec[NumCategories])  + 1)
  for (j in (NumCategories-2):2) {
    tempCount <- countVec[j]
    tempCountPrev <- countVec[j+1]
    
    tempProp <- cumulativePropVec[j]
    
    tempPrevDiff <- boundaryDiffVec[j]
    
    tempP1 <- (tempCount + tempCountPrev) * tempProp
    tempP2 <- tempCountPrev / (exp(tempPrevDiff) - 1)
    
    boundaryDiffVec[j-1] <- log(tempCount / (tempP1 + tempP2 - tempCount) + 1)
    
  }
  
  ## now calculate all of the boundaries
  for (j in 2:(NumCategories-1)) {
    boundaryVec[j+1] <- boundaryDiffVec[j-1] + boundaryVec[j]
  }
  
  ### now define the midpoints for all categories
  midPointVec <- rep(NA,NumCategories)
  midPointVec[1] <- ifelse(propVec[1] < 0.10,boundaryVec[2] - 1,boundaryVec[2] - 1.1)
  midPointVec[NumCategories] <- ifelse(propVec[NumCategories] < 0.10,boundaryVec[NumCategories] + 1,
                                       boundaryVec[NumCategories] + 1.1)
  for (i in 2:(NumCategories-1)) {
    midPointVec[i] <- (boundaryVec[i] + boundaryVec[i+1])/2
  }

  res <- midPointVec[as.numeric(x)]
  return(res)
}