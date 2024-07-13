require(here)
require(dplyr)

WEV <- read.csv(file = here("data/WEV.csv"),header=TRUE)

WEV_Current <- droplevels(subset(WEV,YEAR >= 2016))
WEV_Current <- droplevels(subset(WEV_Current,LR_SELF %in% as.character(1:9)))

## description of Snell's transformation of Likert scale variables 
#   into approximate thermometer scale scores
# https://www.m-hikari.com/ams/ams-password-2007/ams-password57-60-2007/wuchienhoAMS57-60-2007.pdf?q=tfa

## go from observed proportions in each category, to estimated category boundaries
#   to estimated feeling scores. The formulas are simplified because we only have one question.

NumCategories <- length(unique(WEV_Current$LR_SELF))

boundaryVec <- rep(NA,NumCategories+1)
boundaryVec[1] <- -Inf
boundaryVec[2] <- 0 # arbitrary but valid
boundaryVec[NumCategories+1] <- Inf

boundaryDiffVec <- rep(NA,NumCategories-2)
# start counting backwards

countVec <- table(WEV_Current$LR_SELF)
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
WEV_Current$LR_SELF_NUM <- as.numeric(WEV_Current$LR_SELF)
WEV_Current$LR_SELF_SNELL <- midPointVec[WEV_Current$LR_SELF_NUM]


basicVarNames <- c("GENDER","AGE","EDUCATION","RELIGION","UNION",
                   "INTEREST_3","NEWSPAPERS","TELEVISION","INTERNET",
                   "VAA","FACEBOOK_USE","FACEBOOK_POL",
                   "ECONOMY","TRUST","IMMIGRATION",
                   "CNAME")
modelDF <- WEV_Current[,basicVarNames]
modelDF$LR_SELF_SNELL <- as.numeric(WEV_Current$LR_SELF_SNELL)
modelDF$AGE <- as.numeric(modelDF$AGE)

modelDF <- droplevels(subset(modelDF,GENDER != ""))
modelDF <- droplevels(subset(modelDF,EDUCATION != ""))
modelDF <- droplevels(subset(modelDF,UNION != ""))
modelDF <- droplevels(subset(modelDF,RELIGION != ""))

basicM <- lm(LR_SELF_SNELL ~ .,data = modelDF)

m0 <- lm(LR_SELF_SNELL ~ AGE*GENDER*EDUCATION*UNION*RELIGION,data = modelDF)
