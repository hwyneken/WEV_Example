require(here)


modelDF <- readRDS(file = here("data/WEV_Full_Model_Data.RDS"))


## Define formulas for the working model using the transformed response (recommended) and the original response
workingFormulaStr <- "GENDER + EDU_BINARY + RELIGION_BINARY + UNION + ECONOMY_BINARY + IMMIGRATION + CNAME"
workingFormulaStr <- paste0(workingFormulaStr," + GENDER:EDU_BINARY + GENDER:RELIGION_BINARY + GENDER:ECONOMY_BINARY + GENDER:IMMIGRATION + GENDER:UNION")
workingFormulaStr <- paste0(workingFormulaStr," + EDU_BINARY:RELIGION_BINARY + EDU_BINARY:ECONOMY_BINARY + EDU_BINARY:IMMIGRATION + EDU_BINARY:UNION")
workingFormulaStr <- paste0(workingFormulaStr," + RELIGION_BINARY:ECONOMY_BINARY + RELIGION_BINARY:IMMIGRATION + RELIGION_BINARY:UNION") 
workingFormulaStr <- paste0(workingFormulaStr," + ECONOMY_BINARY:IMMIGRATION + ECONOMY_BINARY:UNION + IMMIGRATION:UNION")
workingFormulaStr <- paste0(workingFormulaStr," + AgePoly1 + AgePoly2 + AgePoly3 + ")
workingFormulaStr <- paste0(workingFormulaStr,paste0(c("GENDER","EDU_BINARY","RELIGION_BINARY","UNION","ECONOMY_BINARY","IMMIGRATION"),":AgePoly1",collapse=" + ")," + ")
workingFormulaStr <- paste0(workingFormulaStr,paste0(c("GENDER","EDU_BINARY","RELIGION_BINARY","UNION","ECONOMY_BINARY","IMMIGRATION"),":AgePoly2",collapse=" + ")," + ")
workingFormulaStr <- paste0(workingFormulaStr,paste0(c("GENDER","EDU_BINARY","RELIGION_BINARY","UNION","ECONOMY_BINARY","IMMIGRATION"),":AgePoly3",collapse=" + "))

workingFormulaStr_ToUse <- paste0("LR_SELF_Snell ~ ",workingFormulaStr)
workingFormulaStr_Orig <- paste0("LR_SELF ~ ",workingFormulaStr)


workingFormula_ToUse <- as.formula(workingFormulaStr_ToUse)
mWorking <- lm(workingFormula_ToUse,data = modelDF)

workingFormula_Orig <- as.formula(workingFormulaStr_Orig)
mOrig <- lm(workingFormula_Orig,data = modelDF)

resList <- list(mWorking = mWorking,
                mOrig = mOrig)
saveRDS(resList,file = here("data/workingModelFit.RDS"))