require(here)
require(dplyr)

source(here("active_code/Data_scripts/Snell_Transformation.R"))

WEV <- read.csv(file = here("data/WEV.csv"),header=TRUE)


WEV_Current <- droplevels(subset(WEV,YEAR >= 2016))
WEV_Current <- droplevels(subset(WEV_Current,LR_SELF %in% as.character(1:9)))
varNamesToUse <- c("GENDER","AGE","EDUCATION","RELIGION","UNION",
                   "ECONOMY","IMMIGRATION",
                   "CNAME")

NumVarNamestoUse <- length(varNamesToUse)

for (i in 1:NumVarNamestoUse) {
  tempVarName <- varNamesToUse[i]
  
  WEV_Current <- droplevels(WEV_Current[WEV_Current[,tempVarName] != "",])
}

WEV_Current$LR_SELF_Snell <- Snell_Transformation(WEV_Current$LR_SELF)
WEV_Current$AGE_NUM <- as.numeric(WEV_Current$AGE)
WEV_Current$EDU_BINARY <- ifelse(WEV_Current$EDUCATION == "Medium education","Medium","Low/High")
WEV_Current$RELIGION_BINARY <- ifelse(WEV_Current$RELIGION == "Never","NeverAttends","Attends")
#WEV_Current$INTEREST_BINARY <- ifelse(WEV_Current$INTEREST_3 == "Low interest","Low Interest","Medium/High Interest")
WEV_Current$ECONOMY_BINARY <- ifelse(WEV_Current$ECONOMY == "Improved","Improved","No Change / Worsened")

varNamesFinal <- c("LR_SELF","LR_SELF_Snell","GENDER","AGE_NUM","EDU_BINARY","RELIGION_BINARY","UNION",
                   "ECONOMY_BINARY","IMMIGRATION","CNAME")
modelDF <- WEV_Current[,varNamesFinal]
degreeFourAgeEffect <- poly(modelDF$AGE_NUM,3)
colnames(degreeFourAgeEffect) <- paste0("AgePoly",1:3)
modelDF <- cbind(modelDF,degreeFourAgeEffect)
saveRDS(modelDF,file = here("data/WEV_Full_Model_Data.RDS"))






