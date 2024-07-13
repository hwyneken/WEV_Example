require(here)
require(ggplot2)
require(ggthemes)

# codebook https://cadmus.eui.eu/bitstream/handle/1814/75520/Codebook.pdf?sequence=5&isAllowed=y
WEV <- read.csv(file = here("data/WEV.csv"),header=TRUE)

WEV_Current <- droplevels(subset(WEV,YEAR >= 2016))
WEV_Current <- droplevels(subset(WEV_Current,LR_SELF %in% as.character(1:9)))

plotDF <- data.frame(LR_SELF = WEV_Current$LR_SELF)
plotDF$LR_SELF <- as.numeric(plotDF$LR_SELF)

p1 <- ggplot(data = plotDF,aes(x=LR_SELF)) + geom_histogram()

basicVarNames <- c("GENDER","AGE","EDUCATION","RELIGION","UNION",
                   "INTEREST","NEWSPAPERS","TELEVISION","INTERNET",
                   "VAA","FACEBOOK_USE","FACEBOOK_POL",
                   "ECONOMY","TRUST","IMMIGRATION",
                   "CNAME")
modelDF <- WEV_Current[,basicVarNames]
modelDF$LR_SELF <- as.numeric(WEV_Current$LR_SELF)
modelDF$AGE <- as.numeric(modelDF$AGE)

basicM <- lm(I(LR_SELF^(1/2)) ~ .,data = modelDF)

#fullM <- lm(LR_SELF ~ CNAME*GENDER*AGE*EDUCATION*RELIGION*UNION + ECONOMY + TRUST + IMMIGRATION,data=modelDF)
