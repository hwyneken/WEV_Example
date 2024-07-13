require(here)
require(gt)
require(webshot2)

modelRes <- readRDS(file = here("data/workingModelFit.RDS"))

finalModel <- modelRes$mWorking # use the Snell transformation

CleanVarNames <- c("Intercept",
                   "Gender: Male - Female",
                   "Education: Medium - Low/High",
                   "Religion: NeverAttends - Attends",
                   "Union: Not a Member - Member",
                   "Economy: No Change / Worsened - Improved",
                   "Immigration: Most Important Issue - Not Most Important",
                   "Country: Switzerland - Austria",
                   "Country: Germany - Austria",
                   "Country: Spain - Austria",
                   "Country: Italy - Austria",
                   "Country: Netherlands - Austria",
                   "Country: Norway - Austria",
                   "Age: Baseline Linear Trend",
                   "Age: Baseline Quadratic Trend",
                   "Age: Baseline Cubic Trend",
                   "Male / Medium Education",
                   "Male / No Religious Attendance",
                   "Male / Economy No Change or Worsened",
                   "Male / Immigration Most Important",
                   "Male / Not a Union Member",
                   "Medium Education / No Religious Attendance",
                   "Medium Education / Economy No Change or Worsened",
                   "Medium Education / Immigration Most Important",
                   "Medium Education / Not a Union Member",
                   "No Religious Attendance / Economy No Change or Worsened",
                   "No Religious Attendance / Immigration Most Important",
                   "No Religious Attendance / Not a Union Member",
                   "Economy No Change or Worsened / Immigration Most Important",
                   "Economy No Change or Worsened / Not a Union Member",
                   "Immigration Most Important / Not a Union Member",
                   "Male / Linear Age Trend",
                   "Medium Education / Linear Age Trend",
                   "No Religious Attendance /  Linear Age Trend",
                   "Not a Union Member / Linear Age Trend",
                   "Economy No Change or Worsened / Linear Age Trend",
                   "Immigration Most Important / Linear Age Trend",
                   "Male / Quadratic Age Trend",
                   "Medium Education / Quadratic Age Trend",
                   "No Religious Attendance /  Quadractic Age Trend",
                   "Not a Union Member / Quadractic Age Trend",
                   "Economy No Change or Worsened / Quadratic Age Trend",
                   "Immigration Most Important / Quadratic Age Trend",
                   "Male / Cubic Age Trend",
                   "Medium Education / Cubic Age Trend",
                   "No Religious Attendance /  Cubic Age Trend",
                   "Not a Union Member / Cubic Age Trend",
                   "Economy No Change or Worsened / Cubic Age Trend",
                   "Immigration Most Important / Cubic Age Trend")

confIntFull <- confint(finalModel)
estVec <- round(coef(summary(finalModel))[,1],2)
ciLowerVec <- round(confIntFull[,1],2)
ciUpperVec <- round(confIntFull[,2],2)

tableDF <- data.frame(Var = CleanVarNames,
                      Estimate = estVec,
                      Lower_95_Bound = ciLowerVec,
                      Upper_95_Bound = ciUpperVec)
t1 <- gt(tableDF)
t1 <- t1 %>% cols_label(Var = "Variable",
                        Estimate = "Full Target",
                        Lower_95_Bound = "95% CI Lower Bound",
                        Upper_95_Bound = "95% CI Upper Bound")
t1 <- t1 %>% tab_header(title = "Estimated Full Targets for All Variables",
                        subtitle = "95% Confidence Intervals Based on Data from 19,150 Respondents")
gtsave(t1,file = here("images/CoefTable.docx"))