library(survey)

WORK_DIR <- "C:/Users/jiang/Desktop/nhanes"
setwd(WORK_DIR) #设置工作路径

source <- read.csv("thedata.csv")

dataDesign <- svydesign(
  id = ~gender,
  data = source,
  weights = ~weightex
)

summary(dataDesign)

coxModel <- svycoxph()
