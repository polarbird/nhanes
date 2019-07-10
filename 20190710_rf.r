newdata<-read.csv("newdata20190710.csv")

library(lattice) 
library(MASS)
library(nnet)
library(randomForest)
library(mice)


miceMod <- mice(newdata[, !names(newdata) %in% "seqn"], method="rf")
miceOutput <- complete(miceMod)
print(miceOutput)
write.csv(miceOutput, 'newdata_completena_20190710.csv',row.names = FALSE)
