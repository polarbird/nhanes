work_dir<-"D:/work/nhanes"
setwd(work_dir)#设置工作路径

df<-read.csv("mydata20190710.csv")#新命名为df
df$pregnant <- NULL#删掉pregnant这一列

df$pregnancy[is.na(df$pregnancy)] <- 2

newdata <- subset(df,(pregnancy!=1 | is.na(pregnancy))& age>=20 & !is.na(se) & !is.na(cd))

newdata <- newdata[,-3]#删除缺失值过多的列
newdata <- newdata[,-13]
newdata <- newdata[,-28]
newdata <- newdata[,-38]

write.csv(newdata, 'newdata20190710.csv',row.names = FALSE)

library(lattice) 
library(MASS)
library(nnet)
library(randomForest)
library(mice)


miceMod <- mice(newdata[, !names(newdata) %in% "seqn"], method="rf")
miceOutput <- complete(miceMod)
print(miceOutput)
write.csv(miceOutput, 'thedata.completena.csv',row.names = FALSE)
