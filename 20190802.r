# 从csv文件读取newdata数据
newdata <- read.csv('newdata20190710.csv')

library(nnet)
# 对gender属性使用class.ind函数，并将结果转换为data frame存入genders
genders<-as.data.frame(class.ind(newdata$gender))
# 修改genders的属性名称
names(genders)<-c("gender_1","gender_2")
#将genders合并到newdata中
newdata <-cbind(newdata, genders)

races<-as.data.frame(class.ind(newdata$race))
names(races)<-c("race_1","race_2","race_3","race_4","race_6","race_7")
newdata <-cbind(newdata, races)