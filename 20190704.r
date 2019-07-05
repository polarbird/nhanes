WORK_DIR = "D:/work/nhanes"
setwd(WORK_DIR)

library(psych) #引用psych包

source <- read.csv("thedata.csv")

#剔除掉source数据中已怀孕的行
df<-source[source$pregnancy!=1, ] 

#提取出待计算几何平均数的属性，并执行行转列操作，将4列9919行矩阵转换为9919列4行矩阵
x <-t(data.frame(df$pb, df$cd, df$hg, df$se)) 

#使用geometric.mean函数计算几何平均数
geometric_mean <- geometric.mean(x,na.rm=FALSE)

#将计算的geometric_mean添加至df中
df <- cbind(df, geometric_mean)
