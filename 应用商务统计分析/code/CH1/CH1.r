rm(list=ls())									#清理当前工作空间
a=read.table("D:/Practical Business Data Analysis/case/CH1/roe.txt",header=T)	#读入以空格为分隔符，并带有标题行的文本文件 
round(a[1:10,],4)								#用4位小数点的格式显示a中前10行的数据

a1=a[a$year==2002,-1]								#从a中选出year为2002的数据，并删除第1列，然后赋值给a1
Mean=sapply(a1,mean)								#计算a1中各列的均值
Min=sapply(a1,min)								#计算a1中各列的最小值
Median=sapply(a1,median)							#计算a1中各列的中位数
Max=sapply(a1,max)								#计算a1中各列的最大值
SD=sapply(a1,sd)								#计算a1中各列的标准差
cbind(Mean,Min,Median,Max,SD)							#将均值、最小值、中位数、最大值、标准差集中在一起展示

round(cor(a),3)									#计算相关系数，用3位小数点的格式展示

plot(a1$ROEt,a1$ROE)								#画出ROEt和ROE之间的散点图

lm1=lm(ROE~ROEt+ATO+PM+LEV+GROWTH+PB+ARR+INV+ASSET,data=a1)			#用a1中数据拟合线性回归模型
summary(lm1)									#给出模型lm1中系数估计值、P值等细节

round(a1[1:10,],3)								#用3位小数点的格式显示a1中前10行的数据

par(mfrow=c(2,2))								#设置画图为2x2的格式
plot(lm1,which=c(1:4))								#画出lm1中对应于模型检验的4张图，包括残差图、QQ图和Cook距离图

a1=a1[-47,]									#删除a1中第47行的观测
lm2=lm(ROE~ROEt+ATO+PM+LEV+GROWTH+PB+ARR+INV+ASSET,data=a1)			#用上一行命令得到的新数据a1再次拟合线型回归模型，结果赋值给lm2	
plot(lm2,which=c(1:4))								#画出lm2中对应于模型检验的4张图，包括残差图、QQ图和Cook距离图

library(car)									#载入程序包Car
round(vif(lm2),2)								#计算模型lm2的方差膨胀因子，用2位小数点的格式展示

lm.aic=step(lm2,trace=F)							#根据AIC准则选出最优模型，并赋值给lm.aic
summary(lm.aic)									#给出模型lm.aic中系数估计值、P值等细节

lm.bic=step(lm2,k=log(length(a1[,1])),trace=F)					#根据BIC准则选出最优模型，并赋值给lm.bic
summary(lm.bic)									#给出模型lm.bic中系数估计值、P值等细节


a2=a[a$year==2003,-1]								#从数据a中选出year为2003的观测，并删除第一列，赋值给a2
round(a2[1:5,],3)								#用3为小数点的格式展示a2的前5行数据

y1=predict(lm2,a2)								#用全模型lm2对a2进行预测
y2=predict(lm.aic,a2)								#用模型lm.aic对a2进行预测
y3=predict(lm.bic,a2)								#用模型lm.aic对a2进行预测
y0=a2[,10]									#选出a2中的第10列，即当年的ROE

r0=y0-a2$ROEt									#用当年ROE对下年进行预测的残差
r1=y0-y1									#用全模型lm2预测的残差
r2=y0-y2									#用模型lm.aic预测的残差
r3=y0-y3									#用模型lm.bic预测的残差
resid=abs(as.data.frame(cbind(r0,r1,r2,r3)))					#计算残差的绝对值
sapply(resid,mean)								#计算不同模型的平均绝对偏差，即对残差的绝对值取平均
