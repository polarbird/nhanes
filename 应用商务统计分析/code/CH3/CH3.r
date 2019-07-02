rm(list=ls())										#清空当前工作空间
a=read.csv("D:/Practical Business Data Analysis/case/CH3/teaching.csv",header=T)	#读入csv格式的数据，并赋值给a
attach(a)										#将a中各变量加入工作空间
a[c(1:5),]										#展示a的前5行数据

plot(size,score)									#画出size与score的散点图

boxplot(score~ceiling(size/20))								#画出score与分组的size的盒状图
table(ceiling(size/20))									#计算分组的size的频数
group=1*(size<=20)									#根据size是否大于20生成0、1变量，

par(mfrow=c(3,2))									#设置画图模式为3x2
boxplot(score~title,main="职称")							#画出score与title的盒状图
boxplot(score~gender,main="性别")							#画出score与gender的盒状图
boxplot(score~student,main="学生类别")							#画出score与student的盒状图
boxplot(score~year,main="年份")								#画出score与year的盒状图
boxplot(score~semester,main="学期")							#画出score与semester的盒状图
boxplot(score~group,main="班级规模")							#画出score与group的盒状图
par(mfrow=c(1,1))									#设置画图模式，还原成1x1

lm1=lm(score~as.factor(group)+size)							#用解释性变量group和size拟合线性模型
summary(lm1)										#显示模型lm1的各方面细节，包括参数估计值、P值等
plot(size,score)									#画出size与score的散点图
points(size,lm1$fitted,col=2)								#在size与score的散点图上用红色画出size与lm1$fitted的数据点

lm2=lm(score~as.factor(group)*size)							#拟合带交互作用的线性模型
library(car)										#载入程序包car
Anova(lm2,type="III")									#对模型lm2做三型方差分析

summary(lm2)										#显示模型lm2的各方面细节，包括参数估计值、P值等
plot(size,score)									#画出size与score的散点图
points(size,lm2$fitted,col=2)								#在size与score的散点图上用红色画出size与lm2$fitted的数据点

lm3.1=lm(score~as.factor(title)+as.factor(gender)+as.factor(student)
+as.factor(year)+as.factor(semester)+as.factor(group)*size)				#拟合考虑所有变量的全模型
Anova(lm3.1,type="III")									#对模型lm3.1做三型方差分析

lm3.2=lm(score~as.factor(title)+as.factor(student)+as.factor(year)+
as.factor(group)*size)									#删除全模型中不显著的变量，重新拟合
Anova(lm3.2,type="III")									#对模型lm3.2做三型方差分析
summary(lm3.2)										#显示模型lm3.2的各方面细节，包括参数估计值、P值等

par(mfrow=c(2,2))									#设置画图模式为2x2的格式
plot(lm3.2,which=c(1:4))								#画出模型lm3.2的前4个与模型诊断相关的图，包括残差图、cook距离等
par(mfrow=c(1,1))									#设置画图模式，还原成1x1

Anova(lm3.1,type="III")									#对模型lm3.1做三型方差分析

AIC(lm3.1)										#计算模型lm3.1的AIC值
AIC(lm3.1,k=log(length(score)))								#计算模型lm3.1的BIC值

Anova(lm3.2,type="III")									#对模型lm3.2做三型方差分析

AIC(lm3.2)										#计算模型lm3.2的AIC值
AIC(lm3.2,k=log(length(score)))								#计算模型lm3.2的BIC值


lm.aic=step(lm3.1,trace=F)								#根据AIC准则从lm3.2中选出最优模型
Anova(lm.aic,type="III")								#对模型lm.aic做三型方差分析

lm.bic=step(lm3.1,k=log(length(score)),trace=F)						#根据BIC准则从lm3.2中选出最优模型
Anova(lm.bic,type="III")								#对模型lm.bic做三型方差分析

a0=read.csv("D:/Practical Business Data Analysis/case/CH3/new.csv",header=T)		#读入用作预测的数据，并赋值给a0
a0$group=1*(a0$size<=20)								#根据size是否大于20生成新的0、1变量，并赋值给a0$group
a0											#展示数据a0

score.hat=predict(lm.aic,a0)								#利用lm.aic对a0进行预测预测
a0$score.hat=score.hat									#将预测值赋给a0的变量score.hat
a0											#展示数据a0，此时包括预测值

summary(lm.aic)										#显示模型lm1的各方面细节，包括参数估计值、P值等

a$adj.score=lm.aic$residuals								#将模型lm.aic中的残差赋值给a中的变量adj.score
a[c(1:10),]										#展示数据a0的前10行，此时包括adj.score
