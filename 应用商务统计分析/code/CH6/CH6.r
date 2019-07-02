rm(list=ls())												#清空当前工作空间
a=read.csv("D:/Practical Business Data Analysis/case/CH6/crm.csv")					#读入csv格式的数据，用作选连数据
attach(a)												#将数据a的各个变量加载到工作空间，便于直接调用
a[c(1:5),]												#显示数据a的前5行

Mean=sapply(a[,1:6],mean)										#计算数据a的第1到第6列的均值
Min=sapply(a[,1:6],min)											#计算数据a的第1到第6列的最小值
Median=sapply(a[,1:6],median)										#计算数据a的第1到第6列的中位数
Max=sapply(a[,1:6],max)											#计算数据a的第1到第6列的最大值
SD=sapply(a[,1:6],sd)											#计算数据a的第1到第6列的标准差
round(cbind(Mean,Min,Median,Max,SD),3)									#将均值、最小值、中位数、最大值、标准差集中在一起展示

boxplot(freq1~freq0,xlab="freq0",ylab="freq1")								#根据freq0的不同取值来画freq1的盒状图
boxplot(exp1~freq0,xlab="freq0",ylab="exp1")								#根据freq0的不同取值来画exp1的盒状图

par(mfrow=c(2,3))											#设置画图模式为2x3的格式
boxplot(freq1~freq0,xlab="freq0",ylab="freq1",main="第-1月")						#根据freq0的不同取值来画freq1的盒状图
boxplot(freq2~freq0,xlab="freq0",ylab="freq2",main="第-2月")						#根据freq0的不同取值来画freq2的盒状图
boxplot(freq3~freq0,xlab="freq0",ylab="freq3",main="第-3月")						#根据freq0的不同取值来画freq3的盒状图
boxplot(exp1~freq0,xlab="freq0",ylab="exp1",main="第-1月")						#根据freq0的不同取值来画exp1的盒状图
boxplot(exp2~freq0,xlab="freq0",ylab="exp2",main="第-2月")						#根据freq0的不同取值来画exp2的盒状图
boxplot(exp3~freq0,xlab="freq0",ylab="exp3",main="第-3月")						#根据freq0的不同取值来画exp3的盒状图
par(mfrow=c(1,1))											#设置画图模式为1x1的格式


pos0=glm(freq0~1,family=poisson())									#拟合poisson回归模型，不利用任何变量的空模型		
pos1=glm(freq0~freq1+freq2+freq3+exp1+exp2+exp3,family=poisson())					#拟合possion回归模型，利用全部变量的全模型
anova(pos0,pos1)											#为获取不同模型的deviance,进行模型pos0,pos1的方差分析
1-pchisq(2146.0,df=6)											#计算模型显著性检验的P值

library(car)												#载入程序包car
Anova(pos1,type="III")												#对模型pos1进行三型方差分析

summary(pos1)												#显示模型pos1的各方面细节，包括参数估计值、P值等

pos.aic=step(pos1,trace=F)										#根据AIC准则从全模型pos1中选出最优子模型pos.aic
summary(pos.aic)											#显示模型pos.aic的各方面细节，包括参数估计值、P值等

pos.bic=step(pos1,trace=F,k=log(length(a[,1])))								#根据BIC准则从全模型pos1中选出最优子模型pos.bic
summary(pos.bic)											#显示模型pos.bic的各方面细节，包括参数估计值、P值等

a0=read.csv("D:/Practical Business Data Analysis/case/CH6/new.csv")					#读入csv格式的数据，用作检验
a0[c(1:5),]												#显示数据a0的前5行

a0$lam=exp(predict(pos.bic,a0))										#利用模型pos.bic对数据a0进行预测，并将预测结果存入a0的变量lam中
a0[c(1:5),]												#显示数据a0的前5行，包括预测值

sqrt(mean((a0$freq0-a0$lam)^2))										#计算绝对预测精度
