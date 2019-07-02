rm(list=ls())													#清空当前工作空间
a=read.csv("D:/Practical Business Data Analysis/case/CH5/ceilphone.csv")					#读入csv格式的数据，作为训练数据	
attach(a)													#将数据a中的各个变量放入工作空间，便于直接调用
a[c(1:5),]													#显示数据的前5行

xtabs(~score+W1)												#根据score和W1的取值生成列联表				

plot(c(1,5),c(0,1),type="n",xlab="score",ylab="Percentage",main="Digital Camera")				#生成画图的框架，给出x、y轴的标签和标题
points(c(1:5),tapply(W2,score,mean),type="b")									#画出根据score的取值计算的W2的均值

par(mfrow=c(2,2))												#生成2x2的图形
plot(c(1,5),c(0,1),type="n",xlab="score",ylab="Percentage",main="Television")					#生成画图的框架，给出x、y轴的标签和标题
points(tapply(W3,score,mean),type="b")										#画出根据score的取值计算的W3的均值
plot(c(1,5),c(0,1),type="n",xlab="score",ylab="Percentage",main="Hand Written Pad")				#生成画图的框架，给出x、y轴的标签和标题
points(tapply(W4,score,mean),type="b")										#画出根据score的取值计算的W4的均值
plot(c(1,5),c(0,1),type="n",xlab="score",ylab="Percentage",main="Multiple Entry Phonebook")			#生成画图的框架，给出x、y轴的标签和标题
points(tapply(W5,score,mean),type="b")										#画出根据score的取值计算的W5的均值
plot(c(1,5),c(0,1),type="n",xlab="score",ylab="Percentage",main="MP3")						#生成画图的框架，给出x、y轴的标签和标题
points(tapply(W6,score,mean),type="b")										#画出根据score的取值计算的W6的均值
par(mfrow=c(1,1))												#设置画图的格式成1x1的形式


xtabs(~score+W7)												#根据score和W7的取值生成列联表	


library(MASS)													#载入程序包MASS
probit0=polr(as.factor(score)~1,method="probit",Hess=T)								#拟合probit定序回归，不用任何解释性变量的空模型
probit1=polr(as.factor(score)~W1+W2+W3+W4+W5+W6+W7,method="probit",Hess=T)					#拟合probit定序回归，利用全部解释性变量的全模型
anova(probit0,probit1)												#对模型probit0,probit1进行方差分析，检验模型的显著性

logit0=polr(as.factor(score)~1,method="logistic",Hess=T)							#拟合logistic定序回归，不用任何解释性变量的空模型
logit1=polr(as.factor(score)~W1+W2+W3+W4+W5+W6+W7,method="logistic",Hess=T)					#拟合logistic定序回归，利用全部解释性变量的全模型
anova(logit0,logit1)												#对模型logit0,logit1进行方差分析,检验模型的显著性

library(car)													#载入程序包car
Anova(probit1,type="III")											#对模型probit1进行三型方差分析
summary(probit1)												#显示模型probit1的各方面细节，包括参数估计值、P值等

Anova(logit1,type="III")											#对模型logit1进行三型方差分析
summary(logit1)													#显示模型logit1的各方面细节，包括参数估计值、P值等
	
probit2=polr(as.factor(score)~W1+W2+W3+W4+W5+W6,method="probit",Hess=T)						#拟合probit定序回归，利用变量W1-W6
summary(probit2)												#显示模型probit2的各方面细节，包括参数估计值、P值等

logit2=polr(as.factor(score)~W1+W2+W3+W4+W5+W6,method="logistic",Hess=T)					#拟合logistic定序回归，利用变量W1-W6
summary(logit2)													#显示模型logit2的各方面细节，包括参数估计值、P值等

logit.aic=step(logit1,trace=F)											#根据AIC准则从全模型logit1中选出最优子模型probit.aic
summary(logit.aic)												#显示模型logit.aic的各方面细节，包括参数估计值、P值等															

logit.bic=step(logit1,trace=F,k=log(length(a[,1])))								#根据BIC准则从全模型logit1中选出最优子模型probit.aic
summary(logit.bic)												#显示模型logit.bic的各方面细节，包括参数估计值、P值等

probit.aic=step(probit1,trace=F)										#根据AIC准则从全模型probit1中选出最优子模型probit.aic
summary(probit.aic)												#显示模型probit.aic的各方面细节，包括参数估计值、P值等

probit.bic=step(probit1,trace=F,k=log(length(a[,1])))								#根据BIC准则从全模型probit1中选出最优子模型probit.aic
summary(probit.bic)												#显示模型probit.bic的各方面细节，包括参数估计值、P值等

a0=read.csv("D:/Practical Business Data Analysis/case/CH5/new.csv")						#读入csv格式的数据，用作检验
a0[c(1:5),]													#显示数据a0的前5行

summary(logit.aic)												#显示模型logit.aic的各方面细节，包括参数估计值、P值等

p=predict(probit.aic,a0,type="p")										#利用模型logit.aic预测数据取值为各水平(不同的score)的概率

a0$score.hat=predict(probit.aic,a0)										#利用模型probit.aic对数据a0进行预测，将预测结果存入a0的变量score.hat中
a0[c(1:5),]													#显示数据a0的前5行

table(a0[,c(1,9)])												#根据预测值和真实值生成列联表，展示预测精度
