
a=read.csv("D:/Practical Business Data Analysis/case/CH4/st.csv",header=T)	#读入csv格式的数据，并赋值给a
a1=a[a$year==1999,-1] 								#取出year为1999的数据，并删除第一列，赋值给a1
a2=a[a$year==2000,-1] 								#取出year为2000的数据，并删除第一列,赋值给a2
a1[c(1:5),]									#展示a1的前5行数据

boxplot(ARA~ST,data=a1,main="ARA")						#画出ARA与ST的盒状图

par(mfrow=c(3,2))								#设置画图模式为3x2的格式
boxplot(ASSET~ST,data=a1,main="ASSET")						#画出ASSET与ST的盒状图
boxplot(ATO~ST,data=a1,main="ATO")						#画出ATO与ST的盒状图
boxplot(GROWTH~ST,data=a1,main="GROWTH")					#画出GROWTH与ST的盒状图
boxplot(LEV~ST,data=a1,main="LEV")						#画出LEV与ST的盒状图
boxplot(ROA~ST,data=a1,main="ROA")						#画出ROA与ST的盒状图
boxplot(SHARE~ST,data=a1,main="SHARE")						#画出SHARE与ST的盒状图
par(mfrow=c(1,1))								#设置画图模式为1x1的格式

glm0.a=glm(ST~1,family=binomial(link=logit),data=a1)				#拟合logistic回归，不使用任何变量的空模型
glm1.a=glm(ST~ARA+ASSET+ATO+GROWTH+LEV+ROA+SHARE,
family=binomial(link=logit),data=a1)						#拟合logistic回归，使用所有变量的全模型
anova(glm0.a,glm1.a)								#计算glm0.a与glm1.a的deviance

1-pchisq(30.565,7)								#计算模型显著性检验的P值

glm0.b=glm(ST~1,family=binomial(link=probit),data=a1)				#拟合porbit回归，不使用任何变量的空模型
glm1.b=glm(ST~ARA+ASSET+ATO+GROWTH+LEV+ROA+SHARE,
family=binomial(link=probit),data=a1)						#拟合porbit回归，使用所有变量的全模型
anova(glm0.b,glm1.b)								#计算glm0.b与glm1.b的deviance
1-pchisq(31.702,7)								#计算模型显著性检验的P值

library(car)									#载入程序包car
Anova(glm1.a,type="III")							#对模型glm1.a做三型方差分析

summary(glm1.a)									#显示模型glm1.a的各方面细节，包括参数估计值、P值等

Anova(glm1.b,type="III")							#对模型glm1.b做三型方差分析

summary(glm1.b)									#显示模型glm1.b的各方面细节，包括参数估计值、P值等

glm2.b=glm(ST~ARA+GROWTH+LEV,family=binomial(link=probit),data=a1)		#利用变量ARA,GROWTH和LEV拟合probit回归模型
summary(glm2.b)									#显示模型glm2.b的各方面细节，包括参数估计值、P值等

glm2.a=glm(ST~ARA+GROWTH+LEV,family=binomial(link=logit),data=a1)		#利用变量ARA,GROWTH和LEV拟合logistic回归模型
summary(glm2.a)									#显示模型glm2.a的各方面细节，包括参数估计值、P值等

deviance(glm0.a)								#计算模型glm0.a的deviance
deviance(glm1.a)								#计算模型glm0.b的deviance

AIC(glm0.a)									#计算模型glm0.a的AIC取值
AIC(glm1.a)									#计算模型glm1.a的AIC取值
AIC(glm0.a,k=log(length(a1[,1])))						#计算模型glm0.a的BIC取值
AIC(glm1.a,k=log(length(a1[,1])))						#计算模型glm1.a的BIC取值


logit.aic=step(glm1.a,trace=0)							#根据AIC准则选择最优模型，并赋值给logit.aic
summary(logit.aic)								#显示模型logit.aic的各方面细节，包括参数估计值、P值等

n=length(a1[,1])								#样本大小
logit.bic=step(glm1.a,k=log(n),trace=0)						#根据BIC准则选择最优模型，并赋值给logit.bic
summary(logit.bic)								#显示模型logit.bic的各方面细节，包括参数估计值、P值等

probit.aic=step(glm1.b,trace=0)							#根据AIC准则选择最优模型，并赋值给probit.aic
summary(probit.aic)								#显示模型probit.aic的各方面细节，包括参数估计值、P值等
probit.bic=step(glm1.b,k=log(n),trace=0)					#根据bIC准则选择最优模型，并赋值给probit.bic
summary(probit.bic)								#显示模型probit.bic的各方面细节，包括参数估计值、P值等

summary(glm1.a)									#显示模型glm1.a的各方面细节，包括参数估计值、P值等

p=predict(glm1.a,a2)								#利用模型glm1.a对数据a2进行预测
p=exp(p)/(1+exp(p))								#计算预测得到的概率
a2$ST.pred=1*(p>0.5)								#以0.5为阈值生成预测值
table(a2[,c(8,9)])								#计算预测值与真实值的2维频数表

a2$ST.pred=1*(p>0)								#以0为阈值生成预测值
table(a2[,c(8,9)])								#计算预测值与真实值的2维频数表

a2$ST.pred=1*(p>0.05)								#以0.05为阈值生成预测值
table(a2[,c(8,9)])								#计算预测值与真实值的2维频数表

ngrids=100									#设置格点数为100
TPR=rep(0,ngrids)								#为TPR(true positive ratio)赋初值
FPR=rep(0,ngrids)								#为FPR(false positive ratio)赋初值
for(i in 1:ngrids){
p0=i/ngrids;									#选取阈值p0
ST.true=a2$ST									#从a2中取出真实值并赋值给ST.true
ST.pred=1*(p>p0)								#以0.05为阈值生成预测值
TPR[i]=sum(ST.pred*ST.true)/sum(ST.true)					#计算TPR
FPR[i]=sum(ST.pred*(1-ST.true))/sum(1-ST.true)					#计算FPR
}
plot(FPR,TPR,type="l",col=2)							#画出FPR与TPR的散点图，即ROC曲线
points(c(0,1),c(0,1),type="l",lty=2)						#添加对角线


p=matrix(0,length(a2[,1]),6)							#生成矩阵，用于存储各模型的预测值
p[,1]=predict(glm1.a,a2)							#利用模型glm1.a对数据a2进行预测
p[,2]=predict(logit.aic,a2)							#利用模型logit.aic对数据a2进行预测
p[,3]=predict(logit.bic,a2)							#利用模型logit.bic对数据a2进行预测
p[,c(1:3)]=exp(p[,c(1:3)])/(1+exp(p[,c(1:3)]))					#计算预测得到的概率

p[,4]=predict(glm1.b,a2)							#利用模型glm1.b对数据a2进行预测
p[,5]=predict(probit.aic,a2)							#利用模型probit.aic对数据a2进行预测
p[,6]=predict(probit.bic,a2)							#利用模型probit.bic对数据a2进行预测
p[,c(4:6)]=pnorm(p[,c(4:6)])							#计算预测得到的概率

plot(c(0,1),c(0,1),type="l",main="FPR vs. TPR",xlab="FPR",ylab="TPR")		#画图，生成基本框架
FPR=rep(0,ngrids)								#为FPR赋初值
TPR=rep(0,ngrids)								#为TPR赋初值
for(k in 1:6){
	prob=p[,k]								#取出p中第K列的值，即第K个模型的预测概率
	for(i in 1:ngrids){
		p0=i/ngrids							#选取阈值
		ST.hat=1*(prob>p0)						#根据阈值生成预测值
		FPR[i]=sum((1-ST.true)*ST.hat)/sum(1-ST.true)			#计算FPR
		TPR[i]=sum(ST.true*ST.hat)/sum(ST.true)				#计算TPR
	}
	points(FPR,TPR,type="b",col=k,lty=k,pch=k)				#向图上添加第k个模型的TPR与FPR的散点图
}
legend(0.6,0.5,c("LOGIT FULL MODEL","LOGIT AIC MODEL",	
"LOGIT BIC MODEL","PROBIT FULL MODEL","PROBIT AIC MODEL",
"PROBIT BIC MODEL"),lty=c(1:6),col=c(1:6),pch=c(1:6))				#为6个模型添加标示，区分6个模型