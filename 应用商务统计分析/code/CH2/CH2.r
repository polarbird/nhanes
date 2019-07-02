rm(list=ls())										#清空当前工作空间
a=read.csv("D:/Practical Business Data Analysis/case/CH2/real.csv",header=T)		#读入csv格式的数据，赋值为a
attach(a)										#将数据集a中个变量添加到工作空间，便与直接调用
pairs(a[,c(1:6)])									#对a的前6列做散点图

boxplot(price~ring)									#画出price与ring之间的盒状图

log.price=log(price)									#对price对数变化，并赋值给log.price
boxplot(log.price~ring)									#画出log.price与ring之间的盒状图

par(mfrow=c(2,2))									#设置画图模式2x2的格式
boxplot(log.price~dis)									#画出log.price与dis之间的盒状图
boxplot(log.price~wuye)									#画出log.price与wuye之间的盒状图
boxplot(log.price~fitment)								#画出log.price与fitment之间的盒状图
boxplot(log.price~contype)								#画出log.price与contype之间的盒状图

summary(a[,c(1:5)])									#给出a中前5列的描述性分类统计

lm1=lm(log.price~as.factor(ring))							#用离散变量ring做解释性变量做单因子方差分析
library(car)										#载入程序包car
Anova(lm1,type="III")									#对模型lm1做三型方差分析
summary(lm1)										#显示模型lm1的各方面细节，包括参数估计值、P值等

lm2.1=lm(log.price~as.factor(ring)+as.factor(wuye))					#不带交互作用的双因子方差分析
Anova(lm2.1,type="III")									#对模型lm2.1做三型方差分析
summary(lm2.1)										#显示模型lm2.1的各方面细节，包括参数估计值、P值等

lm2.2=lm(log.price~as.factor(ring)*as.factor(wuye))					#带交互作用的双因子方差分析
Anova(lm2.2,type="III")									#对模型lm2.2做三型方差分析
summary(lm2.2)										#显示模型lm2.2的各方面细节，包括参数估计值、P值等

lm4=lm(log.price~as.factor(dis)*as.factor(ring)						
+as.factor(wuye)+as.factor(fitment)+as.factor(contype))					#包括所有变量的全模型方差分析
summary(lm4)										#显示模型lm4的各方面细节，包括参数估计值、P值等

par(mfrow=c(2,2))									#设置画图模式为2x2
plot(lm4,which=c(1:4))									#画出lm4中对应于模型检验的4张图，包括残差图、QQ图和Cook距离图
par(mfrow=c(1,1))									#设置画图模式为1x1

a0=read.csv("D:/Practical Business Data Analysis/case/CH2/new.csv")			#读入新数据，赋值给a0
a0=a0[,c(1:5)]										#取a0的前5列	
a0											#展示a0的数据

y.pred=exp(predict(lm4,a0))								#用模型lm4对a0做预测
a0$y.pred=y.pred									#将预测结果赋值给a0中的变量y.pred
a0											#展示a0的数据,包括预测值