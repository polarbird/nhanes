a=read.csv("D:/Practical Business Data Analysis/case/CH7/data.csv",header=T)		#读入csv格式的数据，并赋值给a
a[1:20,]										#展示a的前20行数据

ibrary(survival)									#载入程序包survival
a=a[order(a$Time),]									#根据Time的取值进行排序
Surv(a$Time,a$VStatus)									#得到生存分析所用时间格式

a$Time[a$VStatus==1]									#得到没有被截断的时间点

summary(survfit(Surv(a$Time,a$VStatus)))						#计算生存函数的置信区间

plot(survfit(Surv(a$Time,a$VStatus)))							#画出生存函数及其置信区间

status=1*(a$HGB>median(a$HGB))								#根据中位数将HGB分成两组,分别取值0或1
status											#展示上一命令所得到的数据

plot(survfit(Surv(a$Time,a$VStatus)~status),col=c(1,2),	lty=c(1,2))			#根据status的取值画出两组生存函数，用不同的颜色和线型来区分
legend(40,1,c("HGB<Median","HGB>Median"),col=c(1,2),lty=c(1,2))				#对不同的生存函数加上标示

plot(survfit(Surv(a$Time,a$VStatus)~a$Platelet),col=c(1,2),lty=c(1,2))			#根据Platelet的取值画出两组生存函数，用不同的颜色和线型来区分		
legend(40,1,c("Abnormal","normal"),col=c(1,2),lty=c(1,2))				#对不同的生存函数加上标示

age.group=1*(a$Age>median(a$Age))							#根据中位数将Age分成两组，分别取值0或1	
plot(survfit(Surv(a$Time,a$VStatus)~age.group),col=c(1,2),lty=c(1,2))			#根据age.group的取值画出两组生存函数，用不同的颜色和线型来区分	
legend(40,1,c("Age<60","Age>60"),col=c(1,2),lty=c(1,2))					#对不同的生存函数加上标示

par(mfrow=c(2,2))									#设置画图模式为2x2

status=1*(a$LogWBC>median(a$LogWBC))							#根据中位数将LogWBC分成两组,分别取值0或1
plot(survfit(Surv(a$Time,a$VStatus)~status),col=c(1,2),lty=c(1,2))			#根据status的取值画出两组生存函数，用不同的颜色和线型来区分	
legend(30,1,c("LogWBC<Median","LogWBC>Median"),col=c(1,2),lty=c(1,2))			#对不同的生存函数加上标示


status=1*(a$LogPBM>median(a$LogPBM))							#根据中位数将LogPBM分成两组,分别取值0或1
plot(survfit(Surv(a$Time,a$VStatus)~status),col=c(1,2),lty=c(1,2))			#根据status的取值画出两组生存函数，用不同的颜色和线型来区分	
legend(30,1,c("LogPBM<Median","LogPBM>Median"),col=c(1,2),lty=c(1,2))			#对不同的生存函数加上标示


status=1*(a$Protein>median(a$Protein))							#根据中位数将Protein分成两组,分别取值0或1
plot(survfit(Surv(a$Time,a$VStatus)~status),col=c(1,2),lty=c(1,2))			#根据status的取值画出两组生存函数，用不同的颜色和线型来区分	
legend(30,1,c("Protein<Median","Protein>Median"),col=c(1,2),lty=c(1,2))			#对不同的生存函数加上标示

status=1*(a$SCalc>median(a$SCalc))							#根据中位数将SCalc分成两组,分别取值0或1
plot(survfit(Surv(a$Time,a$VStatus)~status),col=c(1,2),lty=c(1,2))			#根据status的取值画出两组生存函数，用不同的颜色和线型来区分	
legend(30,1,c("SCalc<Median","SCalc>Median"),col=c(1,2),lty=c(1,2))			#对不同的生存函数加上标示

par(mfrow=c(1,1))									#设置画图模式,还原为为1x1

 
fit=survreg(Surv(Time,VStatus)~HGB+Platelet+Age+LogWBC+LogPBM+Protein+SCalc,data=a)	#拟合加速死亡模型，利用全部解释性变量
summary(fit)										#显示模型fit的各方面细节，包括估计值、标准差等

fit=survreg(Surv(Time,VStatus)~HGB+Platelet+Age+LogWBC+LogPBM+Protein+SCalc,
dist="exponential",data=a)								#拟合加速死亡模型，设置残差项为指数分布
summary(fit)										#显示模型fit的各方面细节，包括估计值、标准差等

fit.aic=step(fit,trace=F)								#根据AIC准则选择最优模型
summary(fit.aic)									#显示模型fit.aic的各方面细节，包括估计值、标准差等

ss=length(a[,1])									#计算样本大小
fit.bic=step(fit,trace=F,k=log(ss))							#根据BIC准则选择最优模型
summary(fit.bic)									#显示模型fit.bic的各方面细节，包括估计值、标准差等


fit=coxph(Surv(Time,VStatus)~HGB+Platelet+Age+LogWBC+LogPBM+Protein+SCalc,data=a)	#拟合Cox等比例风险模型
summary(fit)										#显示模型fit.bic的各方面细节，包括估计值、标准差等

ph.aic=step(fit,trace=F)								#根据AIC准则选择最优模型
summary(ph.aic)										#显示模型ph.aic的各方面细节，包括估计值、标准差等

ph.bic=step(fit,trace=F,k=log(ss))							#根据BIC准则选择最优模型
summary(ph.bic)										#显示模型ph.bic的各方面细节，包括估计值、标准差等
