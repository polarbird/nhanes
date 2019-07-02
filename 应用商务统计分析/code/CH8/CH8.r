a=read.csv("D:/WORKING/teaching/Practical Business Data Analysis/case/CH4/rate.csv")		#读入csv格式的数据，赋值给a
ts.plot(a$rate)											#画出rate的时间序列图

r=diff(log(a$rate))										#计算rate的对数变化率，赋值给r
ts.plot(r)											#画出r的时间序列图

boxplot(a$rate)											#画rate的盒状图

boxplot(r)											#画r的盒状图

acf(a$rate)											#计算rate的自相关系数

acf(r)												#计算r的自相关系数

ar(r,aic=F,order=4)										#对r拟合4阶的自回归模型

fit=ar(r)											#对r拟合自回归模型，并用AIC准则选择最优模型
fit												#显示模型fit的各方面细节，包括估计值等

plot(fit$aic,type="b")										#画出各阶自回归模型的AIC取值

resid=fit$resid[-c(1:2)]									#从模型fit中取出残差项，赋值给resid
acf(resid)											#计算残差项resid的自相关系数	

ts.plot(resid)											#画出残差项resid的时间序列图

qqnorm(resid)											#画出残差项resid的QQ图，检查其正态性

r1=r[c(1:600)]											#取出r的前600期观测
r2=r[601]											#取出r的第601期观测
fit=ar(r1,aic=F,order=2)									#利用数据r1,即前600期观测，拟合2阶的自回归模型
fit												#显示模型fit的各方面细节，包括估计值等

predict(fit,nhead=1)$pred									#利用模型fit预测下一期的观测值

r.true=r[c(601:707)]										#取出r中的第601期至707期观测，作为比较的真实值
r.hat=rep(0,107)										#为r.hat赋初值0
for(i in 1:107){										#循环107次
	tmp.r=r[c(i:(i+599))]									#取出第i期至i+599期观测,赋值给tmp.r
	tmp.fit=ar(tmp.r,aic=F,order=2)								#利用数据tmp.r拟合2阶的自回归模型
	r.hat[i]=predict(tmp.fit,nhead=1)$pred							#利用模型tmp.fit预测下一期的值，并赋值给r.hat[i]
}

mean((r.hat-r.true)^2)/var(r.true)								#计算自回归模型的预测误差相对常数预测误差的比例


ts.plot(r.true,col=4)										#画出r.true，即真实值的序列图
points(c(1:107),r.hat,type="l",col=2,lty=2)							#画出r.hat，即预测值得序列图
legend(30,-0.075,c("True Value", "Predicted Value"),col=c(4,2),lty=c(1,2))			#为两组数据加上标示，便于区分
