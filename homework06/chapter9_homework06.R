#数据导入
setwd("D:/R/handbook/homework06")
data<-read.csv("./data06.csv",header=TRUE, sep=",")
#检查数据结构
str(data)
#将数值型变量转为分类变量
data$RILAKnew<-as.factor(data$RILAK)
data$NSCDISnew<-as.factor(data$NSCDIS)
data$NPARKnew<-as.factor(data$NPARK)
data$URSUBnew<-as.factor(data$URSUB)
#筛选非学区房数据
data<-subset(data,NSCDIS==0)
data

#因变量正态性检验
shapiro.test(data$HPRICE1)
#因变量正态性检验
shapiro.test(log(data$HPRICE1))
#新建变量H_price便于下面的操作
H_price <-log(data$HPRICE1)
#查看变量的分布直方图
hist(log(data$HPRICE1)) 
#draw Q-Q plot
qqplot(rnorm(length(H_price)),log(data$HPRICE1),
       #“main=”后输入图片的名称
       main="Normal Q-Q Plot of 房价",
       #“xlab="Theoretical Quantiles",”表示理论分位数
       xlab="Theoretical Quantiles",
       #“ylab="Sample Quantiles"”表示实际分位数
       ylab="Sample Quantiles")
#绘制qqline作为判断正态分布的参考线
qqline(H_price)


#散点图绘制
plot(data$HSIZE,data$HPRICE1)
fitline <- lm(data$HPRICE1 ~ data$HSIZE)
abline(fitline)
#散点图阵绘制
plot(data[,c(2,11:13)],main="pairwise comparison")
#散点图阵绘制（对变量取对数后）
plot(log(data[,c(2,11:13)]),main="pairwise comparison (log)")

#相关系数计算
cor(data[,c(2,11:13)])
#建立回归模型
model1<-lm(HPRICE1~D_PMSP,data=data) 
#输出展示模型结果
summary(model1)
#建立回归模型
model2<-lm(HPRICE1~D_PCBD,data=data) 
#输出展示模型结果
summary(model2)
#建立回归模型
model3<-lm(HPRICE1~D_PRAILS,data=data) 
#输出展示模型结果
summary(model3)


#建立多元回归模型
model4<-lm(log(HPRICE1)~log(D_PMSP)+log(D_PCBD)+
             log(D_PRAILS),data=data)
#输出展示模型结果
summary(model4)
#回归系数标准化
model4new<-lm(scale(log(HPRICE1))~scale(log(D_PMSP))+
                scale(log(D_PCBD))+scale(log(D_PRAILS)),data=data)
summary(model4new)
#初步建立多元回归模型
model5<-lm(log(HPRICE1)~log(D_PMSP)+log(D_PCBD)+log(D_PRAILS),data=data)
summary(model5)

##多重共线性诊断
#安装car包
#install.packages("car")
#调用car包
library(car)
#多元回归模型的共线性诊断
vif(model5)
##模型调整与优化
#利用逐步回归进行模型优化调整
model6<-step(model5)
summary(model6)



#将住宅所在区域变量改为二分类变量
data$URSUBnew[data$URSUBnew =="2"] <- "0"
#交互作用建模
model7<-lm(log(HPRICE1)~HFLOOR+URSUBnew+HFLOOR:URSUBnew,data=data)
summary(model7)

#https://chatgptproxy.me/index.html#/questionDetail?code=mFXPsYwrTUBwgWBeUsk5GSpjksUQU6FaKgaelsPfWmzmrmg2JgdQnR_LvgKTEaNq
