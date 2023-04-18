#导入数据
setwd("D:/R/handbook/homework08") 
data<-read.csv("./data08.csv",header=TRUE, sep=",")

#检查数据格式
str(data)

#变量类型转换
data$Regionnew<-as.factor(data$Region)

#因变量过度离散检验
#install.packages("qcc")
library(qcc)
qcc.overdispersion.test(data$Accident,type="poisson")

# 类泊松回归模型建立
model1<-glm(Accident~Month+Regionnew+Volume+Population, 
            family=quasipoisson, data)
summary(model1)

#共线性诊断
#install.packages("car")
library(car)
vif(model1)

#模型优化
model2<-glm(Accident~Month+Regionnew+Population, 
            family=quasipoisson, data)
summary(model2)

#共线性诊断
vif(model2)

#模型解释
exp(coef(model2))

# 以事故发生率作为因变量进行建模
model3<-glm(Accident~Month+Regionnew+Volume+
              offset(log(Population)), family=quasipoisson, data)
summary(model3)

# 模型修正
model4<-glm(Accident~Regionnew+Volume+offset(log(Population)), 
            family=quasipoisson, data)
summary(model4)

#共线性诊断
vif(model4)

#自变量系数的指数变换
exp(coef(model4))















