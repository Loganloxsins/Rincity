#数据导入
setwd("D:/R/handbook/homework05")
data<-read.csv("./data05.csv",header=TRUE, sep=",")
#查看数据库
dim(data)  
#查看数据结构
str(data)
#查看数据结构
head(data)
#查看数据结构
summary(data)

#把数值型数据变成因子型数据
data$N_typenew<-as.factor(data$N_type) 
data$Gendernew<-as.factor(data$Gender) 
data$H_typenew<-as.factor(data$H_type) 
data$Employ<-as.factor(data$Employ) 
#把因子型数据变为数值型数据
data$Staynew<-as.numeric(data$Stay) 
data$Staynew<-as.numeric(as.character(data$Stay)) 
#去除缺失值
datanew<-na.omit(data) 
str(datanew)

#新建变量H_price便于下面的操作
H_price <-datanew$ H_price
#查看变量的分布直方图
hist(datanew$ H_price) 
#draw Q-Q plot
qqplot(rnorm(length(H_price)),datanew $H_price,
       #“main=”后输入图片的名称
       main="Normal Q-Q Plot of House Price",
       #“xlab="Theoretical Quantiles",”表示理论分位数
       xlab="Theoretical Quantiles",
       #“ylab="Sample Quantiles"”表示实际分位数
       ylab="Sample Quantiles")
#绘制qqline作为判断正态分布的参考线
qqline(H_price)
#安装moments包
#install.packages("moments")
#安装fbasics包
#install.packages("fBasics")
#用library先调用fbasics包
library(fBasics)
#先用fbasics包检验偏度与峰度
#用skewness检验变量的偏度
skewness(H_price)
#用kurtosis检验变量的峰度
kurtosis(H_price) 
#计算偏度和峰度Z-score
N<-length(H_price)
ses<-sqrt((6*N*(N-1))/((N-2)*(N+1)*(N+3)))
sek<-2*ses*sqrt((N*N-1)/((N-3)*(N+5)))
skewness(H_price)/ses
kurtosis(H_price)/sek

#计算log(H_price)的记录数
M<-length(log(H_price))
#用ses计算偏度标准误差，用峰度除以峰度的标准误差就是峰度的z-score 
ses<-sqrt((6* M *( M -1))/(( M -2)*( M +1)*( M +3)))
#用sek计算峰度标准误差用偏度除以偏度的标准误差就是偏度的z-score 
sek<-2*ses*sqrt((M*M-1)/((M-3)*(M+5))) 
skewness(log(H_price))/ses
kurtosis(log(H_price))/sek

shapiro.test(log(H_price))

#用table查看居住社区类型有哪些组
table(datanew$N_typenew)

shapiro.test(datanew$N_year[datanew$N_typenew==1])
shapiro.test(datanew$N_year[datanew$N_typenew==2])
shapiro.test(datanew$N_year[datanew$N_typenew==3])
shapiro.test(datanew$N_year[datanew$N_typenew==4])

bartlett.test(N_year ~ N_typenew, data=datanew)

#用aov命令对数据进行方差分析
results<-aov(N_year ~ N_typenew, data=datanew) 
#aov检验的原假设是，不同组之间平均值在统计学意义上是近似的
#用summary(results)查看一下结果
summary(results)
#用aov命令对数据进行方差分析
results<-aov(N_year ~ N_typenew+Employ+N_typenew*Employ, data=datanew)
#aov检验的原假设是，不同组之间平均值在统计学意义上是近似的
#用summary(results)查看一下结果
summary(results)



#先用scatter plot直观查看变量是否具有相关性。X轴是居民年龄，Y轴是房价。
plot(datanew$Age, datanew$H_price, 
     # main=后输入图形的名称
     main="Age V.S. Stay in Nanjing",
     xlab = "Age", ylab = "H_price",
     #x轴根据居民年龄的范围，取值0-100
     xlim = c(0,100),                             
     #pch是数据点的形状，col是数据点的颜色
     pch = 1, col = "blue")
#此处选择居民年龄为因变量，房价为自变量，将回归的结果赋值给fitline
fitline <- lm(datanew$H_price ~ datanew$Age)
#用abline绘制回归后拟合的直线
abline(fitline)



#用cor函数计算变量的相关性系数
cor(datanew$Age, datanew$H_price)




















