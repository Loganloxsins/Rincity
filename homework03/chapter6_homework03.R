#数据导入
setwd("E:/MyProject/s4/Rlan/homework03")
data<-read.csv("./MyData.csv",header=TRUE, sep=",",fileEncoding = 'GBK')
#检查数据结构
head(data)
#删除原始数据中与研究无关的数据或重复的数据
data1<-data[,c("Time","TIEV","TEV","TIV","TIV_YOY","TEV_TIV","evaluation")]
head(data1)
#查看并删除缺失值
sum(is.na(data1)) 
data2<-na.omit(data1)
dim(data1)
dim(data2)
#均值与中位数插补
#install.packages("imputeTS")
library(imputeTS)  #安装并导入R语言包
na.mean(data1,option = "mean")  #均值插补
na.mean(data1,option = "median")  #中位数插补
#K邻近插补
#install.packages("DMwR2")
library(lattice) 
library(grid) 
library(DMwR2)  #安装并导入R语言包
knnout<- knnImputation(data1[,c('TIV','TIV_YOY','TEV_TIV')], k=10) 
#对TIV、TIV_YOY、TEV_TIV字段中缺失值以10个临近数值进行插补
knnout  #查看插补结果
#回归插补（以变量“(TIV)”的缺失值插补为例）
head(data1)
sub <- which(is.na(data1$TIV))  #识别TIV列缺失值所在行
inputfile1 <- data1[-sub, ]  #将完整数据集另存为inputfile1
inputfile2 <- data1[sub, ]  #将缺失数据集另存为inputfile2
model <- lm(TIV~TEV, data = inputfile1)  
#用完整数据集建立回归模型，因变量为TIV，自变量为TEV,因为进出口总额总是有线性相关的规律
inputfile2$TIV <- predict(model, inputfile2)  #用回归模型预测缺失数据集的TIV
result3 <- rbind(inputfile1,inputfile2)  #合并两个数据集
result3


#多重插补（以变量“面积(TIV)”的缺失值插补为例）
#install.packages("mice")
library(lattice)
library(MASS)
library(nnet)
library(mice)  #前三个包是mice的基础
imp <- mice(data1,m=4,seed=7)  #通过MCMC方法，估计缺失值，生成4个没有缺失值的数据集
imp$imp$TIV  #查看插补的值，选择插补模型
result <- complete(imp,action=2)  #本例选择第2套模型
result  #展示结果


data4<-data[,c("Time","TIEV","TIEV_YOY","TEV","TEV_YOY","TIV","TIV_YOY","TEV_TIV","evaluation")]
#查看并删除异常值
data6<-subset(data4, TIV>0 & TEV >0)
dim(data6)


#转换原始数据为方便软件处理的数据类型

data4$level[data4$evaluation %in% c("很好")]<-1
data4$level[data4$evaluation %in% c("好")]<-2
data4$level[data4$evaluation %in% c("一般")]<-3
data4$level<-as.factor(data4$level)
table(data4$evaluation)
table(data4$level)

##数据标准化
#保存数据
write.csv(data4, file="./Mydatanew.csv")
#数据标准化
head(data4)
data5<-data4[,2:4]
scale(data5)














