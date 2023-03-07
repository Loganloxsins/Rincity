.libPaths()##显示库所在位置
library()##显示库内有哪些包
search()#显示哪些包已被加载并可被使用
install.packages()#可选择包进行下载和安装
install.packages("eclust")##下载并安装指定包"eclust"
update.packages()##更新所有已安装的包
installed.packages()##列出已安装的包，包括版本号、依赖信息等
library(eclust)##调用包"eclust"
help(package="eclust")##输出包"eclust"的描述以及包中的函数名称和数据集名称的列表



##数据导入
setwd("E:/MyProject/s4/Rlan")
data<-read.csv("./City_CarbonEmission.csv",header=TRUE, sep=",",fileEncoding = 'GBK')
head(data)
summary(data)
hist(data$CarbonEmssion.Mt,main = "",col="blue",xlab="CarbonEmission(MT)")
##更新R版本
install.packages("installr")
require(installr)
updateR()

dev.new()##生成新的图窗
plot(data$CarbonEmssion.Mt)##绘制图像

















