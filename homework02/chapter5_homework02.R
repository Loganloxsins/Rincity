#设定文件导入路径
setwd("D:/R/handbook/homework02")
#导入csv格式文件
data<-read.csv("./data04.csv",header=TRUE, sep=",")
##查看数据库
data$TIEV#显示data里的变量TIEV这一列的所有数据
data[ , ]	#逗号前面是行，逗号后面是列
data[1:3,2:4]	# 显示data第1至3行，2至4列的所有数据
data[ ,2:4]	# 显示data第2至4列的所有数据
head(data)	#显示数据库内容（默认前6行）
head(data,20)	#显示数据库前20行数据
head(data[,1:3])	#显示数据库第1至3列数据
head(data[,c(1,3,5)])	#显示数据库第1、3、5列数据
#查看数据行列数
dim(data)
#查看数据库结构
str(data)
#查看数据库中各变量的基本统计结果
summary(data)










