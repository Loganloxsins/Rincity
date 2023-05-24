##_____________________________主成分分析______________________________________
#数据导入
setwd("D:/R/handbook/homework10")
data<-read.csv("./data10.csv",header=TRUE, sep=",")
#数据结构检查
dim(data)
names(data)
datanew<-data[,3:8]



#psych包的下载与调用
#.packages('psych',lib="C:/Program Files/R/R-4.1.2/library")
library(psych)
KMO(datanew)
bartlett.test(datanew)

#判断主成分构建数量
fa.parallel(datanew, fa="pc",n.iter=100)



#提取主成分
pca<-principal(datanew, nfactors=2,rotate="varimax", scores=T)
pca
#主成分分析可视化表达
biplot(pca)
fa.diagram(pca)


#提取主成分3元
pcatest<-principal(datanew, nfactors=3,rotate="varimax", scores=T)
pcatest


#数据提取与汇总查看
pca.score<-data.frame(pca$scores)
data1<-cbind(data,pca.score)
head(data1)
#通过lm()函数建立线性模型
model1<-lm(log(HPRICE1)~RC1+RC2,data=data1)
summary(model1)
#对比第六讲线性回归建模
model2<-lm(log(HPRICE1)~HSIZE+HNBATHR+HFLOOR+BUILDY+D_PMSP+D_PCBD,data=data1)
summary(model2)


#提取主成分3元
pca<-principal(datanew, nfactors=3,rotate="varimax", scores=T)
pca
#主成分分析可视化表达
biplot(pca)
fa.diagram(pca)
#数据提取与汇总查看
pca.score<-data.frame(pca$scores)
data1<-cbind(data,pca.score)
head(data1)
#通过lm()函数建立线性模型
model1<-lm(log(HPRICE1)~RC1+RC2+RC3,data=data1)
summary(model1)
##____________________________因子分析_________________________________________
#数据导入
setwd("D:/R/handbook/homework10")
data<-read.csv("./data10.csv",header=TRUE, sep=",")

#数据结构检查
dim(data)
names(data)
datanew<-data[,3:8]

#psych包调用
library(psych)

#判断因子构建数量
fa.parallel(datanew, fa="both",n.iter=100,fm="ml")
#提取公共因子
fa<-fa(datanew, nfactors=2,rotate="varimax", scores=T,fm="ml")
fa
#因子分析可视化表达
factor.plot(fa,labels=rownames(fa$loadings))
fa.diagram(fa)
#数据提取与汇总
fa.score<-data.frame(fa$scores)
data1<-cbind(data,fa.score)

#通过lm()函数建立线性模型
model1<-lm(log(HPRICE1)~ML1+ML2,data=data1)
summary(model1)














