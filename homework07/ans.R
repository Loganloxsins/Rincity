
##__________________________二元逻辑回归模型应用______________________________
#导入数据
setwd("D:/R/handbook/homework07")
data<-read.csv("./data07_1.csv",header=TRUE, sep=",")
#核查数据
str(data)
#数据类型转换
data$modenew<-as.factor(data$mode)
data$gendernew<-as.factor(data$gender)
data$agenew<-as.factor(data$age)
data$familyincomenew<-as.factor(data$familyincome)
data$carownnew<-as.factor(data$carown)
data$ebmotornew<-as.factor(data$ebmotor)
#查看因变量分布
table(data$modenew)
#二元逻辑回归模型构建
model1 <- glm(formula = modenew ~ distance + gendernew +
                agenew + familyincomenew +  carownnew + 
                ebmotornew + orinb_jobmix +desnb_jobmix, 
              data =data, family = 'binomial')
summary(model1)
#计算OR值
or<-exp((summary(model1))$coef[,'Estimate']) 
or
#将OR值与显著性水平组合显示
OR<-data.frame(OR=or)
Pvalue<-data.frame(Pvalue=((summary(model1))$coef[,'Pr(>|z|)']))
result<-data.frame(cbind(OR,Pvalue))
result
##检验模型拟合度
# HL test包安装与调用
#install.packages("generalhoslem")
library(generalhoslem)
#HL检验
logitgof(data$modenew,fitted(model1))
##共线性诊断
# VIF test
library(car)
vif(model1)
##模型优化
#逐步回归优化模型
model2<-step(model1)
summary(model2)
logitgof(data$modenew,fitted(model2))#HL检验

##检验模型预测精度
#训练集与测试集划分
set.seed(6) #设定随机数的种子
index<-sample(x=2,size=nrow(data),replace=TRUE,prob=c(0.7,0.3)) 
# x为被抽样的数据集，size为抽取多少次，replace 是否重复抽样
# prob为每个x取值被抽取的概率，可以不用和为1，但本例取1是将原数据分成两组,70%和30%
train<-data[index==1,]
test<-data[index==2,]
dim(data)
dim(train)
dim(test)
#用train数据集训练模型
trainmodel <- glm(formula = modenew ~ distance  + carownnew +             ebmotornew + orinb_jobmix +desnb_jobmix, 
                  data =train, family = 'binomial')
summary(trainmodel)
or<-exp((summary(trainmodel))$coef[,'Estimate']) #calculate odds ratio
#用test数据集预测分类并计算准确率
prob<-predict(object=trainmodel,newdata=test,type='response')
pred<-cbind(test,prob)
pred<-transform(pred,predict=ifelse(prob<=0.5,0,1))
ta<-table(pred$modenew,pred$predict)
ta
#计算总体预测准确率
sum_diag<-sum(diag(ta))
sum<-sum(ta)
sum_diag/sum # prediction accuracy
#绘制ROC曲线
#install.packages("pROC")
library(pROC)
roc_curve <- roc(test$modenew,prob)
names(roc_curve)
x <- 1-roc_curve$specificities
y <- roc_curve$sensitivities
plot(x=x,y=y,xlim=c(0,1),ylim=c(0,1),xlab="1-Specificity",ylab="Sensitivity",
     main="ROC Curve",type="l",lwd=2.5)
abline(a=0,b=1,col="grey")
auc<-roc_curve$auc
text(0.5,0.4,paste("AUC:", round(auc,digits=2)),col="blue")









