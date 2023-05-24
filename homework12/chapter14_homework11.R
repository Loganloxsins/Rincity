#导入数据
setwd("D:/R/handbook/homework11") 
data<-read.csv("./data11.csv",header=TRUE, sep=",")
#构建测量模型
att<-data[,c("i","j","k")]
norm<-data[,c("e","f","g")]
control<-data[,c("m","n","p")]
intention<-data[,c("q","r")]
#测量模型可靠性检验
library(psych)
psych::alpha(att)$total$std.alpha
psych::alpha(norm)$total$std.alpha
psych::alpha(control)$total$std.alpha
psych::alpha(intention)$total$std.alpha
# 安装并调用lavaan包
#.packages("lavaan")
library(lavaan)
# 结构方程模型描述
model<-'#测量模型建立
        att=~i+j+k
        norm=~e+f+g
        control=~m+n+p
        intention=~q+r
        #结构模型建立
        intention~att+norm+control
         walk~intention+control
'
#  结构方程模型运行与输出
fit<-cfa(model,data=data)
summary(fit,fit.measures=TRUE,standardized=TRUE)#标准化系数
# 模型评价
fitMeasures(fit,c("chisq","df","gfi","agfi","cfi","nfi","nnfi","ifi","rmsea","rmr"))
fitMeasures(fit,"chisq")/fitMeasures(fit,"df")

# MI指数
MI<- modificationindices(fit)
subset(MI,mi>10)
#模型可视化
#.packages("semPlot")
library(semPlot)
#semPaths(fit,what="stand",layout = "tree2")









