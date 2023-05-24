#数据导入
#setwd("D:/1-Nanjing University Work/1-上课/01 城市与区域系统分析/2023 教学/上机手册/homework13")
data<-read.csv("./data12.csv",header=TRUE, sep=",")
#查看数据特征
dim(data) 
names(data) 
#生成时序对象
nj = ts(data[,2],start = c(1995,1),frequency = 1) 
nj
##制作时序图
# 调用扩展包
#install.packages ('forecast')
#install.packages('fUnitRoots')
library (forecast)
library (fUnitRoots)
# 生成时序标签，by参数可以是"day", "week", "month", "quarter" 或 "year"
# length参数表示需要生成的序列个数，需与前面数据量一致
time <- seq.Date(as.Date("1995/1/1"), by = "year", length = 26)
# 函数data.frame用于生成数据框，每列数据用逗号隔开
dat_nj <- data.frame(time, nj)
# 函数plot用于绘制图表，type参数选择绘制图表的类型，“l”表示折线图，xaxt = "n"表示不显示x轴刻度标签，ylim参数用于设定y轴的最大最小值，ylab参数用于设定y轴的名称
plot(dat_nj,type="l", xaxt = "n", ylim=c(0,2000), ylab="Electricity consumption")
# axis.Date用于在图表上添加轴线，第一个参数是轴线位置，“1”表示下方，at参数表示需要添加刻度的数值，format参数用于确定日期格式
axis.Date(1, at = time, format = "%Y")
##平稳性检验
#自相关系数检验
acf(nj)
#偏自相关系数检验
pacf(nj) 
#单位根检验
unitrootTest(nj)
##差分分析
#一阶差分分析
difnj = diff(nj) 
#再次进行ACF、PACF、单位根检验
acf(difnj) 
pacf(difnj)
unitrootTest(difnj)
#二阶差分分析
difnj2 = diff(nj,differences=2)
#再次进行ACF、PACF、单位根检验
acf(difnj2) 
pacf(difnj2)
unitrootTest(difnj2)
# 白噪声检验
Box.test(difnj2,type = "Ljung-Box")
# ARIMA建模
fit = arima(nj,order=c(0,2,2))
fit

##==========ARIMA模型参数显著性检验=================##
# 先计算t统计量，从fit中的Coefficients获得
t1 <- -1.3729/0.2201
t2 <- 1.0000/0.2889
# 再计算显著性水平
pt(t1,df=length(nj)-2,lower.tail=T)
pt(t2,df=length(nj)-2,lower.tail=F)

# 定义模型残差
res = fit$residuals
# 模型残差的白噪声检验
Box.test(res,type = "Ljung-Box")
# 预测
nj_forecast<-forecast(fit,4) 
#预测结果可视化
nj_forecast
plot(forecast(fit,4), xlab = 'Time',ylab = 'Electricity consumption')

#自动计算变为平稳序列需要的差分次数
ndiffs(nj)

#自动定阶
auto.arima(nj)






