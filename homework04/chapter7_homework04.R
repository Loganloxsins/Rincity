#数据导入
setwd("E:/MyProject/s4/Rlan/homework04")
data<-read.csv("./data04.csv",header=TRUE, sep=",")
##数据类型转换
#查看变量类型
class(data$N_type)
#将数值型数据转换为因子型数据
data$N_typenew<-as.factor(data$N_type)
#查看转换后的变量类型
class(data$N_typenew)
#查看变量类型
class(data$Stay)
#将字符串转换为数值型数据
data$Staynew<-as.numeric(data$Stay) 
# 出现报错，原因是数据中含有除数字以外的字符，系统将其判为缺失值
#剔除缺失值
datanew<-na.omit(data)
#检查转换后的变量类型
class(datanew$Staynew)
#异常值处理
datanew<-subset(datanew,Stay<=Age)
datanew


#查看数据结构
dim(data)# 显示数据库行列数，对数据变量数和样本容量进行概览
str(data)# 查看数据集整体信息，包括：变量个数、样本容量、变量名称、变量的数值类型以及部分变量数据
head(data)# 查看数据集前六行数据


# 查看数据集中每个变量的基本统计值，如最小值、四分位数、中位数、平均数、最大值等
summary(datanew)



##数值计算
#计算该变量的平均数，并赋值给m
m<-mean(datanew$Staynew)
#计算该变量的众数，并赋值给mo
mo<-table(datanew$Staynew)[table(datanew$Staynew)==
                             max(table(datanew$Staynew))] 
#计算该变量的中位数，并赋值给me
me<-median(datanew$Staynew) 
#计算该变量的最小值、最大值，并赋值给range
range<-range(datanew$Staynew) 
#计算该变量的四分位数，并赋值给qua
qua<-quantile(datanew$Staynew) 
#计算该变量的方差，并赋值给var
var<-var(datanew$Staynew)
#计算该变量的标准差，并赋值给std
std<-sd(datanew$Staynew)
#查看数据
m
mo
me
range
qua
var
std
##柱状图绘制
#将变量的数据分类状况赋值给type
type<-table(datanew$N_typenew)
#使用barplot命令进行绘制
bar <- barplot(type,#所用数据
               main="Neighbourhood Type Comparison",#图名
               xlab="Neighbourhood Type",#横坐标名称
               ylab="Numbers",#纵坐标名称
               ylim=c(0,200), #纵坐标取值范围
               col=c("cornflowerblue","darkcyan","dodgerblue4",
                     "blue"),#图表填充颜色可网络查询
               beside=TRUE#柱子水平排列
               )
height <- type

#设置柱子上的标签
text(bar,#标签标注在柱子上
     height+5,#位置为柱子顶端向上5个单位
     paste(height)#内容为height的数据
    )

#绘制图例
legend(0.5,200,# 图例位置
       c("Traditional","Social","Work Unit","Commercial"),#类别名称 
       fill=c("cornflowerblue","darkcyan","dodgerblue4",
              "blue"),#填充颜色
       bty="n",#不绘制图例外框
       ncol=1#分1列显示
      )


##饼图绘制
#将饼图中每一块所占比例的数据赋值给prop,*100是将数据扩大为百分比的数字
prop<-round(prop.table(table(datanew$Employ))*100,1)#1表示显示至1位小数

#查看prop
prop
#设置标签，按照顺序为4个类别命名
label<-c("有工作","失业","退休")


#饼图绘制
#pie命令绘制图表
pie(prop,# 绘图所用数据
    main="Employment Status",#图名
    clockwise = TRUE, #内容按顺时针绘制
    radius = 0.7, #设置图表半径
    labels = piepercent,#标签内容
    col=c("cornflowerblue","darkcyan","blue")#图标填充所用颜色
   )

#legend命令设置图例标签
legend(-1.5,1,#图例的位置坐标
       label,#图例名称使用label的内容
       fill=c("cornflowerblue","darkcyan",
              "blue"),#图例填充颜色
       bty="y",#绘制图例外框
       cex=1 #图例文字大小
      )

#设置饼图标签，将每一类所占的比例显示为百分比
piepercent<-prop
piepercent <-paste(piepercent,"%",sep ="")


##箱图绘制
#boxplot命令绘制图表
boxplot(Age~Education, #对education类别下的age绘制
        data=datanew, #所用数据来源
        col = "lightgray", #填充颜色
        main="Boxplot of Age", #图名
        xlab = "Education Level", #横坐标名称
        ylab = "Age" #纵坐标名称
       )



#散点图
plot(datanew$Age,datanew$Stay)














