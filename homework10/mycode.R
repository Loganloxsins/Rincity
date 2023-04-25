#导入数据
setwd("E:/MyProject/s4/Rlan/homework10") 
data<-read.csv("./mydata23.CSV",header=TRUE, sep=",",fileEncoding = "GBK")
#查看数据特征
dim(data) 
names(data) 

#将data的第3-7列赋值给data0
data0<-data
data0
#对data0进行皮尔森相关性检验
cor(data0)	

#将data0的第2-5列赋值给data1
data1<-data0

#.packages("fpc")
#.packages("factoextra")
#.packages("cluster")
library(fpc)
library(factoextra)
library(cluster)

#设置随机数种子为7（具体数值可以自行设定）
set.seed(7) 
#逐一聚类数为1-10的结果，并迭代500次
gap_stat = clusGap(data1, FUN =kmeans , K.max = 10,B = 500) 
#可视化聚类结果
fviz_gap_stat(gap_stat)

stab6<-clusterboot(data1,B=100,bootmethod="boot", 
                   clustermethod= kmeansCBI, krange=4, seed=7)
print(stab6)

#设置随机数种子为7，确定类别数为6，对data1进行K-means聚类
set.seed (7)  
km<-kmeans(data1,4)
# 将基于data1聚类的km数据集中的km$cluster的值与data合并，并存于data中
data<-cbind(data,km$cluster)
# 将data数据集中的变量名km$cluster改为变量名kmcluster
names(data)[5]<-"kmcluster"
# 查看data数据
head(data)


write.csv(data, file= "E:/MyProject/s4/Rlan/homework10/out.csv")

#安装并调用fmsb包
#.packages("fmsb")
library(fmsb) 

# 查看每一类的变量均值
km$centers


# 根据均值设置每个维度变量展示的最大最小值
maxmin <- data.frame(CONSUMPTION=c(0, 1),  ELECTRICITY=c(1, 0), FOREST=c(1,0), GRAIN=c(0,1))
#将maxmin和km$centers纵向合并
radarfig <-rbind(maxmin,km$centers)
radarfig
#将radarfig转化为数据框格式
radarfig <- as.data.frame(radarfig)
radarfig

# 设置轴标签

colnames(radarfig) <- c("人均消费水平", "总发电量", "森林覆盖率","粮食产量")
# 设置雷达图标注字体、字号、图例样式
# 参数pty、seg、pcol、plty、plwd、cglty、cglcol、vlcex用于设置雷达图样式
# 参数 centerzero表示绘制刻度的格式
radarchart(radarfig,pty = 32, axistype=0, axislabcol="black", seg=1, 
           pcol = c('blue','yellow','red','green','black','purple'), 
           plty = 1, plwd = 2, cglty = 1, cglcol = "black", 
           centerzero = TRUE, vlcex = 1, title ="kmeans聚类分析")
#绘制图例
legend("topleft", c("第一类","第二类","第三类","第四类"),
       fill = c('blue','yellow','red','green'),cex = 0.8)

#.packages("eclust")
library(eclust)





#层次聚类，"hclust"表示聚类方式选择“层次聚类”
res.hc<- eclust(data1, k=4,"hclust")

#绘制树状图
fviz_dend(res.hc, rect = TRUE)

# 参数res.hc,geom指定要用于图形的几何图形的文本
# 参数ellipse表示是否围绕每个簇的点绘制轮廓，ellipse = F表示不绘制
# 参数shape表示点的形状，参数pointsize表示点的大小
# 参数show.clust.cent表示是否显示群集中心，show.clust.cent=F表示不显示
fviz_cluster(res.hc,geom = c("point"),ellipse = F, shape = 16, pointsize = 2, show.clust.cent=F)

# 计算距离矩阵，method=方法，该例中使用欧氏距离
ddata<-dist(data1,method="euclidean")
# 参数B表示每个方案的重新采样运行次数
# 参数bootmethod表示测试方式
# 参数clustermethod表示聚类方式, k表示聚类数量
stab<-clusterboot(ddata,B=100,bootmethod="boot", clustermethod=hclustCBI, k=4, seed=7,method="ward")
# 打印结果
print(stab)

# 将基于data1聚类的res数据集中的hc$cluster的值合并到给data中
data<-cbind(data1,res.hc$cluster)
# 将data数据集中的变量名hc$cluster改为变量名hcluster
names(data)[5]<-"hcluster"
# 查看data的变量名
head(data)
# 将数据导出为.csv格式，便于用excel查看以及后续空间可视化
write.csv(data, file="E:/MyProject/s4/Rlan/homework10/out.csv")



































