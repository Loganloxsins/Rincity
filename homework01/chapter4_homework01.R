.libPaths()##��ʾ������λ��
library()##��ʾ��������Щ��
search()#��ʾ��Щ���ѱ����ز��ɱ�ʹ��
install.packages()#��ѡ����������غͰ�װ
install.packages("eclust")##���ز���װָ����"eclust"
update.packages()##���������Ѱ�װ�İ�
installed.packages()##�г��Ѱ�װ�İ��������汾�š�������Ϣ��
library(eclust)##���ð�"eclust"
help(package="eclust")##�����"eclust"�������Լ����еĺ������ƺ����ݼ����Ƶ��б�



##���ݵ���
setwd("E:/MyProject/s4/Rlan")
data<-read.csv("./City_CarbonEmission.csv",header=TRUE, sep=",",fileEncoding = 'GBK')
head(data)
summary(data)
hist(data$CarbonEmssion.Mt,main = "",col="blue",xlab="CarbonEmission(MT)")
##����R�汾
install.packages("installr")
require(installr)
updateR()

dev.new()##�����µ�ͼ��
plot(data$CarbonEmssion.Mt)##����ͼ��
















