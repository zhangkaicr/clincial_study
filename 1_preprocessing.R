#UTF-8 with chinese
#以下中文内容应用UTF8编码，如无法正常显示可以open with encoding
#本部分为数据预处理，将进行数据载入及清洗

##数据载入，请把文件名放在与代码脚本同一个文件夹下
##文件夹请用英文命名
##读入的文件请转换为csv结尾的文件

##载入需要的R包
rm(list = ls()) 
options(stringsAsFactors = T)

library(tidyverse)
library(naniar)
library(UpSetR)

#以下读入文件并进行预览
#以下设定工作目录
setwd("D:/pipeline")


#文件放入工作目录内并进行读取

rawdata <- read.csv("4.csv",header = T,
                    row.names = 1,na.strings = "na")
str(rawdata)#注意变量类型
##输出变量名称及转换为因子变量类型

##以下将需部分变量变为分类变量
vars <- c("ASA ") 
rawdata<- rawdata %>%
  mutate(across(one_of(vars), as.factor))
str(rawdata)

##更改变量因子水平
rawdata$PBD <- relevel(rawdata$PBD,ref = "none")



##查看没有缺失值的行的总体数据集
completa_data<- (rawdata[complete.cases(rawdata),])

##以下查看数据集中缺失值的情况

###以下绘制缺失数据热图
vis_miss(rawdata,
         show_perc = TRUE,
         show_perc_col = TRUE,
         cluster = TRUE)
###以下绘制缺失数据upset图，绘制前10个最多缺失的列
gg_miss_upset(rawdata,nsets = 15)


###一线按照比例进行缺失列内容的删除

na.per<- (dim(rawdata)[1]/1000)###定义每列删除缺失的阈值

###定义删除缺失值函数为naf
naf<-function(x){
  nas<-sum(is.na(x))
  return(nas)
}

###以下按照缺失值阈值进行筛选
na_names<- apply(rawdata,2, naf)%>%
  as.data.frame
newdata<- rawdata[,(na_names[,1]<= na.per)]

以下显示删除的列
colnames(rawdata[,(na_names[,1]>na.per)])


##以下筛选亚组进行保存
#data_pattrn <- subset(newdata,grup=="ablation"&recurrence=="yes")



##以下保存本次处理数据
##newdata为按照阈值删除列后内容
##completa_data为没有缺失的数据集

save(newdata,file = "newdata.RData")

