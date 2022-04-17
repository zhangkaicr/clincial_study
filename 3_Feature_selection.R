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
library(Blasso)
library(survival)
library(rms)
library(randomForestSRC)
library(tidymodels)

#以下读入文件并进行预览
#以下设定工作目录
setwd("D:/pipeline")

#文件放入工作目录内并进行读取

load("crlm_newdata.RData")
explore_data <-newdata
str(explore_data)
names(explore_data)

#以下确定自变量

explore_data%>%select(.,c(2:57),status,OS)->explore_data
str(explore_data)
##以下利用随机生存森林筛选重要变量
class(allvar)

rfsrc<- rfsrc(Surv(OS, status) ~., 
                     data = explore_data, nsplit = 10, 
                     na.action = "na.impute", tree.err = TRUE,  
                     importance = TRUE)
rfsrc$importance

class(rfsrc$importance)
