#UTF-8 with chinese
#以下中文内容应用UTF8编码，如无法正常显示可以open with encoding
#本部分为二分类结果变量数据分析及可视化过程

##载入需要的R包
library(compareGroups)
library(rms)
library(ggpubr)
library(finalfit)
library(gtsummary)
library(tidyverse)

#清空环境变量
rm(list = ls()) 
options(stringsAsFactors = T)

#以下读入文件并进行预览
setwd("D:/pipeline")
load("newdata.RData")
data_final <-newdata
str(data_final)

#以下将所有列名纳入一个向量并选择自变量
names(data_final)%>%dput()->allvar
allvar <- allvar[1:18];allvar 

#以下进行结果变量定义
data_final$Complication.Criteria%>%
             as.factor()->dependent


#以下进行单因素分析
explanatory <- allvar
data_final  %>%
  finalfit(dependent,explanatory,
           metrics=T,  #metrics=T表示输出模型检验的指标
           add_dependent_label=F) -> t2  #add_dependent_label=F表示不在表的左上角添加因变量标签。

write.csv(t2,"Univariate_analysis.csv")
