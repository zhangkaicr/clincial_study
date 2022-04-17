#UTF-8 with chinese
#以下中文内容应用UTF8编码，如无法正常显示可以open with encoding
#本部分为二分类结果变量数据分析及可视化过程

##载入需要的R包
library(forestmodel)
library(rms)
library(ggpubr)
library(finalfit)
library(gtsummary)
library(tidyverse)
library(QualInt)
library(eoffice)

#清空环境变量
rm(list = ls()) 
options(stringsAsFactors = T)

#以下读入文件并进行预览
setwd("D:/pipeline")
load("4newdata.RData")

data_final <-newdata
str(data_final)
names(data_final)
#以下进行亚组分析
data_final%>%filter(.,TBIL>=250)->data_final


data_final$Hb. <- ifelse(data_final$Hb. >=90,"normal","low" )
data_final$PD.duration<- ifelse(data_final$PD.duration >=300,"long","short" )
data_final$Hb. <- as.factor(data_final$Hb.)
data_final$PD.duration<- as.factor(data_final$PD.duration)

#以下将所有列名纳入一个向量并选择自变量
names(data_final)%>%dput()->allvar;allvar 
allvar <- allvar[1:18];allvar 
explanatory <- allvar[-10]

#以下进行结果变量定义
dependent <-"Complication.Criteria" 


#以下进行单因素分析
data_final  %>%
  finalfit(dependent,explanatory,
           metrics=T,  #metrics=T表示输出模型检验的指标
           add_dependent_label=F) -> t2  #add_dependent_label=F表示不在表的左上角添加因变量标签。

write.csv(t2,"1Univariate_analysis.csv")


#以下绘制多因素森林图

multimodel <- glm(Complication.Criteria ~ WBC + PBD + Hb. + blood.transfusion, 
                binomial(link="logit"), 
                data = data_final)
summary(multimodel)
p <- forest_model(multimodel);p
topptx(p,"p.pptx")
