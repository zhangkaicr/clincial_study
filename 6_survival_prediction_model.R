library(survivalmodels)
library(rms)
library(survival)
library(survminer)
library(ggpubr)
library(tidyverse)
library(gtsummary)
library(pec)
library(tidymodels)
library(ggDCA)
library(forestmodel)

rm(list = ls()) 
options(stringsAsFactors = T)



#载入最终用分析数据
setwd("D:/pipeline")
load("new_data.RData")
data_final <- new_data

str(data_final)

#构建模拟数据集
data("colon")
colon <- na.omit(colon)
data_final <- colon
str(data_final)

#区分训练集和验证集

set.seed(2022)
split<-initial_split(data_final,prop=.75)
data_train<-training(split)
data_test<-testing(split) 
str(data_train)
#构建COX回归模型
cox1 <- cph(Surv(time,status)~sex+differ+perfor, 
            data = colon, surv=TRUE)

forest_model(cox1)

#计算验证组不同时间点生存概率
t <- c(365,1095,1825)
survprob <- predictSurvProb(cox1,
                            newdata=data_test,
                            times=t)
head(survprob)

#计算cox回归C指数并绘图
c_index  <- cindex(list("Cox1"=cox1),
                   Surv(time,status)~sex+differ+perfor,
                   data=data_test,
                   eval.times=seq(30,1825,30),
                   splitMethod="bootcv",
                   B=1000)

plot(c_index,xlim = c(0,1825))

#以下绘制校准曲线
calPlot(list("cox1"=cox1),
        time=1095,#设置想要观察的时间点
        data=data_test,
        splitMethod = "BootCv",
        B=1000)

#以下绘制DCA曲线

