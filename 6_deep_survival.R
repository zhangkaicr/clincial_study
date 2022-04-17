library(survivalmodels)
library(survivalsvm)
library(survival)
library(survminer)
library(ggpubr)
library(eoffice)
library(tidyverse)
library(survivalROC)
library(reticulate)
library(tidymodels)
library(pec)
library(rms)

rm(list = ls()) 
options(stringsAsFactors = T)

#以下读入文件并进行预览
#以下设定工作目录
setwd("D:/pipeline")
library(ggDCA)
library(`ggRandomForests-master`)
#文件放入工作目录内并进行读取

load("crlm_newdata.RData")
final_data <-newdata
str(final_data)
names(final_data)

#以下确定自变量
final_data%>%select(.,grup,ptumor_stage1,lm_number,cea_group,
                    status,OS)->final_data
final_data$status <- ifelse(final_data$status=="death",1,0)

split<-initial_split(final_data,prop=.75)
data_train<-training(split)
data_test<-testing(split) 
str(final_data)
# all defaults
deepres <- deepsurv(data = data_train, 
         time_variable = "OS",
         status_variable ="status",
         frac = 0.3, 
         activation = "relu",
         num_nodes = c(4L, 8L, 4L, 2L),
         dropout = 0.1, 
         reverse=F,
         early_stopping = TRUE, 
         epochs = 10L,
         batch_size = 32L)


p <-predict(deepres, type = "", newdata = data_train)

survivalmodels::cindex(risk = p, truth = data_train[,"OS"])


res.cox<- coxph(Surv(OS, status) ~ptumor_stage, data =  data_train) 
summary(res.cox)
res.cox
cph()







