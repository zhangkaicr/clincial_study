library(finalfit)
library(compareGroups)
library(rms)
library(survival)
library(survminer)
library(ggpubr)
library(tidyverse)
library(gtsummary)

rm(list = ls()) 
options(stringsAsFactors = T)

#载入最终用分析数据
setwd("D:/pipeline")
load("crlm_newdata.RData")
data_final <- newdata
str(data_final)
data_final <- read.csv("miniP.csv",header = T)
#以下进行亚组限定
newdata%>%filter(.,team=="late")->data_final


#以下绘制生存曲线
data_final$sur_time <- data_final$OS
data_final$sur_status <- data_final$status
data_final$sur_status<- ifelse(data_final$sur_status=="death",1,0 )

fit <- survfit(Surv(sur_time,sur_status) ~ grup,  # 创建生存对象 
               data = data_final) # 数据集来源

ggsurvplot(fit, # 创建的拟合对象
           data = data_final,  # 指定变量数据来源
           conf.int = F, # 显示置信区间
           pval = TRUE, # 添加P值
           surv.median.line = "hv",  # 添加中位生存时间线
           risk.table = TRUE, # 添加风险表
           xlab = "Follow up time(month)", # 指定x轴标签
           legend = c(0.8,0.75), # 指定图例位置
           break.x.by = 6, # 设置x轴刻度间距
           cumcensor=T,#是否显示删失表
           xlim = c(0, 60)) #横轴取值范围

#以下计算点生存率估计（1,3,5年生存率）
tbl_survfit(fit,
            times=c(12,36,60),
            label_header = "**{time} Month**")

fit#显示中位生存时间

##以下进行单因素分析

cGroupsWUI(port = 8102L)


##以下进行生存多单因素分析
str(data_final)

explanatory = c("grup","gross_type","ptumor_stage1",
                "ptumor_lymph","metastasis_type",
                "lm_number")

dependent= "Surv(rfs, recurrence)"
data_final %>% finalfit(dependent,explanatory,
                        metrics=F,
                        add_dependent_label=F) -> t3
t3
write.csv(t3,"rfs多因素分析.csv")
