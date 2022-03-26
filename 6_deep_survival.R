library(survivalmodels)
library(survivalsvm)
library(survival)
library(survminer)
library(ggpubr)
library(tidyverse)
library(survivalROC)

rm(list = ls()) 
options(stringsAsFactors = T)

#载入最终用分析数据

load("data_matched.RData")
data_final <- data_matched

str(data_final)

#变化为分析需要的格式

data_final <- mutate(data_final,status   = case_when(
  status == "death"~ 1 ,
  status == "alive" ~ 0 ))  

data_final <- mutate(data_final,recurrence   = case_when(
  recurrence == "yes"~ 1 ,
  recurrence == "no" ~ 0 ))  

data_final$gross_type<- factor(data_final$gross_type, 
                               order = F, 
                               levels = c("ulcerative", "protrude","infiltrating"))
data_final$lm_number<- relevel(data_final$lm_number,ref = "single")

str(data_final)

allcol<- dput(names(data_final)) #纳入所有列变量
allcol

#以下绘制生存曲线
data_final$sur_time <- data_final$rfs
data_final$sur_status <- data_final$recurrence
data_final$sur_status<- ifelse(data_final$sur_status=="yes",1,0 )

fit <- survfit(Surv(sur_time,sur_status) ~ grup,  # 创建生存对象 
               data = data_final) # 数据集来源
ggsurvplot()

tbl_survfit(fit,
            times=c(12,36,60),
            label_header = "**{time} Month**")


##以下进行单因素分析

data_final$ttevent <- with(data_final, 
                           Surv(rfs, as.integer(recurrence ==1)))#构建time to event的结局变量
table2<-descrTable(ttevent~ .-rfs-recurrence,
                   show.ratio =TRUE,
                   data = data_final);table2


export2word(table2,"rfs单因素分析.docx")



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
