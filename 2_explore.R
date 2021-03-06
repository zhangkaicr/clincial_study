#UTF-8 with chinese
#以下中文内容应用UTF8编码，如无法正常显示可以open with encoding
#本部分为数据对比及配对

##数据将载入第一部分进行处理好载入
##文件夹请用英文命名
##读入的文件请转换为csv结尾的文件

##载入需要的R包
rm(list = ls()) 
options(stringsAsFactors = F)

library(compareGroups)
library(rms)
library(survival)
library(survminer)
library(explore)
library(ggpubr)
library(finalfit)
library(condsurv)
#以下读入文件并进行预览
load("crlm3_newdata.RData")
explore_data <-newdata
str(explore_data)


data_matched <- mutate(data_matched,grup = case_when(
  grup == 1 ~ "resection",
  grup == 0 ~"ablation"  )) 


#以下将进行亚组的限定
#table(explore_data$recall)
#explore_data <- subset(explore_data, recurrence  == "yes")#需要更改这里
explore_data <- mutate(explore_data,grup_new = case_when(
  rfs <= 12 ~ "early",
  rfs > 12 ~ "late"))  
#以下进行可视化compare操作进行
cGroupsWUI(port = 8102L)

explore(explore_data)

#以下进行代码操作并输出结果

tab_1<- descrTable(grup ~ .,
           method=2,#数值型变量参数非参数检验
           show.all = T,
           data = explore_data)

export2word(tab_1,file="tab_1.doc")

#以下绘制生存曲线
colnames(explore_data)
explore_data <-newdata
explore_data <- subset(explore_data,recurrence_type =="late")


explore_data$sur_time <- explore_data$OS
explore_data$sur_status <- explore_data$status
explore_data$sur_status<- ifelse(explore_data$sur_status=="death",1,0 )

fit <- survfit(Surv(sur_time,sur_status) ~ grup,  # 创建生存对象 
               data = explore_data) # 数据集来源

ggsurvplot(fit, # 创建的拟合对象
           data = explore_data,  # 指定变量数据来源
           conf.int = T, # 显示置信区间
           pval = TRUE, # 添加P值
           surv.median.line = "hv",  # 添加中位生存时间线
           risk.table = TRUE, # 添加风险表
           xlab = "Follow up time(month)", # 指定x轴标签
           legend = c(0.9,0.9), # 指定图例位置
           legend.title = "RFS", # 设置图例标题
           legend.labs = c("ablation", "resction"), # 指定图例分组标签
           break.x.by = 6,# 设置x轴刻度间距
           xlim=c(0,60),#设置X轴最长时间
           facet.by =)

explore_data$grup <- as.factor(explore_data$grup )

table(explore_data$grup)

#以下进行OS单因素分析

explore_data$ttevent <- with(explore_data, 
                           Surv(OS, as.integer(status =="death")))#构建time to event的结局变量
table2<-descrTable(ttevent~ .-OS-status,
                   show.ratio =TRUE,
                   data = explore_data)
table2

export2word(table2,"生存单因素分析.docx")

#以下对OS进行多因素分析

explore_data<- mutate(explore_data,status   = case_when(
  status == "death"~ 1 ,
  status == "alive" ~ 0 ))  

explanatory = c("grup","age_team","hepatitis_type",
                "gross_type","ptumor_stage1",
                "ptumor_lymph","metastasis_type","lm_number",
                "diameter_type","lm_prechemo","cea_type",
                "pre.ca19.9")
dependent= "Surv(OS, status)"
explore_data%>% finalfit(dependent,explanatory,
                        metrics=F,
                        add_dependent_label=F) -> t3;t3

write.csv(t3,"生存多因素分析.csv")



#以下进行亚组分析
noselect <- subset(explore_data,recurrence=="no"&rfs<=10)



ggdensity(explore_data, 
          x = "rfs", 
          color = "grup",
          fill = "grup",
          add = "median", 
          rug = TRUE,
          palette = "ngp")

colnames(rawdata)
ggline(rawdata,
       x="month",
       y="Chi_squar_value",
       title="Minimum P value method",
       palette="jco")

####
str(explore_data)

explore_data <-newdata

explore_data$sur_time <- explore_data$OS
explore_data$sur_status <- explore_data$status
explore_data$sur_status<- ifelse(explore_data$sur_status=="death",1,0 )



explore_data <-subset(explore_data,explore_data$grup=="ablation")

fit1 <- survfit(Surv(sur_time, sur_status) ~ 1, data = explore_data)

cond_times <- seq(0, 12 * 5, 12)

gg_conditional_surv(
  basekm = fit1, 
  at = cond_times,
  main = "Conditional survival",
  xlab = "months"
) 

data(lung)

py_install("pycox")
install.packages("survivalmodels")
reticulate::install_pycox(pip = TRUE, method = "auto",conda = "auto",install_torch = T)
install_keras(pip = TRUE, install_tensorflow = FALSE)
py_install("torch")
# load dependencies
library(survival)
library(survivalmodels)
library(reticulate)

train <- simsurvdata(100)
test <- simsurvdata(50)

fit <- akritas(Surv(time, status) ~ ., data = train)
predict(fit, newdata = test)


# Use distr6 = TRUE to return a distribution
predict_distr <- predict(fit, newdata = test, distr6 = TRUE)
predict_distr$survival(100)

# Return a relative risk ranking with type = "risk"
predict(fit, newdata = test, type = "risk")

Or both survival probabilities and a rank
predict(fit, newdata = test, type = "all", distr6 = TRUE)




