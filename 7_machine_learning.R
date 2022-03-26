library(openxlsx)
library(Boruta)
library(randomForest)
library(caret)
library(pROC)
library(e1071)
library(tidyverse)
library(caret)
library(tableone)
library(gbm)
library(MLmetrics)
library(C50)
library(kernlab)
library(naivebayes)
library(mda)
library(earth)
library(mlbench)
library(caret)
filter=dplyr::filter
select=dplyr::select


train=read.table("./train.txt", header = T,stringsAsFactors = F )
test = read.table("./test.txt", header = T,stringsAsFactors = F )


# 加载数据集
#data(PimaIndiansstate)

#control <- rfeControl(functions = caretFuncs, method = "cv", number = 5)

train <- train[,-c(1,2)]
test <-test[,-c(1,2)]

train$state <- ifelse(train$state=="1","dead","alive")
test$state <- ifelse(test$state=="1","dead","alive")
train$state <- as.factor(train$state)
test$state<- as.factor(test$state)

# df <- PimaIndiansstate

set.seed(13)
# train_index = sample(1:nrow(df), 0.80 * nrow(df))
# train =df[train_index, ]
# test = df[-train_index, ]

MySummary  <- function(data, lev = NULL, model = NULL){
  a1 <- defaultSummary(data, lev, model)
  b1 <- twoClassSummary(data, lev, model)
  c1 <- prSummary(data, lev, model)
  out <- c(a1, b1,c1)
  out}
myControl = trainControl(method = "cv", number = 5, verboseIter = FALSE)
fitControl <- trainControl(#   
  method = "cv",#   
  number = 5,
  summaryFunction=MySummary,
  classProbs=T,#   
  savePredictions = T,#   
  verboseIter = F)# 
myControl=fitControl




set.seed(12)
model_lm =train(state ~ ., 
                data = train, 
                method = "glm",
                family = "binomial",
                trControl = fitControl)
summary(model_lm)

# 2.Random forest model (using ranger)
# mtry (#Randomly Selected Predictors)
# splitrule (Splitting Rule)
# min.node.size (Minimal Node Size)


tgrid = expand.grid( mtry = 2:6,
                     splitrule = c("extratrees","gini"),
                     min.node.size = c(seq(10,50,5)))

tgrid = expand.grid( mtry = 2,
                     splitrule = c("extratrees"),
                     min.node.size =10)
set.seed(12)
model_rf = train(state ~ ., 
                 data = train, 
                 tuneLength = 1,
                 method = "ranger",
                 tuneGrid=tgrid ,
                 importance = 'impurity',
                 num.trees=500,
                 trControl = fitControl)

# confusionMatrix(data=predict(model_rf1, newdata=test), test$Lymph)
# compare <- evalm(list(model_rf, model_rf1), gnames=c('rf1','rf2'))

# 3.GBM
#  n.trees (# Boosting Iterations)
#   interaction.depth (Max Tree Depth)
#   shrinkage (Shrinkage)
#   n.minobsinnode (Min. Terminal Node Size)
tgrid <-  expand.grid(interaction.depth = c(3),
                      n.trees = 550,
                      shrinkage =0.008,
                      n.minobsinnode =10) # you can also put something

set.seed(12)
model_gbm = train(state ~ ., 
                  data = train,
                  tuneLength = 2,
                  method = "gbm",
                  metric = "AUC",  
                  tuneGrid=tgrid,
                  trControl = myControl)
# 4.Bayes
tgrid = data.frame(usekernel=TRUE,
                   laplace = c(0,1,10),
                   adjust=c(2,8,16))

tgrid = data.frame(usekernel=TRUE,
                   laplace = 0,
                   adjust=2)
set.seed(12)
model_bayes = train(state ~ ., 
                    data = train,
                    method = "naive_bayes", 
                    linout = TRUE,
                    metric = "AUC",  
                    tuneGrid=tgrid,
                    trControl = myControl)

#Naive algorithm
set.seed(12)
NaiveModel <- train(state ~ ., 
                    data = train,
                    metric = "AUC",  
                    method = "naive_bayes")


# 5.SVM
# tgrid=expand.grid(sigma=c(seq(0,0.05,0.15,0.2)),
#                   C=c(seq(0,0.05,0.15,0.2)))

tgrid=expand.grid(sigma=c(0.05),
                  C=c(0.15))
set.seed(12)
model_svm = train(state ~ ., 
                  data = train,
                  method='svmRadial' , verbose=F,
                  metric = "ROC",   
                  tuneLength = 10,
                  tuneGrid=tgrid,
                  trControl = myControl,
                  importance = TRUE)

# 6.DT
set.seed(12)
tgrid = expand.grid(
  cp = c(seq(0,1,0.01)))

model_dt = train(state ~ ., 
                 data = train,
                 method = "rpart",
                 metric = "AUC",   
                 tuneLength = 10,
                 tuneGrid=tgrid,
                 trControl = myControl)

# Train a model with above parameters. We will use C5.0 algorithm
tgrid=expand.grid(trials=c(seq(1,20,5)),
                  model="tree",
                  winnow=F)
DecTreeModel = train(state ~ ., 
                     data = train,
                     method = "C5.0",
                     tuneGrid=tgrid,
                     trControl = myControl)


# 7.nnet
set.seed(12)
tgrid = expand.grid(.decay = c(seq(0,0.8,0.1)), .size = c(5, 6, 7))
tgrid = expand.grid(.decay = c(0.7), .size = c(6))
model_nnet = train(state ~ ., 
                   data = train,
                   method='nnet',
                   tuneGrid=tgrid,
                   trControl = myControl)  
model_nnet

# caret-nnet



# 8.Flexible Discriminant Analysis
tgrid=expand.grid(degree = c(0,1,3,8),nprune = c(10,20,100))
model_fda = train(state ~ ., 
                  data = train,
                  method='fda',
                  tuneGrid=tgrid,
                  trControl = myControl)  
model_fda


# 8.xgboost
# model_xgboost=train(state~.,
#                     data=train,
#                     method = "xgbLinear",
#                     metric = "AUC",   
#                     tuneLength = 10,
#                     trControl = myControl)
# 
model_xgboost = train(state ~ ., 
                      data = train,
                      method='xgbTree',
                      trControl = myControl)  
model_xgboost 




## binds all
all=list(LF=model_lm,
         #RF=model_rf,
         GBM=model_gbm,
         Bayes=model_bayes,
         SVM=model_svm,
         DT=model_dt,
         NNET=model_nnet,
         XGBOOST=model_xgboost)

model_all=resamples(all)


## compare with each model
library(MLeval)
x <-evalm(all,rlinethick=0.2,fsize=12,plots=c())


###############################################################
#####
###############################################################
df_imp=c()
for (i in c(1,2,5,6,7)) {
  fit=all[[i]]
  # bind data 
  a=varImp(fit)
  b=as.data.frame(a$importance)
  df= tibble(variables=rownames(b),
             Score=b$Overall,
             model=names(all)[i]) %>% 
    arrange(desc(Score)) %>% mutate(id=1:n())
  df_imp=rbind(df_imp,df)
}


p_im=ggplot(df_imp)+
  geom_bar(aes(x=reorder(-id,Score),
               Score,fill=variables),
           stat = "identity")+
  coord_flip()+
  facet_wrap(model~.)+
  theme_bw() +
  theme(
    panel.border = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), 
    axis.line = element_line(colour = "black"))+
  labs(title = "Variable Importance",y="Importance",x="Variables")

p_im
ggsave("9-Variable Importance.pdf", dpi = 300)




# Plot ROC with all model
# Compute AUC for predicting Class with the model
library(ROCR)
library(pROC)

dfROC=c()
for (i in 1:length(all)) {
  fit=all[[i]]
  result.predicted.prob = predict(fit, newdata=test, type="prob") # Prediction
  result.predicted.prob$labels=test$state
  pred = prediction(result.predicted.prob$dead, result.predicted.prob$labels)
  perf = performance(pred,"tpr","fpr")
  df_Roc = data.frame(x = unlist(perf@"x.values") , y = unlist(perf@"y.values"))
  df_Roc$Model <- names(all)[i]
  dfROC=rbind(dfROC,df_Roc)
}

# plot POC
library(scales)
dfROC=dfROC %>% filter(!Model %in% c("Bayes","SVM"))
ggplot(dfROC) + 
  geom_line(aes(x,y, color = Model), size = 1.2) + 
  #scale_color_manual('',values=brewer.pal(length(unique(daux_roc$score)), "RdBu")) +
  geom_path(data=data.frame(x = c(0,1), y = c(0,1)),
            aes(x,y), colour = "gray", size = 1) +
  scale_x_continuous("False Positive Rate (1 - Specificity)",
                     labels = percent_format(), limits = c(0, 1)) +
  scale_y_continuous("True Positive Rate (Sensivity or Recall)",
                     labels = percent_format(), limits = c(0, 1)) +
  theme_bw() +
  theme(
    panel.border = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), 
    axis.line = element_line(colour = "black"))+
  ggtitle("The area under the curve for all models")->p_roc

p_roc
ggsave("9-ROC-Mdoel.pdf", dpi = 300)



library(PRROC)
calc_auprc <- function(model, data){
  
  index_class2 <- data$state == "alive"
  index_class1 <- data$state == "dead"
  
  predictions <- predict(model, data, type = "prob")
  
  pr.curve(predictions$dead[index_class2],
           predictions$dead[index_class1],
           curve = TRUE)
}

# Get results for all 5 models

model_list_pr <-all %>%
  map(calc_auprc, data = test)

model_list_pr %>%
  map(function(the_mod) the_mod$auc.integral)



# Plot the AUPRC curve for all 5 models
results_list_pr <- list(NA)
num_mod <- 1

for(the_pr in model_list_pr){
  
  results_list_pr[[num_mod]] <- 
    data_frame(recall = the_pr$curve[, 1],
               precision = the_pr$curve[, 2],
               model = names(model_list_pr)[num_mod])
  
  num_mod <- num_mod + 1
  
}

results_df_pr <- bind_rows(results_list_pr)
results_df_pr=results_df_pr %>% filter(!model %in% c("Bayes","SVM"))
ggplot(aes(x = recall, y = precision, group = model),
       data = results_df_pr) +
  geom_line(aes(color = model), size = 1) +
  geom_abline(intercept =
                sum(test$state == "Yes")/nrow(test),
              slope = 0, color = "gray", size = 1)+
  theme_bw() +
  theme(
    panel.border = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), 
    axis.line = element_line(colour = "black"))+
  labs(title = "The precision-recall curve for all models",x="Recall",y="Precision")->p_pr

p_pr

ggsave("9-PR-Mdoel.pdf", dpi = 300)

p_roc+p_pr
ggsave("9-AUCPR.pdf", dpi = 300,width = 10,height =6)



#### get out puts

df_result=c()
for (i in 1:length(all)) {
  fit=all[[i]]
  # bind data 
  # AUC
  library(ROCR)
  library(pROC)
  pre.prob = predict(fit, newdata=test, type="prob") # Prediction
  pre.pred = predict(fit, newdata=test) # Prediction
  df_pre=tibble(obs=test$state,
                Yes=pre.prob$alive,
                No=pre.prob$dead,
                Group="lm") %>% as.data.frame()
  
  result.roc = roc(test$state, pre.prob$alive) 
  PR=calc_auprc(fit,test)
  # all martix
  a=confusionMatrix(data=predict(fit, newdata=test), test$state)
  tocsv = as.tbl(round(data.frame(cbind(t(a$overall),t(a$byClass))),3))
  tocsv=tocsv %>% mutate(AUC=round(as.numeric(result.roc$auc),3),.before=1)
  tocsv=tocsv %>% mutate(PROC=round(PR$auc.integral,3),.before=2)
  
  tocsv=tocsv %>% mutate(model=names(all)[i],.before=1)
  print(tocsv[,1:3])
  df_result=rbind(df_result,tocsv)
}




df_result=df_result %>% arrange(desc(AUC))
#write.csv(df_result,"9-Model output.csv")
df_pre=tibble(obs=test$state,
              Yes=pre.prob$alive,
              No=pre.prob$dead,
              Group="lm") %>% as.data.frame()

result.roc = roc(test$state, pre.prob$dead) 
PR=calc_auprc(fit,test)
# all martix
a=confusionMatrix(data=predict(fit, newdata=test), test$state)
tocsv = as.tbl(round(data.frame(cbind(t(a$overall),t(a$byClass))),3))
tocsv=tocsv %>% mutate(AUC=round(as.numeric(result.roc$auc),3),.before=1)
tocsv=tocsv %>% mutate(PROC=round(PR$auc.integral,3),.before=2)

tocsv=tocsv %>% mutate(model=names(all)[i],.before=1)
print(tocsv[,1:3])
df_result=rbind(df_result,tocsv)


df_result=df_result %>% arrange(desc(AUC))
write.csv(df_result,"Test_Result.csv")
#write.csv(df_result,"9-Model output.csv")

#?��?ͼƬ
df_model=df_imp %>% filter(model=="GBM")
df_model

# #xlab=c("Size","DLNM(Yes)", "Age (Younger)","Gender(Male)","Multifocality (Yes)","Location (Inferior)",
#        "CLT (Yes)","Bilateral (Yes)",
#        "Location (Middle)","Location (Isthmus)")
df_model$xlab= df_model$variables
ggplot(df_model)+
  geom_bar(aes(x=reorder(variables,Score),Score),
           stat = "identity")+
  scale_x_discrete(labels =rev(df_model$xlab))+
  coord_flip()+
  theme_bw() +
  theme(
    panel.border = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), 
    axis.line = element_line(colour = "black"))+
  labs(title = "Variable importance of GBM",y="Importance",x="")->p1
p1
ggsave("9-GBM.pdf",dpi = 300)


### 2.DT 
df_model=df_imp %>% filter(model=="DT") %>% filter(Score>0)
df_model

# xlab=c("Size", "Age (Younger)","DLNM(Yes)","Multifocality (Yes)","Gender(Male)","Location (Inferior)",
#        "Bilateral (Yes)","Location (Middle)", "CLT (Yes)")
df_model$xlab= df_model$variables
ggplot(df_model)+
  geom_bar(aes(x=reorder(id,Score),Score),
           stat = "identity")+
  scale_x_discrete(labels =rev(df_model$xlab))+
  coord_flip()+
  theme_bw() +
  theme(
    panel.border = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), 
    axis.line = element_line(colour = "black"))+
  labs(title = "Variable importance of DT",y="Importance",x="")->p2

p2
ggsave("9-DT.pdf",dpi = 300)

### 3.     LM    NNET      RF XGBOOST 
df_model=df_imp %>% filter(model=="LF") 
df_model

# xlab=c("DLNM(Yes)","Size", "Age (Younger)","Gender(Male)","Multifocality (Yes)","Location (Inferior)",
#        "CLT (Yes)","Location (Isthmus)","Location (Middle)","Bilateral (Yes)")
df_model$xlab= df_model$variables
ggplot(df_model)+
  geom_bar(aes(x=reorder(id,Score),Score),
           stat = "identity")+
  scale_x_discrete(labels =rev(df_model$xlab))+
  coord_flip()+
  theme_bw() +
  theme(
    panel.border = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), 
    axis.line = element_line(colour = "black"))+
  labs(title = "Variable importance of LF",y="Importance",x="")->p3
p3
ggsave("9-LM.pdf",dpi = 300)

### 5.NNET     LM    NNET      RF XGBOOST 
df_model=df_imp %>% filter(model=="NNET") #%>% filter(Score>0)
df_model

# xlab=c("DLNM(Yes)","Size", "Age (Younger)","Multifocality (Yes)","Gender(Male)","Location (Middle)","Location (Inferior)",
#        "Bilateral (Yes)","CLT (Yes)","Location (Isthmus)")
df_model$xlab= df_model$variables
ggplot(df_model)+
  geom_bar(aes(x=reorder(id,Score),Score),
           stat = "identity")+
  scale_x_discrete(labels =rev(df_model$xlab))+
  coord_flip()+
  theme_bw() +
  theme(
    panel.border = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), 
    axis.line = element_line(colour = "black"))+
  labs(title = "Variable importance of NNET",y="Importance",x="")->p4
p4
ggsave("9-NNET.pdf",dpi = 300)

### 5.RF XGBOOST 
df_model=df_imp %>% filter(model=="RF") 
df_model

# xlab=c("DLNM(Yes)","Size", "Age (Younger)","Multifocality (Yes)","Gender(Male)","CLT (Yes)","Location (Inferior)",
#        "Bilateral (Yes)","Location (Middle)","Location (Isthmus)")
df_model$xlab= df_model$variables
ggplot(df_model)+
  geom_bar(aes(x=reorder(id,Score),Score),
           stat = "identity")+
  scale_x_discrete(labels =rev(df_model$xlab))+
  coord_flip()+
  theme_bw() +
  theme(
    panel.border = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), 
    axis.line = element_line(colour = "black"))+
  labs(title = "Variable importance of RF",y="Importance",x="")->p5
p5
ggsave("9-RF.pdf",dpi = 300)

### 7. XGBOOST 
df_model=df_imp %>% filter(model=="XGBOOST") 
df_model
# xlab=c("DLNM (Yes)","Size", "Age (Younger)","Gender(Male)","Multifocality (Yes)","Location (Inferior)",
#        "Bilateral (Yes)","CLT (Yes)","Location (Isthmus)","Location (Middle)")
df_model$xlab= df_model$variables
ggplot(df_model)+
  geom_bar(aes(x=reorder(id,Score),Score),
           stat = "identity")+
  scale_x_discrete(labels =rev(df_model$xlab))+
  coord_flip()+
  theme_bw() +
  theme(
    panel.border = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), 
    axis.line = element_line(colour = "black"))+
  labs(title = "Variable importance of XGBOOST ",y="Importance",x="")->p6
p6
ggsave("9-XGBOOST.pdf",dpi = 300)

library(patchwork)

(p3+p2+p1)/
  (p4+p5+p6)+plot_annotation(tag_levels="A")
ggsave("9-var.pdf",dpi = 300,width = 16, height = 8)
ggsave("9-var.pdf",width = 16, height = 9)



####################################################################################
### Out put the Training results
####################################################################################
df_trainlist = list(model_lm, model_rf,model_gbm,model_dt,
                    model_xgboost,model_nnet)

df_trainResults=c()
for (i in 1:length(df_trainlist)) {
  a=df_trainlist[[i]]$results[1,] %>% as_tibble() %>% 
    select(Accuracy,Kappa,ROC,Sens,Spec,AUC,Precision,Recall,
           'F',AccuracySD,KappaSD,ROCSD,SensSD,SpecSD,AUCSD,PrecisionSD)
  
  df_trainResults=rbind(df_trainResults,a)
}
df_trainResults=df_trainResults %>% mutate(Model=c('LR','RF',"GBM","DT","XGBOOST","NNET"),.before=1)

write.csv(df_trainResults,"Train_Result.csv")

####################################################################################
### Out put the Training ROC
####################################################################################

library(MLeval)
df_trainlist = evalm(list(model_lm, model_rf,model_gbm,model_dt,model_xgboost,model_nnet), 
                     gnames=c('LR','RF',"GBM","DT","XGBOOST","NNET"))

df_trainROC =df_trainlist$roc$data 
colnames(df_trainROC)=colnames(dfROC)

## plot training ROC
ggplot(df_trainROC) + 
  geom_line(aes(x=y,y=x, color = Model), size = 1.2) + 
  #scale_color_manual('',values=brewer.pal(length(unique(daux_roc$score)), "RdBu")) +
  geom_path(data=data.frame(x = c(0,1), y = c(0,1)),
            aes(x,y), colour = "gray", size = 1) +
  scale_x_continuous("False Positive Rate (1 - Specificity)",
                     labels = percent_format(), limits = c(0, 1)) +
  scale_y_continuous("True Positive Rate (Sensivity or Recall)",
                     labels = percent_format(), limits = c(0, 1)) +
  theme_bw() +
  theme(
    panel.border = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), 
    axis.line = element_line(colour = "black"))+
  ggtitle("The area under the curve for all models with train data")

ggsave("Train ROC.pdf",dpi = 300)

