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

rm(list = ls()) 
options(stringsAsFactors = T)

simdata <- simsurvdata(1000)




# all defaults
deepsurv(data = simsurvdata(50))
akritas(Surv(time, status) ~ ., data = simsurvdata(50))
# common parameters
fit<- deepsurv(data = simsurvdata(50), 
         frac = 0.3, 
         activation = "relu",
        num_nodes = c(4L, 8L, 4L, 2L), 
        dropout = 0.1, 
        early_stopping = TRUE, 
        epochs = 100L,
        batch_size = 32L)

summary(fit)

res<- predict(fit, newdata = simsurvdata(50))
res$survival(100)


set.seed(10)
data <- simsurvdata(20)
fit <- deepsurv(data = data[1:10, ])
p <- predict(fit, type = "all", newdata = data[11:20, ])
cindex(risk = p, truth = data[11:20, "time"])

fit <- coxtime(data = simsurvdata(50))
predict(fit, simsurvdata(10), type = "all")
predict(fit, simsurvdata(10), distr6 = TRUE)
res<- deephit(data = simsurvdata(50), frac = 0.3, activation = "relu",
        num_nodes = c(4L, 8L, 4L, 2L), dropout = 0.1, early_stopping = TRUE, epochs = 100L,
        batch_size = 32L)
summary(res)
coxtime(data = simsurvdata(50), frac = 0.3, activation = "relu",
        num_nodes = c(4L, 8L, 4L, 2L), dropout = 0.1, early_stopping = TRUE, epochs = 100L,
        batch_size = 32L)





