
x <- seq(1:24)

library(survival)
data(colon)

survaldata <- colon


for (i in x) {
  if ( survaldata$time > x){
    next
  }
  x[i] = i
}