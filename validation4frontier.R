

rm(list=ls())
library(dplyr)
library(forecast)
library(keras)

setwd('/Users/wenrurumon/Documents/posdoc/wuhan/data/github')
raw <- read.csv('global.csv') %>% select(date=X,accum=China)
raw$new <- c(0,diff(raw$accum))

test <- function(h,tl=47){
  x <- raw[1:(47-h),]$accum
  idx <- 1:(47-h)
  model1 <- lm(x~I(log(idx)))
  x2 <- x-predict(model1)
  x2 <- cbind(1,log(1:tl)) %*% coef(model1) + c(x2,forecast(auto.arima(x2),h=tl-length(x))$mean)
  x2 <- diff(c(0,x2)); x2[x2<0] <- 0
  x2
}
apply(sapply(1:5,test)[1:47,],2,cumsum)[43:47,]

################################################

h <- 0
tl <- nrow(raw)
x <- raw[1:(47-h),]$accum
idx <- 1:(47-h)
model1 <- lm(x~I(log(idx)))
x2 <- x-predict(model1)
x2 <- cbind(1,log(1:tl)) %*% coef(model1) + c(x2,forecast(auto.arima(x2),h=tl-length(x))$mean)
x2 <- diff(c(0,x2)); x2[x2<0] <- 0

cbind(raw,x2)
cumsum(x2)
