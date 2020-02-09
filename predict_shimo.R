
rm(list=ls())
library(openxlsx)
library(data.table)
library(dplyr)
library(keras)
check <- function(x){data.frame(date=datemap,raw[names(raw)==x])}

############################
# Data Processing
############################

setwd('/Users/wenrurumon/Documents/posdoc/wuhan')
raw <- read.xlsx('data0209.xlsx')
colnames(raw) <- c('date','province','city','confirm','heal','dead','source')
nat <- "湖北,上海,北京,四川,广东,云南,天津,山东,河南,浙江,重庆,黑龙江,宁夏,安徽,山西,广西,江苏,江西,河北,海南,湖南,福建,贵州,辽宁,内蒙古,吉林,新疆,甘肃,陕西,青海,西藏"
nat <- strsplit(nat,',')[[1]]

raw <- mutate(raw,city=ifelse(is.na(city),province,city)) %>% 
  arrange(date,province,city) %>% 
  group_by(date,province,city) %>% 
  summarise(confirm=sum(confirm),heal=sum(heal),dead=sum(dead))
raw$date <- sapply(strsplit(raw$date,'月|日'),function(x){as.numeric(x[1])*100+as.numeric(x[2])})
raw <- raw %>% group_by(date,province,city) %>% summarise(
  confirm = max(confirm), heal=max(heal),dead=max(dead)
)

raw <- raw %>% mutate(citykey=paste(date,city),provkey=paste(date,province)) 
datemap <- c(min(raw$date):131,201:max(raw$date))
citymap <- unique(select(as.data.frame(raw),province,city))
raw <- raw %>% group_by(provkey,date,province) %>% 
  summarise(confirm=sum(confirm),heal=sum(heal),dead=sum(dead))
raw.idx <- raw %>% group_by(province) %>% summarise(
  cidx = sum(confirm), hidx = sum(heal), didx = sum(dead)
)
raw <- lapply(unique(citymap$province),function(i){
  x <- (raw[match(paste(datemap,i),raw$provkey),])
  x[is.na(x)] <- 0
  x <- apply(select(as.data.frame(x),confirm,heal,dead),2,cumsum)
  x
})
names(raw) <- unique(citymap$province)
raw.cn <- sapply(raw[(names(raw)%in%nat)],function(x){x[,1]})

# raw.cn <- log(raw.cn+1)
f.cn <- max(raw.cn)*1.2
raw.cn <- raw.cn/f.cn

############################
# Model File
############################

# x <- raw.cn
# i <- 1
# p <- 8
# h <- 1
get_model_file <- function(x,i,p,h){
  y <- t(x[1:h-1+p+i,,drop=F])
  x <- t(x[(1:p)-1+i,,drop=F])
  y <- y/x[,ncol(x)]
  y[is.na(y)] <- 1
  y[is.infinite(y)] <- 1
  y <- log(y)
  # y <- y-1
  list(x=x,y=y)
}
process_model_file <- function(data.model){
  X <- do.call(rbind,lapply(data.model,function(x){x$x}))
  Y <- do.call(rbind,lapply(data.model,function(x){x$y}))
  list(X=X,Y=Y)
}

############################
# Modeling China
############################

#China by city

p <- 8
h <- 1
train.cn <- lapply(1:(length(datemap)-p),get_model_file,h=h,p=p,x=raw.cn) 
# w <- c(1:18,0,0,0)
w <- c(rep(1,18),0,0,0)
train.cn <- rep(train.cn,w) %>% process_model_file()

X <- train.cn$X
Y <- train.cn$Y
dims <- c(32,16,4)
models <- rep('sigmoid',4)
e.input <- layer_input(shape=ncol(X))
e.layer <- layer_dense(e.input,dims[1],activation=models[1])
l.layer <- layer_dense(e.layer,dims[2],activation=models[2])
d.layer <- layer_dense(units=dims[3],activation=models[3])
d.output <- layer_dense(units=ncol(Y),activation=models[4])
d.input <- layer_input(shape=dims[3])
model <- keras_model(e.input,d.output(d.layer(l.layer)))
model %>% compile(
  loss = "mean_squared_error", 
  optimizer = "adam",
  metrics = c('mae')
)
system.time(history <- model %>% fit(
  x = X, 
  y = Y,
  epochs = 4000,
  verbose = 2
))

get_x <- function(x,i,p,h){
  x <- t(x[(1:p)-1+i,,drop=F])
  x
}

Y.fit <- sapply((9:29)-8,function(i){
    temp <- get_x(raw.cn,i,p,h)
    temp <- temp[,ncol(temp)] * exp((model %>% predict(temp)))
    # temp <- temp[,ncol(temp)] * ((model %>% predict(temp))+1)
    # temp <- model %>% predict(temp)
    sum(temp)
  }) * f.cn
Y.actual <- rowSums(raw.cn)[-1:-8] * f.cn
cbind(actual = Y.actual, fit = Y.fit, error = Y.fit/Y.actual-1)


