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
raw <- read.xlsx('data0209.xlsx',sheet='shimo')
colnames(raw) <- c('date','province','city','confirm','heal','dead','source')
nat <- "湖北,上海,北京,四川,广东,云南,天津,山东,河南,浙江,重庆,黑龙江,宁夏,安徽,山西,广西,江苏,江西,河北,海南,湖南,福建,贵州,辽宁,内蒙古,吉林,新疆,甘肃,陕西,青海,西藏"#,台湾,香港,澳门"
nat <- strsplit(nat,',')[[1]]
raw <- mutate(raw,city=ifelse(is.na(city),province,city)) %>% 
  arrange(date,province,city) %>% 
  group_by(date,province,city) %>% 
  summarise(confirm=sum(confirm),heal=sum(heal),dead=sum(dead))
raw$date <- sapply(strsplit(raw$date,'月|日'),function(x){as.numeric(x[1])*100+as.numeric(x[2])})
raw <- raw %>% 
  filter(province%in%nat) %>% 
  group_by(date,province) %>% 
  summarise(confirm = sum(confirm)) %>% 
  mutate(provkey=paste(date,province)) 
datemap <- c(min(raw$date):131,201:max(raw$date))
provmap <- unique(raw$province)
raw <- data.table(date=datemap,sapply(provmap,function(i){
  x <- raw[match(paste(datemap,i),raw$provkey),]$confirm
  x[is.na(x)] <- 0
  cumsum(x)
}))

############################
# WHO data
############################

who <- read.xlsx('data0209.xlsx',sheet='who')
citymap <- read.xlsx('data0209.xlsx',sheet='citymap')
read.who <- function(x){
  x <- strsplit(x,'\ ')[[1]][-1:-3]
  x.value <- sort(which(!is.na(as.numeric(x))),decreasing=T)
  for(i in 2:(length(x.value))){
    if((x.value[i-1] - x.value[i])==1){
      x[x.value[i]] <- paste0(x[x.value[i]],x[x.value[i-1]])
      x[x.value[i-1]] <- ''
    }
  }
  x <- x[x!='']
  x.value <- sort(which(is.na(as.numeric(x))),decreasing=T)
  for(i in 2:(length(x.value))){
    if((x.value[i-1] - x.value[i])==1){
      x[x.value[i]] <- paste0(x[x.value[i]],'_',x[x.value[i-1]])
      x[x.value[i-1]] <- ''
    }
  }
  x <- t(matrix(x[x!=''],2))
  x <- data.table(city=tolower(x[,1]),confirm=as.numeric(x[,2]))
  x <- merge(x,citymap,by='city') %>% select(city=zh_name,confirm)
  rlt <- x$confirm
  names(rlt) <- x$city
  rlt
}
data.who <- sapply(who[,2],read.who)
data.who <- as.data.table(t(data.who[match(colnames(raw),rownames(data.who)),]))
colnames(data.who)[1] <- 'date'
data.who[,1] <- paste0('who',who[,1])
raw <- rbind(filter(raw,date<206),data.who)
data.model <- raw[,-1]
data.model.max <- max(data.model)*3
data.model <- data.model / data.model.max

############################
# Data Setup
############################

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
get_x <- function(x,i,p,h){
  x <- t(x[(1:p)-1+i,,drop=F])
  x
}

############################
# Validation
############################

sel <- 1
p <- 8
h <- 1

autoforecasting <- function(sel,p=8,h=1){
  print(paste(sel,Sys.time()))
  mfile <- lapply(1:(length(datemap)-p),get_model_file,h=h,p=p,x=data.model)
  mfile <- mfile[1:(length(mfile)-sel)]
  X <- do.call(rbind,lapply(mfile,function(x){x$x}))
  Y <- do.call(rbind,lapply(mfile,function(x){x$y}))
  dims <- c(32,16,8)
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
    batch = 128,
    epochs = 5000,
    verbose = 0
  ))
  Y.fit <- sapply((9:29)-8,function(i){
    temp <- get_x(data.model,i,p,h)
    temp <- temp[,ncol(temp)] * exp((model %>% predict(temp)))
    sum(temp)
  }) * data.model.max
  Y.actual <- rowSums(data.model)[-1:-8] * data.model.max
  rlt <- cbind(date=datemap[-1:-8], actual = Y.actual, fit = Y.fit, error = Y.fit/Y.actual-1)
  list(sel=sel,p=p,h=h,history=history,model=model,rlt=rlt)
}

rlt <- lapply(1:5,autoforecasting)
t(sapply(rlt,function(i){i$rlt[nrow(i$rlt)-i$sel+1,]}))

############################
# Modeling
############################

raw <- raw[,-1]
colnames(raw) <- citymap$city[match(colnames(raw),citymap$zh_name)]
