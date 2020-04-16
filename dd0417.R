
###################################
# Module
###################################

rm(list=ls())
library(data.table)
library(dplyr)
library(keras)
library(forecast)

#Get data from github
setwd('/Users/wenrurumon/Documents/posdoc/wuhan/data/github')
raw <- lapply(c('china.csv','global.csv','us.csv'),function(x){
  key <- x
  x1 <- fread(x)
  for(i in (nrow(x1)-1):1){
    x1[i,] <- ifelse(x1[i,]>x1[i+1,],x1[i+1,],x1[i,])
  }
  x2 <- data.table(V1=x1$V1,apply(rbind(0,as.matrix(x1[,-1])),2,diff))
  x <- cbind(melt(x1,id='V1'),melt(x2,id='V1')$value)
  colnames(x) <- c('date','state','accum','new')
  data.frame(scope=gsub('.csv','',key),fact='confirmed',x)
})
raw <- do.call(rbind,raw)
raw <- 
  merge(raw,(raw %>% group_by(scope,state,fact) %>% 
               summarise(peak=max(new))),by=c('scope','state','fact')) %>% 
  filter(new==peak) %>% group_by(scope,state,fact) %>% summarise(peak=min(date)) %>% 
  merge(raw,by=c('scope','state','fact')) %>% mutate(d=ifelse(date>peak&(scope=='china'|state=='China'),1,0)) %>%
  mutate(key=paste(scope,fact,state),i=as.numeric((as.POSIXct(date)-as.POSIXct(min(raw$date)))/3600/24)+1) %>%
  mutate(gap=1/(as.numeric(paste(as.POSIXct(peak)-as.POSIXct(min(raw$date))+as.POSIXct(peak)-as.POSIXct(date)))/3600/24+1))
raw <- raw %>% mutate(d=ifelse(gap>0&gap!=Inf,gap,1)) %>% 
  select(-peak,-gap)%>% mutate(d=1/(1/d+i-1)*i)
raw <- raw %>% mutate(d=ifelse(state%in%'China'|scope=='china',1,d))
raw <- raw %>% mutate(d=ifelse(state=='Janpan',0.528888,d))
raw <- raw %>% mutate(d=ifelse(state=='KoreaSouth',1,d))
raw <- raw %>% mutate(d=ifelse(state=='Singapore',0.509999,d))

#Module
getlog <- function(I,keyI,log,p=8){
  logi <- filter(log,key==keyI&i%in%(0:p+I)) %>% arrange(i)
  x <- logi[1:p,]
  y <- logi[-1:-p,]
  list(x=x,y=y)
}
af <- function(x){
  forecast(auto.arima(x),h=1)$mean
}

###################################
# D Model (X9 model)
###################################

log <- filter(raw,fact=='confirmed') 
mfile <- do.call(c,lapply(unique(log$key),function(K){
  lapply(1:(max(log$i)-8),function(I){getlog(I,K,log,8)})
}))
X <- (t(sapply(mfile,function(x){c(c(x$x$accum,x$y$accum)/(mean(x$x$accum)+1))})))
Y <- cbind(sapply(mfile,function(x){x$x$d[8]}))

e.input <- layer_input(shape=ncol(X))
e.layer <- layer_dense(e.input,16,activation='relu')
l.layer <- layer_dense(e.layer,4,activation='relu')
d.output <- layer_dense(units=ncol(Y),activation='sigmoid')
model <- keras_model(e.input,d.output(l.layer))
model %>% compile(loss = "mean_squared_error", optimizer = "adam",metrics = c('mae'))

D <- -Y
i <- 0
while(cor(Y,D)<=0.8|i>10){
  system.time(model %>% fit(x = X,y = Y,batch = 128,epochs = 1000,verbose = 0))
  Y <- ifelse(Y%in%c(0.528888,1,0.509999),Y,model %>% predict(X))
  model %>% fit(x = X,y = Y,batch = 128,epochs = 100,verbose = 2)
  D <- model %>% predict(X)
  i <- i+1
  print(paste(i,cor(Y,D)))
}

for(i in 1:length(D)){
  if(mfile[[i]]$x$i[1]==1){
    mfile[[i]]$x$d <- D[i]
  } else {
    mfile[[i]]$x <- NULL
  }
  mfile[[i]]$y$d <- D[i]
  mfile[[i]] <- do.call(rbind,mfile[[i]])
}
raw <- do.call(rbind,mfile)

###################################
# New Model
###################################

log <- filter(raw,fact=='confirmed') 
mfile <- do.call(c,lapply(unique(log$key),function(K){
  lapply(1:(max(log$i)-8),function(I){getlog(I,K,log,8)})
}))
X <- sapply(mfile,function(x){c(x$x$new/(mean(x$x$new+1)),mean(x$x$d))}) %>% t
Y <- cbind(sapply(mfile,function(x){c(x$y$new/(mean(x$x$new+1)))}))

e.input <- layer_input(shape=ncol(X))
e.layer <- layer_dense(e.input,16,activation='relu')
l.layer <- layer_dense(e.layer,4,activation='relu')
d.output <- layer_dense(units=ncol(Y))
model <- keras_model(e.input,d.output(l.layer))
model %>% compile(loss = "mean_squared_error", optimizer = "adam",metrics = c('mae'))

system.time(model %>% fit(x = X,y = Y,batch = 128,epochs = 1000,verbose = 0))
model %>% fit(x = X,y = Y,batch = 128,epochs = 100,verbose = 2)

###################################
# Prediction
###################################

par(mfrow=c(2,2))
test <- lapply(unique(filter(log,scope=='global')$key),function(K){
  print(K)
  temp <- filter(log,key==K)
  for(i in 1:365){
    tempi <- getlog(nrow(temp)-7,temp$key[1],temp,8)
    if(sum(tempi$x$new)==0){
      tempi.new <- 0
    } else {
      tempi.new <- model %>% 
        predict(matrix(c(tempi$x$new/mean(tempi$x$new+1),mean(tempi$x$d)),nrow=1)) * 
        mean(tempi$x$new+1)
    }
    tempi.new <- floor(ifelse(tempi.new>0,tempi.new,0))
    tempi.d <- temp$d[nrow(temp)]+ (temp$d[length(temp$d)]-temp$d[1])/nrow(temp)
    tempi.d <- (ifelse(tempi.d>1,1,tempi.d))
    temp <- rbind(temp,tempi$x[8,] %>% mutate(new=tempi.new,accum=accum+tempi.new,d=tempi.d,i=i+1,date=paste(as.POSIXct(date)+3600*24)))
  }
  plot.ts(temp$new,main=K)
  temp
})

K <- 'global confirmed US'
temp <- filter(log,key==K)
for(i in 1:365){
  tempi <- getlog(nrow(temp)-7,temp$key[1],temp,8)
  if(sum(tempi$x$new)==0){
    tempi.new <- 0
  } else {
    tempi.new <- model %>% 
      predict(matrix(c(tempi$x$new/mean(tempi$x$new+1),mean(tempi$x$d)),nrow=1)) * 
      mean(tempi$x$new+1)
  }
  tempi.new <- floor(ifelse(tempi.new>0,tempi.new,0))
  tempi.d <- temp$d[nrow(temp)]+ ((temp$d[length(temp$d)]-temp$d[1])/nrow(temp)*2)
  tempi.d <- (ifelse(tempi.d>1,1,tempi.d))
  temp <- rbind(temp,tempi$x[8,] %>% mutate(new=tempi.new,accum=accum+tempi.new,d=tempi.d,i=i+1,date=paste(as.POSIXct(date)+3600*24)))
}
# plot.ts(temp$new,main=K)
lines(temp$new,col=4)
temp
