
rm(list=ls())
setwd('/Users/wenrurumon/Documents/posdoc/wuhan/data/github')
library(data.table)
library(dplyr)
library(keras)
load("raw0424.rda")
model.d <- lapply(1:5,function(i){keras::load_model_hdf5(paste0('/Users/wenrurumon/Documents/posdoc/wuhan/model/model0424_d_',i,'.model'))})
model.m <- lapply(1:5,function(i){keras::load_model_hdf5(paste0('/Users/wenrurumon/Documents/posdoc/wuhan/model/model0424_m_',i,'.model'))})

setwd('/Users/wenrurumon/Documents/posdoc/wuhan/summary')
raw <- data.table(raw,status='historical')
getlog <- function(I,keyI,log,p=8){
  logi <- filter(log,key==keyI&i%in%(0:p+I)) %>% arrange(i)
  x <- logi[1:p,]
  y <- logi[-1:-p,]
  list(x=x,y=y)
}
getdate <- function(x){
  paste(as.POSIXct('2020-01-22')+3600*(x-1)*24)
}

rlt <- lapply(1:10,function(h){
  print(h)
  log <- filter(raw,state=='US'&i<=max(raw$i)-h)
  mfile <- do.call(c,lapply(unique(log$key),function(K){
    lapply(1:(max(log$i)-8),function(I){getlog(I,K,log,8)})
  }))
  X <- sapply(mfile,function(x){c(x$x$new/(mean(x$x$new+1)),mean(x$x$d))}) %>% t
  Y <- cbind(sapply(mfile,function(x){c(x$y$new/(mean(x$x$new+1)))}))
  model3 <- function(i){
    print(paste(i,Sys.time()))
    e.input <- layer_input(shape=ncol(X))
    e.layer <- layer_dense(e.input,16,activation='relu')
    l.layer <- layer_dense(e.layer,4,activation='relu')
    d.output <- layer_dense(units=ncol(Y))
    model <- keras_model(e.input,d.output(l.layer))
    model %>% compile(loss = "mean_squared_error", optimizer = "adam",metrics = c('mae'))
    system.time(model %>% fit(x = X,y = Y,batch = 128,epochs = 3000,verbose = 0))
    model %>% fit(x = X,y = Y,batch = 128,epochs = 100,verbose = 0)
    return(model)
  }
  rlt.model3 <- lapply(1:5,model3)
  model.m <- rlt.model3
  
  temp <- filter(raw,state=='US'&i<=max(raw$i)-h)
  for(j in 1:h){
    tempi <- getlog(nrow(temp)-7,temp$key[1],temp,8)
    if(sum(tempi$x$new)==0){
      tempi.new <- 0
    } else {
      tempi.new <- matrix(c(tempi$x$new/mean(tempi$x$new+1),mean(tempi$x$d)),nrow=1)
      tempi.new <- mean(sapply(model.m,function(m){m %>% predict(tempi.new)}) * mean(tempi$x$new+1))
    }
    tempi.new <- floor(ifelse(tempi.new>0,tempi.new,0))
    if((mean(tempi$x$new)==tempi.new)&tempi.new>0){
      tempi.new <- tempi.new - 1
    }
    tempi.d <- filter(raw,state=='US'&i==max(raw$i)-h+j)$d
    temp <- rbind(temp,tempi$x[8,] %>% 
                    mutate(new=tempi.new,accum=accum+tempi.new,d=tempi.d,i=i+1,
                           date=paste(as.POSIXct(date)+3600*24),status='prediction'))
  }
  temp
})
test <- apply(cbind(filter(raw,state=='US')$new,sapply(rlt,function(x){x$new})),2,cumsum)
tail(1-test/test[,1],10)
test <- t(apply(tail(test[,-1]/test[,1]-1,10),2,function(x){x <- x[x!=0]; c(x,rep(NA,10-length(x)))}))
colnames(test) <- paste(1:ncol(test),'step error')
test <- (rbind(test,average=colMeans(abs(test),na.rm=T)))
write.csv(test,'rlt_US_0402_10steperror_predict.csv')

################################

rlt <- lapply(1:10,function(h){
  print(h)
  temp <- filter(raw,state=='US'&i<=max(raw$i)-h)
  for(j in 1:h){
    tempi <- getlog(nrow(temp)-7,temp$key[1],temp,8)
    if(sum(tempi$x$new)==0){
      tempi.new <- 0
    } else {
      tempi.new <- matrix(c(tempi$x$new/mean(tempi$x$new+1),mean(tempi$x$d)),nrow=1)
      tempi.new <- mean(sapply(model.m,function(m){m %>% predict(tempi.new)}) * mean(tempi$x$new+1))
    }
    tempi.new <- floor(ifelse(tempi.new>0,tempi.new,0))
    if((mean(tempi$x$new)==tempi.new)&tempi.new>0){
      tempi.new <- tempi.new - 1
    }
    tempi.d <- (matrix(c(tempi$x$accum/mean(tempi$x$accum+1),mean(tempi$x$d)),nrow=1))
    tempi.d <- mean(sapply(model.d,function(m){m %>% predict(tempi.d)}))
    tempi.d <- max(tempi.d,(tempi$x$d[8]))
    tempi.d <- (ifelse(tempi.d>1,1,tempi.d))
    tempi.d <- (ifelse(tempi.d<0,0,tempi.d))
    temp <- rbind(temp,tempi$x[8,] %>% 
                    mutate(new=tempi.new,accum=accum+tempi.new,d=tempi.d,i=i+1,
                           date=paste(as.POSIXct(date)+3600*24),status='prediction'))
  }
  temp
})
test <- apply(cbind(filter(raw,state=='US')$new,sapply(rlt,function(x){x$new})),2,cumsum)
tail(1-test/test[,1],10)
test <- t(apply(tail(test[,-1]/test[,1]-1,10),2,function(x){x <- x[x!=0]; c(x,rep(NA,10-length(x)))}))
colnames(test) <- paste(1:ncol(test),'step error')
test <- (rbind(test,average=colMeans(abs(test),na.rm=T)))
write.csv(test,'rlt_US_0424_10steperror_fit.csv')

