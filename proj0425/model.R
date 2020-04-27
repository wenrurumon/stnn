
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

#Module
getlog <- function(I,keyI,log,p=8){
  logi <- filter(log,key==keyI&i%in%(0:p+I)) %>% arrange(i)
  x <- logi[1:p,]
  y <- logi[-1:-p,]
  list(x=x,y=y)
}
max2 <- function(x){
  x1 <- which(x>0)
  if(length(x1)==0){
    0
  } else{
    min(length(x),max(x1))
  }
}
getdate <- function(x){
  paste(as.POSIXct('2020-01-22')+3600*(x-1)*24)
}

###################################
# D Model (X9 model)
###################################

#D with accumulated case

log <- filter(raw,fact=='confirmed') 

mfile <- do.call(c,lapply(unique(log$key),function(K){
  lapply(1:(max(log$i)-8),function(I){getlog(I,K,log,8)})
}))
X <- (t(sapply(mfile,function(x){c(c(x$x$accum,x$y$accum)/(mean(x$x$accum)+1))})))
w <- (cbind(sapply(mfile,function(x){mean(x$x$accum)+1})))
Y <- cbind(sapply(mfile,function(x){x$x$d[8]}))

model1 <- function(i){
  print(paste(i,Sys.time()))
  e.input <- layer_input(shape=ncol(X))
  e.layer <- layer_dense(e.input,16,activation='relu')
  l.layer <- layer_dense(e.layer,4,activation='relu')
  d.output <- layer_dense(units=ncol(Y),activation='sigmoid')
  model <- keras_model(e.input,d.output(l.layer))
  model %>% compile(loss = "mean_squared_error", optimizer = "adam",metrics = c('mae'))
  system.time(model %>% fit(x = X,y = Y,batch = 128,epochs = 2000,verbose = 0))
  model %>% fit(x = X,y = Y,batch = 128,epochs = 100,verbose = 0)
  D <- model %>% predict(X)
  D
}
rlt.model1 <- lapply(1:5,model1)
D <- sapply(rlt.model1,function(x){x}) %>% rowMeans

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

raw %>% filter(scope=='global') %>% group_by(state) %>% summarise(d=mean(d)) %>% arrange(desc(d))
raw %>% filter(scope=='global') %>% group_by(state) %>% summarise(d=mean(d)) %>% filter(
  state%in%c('China','UnitedKingdom','US')
)

#D Model

log <- filter(raw,fact=='confirmed') 
mfile <- do.call(c,lapply(unique(log$key),function(K){
  lapply(1:(max(log$i)-8),function(I){getlog(I,K,log,8)})
}))
X <- (t(sapply(mfile,function(x){c(c(x$x$accum)/(mean(x$x$accum)+1),mean(x$x$d))})))
w <- (cbind(sapply(mfile,function(x){mean(x$x$accum)+1})))
Y <- cbind(sapply(mfile,function(x){x$x$d[8]}))

model2 <- function(i){
  print(paste(i,Sys.time()))
  e.input <- layer_input(shape=ncol(X))
  e.layer <- layer_dense(e.input,16,activation='relu')
  l.layer <- layer_dense(e.layer,4,activation='relu')
  d.output <- layer_dense(units=ncol(Y),activation='sigmoid')
  model <- keras_model(e.input,d.output(l.layer))
  model %>% compile(loss = "mean_squared_error", optimizer = "adam",metrics = c('mae'))
  system.time(model %>% fit(x = X,y = Y,batch = 128,epochs = 1000,verbose = 0))
  model %>% fit(x = X,y = Y,batch = 128,epochs = 100,verbose = 0)
  D <- model %>% predict(X)
  list(model=model,D=D)
}
rlt.model2 <- lapply(1:5,model2)
D <- sapply(rlt.model2,function(x){x$D}) %>% rowMeans

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

raw %>% filter(scope=='global') %>% group_by(state) %>% summarise(d=mean(d)) %>% arrange(desc(d))
raw %>% filter(scope=='global') %>% group_by(state) %>% summarise(d=mean(d)) %>% filter(
  state%in%c('China','UnitedKingdom','US')
)

plot.ts(filter(raw,state=='US')$d)
plot.ts(filter(raw,state=='China')$d)
plot.ts(filter(raw,state=='KoreaSouth')$d)
plot.ts(filter(raw,state=='Singapore')$d)

for(i in 1:5){
  keras::save_model_hdf5(rlt.model2[[i]]$model,paste0('/Users/wenrurumon/Documents/posdoc/wuhan/model/model0425_d_',i,'.model'),overwrite = TRUE,include_optimizer = TRUE)
}

###################################
# New Prediction Model
###################################

log <- filter(raw,fact=='confirmed') 
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
  model %>% fit(x = X,y = Y,batch = 128,epochs = 100,verbose = 2)
  return(model)
}
rlt.model3 <- lapply(1:5,model3)
for(i in 1:5){
  keras::save_model_hdf5(rlt.model3[[i]],paste0('/Users/wenrurumon/Documents/posdoc/wuhan/model/model0425_m_',i,'.model'),overwrite = TRUE,include_optimizer = TRUE)
}
save(raw,file='raw0425.rda')

###################################
# Prediction
###################################

model.d <- lapply(1:5,function(i){
  keras::load_model_hdf5(paste0('/Users/wenrurumon/Documents/posdoc/wuhan/model/model0425_d_',
                                i,'.model'))
})
model.m <- lapply(1:5,function(i){
  keras::load_model_hdf5(paste0('/Users/wenrurumon/Documents/posdoc/wuhan/model/model0425_m_',
                                i,'.model'))
})

# K <- 'global confirmed US'
rlt <- lapply(unique(filter(raw,state=='US')$key),function(K){
  rlt <- list()
  print(K)
  for(j in 1:3){
    temp <- filter(log,key==K)
    temp$d <- temp$d *(1-0.2*(j-1))
    for(i in 1:100){
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
                             date=paste(as.POSIXct(date)+3600*24)))
    }
    rlt[[j]] <- temp
  }
  plot.ts(rlt[[3]]$new,main=K)
  lines(rlt[[2]]$new,col=2)
  lines(rlt[[1]]$new,col=3)
  rlt
})

sapply(rlt[[1]],function(x){x$new}) %>% plot.ts


