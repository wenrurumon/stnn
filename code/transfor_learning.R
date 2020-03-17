
rm(list=ls())
library(dplyr)
library(data.table)
library(keras)

MSAE <- function(X,Y,dims,activations,batch,epochs,verbose){
  e.input <- layer_input(shape=ncol(X))
  e.layer <- layer_dense(e.input,dims[1],activation=activations[1])
  l.layer <- layer_dense(e.layer,dims[2],activation=activations[2])
  d.output <- layer_dense(units=ncol(Y),activation=NULL)
  model <- keras_model(e.input,d.output(l.layer))
  encoder <- keras_model(e.input,l.layer)
  d.input <- layer_input(shape=dims[2])
  decoder <- keras_model(d.input,d.output(d.input))
  model %>% compile(
    loss = "mean_squared_error", 
    optimizer = "adam",
    metrics = c('mae')
  )
  system.time(history <- model %>% fit(
    x = X, 
    y = Y,
    batch = batch,
    epochs = epochs,
    verbose = verbose
  ))
  list(model=model,encoder=encoder,decoder=decoder,history=history)
}
MSAE2 <- function(X,Y,model,batch,epochs,verbose){
  model %>% compile(
    loss = "mean_squared_error", 
    optimizer = "adam",
    metrics = c('mae')
  )
  system.time(history <- model %>% fit(
    x = X, 
    y = Y,
    batch = batch,
    epochs = epochs,
    verbose = verbose
  ))
  list(model=model)
}
getx <- function(temp,keyi){
  x <- filter(temp,date>max(temp$date)-8*3600*24&key==keyi)
  d <- mean(x$dum)
  f <- mean(x$case+1)
  list(log=x[nrow(x),],x=c(x=log(x$case/f+1),d=d,f=f,w=x$w[nrow(x)]))
}
refreshi <- function(temp,model,d=0){
  do.call(rbind,lapply(unique(temp$key),function(keyi){
    x <- getx(temp,keyi)
    y <- sapply(model,function(m){m$model %>% predict(matrix(x$x,nrow=1))})
    y <- ifelse(mean(y)<0,0,mean(y))
    # y <- list(y=floor(((exp(mean(y))-1)*x$x[10])))
    y <- list(y=floor(y)*x$x[10])
    y$log <- x$log
    y$log$date <- y$log$date+3600*24
    y$log$case <- y$y
    y$log$acase <- y$log$acase+y$log$case
    y$log$dum <- d
    y$log$w <- y$log$w+1
    y$log
  }))
}

############################
#Modeling
############################

##Modeling - China Model

# model.china<- lapply(
# paste0('/Users/wenrurumon/Documents/posdoc/wuhan/model/china_',1:5,'.model'),
# function(x){list(model=keras::load_model_hdf5(x))})
load('/Users/wenrurumon/Documents/posdoc/wuhan/data/mfile.rda')
mfile.w <- sapply(mfile,function(x){
  w <- ceiling((x$w+1)/14)
  w <- ifelse(x$log$y$scope=='CHINA',w,0)
  w
})
mfile <- rep(mfile,mfile.w)
mfile.x <- sapply(mfile,function(x){c(x$x,f=x$f,w=x$w)}) %>% t
mfile.y <- cbind(y=sapply(mfile,function(x){c(x$y)}))
model.china <- lapply(1:5,function(i){
  print(paste(i,Sys.time()))
  MSAE(
    X=mfile.x,Y=mfile.y,
    dims=c(16,4),activations=c('relu','relu'),
    batch=128,epochs=1000,verbose=0)
})
for(i in 1:5){keras::save_model_hdf5(model.china[[i]]$model,paste0('/Users/wenrurumon/Documents/posdoc/wuhan/model/china_',i,'.model'),overwrite = TRUE,include_optimizer = TRUE)}

##Modeling - USA Model

# model.china<- lapply(
# paste0('/Users/wenrurumon/Documents/posdoc/wuhan/model/china_',1:5,'.model'),
# function(x){list(model=keras::load_model_hdf5(x))})
load('/Users/wenrurumon/Documents/posdoc/wuhan/data/mfile.rda')
mfile.w <- sapply(mfile,function(x){
  w <- ceiling((x$w+1)/14)
  w <- ifelse(x$log$y$scope=='USA',w,0)
  w
})
mfile <- rep(mfile,mfile.w)
mfile.x <- sapply(mfile,function(x){c(x$x,f=x$f,w=x$w)}) %>% t
mfile.y <- cbind(y=sapply(mfile,function(x){c(x$y)}))
model.usa <- lapply(1:5,function(i){
  print(paste(i,Sys.time()))
  MSAE(
    X=mfile.x,Y=mfile.y,
    dims=c(16,4),activations=c('relu','relu'),
    batch=128,epochs=1000,verbose=0)
})
for(i in 1:5){keras::save_model_hdf5(model.usa[[i]]$model,paste0('/Users/wenrurumon/Documents/posdoc/wuhan/model/usa_',i,'.model'),overwrite = TRUE,include_optimizer = TRUE)}

##Modeling - GLOBAL Model

# model.china<- lapply(
# paste0('/Users/wenrurumon/Documents/posdoc/wuhan/model/china_',1:5,'.model'),
# function(x){list(model=keras::load_model_hdf5(x))})
load('/Users/wenrurumon/Documents/posdoc/wuhan/data/mfile.rda')
mfile.w <- sapply(mfile,function(x){
  w <- ceiling((x$w+1)/14)
  w <- ifelse(x$log$y$scope=='GLOBAL',w,0)
  w
})
mfile <- rep(mfile,mfile.w)
mfile.x <- sapply(mfile,function(x){c(x$x,f=x$f,w=x$w)}) %>% t
mfile.y <- cbind(y=sapply(mfile,function(x){c(x$y)}))
model.global <- lapply(1:5,function(i){
  print(paste(i,Sys.time()))
  MSAE(
    X=mfile.x,Y=mfile.y,
    dims=c(16,4),activations=c('relu','relu'),
    batch=128,epochs=1000,verbose=0)
})
for(i in 1:5){keras::save_model_hdf5(model.global[[i]]$model,paste0('/Users/wenrurumon/Documents/posdoc/wuhan/model/global_',i,'.model'),overwrite = TRUE,include_optimizer = TRUE)}

############################

############################
#Validation
############################

# rm(mfile)
# load('/Users/wenrurumon/Documents/posdoc/wuhan/data/mfile.rda')
# mfile.x <- sapply(mfile,function(x){c(x$x,f=x$f,w=x$w)}) %>% t
# mfile.y <- cbind(y=sapply(mfile,function(x){c(x$y)}))
# mfile.key <- do.call(rbind,lapply(mfile,function(x){
#   select(x$log$y,date,scope,state,acase)
# }))
# rlt.test <- lapply(model.test,function(m){
#   x <- data.frame(mfile.key,f=mfile.x[,10],y=mfile.y,
#                   pred=m$model %>% predict(mfile.x)) %>% mutate(
#                     y = (exp(y)-1)*f, pred = round((exp(pred)-1)*f)
#                   ) %>% 
#     group_by(date,scope) %>% 
#     summarise(acase=sum(acase),y=sum(y),pred=sum(pred)) %>% 
#     mutate(apred=acase-y+pred,error=(apred-acase)^2)
#   x <- merge(x,x %>% group_by(scope) %>% summarise(my=mean(acase)),by='scope') %>% 
#     mutate(bench=(my-acase)^2)
#   x %>% group_by(scope) %>% summarise(error=mean(error)/mean(bench))
# })

############################
#Preidction
############################

model <- c(model.china,model.usa,model.global)

temp <- filter(raw,scope=='USA')
Sys.time()
for(i in 1:100){
      # print(i)
  tempi <- refreshi(temp,model,d=0)
  temp <- rbind(temp,tempi)
  tail(temp)
  x <- temp %>% group_by(date) %>% summarise(x=sum(acase))
  plot.ts((x$x)%>%diff,col=2);
  lines(((temp %>% group_by(date) %>% summarise(x=sum(acase)))$x)%>%diff,col=4)
}
Sys.time()
temp1 <- temp
(temp %>% group_by(date) %>% summarise(x=sum(case))) %>% as.matrix

temp <- filter(raw,scope=='USA')
Sys.time()
for(i in 1:100){
  # print(i)
  if(i < 7){
    tempi <- refreshi(temp,d=0)
  } else {
    tempi <- refreshi(temp,d=1)
  }
  temp <- rbind(temp,tempi)
  tail(temp)
  x <- temp %>% group_by(date) %>% summarise(x=sum(acase))
  plot.ts(x$x,col=2);
  lines((temp1 %>% group_by(date) %>% summarise(x=sum(acase)))$x,col=4)
}
Sys.time()
temp2 <- temp
(temp %>% group_by(date) %>% summarise(x=sum(case))) %>% as.matrix

temp <- filter(raw,scope=='USA')
Sys.time()
for(i in 1:300){
  # print(i)
  if(i < 14){
    tempi <- refreshi(temp,d=0)
  } else {
    tempi <- refreshi(temp,d=1)
  }
  temp <- rbind(temp,tempi)
  tail(temp)
  x <- temp %>% group_by(date) %>% summarise(x=sum(acase))
  plot.ts(x$x,col=2);
  lines((temp1 %>% group_by(date) %>% summarise(x=sum(acase)))$x,col=4)
  lines((temp2 %>% group_by(date) %>% summarise(x=sum(acase)))$x,col=5)
}
Sys.time()
temp3 <- temp

temp <- filter(raw,scope=='USA')
Sys.time()
for(i in 1:300){
  # print(i)
  if(i < 28){
    tempi <- refreshi(temp,d=0)
  } else {
    tempi <- refreshi(temp,d=1)
  }
  temp <- rbind(temp,tempi)
  tail(temp)
  x <- temp %>% group_by(date) %>% summarise(x=sum(acase))
  plot.ts(x$x,col=2);
  lines((temp1 %>% group_by(date) %>% summarise(x=sum(acase)))$x,col=4)
  lines((temp2 %>% group_by(date) %>% summarise(x=sum(acase)))$x,col=5)
}
Sys.time()
temp4 <- temp

plot.ts((temp1 %>% group_by(date) %>% summarise(case=sum(acase)))$case[1:150])
plot.ts((temp2 %>% group_by(date) %>% summarise(case=sum(acase)))$case)
