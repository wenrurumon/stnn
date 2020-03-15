
rm(list=ls())
library(data.table)
library(dplyr)
library(keras)

############################
#Load data
############################

load('/Users/wenrurumon/Documents/posdoc/wuhan/data/mfile.rda')

############################
#Module
############################

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
getx <- function(temp,keyi){
  x <- filter(temp,date>max(temp$date)-8*3600*24&key==keyi)
  d <- mean(x$dum)
  f <- mean(x$case+1)
  list(log=x[nrow(x),],x=c(x=log(x$case/f+1),d=d,f=f,w=x$w[nrow(x)]))
}
refreshi <- function(temp,d=0){
  do.call(rbind,lapply(unique(temp$key),function(keyi){
    x <- getx(temp,keyi)
    y <- sapply(model.test,function(m){m$model %>% predict(matrix(x$x,nrow=1))})
    y <- list(y=round(mean((exp(y)-1)*x$x[10])))
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

#Model Data
mfile.w <- ceiling((sapply(mfile,function(x){x$w})+1)/14)
mfile <- rep(mfile,mfile.w)
mfile.x <- sapply(mfile,function(x){c(x$x,f=x$f,w=x$w)}) %>% t
mfile.y <- cbind(y=sapply(mfile,function(x){c(x$y)}))

#Modeling

model.test<- lapply(
  paste0('/Users/wenrurumon/Documents/posdoc/wuhan/model/test_',1:5,'.model'),
  function(x){list(model=keras::load_model_hdf5(x))})
# model.test <- lapply(1:5,function(i){
#   print(paste(i,Sys.time()))
#   MSAE(
#     X=mfile.x,Y=mfile.y,
#     dims=c(16,4),activations=c('relu','relu'),
#     batch=128,epochs=1000,verbose=0)
# })
# for(i in 1:5){keras::save_model_hdf5(model.test[[i]]$model,paste0('/Users/wenrurumon/Documents/posdoc/wuhan/model/test_',i,'.model'),overwrite = TRUE,include_optimizer = TRUE)}

#Validation Data

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

#Prediction

temp <- filter(raw,scope=='USA')
temp <- filter(temp,date>max(temp$date)-8*3600*24)
for(i in 1:200){
  # print(i)
  tempi <- refreshi(temp,d=0)
  temp <- rbind(temp,tempi)
}
temp1 <- temp
x <- temp %>% group_by(date) %>% summarise(x=sum(acase))
plot.ts(x$x)

temp <- filter(raw,scope=='USA')
temp <- filter(temp,date>max(temp$date)-8*3600*24)
for(i in 1:200){
  # print(i)
  tempi <- refreshi(temp,d=ifelse(i>7,1,0))
  temp <- rbind(temp,tempi)
}
temp2 <- temp
x <- temp %>% group_by(date) %>% summarise(x=sum(acase))
plot.ts(x$x)

temp <- filter(raw,scope=='USA')
temp <- filter(temp,date>max(temp$date)-8*3600*24)
for(i in 1:200){
  # print(i)
  tempi <- refreshi(temp,d=ifelse(i>28,1,0))
  temp <- rbind(temp,tempi)
}
temp3 <- temp
x <- temp %>% group_by(date) %>% summarise(x=sum(acase))
plot.ts(x$x)
