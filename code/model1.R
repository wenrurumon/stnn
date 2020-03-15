
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

############################
#Modeling
############################

#Model Data
mfile.w <- ceiling((sapply(mfile,function(x){x$w})+1)/14)
mfile <- rep(mfile,mfile.w)
mfile.x <- sapply(mfile,function(x){c(x$x,f=x$f,w=x$w)}) %>% t
mfile.y <- cbind(y=sapply(mfile,function(x){c(x$y)}))

#Modeling
model.test <- lapply(1:5,function(i){
  print(paste(i,Sys.time()))
  MSAE(
    X=mfile.x,Y=mfile.y,
    dims=c(16,4),activations=c('relu','relu'),
    batch=128,epochs=1000,verbose=0)
})

#Validation Data

rm(mfile)
load('/Users/wenrurumon/Documents/posdoc/wuhan/data/mfile.rda')
mfile.x <- sapply(mfile,function(x){c(x$x,f=x$f,w=x$w)}) %>% t
mfile.y <- cbind(y=sapply(mfile,function(x){c(x$y)}))
mfile.key <- do.call(rbind,lapply(mfile,function(x){
  select(x$log$y,date,scope,state,acase)
}))
rlt.test <- lapply(model.test,function(m){
  x <- data.frame(mfile.key,f=mfile.x[,10],y=mfile.y,
             pred=m$model %>% predict(mfile.x)) %>% mutate(
               y = (exp(y)-1)*f, pred = round((exp(pred)-1)*f)
             ) %>% 
    group_by(date,scope) %>% 
    summarise(acase=sum(acase),y=sum(y),pred=sum(pred)) %>% 
    mutate(apred=acase-y+pred,error=(apred-acase)^2)
  x <- merge(x,x %>% group_by(scope) %>% summarise(my=mean(acase)),by='scope') %>% 
    mutate(bench=(my-acase)^2)
  x %>% group_by(scope) %>% summarise(error=mean(error)/mean(bench))
})
