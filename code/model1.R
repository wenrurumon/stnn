
rm(list=ls())
library(data.table)
library(dplyr)
library(keras)

############################
#Load data
############################

load('/Users/wenrurumon/Documents/posdoc/wuhan/data/mfile.rda')
mfile.w <- ceiling((sapply(mfile,function(x){x$w})+1)/14)
mfile <- rep(mfile,mfile.w)
mfile.x <- sapply(mfile,function(x){c(x$x,f=x$f,w=x$w)}) %>% t
mfile.y <- sapply(mfile,function(x){c(x$y)})
mfile.key <- do.call(rbind,lapply(mfile,function(x){select(x$log$y,date,scope,state)}))
mfile <- data.table(mfile.key,mfile.x,y=mfile.y)
head(mfile)

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

X <- mfile %>% select(x1,x2,x3,x4,x5,x6,x7,x8,d,f,w) %>% as.matrix
Y <- mfile %>% select(y) %>% as.matrix
system.time(model.test <- MSAE(
  X=X,Y=Y,
  dims=c(8,4),activations=c('relu','relu'),
  batch=128,epochs=1000,verbose=1))

mfile2 <- data.frame(mfile,pred=model.test$model %>% predict(X)) %>% mutate(
  y = (exp(y)-1)*f, pred = (exp(pred)-1)*f
)
mfile2 <- mfile2 %>% group_by(date,scope,state) %>% summarise(y=sum(y),pred=sum(pred))


