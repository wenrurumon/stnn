
rm(list=ls())
setwd('/Users/wenrurumon/Documents/posdoc/wuhan/software')
raw <- read.csv('data.csv')
Y.actual <- raw[,-1]
Y.model <- apply(Y.actual,2,diff)

############################
# Data Setup
############################

get_model_file <- function(x,i,p,h){
  y <- t(x[1:h-1+p+i,,drop=F])
  x <- t(x[(1:p)-1+i,,drop=F])
  f <- rowMeans(x)
  y <- y/f; y[is.na(y)] <- 0; y[y==Inf] <- 0
  x <- x/f; x[is.na(x)] <- 0
  list(x=x,y=y,f=f)
}
process_model_file <- function(x){
  X <- do.call(rbind,lapply(x,function(x){x$x}))
  Y <- do.call(rbind,lapply(x,function(x){x$y}))
  list(X=X,Y=Y)
}
get_x <- function(x,i,p){
  x <- t(x[(1:p)-1+i,,drop=F])
  f <- rowMeans(x)
  x <- x/f
  x[is.na(x)] <- 0
  list(x=x,f=f)
}
as.n <- function(x){
  as.numeric(paste(x))
}
pred <- function(x,model){
  model %>% predict(x)
}
mean2 <- function(x){
  x <- sort(x[!is.na(x)])
  x <- x[-c(1,length(x))]
  mean(x)
}
process_data <- function(sel,p=8,h=1,w=NULL,x=Y.model,wl=NULL){
  mfile <- lapply(1:(nrow(x)-p-h+1),get_model_file,h=h,p=p,x=x)
  if(is.null(wl)){
    w <- 1
  } else {
    w <- rep(1:ceiling((length(mfile)-sel)/wl),each=wl)
    w <- w[1:((length(mfile)-sel))]
  }
  X <- do.call(rbind,
               lapply(rep(mfile[1:(length(mfile)-sel)],w),
                      function(x){x$x}
               )
  )
  Y <- do.call(rbind,
               lapply(rep(mfile[1:(length(mfile)-sel)],w),
                      function(x){x$y}
               )
  )
  list(X=X,Y=Y)
}

############################
# Auto Forecasting
############################

MSAE <- 
  function(X,Y,p=8,h=1,dims=c(32,4),activations=c('relu','relu'),drops=rep(0,2),
           batch=128,epochs=100,verbose=1){
    e.input <- layer_input(shape=ncol(X))
    e.layer <- layer_dense(e.input,dims[1],activation=activations[1]) %>% 
      layer_dropout(rate=drops[1])
    l.layer <- layer_dense(e.layer,dims[2],activation=activations[2]) %>% 
      layer_dropout(rate=drops[2])
    d.output <- layer_dense(units=ncol(Y),activation=NULL)
    d.input <- layer_input(shape=dims[2])
    model <- keras_model(e.input,d.output(l.layer))
    encoder <- keras_model(e.input,l.layer)
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
    list(model=model,encoder=encoder,decoder=decoder)
  }
predict.MSAE <- function(MSAE,x,f){
  (MSAE$model %>% predict(x))*f
}

############################
# Demo
############################

#Generate Modeling dataset
pdata <- process_data(sel=1,p=8,h=1,x=Y.model,wl=8)
#Run MSAE
pdata.MSAE <- MSAE(pdata$X,pdata$Y,
                   p=8,h=1,dims=c(32,4),activations=c('relu','relu'),drops=rep(0,2),
                   batch=128,epochs=200,verbose=2)
#Data for Validation
vdata <- get_x(Y.model,i=nrow(Y.model)-1-8,p=8)
#
rbind(
  actual = sum(Y.model[nrow(Y.model),]) + sum(Y.model[nrow(Y.model)-1,]),
  predict = sum(predict.MSAE(pdata.MSAE,vdata$x,vdata$f)) + sum(Y.model[nrow(Y.model)-1,])
)
