
rm(list=ls())
library(data.table)
library(dplyr)
setwd('/Users/wenrurumon/Documents/posdoc/wuhan/data')
raw <- lapply(dir(patter='model'),function(x){data.table::fread(x)[,-1:-2] %>% as.matrix})
names(raw) <- dir(patter='model')

#Process
raw.date <- as.POSIXct( "2020-01-10") + 3600*24*1:length(1:nrow(raw[[1]]))
raw[[2]] <- rbind(do.call(rbind,rep(list(rep(0,ncol(raw[[2]]))),10)),raw[[2]])
raw[[3]] <- rbind(do.call(rbind,rep(list(rep(0,ncol(raw[[3]]))),10)),raw[[3]])
raw[[3]] <- as.matrix(as.data.table(raw[[3]]) %>% select(-Grand,-Diamond,-Wuhan))
raw[[2]][,1] <- rowSums(raw[[1]])
i <- 0
raw <- lapply(raw,function(x){
  i <<- i+1
  rlt1 <- melt(data.frame(date=raw.date,scope=c('china','global','usa')[i],x),id=c('date','scope'))
  x <- as.data.table(rbind(0,x))
  x <- apply(x,2,diff)
  rlt2 <- melt(data.frame(date=raw.date,scope=c('china','global','usa')[i],x),id=c('date','scope'))
  list(rlt1,rlt2)
})
raw1 <- do.call(rbind,lapply(raw,function(x){x[[1]]})) %>% as.data.table
raw2 <- do.call(rbind,lapply(raw,function(x){x[[2]]})) %>% as.data.table
raw <- data.table(raw2,acase=raw1$value)
colnames(raw) <- c('date','scope','state','case','acase')
raw <- raw %>% mutate(
  dum = ifelse((state=='china'|scope=='china')&date>='2020-01-23',1,0),
  scope = toupper(scope), state = toupper(state),
  key = paste(scope,state,sep="::")
)
raw <- merge(raw,raw %>% filter(case>0) %>% group_by(key) %>% summarise(w=min(date)),by='key') %>%
  mutate(w=as.numeric((date-w)/3600/24)+1,w=ifelse(w<0,0,w))

########################
#Model Filing
########################

#Module
getx <- function(keyi,i,p){
  x <- filter(raw,date%in%(as.POSIXct("2020-01-10")+(i:(p+i-1))*3600*24)&key==keyi)
  y <- filter(raw,date%in%(as.POSIXct("2020-01-10")+(p+i)*3600*24)&key==keyi)
  list(x=x,y=y)
}
# procx <- function(x){
#   raw <- x
#   y <- x$y %>% mutate(case=ifelse(case<0,0,case)) 
#   x <- x$x %>% mutate(case=ifelse(case<0,0,case))
#   x.dum <- mean(x$dum)
#   f <- mean(x$case+1)
#   w <- y$w
#   xi <- c(x=log(x$case/f+1),d=x.dum)
#   yi <- log(y$case/f+1)
#   list(x=xi,y=yi,f=f,w=w,log=raw)
# }
procx <- function(x){
  raw <- x
  y <- x$y %>% mutate(case=ifelse(case<0,0,case)) 
  x <- x$x %>% mutate(case=ifelse(case<0,0,case))
  x.dum <- mean(x$dum)
  f <- mean(x$case+1)
  w <- y$w
  xi <- c(x=x$case/f,d=x.dum)
  yi <- y$case/f
  list(x=xi,y=yi,f=f,w=w,log=raw)
}
proci <- function(keyi,i,p,y=TRUE){
  x <- getx(keyi,i,p)
  return(procx(x))
}

#Filing
mfile <- do.call(c,lapply(unique(raw$key),function(keyi){
  lapply(1:(length(unique(raw$date))-8),function(i){
    proci(keyi,i,8)
  })
}))
save(raw,mfile,file='mfile.rda')

############################
#Load data
############################

rm(list=ls())
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
    # y <- list(y=floor(((exp(mean(y))-1)*x$x[10])))
    y <- list(y=floor(mean(y)*x$x[10]))
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

# model.test<- lapply(
# paste0('/Users/wenrurumon/Documents/posdoc/wuhan/model/test_',1:5,'.model'),
# function(x){list(model=keras::load_model_hdf5(x))})

model.test <- lapply(1:5,function(i){
  print(paste(i,Sys.time()))
  MSAE(
    X=mfile.x,Y=mfile.y,
    dims=c(16,4),activations=c('relu','relu'),
    batch=128,epochs=1000,verbose=0)
})
for(i in 1:5){keras::save_model_hdf5(model.test[[i]]$model,paste0('/Users/wenrurumon/Documents/posdoc/wuhan/model/test2_',i,'.model'),overwrite = TRUE,include_optimizer = TRUE)}

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
for(i in 1:100){
  print(i)
  tempi <- refreshi(temp,d=1)
  temp <- rbind(temp,tempi)
  tail(temp)
  x <- temp %>% group_by(date) %>% summarise(x=sum(acase))
  plot.ts(x$x,col=2); lines((filter(raw,scope=='USA')%>%group_by(date)%>%summarise(x=sum(acase)))$x)
}
temp1 <- temp

temp <- filter(raw,scope=='USA')
for(i in 1:100){
  print(i)
  for(i < 7){
    tempi <- refreshi(temp,d=0)
  } else {
    tempi <- refresh(temp,d=1)
  }
  temp <- rbind(temp,tempi)
  tail(temp)
  x <- temp %>% group_by(date) %>% summarise(x=sum(acase))
  plot.ts(x$x,col=2); lines((filter(raw,scope=='USA')%>%group_by(date)%>%summarise(x=sum(acase)))$x)
}
temp2 <- temp
