
############################
# Module
############################

rm(list=ls())
library(MASS)
library(plyr)
library(openxlsx)
library(data.table)
library(dplyr)
library(keras)
library(ggplot2)

#Get Model File
rmna <- function(x){ifelse(is.na(x),0,x)}
get_model_file <- function(x,i,p,gety=TRUE){
  if(gety){y <- t(x[p+i,,drop=F])}
  x <- t(x[1:p+i-1,,drop=F])
  if(gety){y[y<0] <- 0}
  x[x<0] <- 0
  f <- rowMeans(x)
  if(gety){y <- y/f; y[is.na(y)] <- 0; y[y==Inf] <- 0}
  x <- x/f; x[is.na(x)] <- 0
  if(!gety){y <- NULL}
  list(x=x,y=y,f=f,i=i)
}
get_model_xy <- function(x,p,gety,w,sel){
  out <- lapply(1:(nrow(x)-p-gety-sel),get_model_file,x=x,p=p,gety=gety)
  out <- rep(out,ceiling(sapply(out,function(x){x$i})/w))
  X <- do.call(rbind,lapply(out,function(x){x$x}))
  Y <- do.call(rbind,lapply(out,function(x){x$y}))
  list(Y=Y,X=X)
}

#MSAE
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

#plot

ggstack2 <- function(temp,sel=0.75,sel2=c('IRAN','SPAIN','ITALY','GERMANY','USA','FRANCE','CHINA')){
  temp <- data.table(date=as.POSIXct( "2020-01-21") + 3600*24*1:nrow(temp),temp) %>% as.data.frame
  temp <- melt(temp,id='date'); colnames(temp) <- c('date','state','case')
  if(!is.null(sel2)){
    sel <- sel2
  } else {
    sel <- which((cumsum((temp %>% dplyr::group_by(state) %>% dplyr::summarise(case=sum(case)) %>% arrange(desc(case)))$case)/sum(temp$case))>=sel)[1]
    sel <- ((temp %>% dplyr::group_by(state) %>% dplyr::summarise(case=sum(case)) %>% dplyr::arrange(dplyr::desc(case))))$state[1:sel]
  }
  temp <- mutate(temp,state=ifelse(state%in%sel,paste(state),'OTHERS'))
  temp <- temp %>% dplyr::group_by(state,date) %>% dplyr::summarise(case=sum(case))
  colnames(temp) <- c('State','Date','Cases')
  sel <- toupper(paste(sel))
  temp$State <- toupper(paste(temp$State))
  temp$State <- factor(temp$State,c('OTHERS',sel[length(sel):1]))
  # temp <- temp %>% filter(State%in%sel2)
  ggplot2::ggplot(temp,ggplot2::aes(fill=State,y=Cases,x=Date)) + ggplot2::geom_bar(position="stack", stat="identity")
}

ggline2 <- function(temp,sel=0.5,sel2=c('IRAN','SPAIN','ITALY','GERMANY','USA','FRANCE','CHINA')){
  temp <- data.table(date=as.POSIXct( "2020-01-20") + 3600*24*1:nrow(temp),temp) %>% as.data.frame
  temp <- melt(temp,id='date'); colnames(temp) <- c('date','state','case')
  if(!is.null(sel2)){
    sel <- sel2
  } else {
    sel <- which((cumsum((temp %>% dplyr::group_by(state) %>% dplyr::summarise(case=sum(case)) %>% arrange(desc(case)))$case)/sum(temp$case))>=sel)[1]
    sel <- ((temp %>% dplyr::group_by(state) %>% dplyr::summarise(case=sum(case)) %>% dplyr::arrange(dplyr::desc(case))))$state[1:sel]
  }
  temp <- mutate(temp,state=ifelse(state%in%sel,paste(state),'OTHERS'))
  temp <- temp %>% dplyr::group_by(state,date) %>% dplyr::summarise(case=sum(case))
  colnames(temp) <- c('State','Date','Cases')
  sel <- toupper(paste(sel))
  temp$State <- toupper(paste(temp$State))
  temp$State <- factor(temp$State,c('OTHERS',sel[length(sel):1]))
  temp <- temp %>% filter(State%in%sel2)
  ggplot2::ggplot(temp,ggplot2::aes(colour=State,y=Cases,x=Date)) + ggplot2::geom_line(size=1)
}

#todate
todate <- function(x){
  x <- as.POSIXct( "2020-01-20") + 3600*24*x
  paste(x)
}

############################
# Data Processing
############################

setwd('/Users/wenrurumon/Documents/posdoc/wuhan/data/github')
x <- fread(dir(pattern='model_')[7])
raw.c <- x[,-1] %>% as.matrix()

############################
# Prediction
############################

#Setup

Y.raw <- rbind(0,raw.c)
Y.model <- apply(Y.raw,2,diff)

#Modeling China Weight 10
sel <- 0
w <- 14
set.seed(7)
mfile <- get_model_xy(
  cbind(Y.model[,1,drop=F],global=rowSums(Y.model[,-1]))
  ,p=8,gety=T,w=w,sel=sel)
models.pred <- lapply(1:5,function(i){
  print(paste(i,Sys.time()))
  MSAE(X=cbind(mfile$X[rownames(mfile$X)=='CHINA',,drop=F],1),
       Y=mfile$Y[rownames(mfile$Y)=='CHINA',,drop=F],
       dims=c(32,4),activations=c('relu','relu'),
       batch=128,epochs=100,verbose=0)
})
mfile <- get_model_xy(Y.model,p=8,gety=T,w=w,sel=sel)
mfile$Y <- mfile$Y[c(1:nrow(mfile$X),rep(which(rownames(mfile$X)=='CHINA'),1)),,drop=F]
mfile$X <- mfile$X[c(1:nrow(mfile$X),rep(which(rownames(mfile$X)=='CHINA'),1)),,drop=F]
mfile$X <- cbind(mfile$X,state=as.numeric(rownames(mfile$X)=='CHINA'))
models.pred2 <- lapply(1:5,function(i){
  print(paste(i,Sys.time()))
  MSAE2(X=mfile$X,Y=mfile$Y,
        models.pred[[i]]$model,
        # dims=c(32,4),activations=c('relu','relu'),
        batch=128,epochs=100,verbose=0)
})
for(i in 1:5){keras::save_model_hdf5(models.pred2[[i]]$model,paste0('/Users/wenrurumon/Documents/posdoc/wuhan/model/af_2step_down',i,'.model'),overwrite = TRUE,include_optimizer = TRUE)}

# Model china weights 1
mfile <- get_model_xy(
  cbind(Y.model[,1,drop=F],global=rowSums(Y.model[,-1]))
  ,p=8,gety=T,w=w,sel=sel)
models.pred <- lapply(1:5,function(i){
  print(paste(i,Sys.time()))
  MSAE(X=cbind(mfile$X[rownames(mfile$X)=='CHINA',,drop=F],1),
       Y=mfile$Y[rownames(mfile$Y)=='CHINA',,drop=F],
       dims=c(32,4),activations=c('relu','relu'),
       batch=128,epochs=100,verbose=0)
})
mfile <- get_model_xy(Y.model,p=8,gety=T,w=w,sel=sel)
mfile$Y <- mfile$Y[c(1:nrow(mfile$X),rep(which(rownames(mfile$X)=='CHINA'),0)),,drop=F]
mfile$X <- mfile$X[c(1:nrow(mfile$X),rep(which(rownames(mfile$X)=='CHINA'),0)),,drop=F]
mfile$X <- cbind(mfile$X,state=as.numeric(rownames(mfile$X)=='CHINA'))
models.pred3 <- lapply(1:5,function(i){
  print(paste(i,Sys.time()))
  MSAE2(X=mfile$X,Y=mfile$Y,
        models.pred[[i]]$model,
        # dims=c(32,4),activations=c('relu','relu'),
        batch=128,epochs=100,verbose=0)
})
for(i in 1:5){keras::save_model_hdf5(models.pred3[[i]]$model,paste0('/Users/wenrurumon/Documents/posdoc/wuhan/model/af_2step_up',i,'.model'),overwrite = TRUE,include_optimizer = TRUE)}

#models.pred

models.pred2 <- lapply(
  paste0('/Users/wenrurumon/Documents/posdoc/wuhan/model/af_2step_down',1:5,'.model'),function(x){
    list(model=keras::load_model_hdf5(x))})
models.pred3 <- lapply(
  paste0('/Users/wenrurumon/Documents/posdoc/wuhan/model/af_2step_up',1:5,'.model'),function(x){
    list(model=keras::load_model_hdf5(x))})

#Prediction
rlts <- list()
Y.raw <- rbind(0,raw.c)
Y.model <- apply(Y.raw,2,diff)

# ASAP
#1
temp <- Y.model
for(i in 1:(365-nrow(Y.model))){
  x1 <- x2 <- x3 <- get_model_file(temp,i=nrow(temp)-7,p=8,gety=F)
  x1$x <- cbind(x1$x,1); x1$x[rownames(x1$x)=='china',9] <- 1
  x2$x <- cbind(x2$x,0.5); x2$x[rownames(x2$x)=='china',9] <- 1
  x3$x <- cbind(x3$x,0); x3$x[rownames(x3$x)=='china',9] <- 1
  x1 <- (sapply(models.pred2,function(m){m$model %>% predict(x1$x)}) * x1$f) %>% rowMeans
  x2 <- (sapply(models.pred2,function(m){m$model %>% predict(x2$x)}) * x2$f) %>% rowMeans
  x3 <- (sapply(models.pred2,function(m){m$model %>% predict(x3$x)}) * x3$f) %>% rowMeans
  x1 <- ifelse(x1<0,0,x1); x2 <- ifelse(x2<x1,x1,x2); x3 <- ifelse(x3<x2,x2,x3)
  if(i %in% 1:7){
    temp <- rbind(temp,x2)
  } else {
    temp <- rbind(temp,x1)
  }
}
temp[-1:-nrow(Y.model),apply(Y.model,2,function(x){sum(x>0)<5})] <- 0
rlts[[length(rlts)+1]] <- temp
ggstack2(temp[1:150,],sel2=NULL)
sum(temp)

# Other Scenarios
#0.5,1
temp <- Y.model
for(i in 1:(365-nrow(Y.model))){
  x1 <- x2 <- x3 <- get_model_file(temp,i=nrow(temp)-7,p=8,gety=F)
  x1$x <- cbind(x1$x,1); x1$x[rownames(x1$x)=='china',9] <- 1
  x2$x <- cbind(x2$x,0.5); x2$x[rownames(x2$x)=='china',9] <- 1
  x3$x <- cbind(x3$x,0); x3$x[rownames(x3$x)=='china',9] <- 1
  x1 <- (sapply(models.pred2,function(m){m$model %>% predict(x1$x)}) * x1$f) %>% rowMeans
  x2 <- (sapply(models.pred2,function(m){m$model %>% predict(x2$x)}) * x2$f) %>% rowMeans
  x3 <- (sapply(models.pred2,function(m){m$model %>% predict(x3$x)}) * x3$f) %>% rowMeans
  x1 <- ifelse(x1<0,0,x1); x2 <- ifelse(x2<x1,x1,x2); x3 <- ifelse(x3<x2,x2,x3)
  if(i %in% 1:7){
    temp <- rbind(temp,x3)
  }else if(i%in%8:14){
    temp <- rbind(temp,x2)
  } else {
    temp <- rbind(temp,x1)
  }
}
temp[-1:-nrow(Y.model),apply(Y.model,2,function(x){sum(x>0)<5})] <- 0
rlts[[length(rlts)+1]] <- temp
ggstack2(temp[1:150,],sel2=NULL)
sum(temp)

#0,0.5,1
temp <- Y.model
for(i in 1:(365-nrow(Y.model))){
  x1 <- x2 <- x3 <- get_model_file(temp,i=nrow(temp)-7,p=8,gety=F)
  x1$x <- cbind(x1$x,1); x1$x[rownames(x1$x)=='china',9] <- 1
  x2$x <- cbind(x2$x,0.5); x2$x[rownames(x2$x)=='china',9] <- 1
  x3$x <- cbind(x3$x,0); x3$x[rownames(x3$x)=='china',9] <- 1
  x1 <- (sapply(models.pred2,function(m){m$model %>% predict(x1$x)}) * x1$f) %>% rowMeans
  x2 <- (sapply(models.pred2,function(m){m$model %>% predict(x2$x)}) * x2$f) %>% rowMeans
  x3 <- (sapply(models.pred2,function(m){m$model %>% predict(x3$x)}) * x3$f) %>% rowMeans
  x1 <- ifelse(x1<0,0,x1); x2 <- ifelse(x2<x1,x1,x2); x3 <- ifelse(x3<x2,x2,x3)
  if(i %in% 1:14){
    temp <- rbind(temp,x3)
  }else if(i%in%15:21){
    temp <- rbind(temp,x2)
  } else {
    temp <- rbind(temp,x1)
  }
}
temp[-1:-nrow(Y.model),apply(Y.model,2,function(x){sum(x>0)<5})] <- 0
rlts[[length(rlts)+1]] <- temp
ggstack2(temp[1:150,],sel2=NULL)
sum(temp)

#0,0.5,0.5,0.5,1
temp <- Y.model
for(i in 1:(365-nrow(Y.model))){
  x1 <- x2 <- x3 <- get_model_file(temp,i=nrow(temp)-7,p=8,gety=F)
  x1$x <- cbind(x1$x,1); x1$x[rownames(x1$x)=='china',9] <- 1
  x2$x <- cbind(x2$x,0.5); x2$x[rownames(x2$x)=='china',9] <- 1
  x3$x <- cbind(x3$x,0); x3$x[rownames(x3$x)=='china',9] <- 1
  x1 <- (sapply(models.pred2,function(m){m$model %>% predict(x1$x)}) * x1$f) %>% rowMeans
  x2 <- (sapply(models.pred2,function(m){m$model %>% predict(x2$x)}) * x2$f) %>% rowMeans
  x3 <- (sapply(models.pred2,function(m){m$model %>% predict(x3$x)}) * x3$f) %>% rowMeans
  x1 <- ifelse(x1<0,0,x1); x2 <- ifelse(x2<x1,x1,x2); x3 <- ifelse(x3<x2,x2,x3)
  if(i %in% 1:21){
    temp <- rbind(temp,x3)
  }else if(i%in%22:28){
    temp <- rbind(temp,x2)
  } else {
    temp <- rbind(temp,x1)
  }
}
temp[-1:-nrow(Y.model),apply(Y.model,2,function(x){sum(x>0)<5})] <- 0
rlts[[length(rlts)+1]] <- temp
ggstack2(temp[1:150,],sel2=NULL)
sum(temp)

#0,0.5,0.5,0.5,0.5
temp <- Y.model
for(i in 1:(365-nrow(Y.model))){
  x1 <- x2 <- x3 <- get_model_file(temp,i=nrow(temp)-7,p=8,gety=F)
  x1$x <- cbind(x1$x,1); x1$x[rownames(x1$x)=='china',9] <- 1
  x2$x <- cbind(x2$x,0.5); x2$x[rownames(x2$x)=='china',9] <- 1
  x3$x <- cbind(x3$x,0); x3$x[rownames(x3$x)=='china',9] <- 1
  x1 <- (sapply(models.pred2,function(m){m$model %>% predict(x1$x)}) * x1$f) %>% rowMeans
  x2 <- (sapply(models.pred2,function(m){m$model %>% predict(x2$x)}) * x2$f) %>% rowMeans
  x3 <- (sapply(models.pred2,function(m){m$model %>% predict(x3$x)}) * x3$f) %>% rowMeans
  x1 <- ifelse(x1<0,0,x1); x2 <- ifelse(x2<x1,x1,x2); x3 <- ifelse(x3<x2,x2,x3)
  if(i %in% 1:28){
    temp <- rbind(temp,x3)
  }else if(i%in%29:36){
    temp <- rbind(temp,x2)
  } else {
    temp <- rbind(temp,x2)
  }
}
temp[-1:-nrow(Y.model),apply(Y.model,2,function(x){sum(x>0)<5})] <- 0
rlts[[length(rlts)+1]] <- temp
ggstack2(temp[1:150,],sel2=NULL)
sum(temp)

#Resulting
rlts <- lapply(rlts,function(x){floor(x)})
for(i in 1:4){
  for(j in 1:ncol(rlts[[1]])){
    rlts[[i+1]][,j] <- ifelse(rlts[[i]][,j]>rlts[[i+1]][,j],rlts[[i]][,j],rlts[[i+1]][,j])
  }
}

######################
#Summary
######################

cbind(sapply(rlts,sum))
rlts <- lapply(rlts,function(x){cbind(total=rowSums(x),x)})

#CSV
rlts.index <- lapply(rlts,function(x){
  x <- round(x)
  x.date_begin <- apply(x,2,function(x){which(x>0)[1]})
  x.date_max <- apply(x,2,function(x){which(x==max(x))[1]})
  x.date_end <- apply(x,2,function(x){max(which(x>0))})
  x.value_max <- sapply(1:length(x.date_max),function(i){apply(x,2,cumsum)[x.date_max[i],i]})
  x.value2_max <- apply(x,2,max)
  x.value_end <- apply(x,2,sum)
  x.value_now <- colSums(x[1:nrow(Y.model),])
  x.duration <- x.date_end-x.date_begin+1
  # x.date_begin <- todate(x.date_begin)
  # x.date_max<- todate(x.date_max)
  # x.date_end <- todate(x.date_end)
  data.table(state=colnames(x),
             date_begin=x.date_begin,date_max=x.date_max,date_end=x.date_end,
             duration=x.duration,value_max=x.value_max,value2_max=x.value2_max,
             value_now=x.value_now,value_end=x.value_end)
})
for(i in 2:length(rlts)){
  rlts.index[[i]]$value_end <- ifelse(rlts.index[[i]]$value_end>rlts.index[[i-1]]$value_end,rlts.index[[i]]$value_end,rlts.index[[i-1]]$value_end)
  rlts.index[[i]]$date_end <- ifelse(rlts.index[[i]]$date_end>rlts.index[[i-1]]$date_end,rlts.index[[i]]$date_end,rlts.index[[i-1]]$date_end)
}
rlts.index <- lapply(1:length(rlts),function(i){
  Y.raw <- cbind(total=rowSums(Y.raw),Y.raw)
  temp <- c(nrow(Y.raw),nrow(Y.raw)+7,nrow(Y.raw)+14,nrow(Y.raw)+28)
  idxi <- rlts.index[[i]]
  rlti <- rlts[[i]]
  data.table(idxi,sapply(1:nrow(idxi),function(j){
    # c(slopeup = mean(diffi[1:(temp[i]-1),j]),
    #   slopedown = mean(diffi[c(temp[i]:(idxi$date_end[j]-1)),j]))
    # c(slopeup = mean(diffi[1:idxi$date_max[j]]),
    #   slopedown = mean(diffi[idxi$date_max[j]:idxi$date_end[j]]))
    c(slopeup = (rlti[temp[i],j]-rlti[idxi$date_begin[j],j])/(temp[i]-idxi$date_begin[j]+1),
      slopedown = (rlti[idxi$date_end[j],j]-rlti[temp[i],j])/(idxi$date_end[j]-temp[i]+1))
  }) %>% t) %>% mutate(date_begin=todate(date_begin),
                       date_max=todate(date_max),
                       date_end=todate(date_end))
})
sapply(rlts.index,function(x){c(x$slopeup[1],x$slopedown[1])})
write.csv(do.call(cbind,rlts.index),'/Users/wenrurumon/Documents/posdoc/wuhan/summary/temp.csv')

#Risk Calculation
# rlts.index <- do.call(cbind,rlts.index)
# rlt <- data.table(state=rlts.index[,c(1,4,8)],rlts.index[,colnames(rlts.index) %in% c('date_end','value_end')])
# for(i in c(4,6,8,10)){
#   rlt <- cbind(rlt,risk_date=rlt[[i]]-rlt[[2]])
#   rlt <- cbind(rlt,risk_value=rlt[[i+1]]-rlt[[3]])
# }
# colnames(rlt) <- c('state',
#   paste(rep(c('date_end','value_end'),4),rep(c('now','scenario1','scenario2','scenario3','scenario4'),each=2),sep='_'),
#   paste(rep(c('risk_date','risk_end'),4),rep(c('scenario1','scenario2','scenario3','scenario4'),each=2),sep='_'))
# write.csv(rlt,'temp.csv')

#Nat Plot

grid::grid.newpage() 
grid::pushViewport(grid::viewport(layout = grid::grid.layout(4,2))) 
vplayout <- function(x,y){grid::viewport(layout.pos.row = x, layout.pos.col = y)} 

p <- ggline2((rlts[[2]][1:150,-1]));print(p+labs(y='New Cases'), vp = vplayout(1,1))
p <- ggline2(apply(rlts[[2]][1:150,-1],2,cumsum));print(p+labs(y='Accumulated Cases'), vp = vplayout(1,2))
p <- ggline2((rlts[[3]][1:150,-1]));print(p+labs(y='New Cases'), vp = vplayout(2,1))
p <- ggline2(apply(rlts[[3]][1:150,-1],2,cumsum));print(p+labs(y='Accumulated Cases'), vp = vplayout(2,2))
p <- ggline2((rlts[[4]][1:150,-1]));print(p+labs(y='New Cases'), vp = vplayout(3,1))
p <- ggline2(apply(rlts[[4]][1:150,-1],2,cumsum));print(p+labs(y='Accumulated Cases'), vp = vplayout(3,2))
p <- ggline2((rlts[[5]][1:150,-1]));print(p+labs(y='New Cases'), vp = vplayout(4,1))
p <- ggline2(apply(rlts[[5]][1:150,-1],2,cumsum));print(p+labs(y='Accumulated Cases'), vp = vplayout(4,2))

write.csv(sapply(rlts[-1],function(x){x[,1]}),'/Users/wenrurumon/Documents/posdoc/wuhan/summary/temp.csv')

grid::grid.newpage() 
grid::pushViewport(grid::viewport(layout = grid::grid.layout(2,2)))
vplayout <- function(x,y){grid::viewport(layout.pos.row = x, layout.pos.col = y)} 
print(ggstack2(rlts[[2]][1:150,-1]),vp=vplayout(1,1))
print(ggstack2(rlts[[3]][1:150,-1]),vp=vplayout(1,2))
print(ggstack2(rlts[[4]][1:150,-1]),vp=vplayout(2,1))
print(ggstack2(rlts[[5]][1:150,-1]),vp=vplayout(2,2))

######################
#ARIMA
######################

y <- raw.d
x <- raw.d/raw.c
temp <- data.frame(date=as.POSIXct("2020-01-20") + 3600*24*1:nrow(x),x[,match(names(-sort(-apply(y,2,max)))[c(1:11)],colnames(x))])
colnames(temp)
temp <- temp[,-8]
colnames(temp) <- toupper(colnames(temp))
temp <- melt(temp,id='DATE') %>% 
  filter(variable!='IRAN'&!is.na(value)) %>%
  mutate(value=value*100)
colnames(temp)[1:3] <- c('Date','Country','Death')
ggplot(temp,aes(colour=Country,y=Death,x=Date)) + geom_line(size=1) + labs(y='Case Fatality%')

y <- rowSums(raw.d)
x <- rowSums(raw.c)
y <- y/x
y1 <- c(rep(NA,length=length(y)),rep(mean(y[-1:-35]),150))[1:150]
y2 <- c(y,rep(NA,150))[1:150]
x <- data.frame(date=as.POSIXct("2020-01-20") + 3600*24*1:150,Actual=y2,Predict=y1)
x <- melt(x,id='date') %>% mutate(value=value*100,Legend=variable)
ggplot(x,aes(colour=Legend,y=value,x=date)) + geom_line(size=1) + ylim(0,5) + labs(x='Date',y='Case Fatality(%)')


x <- raw.c[,colSums(raw.d)>0]
y <- raw.d[,colSums(raw.d)>0]
write.csv(cbind(x,y),'temp.csv')
