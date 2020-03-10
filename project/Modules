
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

#plot
ggstack <- function(temp){
  temp <- data.table(date=as.POSIXct( "2020-01-20") + 3600*24*1:nrow(temp),temp) %>% as.data.frame
  temp <- melt(temp,id='date'); colnames(temp) <- c('Date','Country','Cases')
  temp <- mutate(temp,Country=ifelse(Country%in%c('china','iran','korea','italy','usa','germany','france'),toupper(Country),'OTHERS'))
  temp <- temp %>% group_by(Date,Country) %>% summarise(Cases=sum(Cases))
  temp <- merge(temp,(temp %>% group_by(Country) %>% summarise(ttl=sum(Cases)) %>% arrange(ttl)),by='Country') %>% arrange(ttl)
  temp$Country <- factor(temp$Country,c('OTHERS','GERMANY','FRANCE',"USA",'ITALY','KOREA','IRAN','CHINA'))
  ggplot(temp,aes(fill=Country,y=Cases,x=Date)) + geom_bar(position="stack", stat="identity")
}

ggline <- function(temp){
  temp <- data.table(date=as.POSIXct( "2020-01-20") + 3600*24*1:nrow(temp),temp) %>% as.data.frame
  temp <- melt(temp,id='date'); colnames(temp) <- c('Date','Country','Cases')
  temp <- mutate(temp,Country=ifelse(Country%in%c('china','iran','korea','italy','usa','germany','france'),toupper(Country),'OTHERS'))
  temp <- temp %>% group_by(Date,Country) %>% summarise(Cases=sum(Cases))
  temp <- merge(temp,(temp %>% group_by(Country) %>% summarise(ttl=sum(Cases)) %>% arrange(ttl)),by='Country') %>% arrange(ttl)
  temp$Country <- factor(temp$Country,c('OTHERS','GERMANY','FRANCE',"USA",'ITALY','KOREA','IRAN','CHINA'))
  ggplot(temp,aes(colour=Country,y=Cases,x=Date)) + geom_line(size=1)
}

#todate
todate <- function(x){
  x <- as.POSIXct( "2020-01-20") + 3600*24*x
  paste(x)
}
