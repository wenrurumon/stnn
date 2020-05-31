
rm(list=ls())
setwd("/Users/wenrurumon/Documents/posdoc/wuhan/data/wtest")
library(data.table)
library(dplyr)
library(ggplot2)
library(reshape2)
library(keras)

#####################################################################################
# Rawdata
#####################################################################################

raw <- fread('daily.csv') %>% 
  select(date,state,positive,death,test=total) %>% 
  mutate(date=as.Date(date),
         positive=ifelse(is.na(positive),0,positive),
         death=ifelse(is.na(death),0,death)) %>% 
  arrange(state,date) %>% 
  merge(fread('uspop.csv') %>% select(state=V1,pop=V2),id='state') %>%
  melt(id=c('state','date')) %>% 
  acast(date~state~variable)
raw[is.na(raw)] <- 0
dimnames(raw)

raw.date <- as.Date(rownames(raw))
raw.weekday <- weekdays(as.Date(rownames(raw)))
raw.weekday <- match(raw.weekday,c(unique(raw.weekday)[-1:-5],unique(raw.weekday)[1:5]))

for(i in 2:nrow(raw)){
  raw[i,,1] <- ifelse(raw[i,,1]<raw[i-1,,1],raw[i-1,,1],raw[i,,1])
  raw[i,,2] <- ifelse(raw[i,,2]<raw[i-1,,2],raw[i-1,,2],raw[i,,2])
  raw[i,,3] <- ifelse(raw[i,,3]<raw[i-1,,3],raw[i-1,,3],raw[i,,3])
}
for(i in 1:ncol(raw)){raw[,i,4] <- max(raw[,i,4])}

pos <- raw.pos <- apply(rbind(0,raw[,,1]),2,diff)
death <- raw.death <- apply(rbind(0,raw[,,2]),2,diff)
trate <- raw.trate <- raw[,,3]/raw[,,4]

test <- data.frame(date=as.Date(rownames(raw)),value=rowSums(pos)) %>% mutate(weekday=weekdays(date))
test$rate <- diff(c(0,test$value))/test$value
boxplot(rate~weekday,data=test)

#####################################################################################
# Prepare for POS model
#####################################################################################

getpos <- function(i,train=T){
  x.pos <- t(pos[i+0:7,])
  f.pos <- (rowMeans(x.pos))+1
  x.pos <- x.pos/f.pos
  x.trate <- t(trate[i+0:7,])
  if(train){
    y.trate <- t(trate[i+8,,drop=F])
    y.pos <- t(pos[i+8,,drop=F])/f.pos
    y.a <- cbind(pos[i+8,]/(apply(pos,2,cumsum)[i+7,]+1))
  } else {
    y.trate <- NULL
    y.pos <- NULL
    y.a <- NULL
  }
  list(x.pos=x.pos,x.trate=x.trate,
       y.pos=y.pos,y.trate=y.trate,
       f.pos=f.pos,y.a=y.a)
}

data4train <- lapply(1:(nrow(pos)-8),getpos,train=T)
x.pos <- do.call(rbind,lapply(data4train,function(x){x$x.pos}))
y.pos <- do.call(rbind,lapply(data4train,function(x){x$y.pos}))
x.trate <- do.call(rbind,lapply(data4train,function(x){cbind(trate=rowMeans(x$x.trate))}))
f.pos <- do.call(rbind,lapply(data4train,function(x){cbind(x$f.pos)}))
x.wd <- rep(raw.weekday[-1:-8],each=51)
X <- cbind(x.pos,x.trate,weekday=outer(x.wd,1:7,'==')+0)
Y <- cbind(y.pos)

model <- function(X,Y){
  e.input <- layer_input(shape=ncol(X))
  e.layer <- layer_dense(e.input,16,activation='relu') %>% layer_dropout(rate=0.2)
  l.layer <- layer_dense(e.layer,4,activation='relu') %>% layer_dropout(rate=0.2)
  d.output <- layer_dense(units=ncol(Y))
  model <- keras_model(e.input,d.output(l.layer))
  model %>% compile(loss = "mean_squared_error", optimizer = "adam")
  loss <- Inf
  itv <- 1
  system.time(temp <- model %>% fit(x = X,y = Y,batch = 128,epochs = 100,verbose = 0))
  print(paste(loss,mean(temp$metrics$loss),itv<-itv+1,'100 epochs',Sys.time()))
  while(mean(temp$metrics$loss)<loss){
    loss <- mean(temp$metrics$loss)
    system.time(temp <- model %>% fit(x = X,y = Y,batch = 128,epochs = 100,verbose = 0))
    print(paste(loss,mean(temp$metrics$loss),itv<-itv+1,'100 epochs',Sys.time()))
    if(mean(temp$metrics$loss)>loss){
      system.time(temp <- model %>% fit(x = X,y = Y,batch = 128,epochs = 1000,verbose = 0))
      print(paste(loss,mean(temp$metrics$loss),itv<-itv+1,'1000 epochs',Sys.time()))
    }
    if(mean(temp$metrics$loss)>loss){
      system.time(temp <- model %>% fit(x = X,y = Y,batch = 128,epochs = 1000,verbose = 0))
      print(paste(loss,mean(temp$metrics$loss),itv<-itv+1,'1000 epochs',Sys.time()))
    }
  }
  return(list(model=model,history=temp))
}

#####################################################################################
# Confirmed
#####################################################################################

#Confirmed Model
model1 <- lapply(1:5,function(i){print(i);model(X,Y)})
X <- lapply(lapply(1:(nrow(pos)-8),getpos,train=T),function(x){
  list(x=cbind(x$x.pos,rowMeans(x$x.trate),
               wy=matrix((rep(1:7,each=51)==raw.weekday[which(raw.date==colnames(x$y.pos))]),nrow=51)+0
               ),
       f1=x$f.pos)
})
Y <- lapply(lapply(1:(nrow(pos)-8),getpos,train=T),function(x){
  list(y=cbind(x$y.pos),f1=x$f.pos)
})

#Fit
pos.pred <- lapply(model1,function(model){t(sapply(X,function(x){(model$model %>% predict(x$x)) * x$f1}))})
pos.pred <- sapply(pos.pred,rowSums)
pos.actual <- t(sapply(Y,function(x){x$y*x$f1})) %>% rowSums
rownames(pos.pred) <- paste(raw.date[-1:-8])
colnames(pos.pred) <- paste0('fit',1:length(model1))
pos.pred <- data.frame(date=as.Date(rownames(pos.pred)),
                       fit=rowMeans(pos.pred),actual=pos.actual) %>% melt(id='date')
ggplot() + geom_line(data=pos.pred,aes(x=date,y=value,colour=variable),size=1)

#Prediction

for(r in c(0.5,1,2)){
  pos <- raw.pos
  trate <- raw.trate
  Xnew <- getpos(nrow(pos)-7,train=F)
  raw.date <- c(raw.date,max(raw.date)+1)
  raw.weekday <- c(raw.weekday,
                   ifelse(raw.weekday[length(raw.weekday)]+1>7,1,raw.weekday[length(raw.weekday)]+1))
  pos <- rbind(pos,rowMeans(sapply(model1,function(model){
    model$model %>% predict(cbind(Xnew$x.pos,rowMeans(Xnew$x.trate)
                                  ,matrix(rep((1:7)==raw.weekday[length(raw.weekday)],each=51),ncol=7)+0))
  }) * Xnew$f.pos)) %>% floor
  pos <- ifelse(pos<0,0,pos)
  trate <- rbind(trate,apply(Xnew$x.trate,1,max)+colMeans(apply(Xnew$x.trate,1,diff)))
  trate <- ifelse(trate>1,1,trate)
  for(i in 1:300){
    print(i)
    Xnew <- getpos(nrow(pos)-7,train=F)  
    raw.date <- c(raw.date,max(raw.date)+1)
    raw.weekday <- c(raw.weekday,
                     ifelse(raw.weekday[length(raw.weekday)]+1>7,1,raw.weekday[length(raw.weekday)]+1))
    pos <- rbind(pos,rowMeans(sapply(model1,function(model){
      model$model %>% predict(cbind(Xnew$x.pos,rowMeans(Xnew$x.trate)
                                    ,matrix(rep((1:7)==raw.weekday[length(raw.weekday)],each=51),ncol=7)+0))
    }) * Xnew$f.pos)) %>% floor
    pos <- ifelse(pos<0,0,pos)
    trate <- rbind(trate,apply(Xnew$x.trate,1,max)+colMeans(apply(Xnew$x.trate,1,diff))*r)
    trate <- ifelse(trate>1,1,trate)
  }
  if(r==0.5){
    plot.ts(rowSums(pos))
  } else {
    lines(rowSums(pos),col=c(2,4)[r])  
  }
}



