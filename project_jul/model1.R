
rm(list=ls())
setwd("/Users/wenrurumon/Documents/posdoc/wuhan/data/wtest")
library(data.table)
library(dplyr)
library(ggplot2)
library(reshape2)
library(keras)
library(forecast)

weekday <- function(x){match(weekdays(as.Date(x)),weekdays(as.Date(paste('2010-1-10'))+1:7))}

#####################################################################################
# Rawdata
#####################################################################################

#Function

getya <- function(i,train=T,h=8){
  y1 <- (t(Y[i+0:(h-1),])+1)
  f <- ((rowMeans(y1)))
  y1 <- y1/f
  a1 <- (t(A[i+0:(h-1),]))
  if(train){
    y2 <- (t(Y[i+h,,drop=F])+1)/f
    a2 <- t(A[i+h,,drop=F])
  } else {
    a2 <- y2 <- NULL
  }
  list(y1=y1,y2=y2,a1=a1,a2=a2,f=f)
}

model.conti <- function(X,Y){
  e.input <- layer_input(shape=ncol(X))
  e.layer <- layer_dense(e.input,16,activation='relu') %>% layer_dropout(rate=0.2)
  l.layer <- layer_dense(e.layer,4,activation='relu') %>% layer_dropout(rate=0.2)
  d.output <- layer_dense(units=ncol(Y))
  model <- keras_model(e.input,d.output(l.layer))
  model %>% compile(loss = "mean_squared_error", optimizer = "adam",metric='mae')
  loss <- Inf
  itv <- 0
  system.time(temp <- model %>% fit(x = X,y = Y,batch = 128,epochs = 1000,verbose = 0))
  print(paste(loss,mean(temp$metrics$loss),itv<-itv+1,'first epochs',Sys.time(),sep=', '))
  while(mean(temp$metrics$loss)<loss){2
    loss <- mean(temp$metrics$loss)
    system.time(temp <- model %>% fit(x = X,y = Y,batch = 128,epochs = 100,verbose = 0))
    print(paste(loss,mean(temp$metrics$loss),itv<-itv+1,'new epoch',Sys.time(),sep=', '))
    if(mean(temp$metrics$loss)>loss){
      system.time(temp <- model %>% fit(x = X,y = Y,batch = 128,epochs = 100,verbose = 0))
      print(paste(loss,mean(temp$metrics$loss),itv<-itv+1,'2nd epoch',Sys.time(),sep=', '))
    }
    if(mean(temp$metrics$loss)>loss){
      system.time(temp <- model %>% fit(x = X,y = Y,batch = 128,epochs = 100,verbose = 0))
      print(paste(loss,mean(temp$metrics$loss),itv<-itv+1,'3rd epoch',Sys.time(),sep=', '))
    }
  }
  return(model)
}

#Import

raw_china <- read.csv('/Users/wenrurumon/Documents/posdoc/wuhan/data/github/china.csv',row.names=1)
colnames(raw_china) <- tolower(colnames(raw_china))
for(i in 2:nrow(raw_china)){
  raw_china[i,] <- ifelse(raw_china[i,]<raw_china[i-1,],raw_china[i-1,],raw_china[i,])
}
raw_china <- apply(rbind(0,raw_china),2,diff) %>% as.data.frame

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

for(i in (nrow(raw)-1):1){
  raw[i,,1] <- ifelse(raw[i,,1]>raw[i+1,,1],raw[i+1,,1],raw[i,,1])
  raw[i,,2] <- ifelse(raw[i,,2]>raw[i+1,,2],raw[i+1,,2],raw[i,,2])
  raw[i,,3] <- ifelse(raw[i,,3]>raw[i+1,,3],raw[i+1,,3],raw[i,,3])
}
for(i in 1:ncol(raw)){raw[,i,4] <- max(raw[,i,4])}

raw2 <- raw
pos <- raw.pos <- apply(rbind(0,raw2[,,1]),2,diff) 
death <- raw.death <- apply(rbind(0,raw2[,,2]),2,diff)
trate <- raw.trate <- raw2[,,3]/raw2[,,4]

plot.ts(rowSums(pos))
plot.ts(rowSums(death))
plot.ts(rowMeans(trate))

#Model Filing

pos_china <- raw_china[rownames(raw_china)%in%rownames(raw),]
a_china <- sapply(1:ncol(pos_china),function(i){
  (1:nrow(pos))/which.max(pos_china[1:60,i])/2
})
a_china <- ifelse(a_china>1,1,a_china)
colnames(a_china) <- colnames(pos_china) 

#####################################################################################
# Find A
#####################################################################################

#Setup
Y <- pos_china
A <- a_china
modelfile <- lapply(1:(nrow(Y)-8),getya,train=T,h=8)
X.mf <- do.call(rbind,lapply(modelfile,function(x){cbind(x$y1,rowMeans(x$a1))}))
Y.mf <- do.call(rbind,lapply(modelfile,function(x){cbind(x$y2,rowMeans(x$a2))}))

for(i in 1:5){
  modeli <- model.conti(X.mf,Y.mf[,1,drop=F])
  Y.mf_pred <- sapply((1:20)/20,function(a){
    X.temp <- X.mf
    X.temp[,ncol(X.temp)] <- a
    modeli %>% predict(X.temp)
  })
  print(summary(abs((modeli%>%predict(X.mf)/Y.mf[,1])-1)))
  X.mf[,ncol(X.mf)] <- 
    ((1:20)/20)[apply(abs(Y.mf_pred/Y.mf[,1]-1),1,which.min)] * 0.5 + 
    X.mf[,ncol(X.mf)] * 0.5
  plot.ts(as.numeric(tapply(X.mf[,ncol(X.mf)],rep(1:length(modelfile),each=ncol(Y)),mean)))
  print(summary(abs((modeli%>%predict(X.mf)/Y.mf[,1])-1)))
}


#####################################################################################

#####################################################################################
# Prepare for POS model
#####################################################################################

gety <- function(i,train=T,h=8,fun=NULL){
  fun <- ifelse(is.null(fun),function(x){return(x)},fun)
  x.pos <- fun(t(pos[i+0:(h-1),])+1)
  f.pos <- ((rowMeans(x.pos))+1)
  x.pos <- x.pos/f.pos
  if(train){
    y.pos <- fun(t(pos[i+h,,drop=F])+1)/f.pos
  } else {
    y.pos <- NULL
  }
  list(x.pos=x.pos,y.pos=y.pos,f.pos=f.pos)
}

getxy <- function(i,train=T,h=8,fun=NULL){
  fun <- ifelse(is.null(fun),function(x){return(x)},fun)
  x.pos <- fun(t(pos[i+0:(h-1),])+1)
  f.pos <- ((rowMeans(x.pos))+1)
  x.pos <- x.pos/f.pos
  x.trate <- t(trate[i+0:(h-1),])
  if(train){
    y.pos <- fun(t(pos[i+h,,drop=F])+1)/f.pos
    y.trate <- t(trate[i+h,,drop=F])
  } else {
    y.pos <- NULL
    y.trate <- NULL
  }
  list(x.pos=x.pos,y.pos=y.pos,x.trate=x.trate,y.trate=y.trate,f.pos=f.pos)
}

model <- function(X,Y){
  e.input <- layer_input(shape=ncol(X))
  e.layer <- layer_dense(e.input,16,activation='relu') %>% layer_dropout(rate=0.2)
  l.layer <- layer_dense(e.layer,4,activation='relu') %>% layer_dropout(rate=0.2)
  d.output <- layer_dense(units=ncol(Y))
  model <- keras_model(e.input,d.output(l.layer))
  model %>% compile(loss = "mean_squared_error", optimizer = "adam",metric='mae')
  loss <- Inf
  itv <- 0
  system.time(temp <- model %>% fit(x = X,y = Y,batch = 128,epochs = 100,verbose = 0))
  print(paste(loss,mean(temp$metrics$loss),itv<-itv+1,'first epochs',Sys.time(),sep=', '))
  while(mean(temp$metrics$loss)<loss){2
    loss <- mean(temp$metrics$loss)
    system.time(temp <- model %>% fit(x = X,y = Y,batch = 128,epochs = 100,verbose = 0))
    print(paste(loss,mean(temp$metrics$loss),itv<-itv+1,'new epoch',Sys.time(),sep=', '))
    if(mean(temp$metrics$loss)>loss){
      system.time(temp <- model %>% fit(x = X,y = Y,batch = 128,epochs = 1000,verbose = 0))
      print(paste(loss,mean(temp$metrics$loss),itv<-itv+1,'2nd epoch',Sys.time(),sep=', '))
    }
    if(mean(temp$metrics$loss)>loss){
      system.time(temp <- model %>% fit(x = X,y = Y,batch = 128,epochs = 1000,verbose = 0))
      print(paste(loss,mean(temp$metrics$loss),itv<-itv+1,'3rd epoch',Sys.time(),sep=', '))
    }
  }
  return(model)
}

#####################################################################################
# Y.us = Y.us
#####################################################################################

#Modeling
data4train <- lapply(1:(nrow(pos)-8),gety,train=T,h=8,fun=NULL)
data4train <- rep(data4train,ceiling((1:length(data4train))/1000))
X <- do.call(rbind,lapply(data4train,function(x){cbind((x$x.pos))}))
Y <- do.call(rbind,lapply(data4train,function(x){cbind((x$y.pos))}))
X.wd <- outer(rep(weekday(rownames(trate)[-1:-8]),each=51),1:7,'==')+0

X <- cbind(X,X.wd)
model1 <- keras::load_model_hdf5('/Users/wenrurumon/Documents/posdoc/wuhan/model/yus2.model')
# set.seed(2)
# model1 <- model(X,Y)
# save_model_hdf5(model1,paste0('/Users/wenrurumon/Documents/posdoc/wuhan/model/yus.model'))

#Fit
data4train <- lapply(1:(nrow(pos)-8),gety,train=T,h=8,fun=NULL)
X.f <- do.call(rbind,lapply(data4train,function(x){cbind((x$f.pos))}))
X <- do.call(rbind,lapply(data4train,function(x){cbind((x$x.pos))}))
X <- cbind(X)
Y <- do.call(rbind,lapply(data4train,function(x){cbind((x$y.pos))}))
Y.fit <- model1 %>% predict(X)
plot.ts(tapply((Y*X.f-1),ceiling((1:nrow(Y))/51),sum)%>%as.numeric)
lines(tapply((Y.fit*X.f-1),ceiling((1:nrow(Y))/51),sum)%>%as.numeric,col=2)
X.wd <- outer(rep(weekday(rownames(trate)[-1:-8]),each=51),1:7,'==')+0
Y.fit2 <- predict(model1.wd <- lm(Y~Y.fit+X.wd-1))
lines(tapply((Y.fit2*X.f-1),ceiling((1:nrow(Y))/51),sum)%>%as.numeric,col=4)

#Prediction
pos <- raw.pos
for(j in 1:(365*3-nrow(pos))){
  x.temp <- gety(nrow(pos)-7,train=F,h=8,fun=NULL)
  x.temp <- as.numeric(floor(
    ((model1 %>% predict(cbind(x.temp$x.pos))) * coef(model1.wd)[1] + 
       coef(model1.wd)[weekday(max(as.Date(rownames(pos)))+j)+1]) * 
      x.temp$f.pos - 1))
  x.temp <- ifelse(tail(pos,1)==x.temp,x.temp-1,x.temp)
  x.temp[x.temp<0] <- 0
  pos <- rbind(pos,x.temp)
}
plot.ts(rowSums(predict.us <- pos))
sum(pos)

#####################################################################################
# Y.all = Y.all + X.all
#####################################################################################

pos <- cbind(raw.pos,pos_china)
trate <- cbind(raw.trate,trate_china)
trate <- rbind(trate,apply(trate,2,function(x){forecast(auto.arima(x),h=365*3-nrow(pos))$mean}))
trate <- ifelse(trate>1,1,trate)

#Modeling
# for(A in (10:20)/20){
#   print(A)
#   pos <- cbind(raw.pos,pos_china)
#   trate <- cbind(raw.trate,trate_china)
#   trate <- rbind(trate,apply(trate,2,function(x){forecast(auto.arima(x),h=365*3-nrow(pos))$mean}))
#   trate <- ifelse(trate>1,1,trate)
#   trate[,-(1:ncol(raw.pos))] <- trate[,-(1:ncol(raw.pos))]*A
#   data4train <- lapply(1:(nrow(pos)-8),getxy,train=T,h=8,fun=NULL)
#   X <- do.call(rbind,lapply(data4train,function(x){cbind(x$x.pos,x$y.trate)}))
#   Y <- do.call(rbind,lapply(data4train,function(x){cbind((x$y.pos))}))
#   set.seed(1)
#   model2 <- model(X,Y)
#   save_model_hdf5(model2,paste0('/Users/wenrurumon/Documents/posdoc/wuhan/model/yxall2_',A,'.model'))
# }
model2 <- lapply((10:20)/20,function(A){
  keras::load_model_hdf5(paste0('/Users/wenrurumon/Documents/posdoc/wuhan/model/yxall2_',A,'.model'))
})

#Fit
pos <- cbind(raw.pos)
trate <- cbind(raw.trate)
fit.model2 <- lapply(1:length(model2),function(i){
  pos <- raw.pos
  trate <- raw.trate
  data4train <- lapply(1:(nrow(pos)-8),getxy,train=T,h=8,fun=NULL)
  X <- lapply(data4train,function(x){list(x=cbind(x$x.pos,x$y.trate),f=x$f.pos)})
  Y <- lapply(data4train,function(x){cbind((x$y.pos))})
  Y.fit <- (lapply(X,function(x){model2[[i]] %>% predict(x$x)}))
  Y.fit <- do.call(rbind,Y.fit)
  X.wd <- outer(rep(sapply(Y,function(x){weekday(colnames(x))}),each=51),1:7,'==')+0
  Y2 <- do.call(rbind,Y)
  test <- cbind(Y2,Y.fit,X.wd)
  Y2 <- matrix(predict(lm(test[,1]~test[,-1]-1)) * unlist(lapply(X,function(x){x$f}))-1,nrow=51)
  Y2[Y2<0] <- 0
  plot.ts(rowSums(pos)[-1:-8],main=i); lines(colSums(Y2),col=2)
  Y <- unlist(Y)
  rlt <- coef(lm(Y~Y.fit+X.wd-1))
  return(rlt)
})

#Prediction
rlt.model2 <- list()
for(i in 1:length(model2)){
  modeli <- model2[[i]]
  coefi <- fit.model2[[i]]
  pos <- raw.pos
  trate <- raw.trate
  trate2 <- apply(
    rbind(trate[nrow(trate),],
          apply(trate,2,function(x){forecast(auto.arima(x),h=365*3-nrow(pos))$mean})),2,diff
  )
  trate2 <- t(t(apply(trate2,2,cumsum))+trate[nrow(trate),])
  trate <- rbind(trate,trate2)
  trate <- ifelse(trate>1,1,trate)
  # trate <- ifelse(trate>((10:20)/20)[i],((10:20)/20)[i],trate)
  rownames(trate) <- paste(min(as.Date(rownames(trate)),na.rm=T) + 1:nrow(trate) - 1)
  for(j in 1:(365*3-nrow(pos))){
    x.temp <- getxy(nrow(pos)-7,train=F,h=8,fun=NULL)
    y.trate <- trate[nrow(pos)+1,]
    x.temp <- as.numeric(
      floor(((modeli %>% predict(cbind(x.temp$x.pos,y.trate))) * 
               coefi[1] + coefi[weekday(rownames(trate)[nrow(pos)+1])+1]) * x.temp$f.pos - 1)
    )
    x.temp <- ifelse(tail(pos,1)==x.temp,x.temp-1,x.temp)
    x.temp[x.temp<0] <- 0
    pos <- rbind(pos,x.temp)
  }
  rlt.model2[[i]] <- pos
}
plot.ts(sapply(rlt.model2,rowSums)[,-1])
sapply(rlt.model2,sum)

#####################################################################################
# Scenarios
#####################################################################################

rlt.scenarios <- list()
i <- 5
modeli <- model2[[i]]
A <- ((10:20)/20)[i]
print(A)

for(i in 1:6){
  r <- c(0.1,0.25,0.5,1,1.5,2)[i]
  pos <- raw.pos
  trate <- raw.trate
  trate2 <- t(matrix(rep(colMeans(apply(raw.trate[nrow(raw.trate)-7:0,],2,diff)),365*3-nrow(pos)),
                     nrow=ncol(pos)))*r
  print(sum(unique(trate2)*unique(raw[,,4])))
  trate2 <- t(t(apply(trate2,2,cumsum))+trate[nrow(trate),])
  trate <- rbind(trate,trate2)
  trate <- ifelse(trate>1,1,trate)
  rownames(trate) <- paste(min(as.Date(rownames(trate)),na.rm=T) + 1:nrow(trate) - 1)
  for(j in 1:(365*3-nrow(pos))){
    x.temp <- getxy(nrow(pos)-7,train=F,h=8,fun=NULL)
    y.trate <- trate[nrow(pos)+1,]
    x.temp <- as.numeric(
      floor(((modeli %>% predict(cbind(x.temp$x.pos,y.trate))) * 
               coefi[1] + coefi[weekday(rownames(trate)[nrow(pos)+1])+1]) * x.temp$f.pos - 1)
    )
    x.temp <- ifelse(tail(pos,1)==x.temp,x.temp-1,x.temp)
    x.temp[x.temp<0] <- 0
    pos <- rbind(pos,x.temp)
  }
  rlt.scenarios[[i]] <- list(pos=pos,trate=trate)
}
for(i in 1:length(rlt.scenarios)){rownames(rlt.scenarios[[i]]$pos) <- rownames(trate)}
plot.ts(do.call(cbind,lapply(rlt.scenarios,function(x){cbind(pos=rowSums(x$pos))})))
sapply(rlt.scenarios,function(x){sum(x$pos)})
tail(sapply(rlt.scenarios,function(x){rowSums(x$pos)}))

#####################################################################################
# Death
#####################################################################################

pos <- apply(raw.pos,2,cumsum)
death <- apply(raw.death,2,cumsum)
getyd <- function(i,train=T,h=8,fun=NULL){
  x.pos <- t(pos[i+0:(h-1),,drop=F])
  f.pos <- rowMeans(x.pos)+1
  x.pos <- x.pos/f.pos
  x.death <- t(death[i+0:(h-1),,drop=F])
  f.death <- rowMeans(x.death)+1
  x.death <- x.death/f.death
  if(train){
    y.death <- t(death[i+(h),,drop=F])/f.death
  } else {
    y.death <- NULL
  }
  list(x.pos=x.pos,x.death=x.death,y.death=y.death,f.pos=f.pos,f.death=f.death)
}
data4train <- lapply(1:(nrow(pos)-8),getyd,train=T,h=8,fun=NULL)
X <- do.call(rbind,lapply(data4train,function(x){cbind(x$x.pos)}))
Y <- do.call(rbind,lapply(data4train,function(x){cbind(x$y.death)}))
# model3 <- model(X,Y)
# save_model_hdf5(model3,paste0('/Users/wenrurumon/Documents/posdoc/wuhan/model/death2.model'))

model3 <- keras::load_model_hdf5('/Users/wenrurumon/Documents/posdoc/wuhan/model/death2.model')
for(i in 1:length(rlt.scenarios)){
  death <- apply(raw.death,2,cumsum)
  pos <- apply(rlt.scenarios[[i]]$pos,2,cumsum)
  for(j in 1:(365*3-nrow(raw.pos))){
    x.temp <- getyd(nrow(death)-7,train=F,h=8,fun=NULL)
    x.temp2 <- as.numeric(floor((model3 %>% predict(x.temp$x.pos))*x.temp$f.death))
    x.temp <- ifelse(x.temp2<death[nrow(death),]|rowSums(!x.temp$x.pos==rowMeans(x.temp$x.pos))==0
                     ,death[nrow(death),],x.temp2)
    death <- rbind(death,x.temp)
  }
  rlt.scenarios[[i]]$death <- apply(rbind(0,death),2,diff)
  rownames(rlt.scenarios[[i]]$death) <- rownames(trate)
}

#####################################################################################
# Results
#####################################################################################

rownames(trate)[nrow(trate)] <- '++'
rlt <- lapply(rlt.scenarios,function(x){
  pos <- cbind(Total=rowSums(x$pos),x$pos)
  rownames(pos) <- rownames(trate)
  test <- (t(t(x$trate) * raw[1,,4]))
  test <- round(cbind(Total=rowSums(test),test))
  death <- cbind(Total=rowSums(x$death),x$death)
  rlt <- sapply(1:ncol(pos),function(i){
    c(date.max=rownames(trate)[which.max(pos[,i])],
      value2.max=max(pos[,i]),value.max=sum(pos[,i][1:which.max(pos[,i])]),
      test.max=test[which.max(pos[,i]),i],
      date.end=rownames(trate)[which.max(cumsum(pos[,i]))[1]],
      value.end=sum(pos[,i]),test.end=test[which.max(cumsum(pos[,i]))[1],i],
      death.end=sum(death[,i]),drate.end=sum(death[,i])/sum(pos[,i]),
      value.aug=sum(pos[1:223,i]),test.aug=test[223,i],prop.aug=round(sum(pos[,i][1:223])/sum(pos[,i]),2),
      death.aug=sum(death[1:223,i]),drate.aug=sum(death[1:223,i])/sum(pos[1:223,i]),
      value.sep=sum(pos[,i][1:253]),test.sep=test[253,i],prop.sep=round(sum(pos[,i][1:253])/sum(pos[,i]),2),
      death.sep=sum(death[1:253,i]),drate.sep=sum(death[1:253,i])/sum(pos[1:253,i]),
      value.oct=sum(pos[,i][1:284]),test.oct=test[284,i],prop.ocv=round(sum(pos[,i][1:284])/sum(pos[,i]),2),
      death.oct=sum(death[1:284,i]),drate.oct=sum(death[1:284,i])/sum(pos[1:284,i]),
      value.nov=sum(pos[,i][1:314]),test.nov=test[314,i],prop.nov=round(sum(pos[,i][1:314])/sum(pos[,i]),2),
      death.nov=sum(death[1:314,i]),drate.nov=sum(death[1:314,i])/sum(pos[1:314,i]),
      value.dec=sum(pos[,i][1:345]),test.dec=test[345,i],prop.dec=round(sum(pos[,i][1:345])/sum(pos[,i]),2),
      death.dec=sum(death[1:345,i]),drate.dec=sum(death[1:345,i])/sum(pos[1:345,i])
    )
  })
  data.table(state=colnames(pos),t(rlt))
})

for(i in 1:length(rlt)){rlt[[i]] <- cbind(scenario=i,rlt[[i]])}
write.csv(do.call(rbind,rlt),paste0('rlt_y~y+x.csv'))

p <- melt(sapply(rlt.scenarios,function(x){rowSums(x$pos)})) %>% 
  mutate(Var2=ifelse(Var1%in%rownames(raw.pos),'Actual',paste0('Scenarios',Var2)),
         Var1=as.Date(Var1)) %>% unique
ggplot() + geom_line(data=p,aes(x=Var1,y=value,colour=Var2),size=1) + 
  labs(x='Date',y='New Cases',colour='')

p <- melt(sapply(rlt.scenarios,function(x){rowSums(x$pos)})) %>% 
  filter(as.Date(Var1)<=as.Date("2021-06-30")) %>% 
  mutate(Var2=ifelse(Var1%in%rownames(raw.pos),'Actual',paste0('Scenarios',Var2)),
         Var1=as.Date(Var1)) %>% unique
ggplot() + geom_line(data=p,aes(x=Var1,y=value,colour=Var2),size=1) + 
  labs(x='Date',y='New Cases',colour='')

p <- melt(sapply(rlt.scenarios,function(x){cumsum(rowSums(x$death))/cumsum(rowSums(x$pos))})) %>% 
  filter(as.Date(Var1)<=as.Date("2021-06-30")) %>% 
  mutate(Var2=ifelse(Var1%in%rownames(raw.pos),'Actual',paste0('Scenarios',Var2)),
         Var1=as.Date(Var1)) %>% unique
ggplot() + geom_line(data=p,aes(x=Var1,y=value,colour=Var2),size=0.5) + 
  labs(x='Date',y='Fatality rate',colour='') + ylim(c(0,0.15))

p <- melt(sapply(rlt.scenarios,function(x){colSums(t(x$trate)*raw[1,,4])})) %>% 
  filter(as.Date(Var1)<=as.Date("2021-06-30")) %>% 
  mutate(Var2=ifelse(Var1%in%rownames(raw.pos),'Actual',paste0('Scenarios',Var2)),
         Var1=as.Date(Var1)) %>% unique
ggplot() + geom_line(data=p,aes(x=Var1,y=value,colour=Var2),size=0.5) + 
  labs(x='Date',y='Number of tested',colour='')


t(do.call(rbind,lapply(rlt,head,1)))

