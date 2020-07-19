
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

gety <- function(i,train=T,h=8){
  y1 <- (t(Y[i+0:(h-1),])+1)
  f <- ((rowMeans(y1)))
  y1 <- y1/f
  if(train){
    y2 <- (t(Y[i+h,,drop=F])+1)/f
  } else {
    y2 <- NULL
  }
  list(y1=y1,y2=y2,f=f)
}

geta <- function(i,train=T,h=8){
  a1 <- (t(A[i+0:(h-1),]))
  if(train){
    a2 <- t(A[i+h,,drop=F])
  } else {
    a2 <- NULL
  }
  list(a1=a1,a2=a2)
}

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

model.conti <- function(X,Y,epoch=100){
  e.input <- layer_input(shape=ncol(X))
  e.layer <- layer_dense(e.input,16,activation='relu') %>% layer_dropout(rate=0.2)
  l.layer <- layer_dense(e.layer,4,activation='relu') %>% layer_dropout(rate=0.2)
  d.output <- layer_dense(units=ncol(Y))
  model <- keras_model(e.input,d.output(l.layer))
  # model %>% compile(loss = "mean_squared_error", optimizer = "adam",metric='mae')
  model %>% compile(loss = "mae", optimizer = "adam")
  loss <- Inf
  itv <- 0
  system.time(temp <- model %>% fit(x = X,y = Y,batch = 128,epochs = 1000,verbose = 0))
  print(paste(loss,mean(temp$metrics$loss),itv<-itv+1,'first epochs',Sys.time(),sep=', '))
  while(mean(temp$metrics$loss)<loss){2
    loss <- mean(temp$metrics$loss)
    system.time(temp <- model %>% fit(x = X,y = Y,batch = 128,epochs = epoch,verbose = 0))
    print(paste(loss,mean(temp$metrics$loss),itv<-itv+1,'new epoch',Sys.time(),sep=', '))
    if(mean(temp$metrics$loss)>loss){
      system.time(temp <- model %>% fit(x = X,y = Y,batch = 128,epochs = epoch,verbose = 0))
      print(paste(loss,mean(temp$metrics$loss),itv<-itv+1,'2nd epoch',Sys.time(),sep=', '))
    }
    if(mean(temp$metrics$loss)>loss){
      system.time(temp <- model %>% fit(x = X,y = Y,batch = 128,epochs = epoch,verbose = 0))
      print(paste(loss,mean(temp$metrics$loss),itv<-itv+1,'3rd epoch',Sys.time(),sep=', '))
    }
  }
  return(model)
}

model.sigmoid <- function(X,Y,epoch=100){
  e.input <- layer_input(shape=ncol(X))
  e.layer <- layer_dense(e.input,16,activation='relu') %>% layer_dropout(rate=0.2)
  l.layer <- layer_dense(e.layer,4,activation='relu') %>% layer_dropout(rate=0.2)
  d.output <- layer_dense(units=ncol(Y),activation='sigmoid')
  model <- keras_model(e.input,d.output(l.layer))
  # model %>% compile(loss = "mean_squared_error", optimizer = "adam",metric='mae')
  model %>% compile(loss = "mae", optimizer = "adam",metric='mae')
  loss <- Inf
  itv <- 0
  system.time(temp <- model %>% fit(x = X,y = Y,batch = 128,epochs = 1000,verbose = 0))
  print(paste(loss,mean(temp$metrics$loss),itv<-itv+1,'first epochs',Sys.time(),sep=', '))
  while(mean(temp$metrics$loss)<loss){
    loss <- mean(temp$metrics$loss)
    system.time(temp <- model %>% fit(x = X,y = Y,batch = 128,epochs = epoch,verbose = 0))
    print(paste(loss,mean(temp$metrics$loss),itv<-itv+1,'new epoch',Sys.time(),sep=', '))
    if(mean(temp$metrics$loss)>loss){
      system.time(temp <- model %>% fit(x = X,y = Y,batch = 128,epochs = epoch,verbose = 0))
      print(paste(loss,mean(temp$metrics$loss),itv<-itv+1,'2nd epoch',Sys.time(),sep=', '))
    }
    if(mean(temp$metrics$loss)>loss){
      system.time(temp <- model %>% fit(x = X,y = Y,batch = 128,epochs = epoch,verbose = 0))
      print(paste(loss,mean(temp$metrics$loss),itv<-itv+1,'3rd epoch',Sys.time(),sep=', '))
    }
  }
  return(model)
}

ggpline <- function(x,y){
  ggplot() +
    geom_line(data=data.frame(id=1:length(x),
                              x=((x-min(x))/(max(x)-min(x))),
                              y=((y-min(y))/(max(y)-min(y)))) %>% 
                melt(id='id'),
              aes(x=id,y=value,colour=variable),size=1)
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

pos <- t(sapply(1:nrow(pos),function(i){colMeans(rbind(0,0,0,0,0,0,pos)[i:(i+6),])}))
dimnames(pos) <- dimnames(raw.pos)
death <- t(sapply(1:nrow(death),function(i){colMeans(rbind(0,0,0,0,0,0,death)[i:(i+6),])}))
dimnames(death) <- dimnames(raw.death)
trate <- t(sapply(1:nrow(trate),function(i){colMeans(rbind(0,0,0,0,0,0,trate)[i:(i+6),])}))
dimnames(trate) <- dimnames(raw.trate)

plot.ts(rowSums(pos))
plot.ts(rowSums(death))
plot.ts(rowMeans(trate))

ggpline(rowSums(pos),rowSums(death))
ggpline(rowSums(pos),rowSums(trate))

#Model Filing

pos_china <- raw_china[rownames(raw_china)%in%rownames(raw),]
rowSums(pos_china)/cumsum(rowSums(pos_china))

a_china <- sapply(1:ncol(pos_china),function(i){
  1-rowSums(pos_china)/cumsum(rowSums(pos_china))
  #(1:nrow(pos))/which.max(pos_china[1:60,i])/2
})
a_china <- ifelse(a_china>1,1,a_china)
colnames(a_china) <- colnames(pos_china) 

#####################################################################################
# Find A
#####################################################################################

# Y ~ Y + A
Y <- pos_china
A <- a_china
modelfile <- lapply(1:(nrow(Y)-8),getya,train=T,h=8)
X.mf <- do.call(rbind,lapply(modelfile,function(x){cbind(x$y1,rowMeans(x$a1))}))
Y.mf <- do.call(rbind,lapply(modelfile,function(x){cbind(x$y2,rowMeans(x$a2))}))

for(i in 1:10){
  modeli <- model.conti(X.mf,Y.mf[,1,drop=F])
  Y.mf_pred <- sapply((1:20)/20,function(a){
    X.temp <- X.mf
    X.temp[,ncol(X.temp)] <- a
    modeli %>% predict(X.temp)
  })
  print(summary(abs((modeli%>%predict(X.mf)/Y.mf[,1])-1)))
  X.mf[,ncol(X.mf)] <- 
    ((1:20)/20)[apply(abs(Y.mf_pred/Y.mf[,1]-1),1,which.min)] * 0.9 + 
    X.mf[,ncol(X.mf)] * 0.1
  plot.ts(as.numeric(tapply(X.mf[,ncol(X.mf)],rep(1:length(modelfile),each=ncol(pos_china)),mean)))
  print(summary(abs((modeli%>%predict(X.mf)/Y.mf[,1])-1)))
}
model.inita <- lapply(1:5,function(i){model.sigmoid(X.mf,X.mf[,ncol(X.mf),drop=F],100)})

# A = Y
Y <- pos
modelfile <- lapply(1:(nrow(Y)-8),gety,train=T,h=8)
X.mfus <- do.call(rbind,lapply(modelfile,function(x){cbind(x$y1,x$y2)}))
Y.mfus <- sapply(model.inita,function(x){x%>%predict(X.mfus)})%>%rowMeans
Y.mfus <- Y.mfus/2
plot.ts(tapply(Y.mfus,rep(1:(nrow(Y)-8),each=ncol(Y)),mean)%>%as.numeric)

A_us <- Y.mfus
A_us <- apply(matrix(A_us,nrow=ncol(Y)),1,function(x){
  sapply(1:length(x),function(i){
    xi <- rep(NA,nrow(Y))
    xi[0:7+i] <- x[i]
    xi
  }) %>% rowMeans(na.rm=T)
})
A_us[nrow(A_us),] <- A_us[nrow(A_us)-1,]
dimnames(A_us) <- dimnames(Y)
A_us <- A_us
plot.ts(rowMeans(A_us))

# Y ~ Y + A
par(mfrow=c(1,2))
Y <- cbind(pos,pos_china)
A <- cbind(A_us,a_china)
modelfile <- lapply(1:(nrow(Y)-8),getya,train=T,h=8)
X.mf <- do.call(rbind,lapply(modelfile,function(x){cbind(x$y1,rowMeans(x$a1))}))
Y.mf <- do.call(rbind,lapply(modelfile,function(x){cbind(x$y2,rowMeans(x$a2))}))
for(i in 1:10){
  modeli <- model.conti(X.mf,Y.mf[,1,drop=F])
  Y.mf_pred <- sapply((1:20)/20,function(a){
    X.temp <- X.mf
    X.temp[,ncol(X.temp)] <- a
    modeli %>% predict(X.temp)
  })
  print(summary(abs((modeli%>%predict(X.mf)/Y.mf[,1])-1)))
  X.mf2 <- ((1:20)/20)[apply(abs(Y.mf_pred/Y.mf[,1]-1),1,which.min)]
  X.mf2 <- ifelse(rownames(X.mf)%in%colnames(raw.pos),
         X.mf2*0.5+X.mf[,ncol(X.mf)]*0.5,X.mf2*0.8+X.mf[,ncol(X.mf)]*0.2)
  temp <- X.mf[rownames(X.mf)%in%colnames(raw.pos),ncol(X.mf)]
  plot.ts(as.numeric(tapply(temp,rep(1:length(modelfile),each=ncol(raw.pos)),mean)))
  temp <- X.mf[rownames(X.mf)%in%colnames(raw_china),ncol(X.mf)]
  plot.ts(as.numeric(tapply(temp,rep(1:length(modelfile),each=ncol(raw_china)),mean)))
  print(summary(abs((modeli%>%predict(X.mf)/Y.mf[,1])-1)))
}

# A

temp <- X.mf[rownames(X.mf)%in%colnames(raw_china),ncol(X.mf)]
temp <- apply(matrix(temp,nrow=ncol(raw_china)),1,function(x){
  sapply(1:length(x),function(i){
    xi <- rep(NA,nrow(Y))
    xi[0:7+i] <- x[i]
    xi
  }) %>% rowMeans(na.rm=T)
})
temp[nrow(temp),] <- temp[nrow(temp)-1,]
dimnames(temp) <- dimnames(raw_china)
A_china <- temp

temp <- X.mf[rownames(X.mf)%in%colnames(raw.pos),ncol(X.mf)]
temp <- apply(matrix(temp,nrow=ncol(raw.pos)),1,function(x){
  sapply(1:length(x),function(i){
    xi <- rep(NA,nrow(Y))
    xi[0:7+i] <- x[i]
    xi
  }) %>% rowMeans(na.rm=T)
})
temp[nrow(temp),] <- temp[nrow(temp)-1,]
dimnames(temp) <- dimnames(raw.pos)
A_us <- temp

p <- data.frame(date=rownames(A_china),CHINA=rowMeans(A_china),US=rowMeans(A_us)) %>% melt(id='date')
ggplot() + geom_line(data=p,aes(x=as.Date(date),y=value,colour=variable),size=1) + 
  labs(x='Date',y='A(t)',colour='Country')
ggpline(rowMeans(A_china),rowMeans(A_us))

#####################################################################################
# Go for US
#####################################################################################

# Refresh A

Y <- pos
A <- A_us
modelfile <- lapply(1:(nrow(Y)-8),getya,train=T,h=8)
X.y <- do.call(rbind,lapply(modelfile,function(x){x$y1}))
X.a <- cbind(rowMeans(do.call(rbind,lapply(modelfile,function(x){x$a1}))))
Y.y <- do.call(rbind,lapply(modelfile,function(x){x$y2}))
Y.a <- do.call(rbind,lapply(modelfile,function(x){x$a2}))
for(i in 1:10){
  modely <- model.conti(cbind(X.y,rowMeans(X.a)),Y.y,100)
  modely.asel <- sapply((1:20)/20,function(a){modeli %>% predict(cbind(X.y,a))})
  print(summary(abs((modely%>%predict(cbind(X.y,rowMeans(X.a)))/Y.y)-1)))
  X.a <- X.a*0.8+((1:20)/20)[apply(abs(modely.asel/as.numeric(Y.y)-1),1,which.min)]*0.2
  plot.ts(as.numeric(tapply(X.a,rep(1:length(modelfile),each=ncol(Y)),mean)))
  print(summary(abs((modely%>%predict(cbind(X.y,rowMeans(X.a)))/Y.y)-1)))
}
temp <- apply(matrix(X.a,nrow=ncol(raw.pos)),1,function(x){
  sapply(1:length(x),function(i){
    xi <- rep(NA,nrow(Y))
    xi[0:7+i] <- x[i]
    xi
  }) %>% rowMeans(na.rm=T)
})
temp[nrow(temp),] <- temp[nrow(temp)-1,]
dimnames(temp) <- dimnames(raw.pos)
A_us <- temp

# Y ~ Y + A

# Y <- pos
# A <- A_us
Y <- cbind(pos_china)
A <- cbind(A_china)
modelfile <- lapply(1:(nrow(Y)-8),getya,train=T,h=8)
X.y <- do.call(rbind,lapply(modelfile,function(x){x$y1}))
X.a <- do.call(rbind,lapply(modelfile,function(x){x$a1}))
Y.y <- do.call(rbind,lapply(modelfile,function(x){x$y2}))
Y.a <- do.call(rbind,lapply(modelfile,function(x){x$a2}))
modely <- lapply(1:5,function(i){
  print(i)
  model.conti(cbind(X.y,rowMeans(X.a)),Y.y,100)
})
modela <- lapply(1:5,function(i){
  print(i)
  model.sigmoid(cbind(X.a,rowMeans(X.y)),Y.a,100)
})

#####################################################################################
# Prediction
#####################################################################################

ggpline(rowMeans(pos),rowMeans(A_us))
ggpline(rowMeans(A_us),rowMeans(A_china))
ggplot()+
  geom_line(aes(x=as.Date('2020-01-21')+1:nrow(pos),
                y=rowSums(A_us*apply(pos,2,cumsum))/rowSums(apply(pos,2,cumsum))))+
  labs(x='date',y='A')

Y <- pos
A <- A_us
for(i in 1:(365-nrow(pos))){
  temp <- getya(nrow(Y)-7,F)
  Y2 <- (sapply(modely,function(modeli){
    modeli %>% predict(cbind(temp$y1,rowMeans(temp$a1)))
  }) %>% rowMeans) * temp$f %>% floor
  A2 <- sapply(modela,function(modeli){
    modeli %>% predict(cbind(temp$a1,rowMeans(temp$y1)))
  }) %>% rowMeans
  Y <- rbind(Y,Y2)
  A <- rbind(A,A2)
}
Y[1:nrow(raw.pos),] <- raw.pos
for(j in (nrow(pos)+1):(nrow(Y))){Y[j,] <- Y[j,]*7 - colSums(Y[j-1:6,])}

ggplot(data=
         data.frame(
           date=as.Date('2020-01-21')+1:365,Y=rowSums(Y),
           A=rowSums(A*apply(Y,2,cumsum))/rowSums(apply(Y,2,cumsum))))+
  geom_line(aes(x=date,y=Y,colour='Y'),size=1) +
  geom_line(aes(x=date,y=A/max(A)*max(Y),colour='A'),size=1) +
  labs(colour='Legend',x='Date')

# go to 0.8
Y <- pos
A <- A_us
A_a <- 0.05
A_max <- 1
for(i in 1:(365-nrow(pos))){
  temp <- getya(nrow(Y)-7,F)
  Y2 <- (sapply(modely,function(modeli){
    modeli %>% predict(cbind(temp$y1,rowMeans(temp$a1)))
  }) %>% rowMeans) * temp$f %>% floor
  A2 <- A[nrow(A),]+A_a
  A2 <- ifelse(A2>A_max,A_max,A2)
  Y <- rbind(Y,Y2)
  A <- rbind(A,A2)
}
Y[1:nrow(raw.pos),] <- raw.pos
for(j in (nrow(pos)+1):(nrow(Y))){Y[j,] <- Y[j,]*7 - colSums(Y[j-1:6,])}

ggplot(data=
         data.frame(
           date=as.Date('2020-01-21')+1:365,Y=rowSums(Y),
           A=rowSums(A*apply(Y,2,cumsum))/rowSums(apply(Y,2,cumsum))))+
  geom_line(aes(x=date,y=Y,colour='Y'),size=1) +
  geom_line(aes(x=date,y=A/max(A)*max(Y),colour='A'),size=1) +
  labs(colour='Legend',x='Date')



