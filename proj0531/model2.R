
rm(list=ls())
setwd("/Users/wenrurumon/Documents/posdoc/wuhan/data/wtest")
library(data.table)
library(dplyr)
library(ggplot2)
library(reshape2)
library(keras)
library(forecast)

#####################################################################################
# Rawdata
#####################################################################################

#Import

raw_china <- read.csv('/Users/wenrurumon/Documents/posdoc/wuhan/data/github/china.csv',row.names=1)
colnames(raw_china) <- tolower(colnames(raw_china))
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

for(i in 2:nrow(raw)){
  raw[i,,1] <- ifelse(raw[i,,1]<raw[i-1,,1],raw[i-1,,1],raw[i,,1])
  raw[i,,2] <- ifelse(raw[i,,2]<raw[i-1,,2],raw[i-1,,2],raw[i,,2])
  raw[i,,3] <- ifelse(raw[i,,3]<raw[i-1,,3],raw[i-1,,3],raw[i,,3])
}
for(i in 1:ncol(raw)){raw[,i,4] <- max(raw[,i,4])}

pos <- raw.pos <- apply(rbind(0,raw[,,1]),2,diff) 
death <- raw.death <- apply(rbind(0,raw[,,2]),2,diff)
trate <- raw.trate <- raw[,,3]/raw[,,4]

#Model Filing

pos_china <- raw_china[rownames(raw_china)%in%rownames(raw),]
trate_china <- sapply(1:ncol(pos_china),function(i){
  (1:nrow(pos))/which.max(pos_china[1:60,i])/2
})
trate_china <- ifelse(trate_china>1,1,trate_china)

#####################################################################################
# Prepare for POS model
#####################################################################################

getpos <- function(i,train=T,h=8){
  x.pos <- t(pos[i+0:(h-1),])
  f.pos <- (rowMeans(x.pos))+1
  x.pos <- x.pos/f.pos
  x.trate <- t(trate[i+0:(h-1),])
  if(train){
    y.trate <- t(trate[i+h,,drop=F])
    y.pos <- t(pos[i+h,,drop=F])/f.pos
    y.a <- cbind(pos[i+h,]/(apply(pos,2,cumsum)[i+(h-1),]+1))
  } else {
    y.trate <- NULL
    y.pos <- NULL
    y.a <- NULL
  }
  list(x.pos=x.pos,x.trate=x.trate,
       y.pos=y.pos,y.trate=y.trate,
       f.pos=f.pos,y.a=y.a)
}

getx <- function(i,train=T,h=8){
  x.pos <- t(pos[i+0:(h-1),])
  x.death <- t(death[i+0:(h-1),])
  f.pos <- (rowMeans(x.pos))+1
  f.death <- (rowMeans(x.death))+1
  x.pos <- x.pos/f.pos
  x.death <- x.death/f.death
  x.trate <- t(trate[i+0:(h-1),])
  if(train){
    y.trate <- t(trate[i+h,,drop=F])
    y.pos <- t(pos[i+h,,drop=F])/f.pos
    y.death <- t(death[i+h,,drop=F])/f.death
    y.a <- cbind(pos[i+h,]/(apply(pos,2,cumsum)[i+(h-1),]+1))
  } else {
    y.trate <- NULL
    y.pos <- NULL
    y.death <- NULL
    y.a <- NULL
  }
  list(x.pos=x.pos,x.trate=x.trate,x.death=x.death,
       y.pos=y.pos,y.trate=y.trate,y.death=y.death,
       f.pos=f.pos,f.death=f.death,y.a=y.a)
}

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
  return(list(model=model,history=temp))
}

#####################################################################################
# Test model
#####################################################################################

#Data Prepare
data4train <- lapply(1:(nrow(pos)-3),getpos,train=T,h=3)
X <- do.call(rbind,lapply(data4train,function(x){cbind(trate=(x$x.trate))}))
Y <- do.call(rbind,lapply(data4train,function(x){cbind(trate=(x$y.trate))}))

#Modeling
summary(model_trate <- lm(Y~X-1))

#Validate
X <- lapply(lapply(1:(nrow(pos)-3),getpos,train=T,h=3),function(x){x$x.trate})
Y <- lapply(lapply(1:(nrow(pos)-3),getpos,train=T,h=3),function(x){x$y.trate * raw[1,,4]})
Y.pred <- lapply(X,function(x){x %*% coef(model_trate) * raw[1,,4]})

#Prediction
rlt <- list()
for(i in 1:4){
  trate <- raw.trate
  pos <- raw.pos
  for(j in 1:(365-nrow(pos))){
    trate2 <- as.numeric(getpos((nrow(trate)-2),train=F,h=3)$x.trate %*% coef(model_trate))
    trate2 <- apply(trate,2,max) + (trate2 - apply(trate,2,max)) * c(0.25,0.5,1,2)[i]
    trate <- rbind(trate,trate2)
    pos <- rbind(pos,0)
    trate <- ifelse(trate>1,1,trate)
  }
  rlt[[i]] <- t(trate)*raw[1,,4]
}
rlt <- sapply(rlt,colSums)/sum(raw[1,,4])
colnames(rlt) <- paste('Scenario',1:4)
rlt <- data.frame(date=(raw.date[1] + (1:nrow(rlt)) - 1),rlt) %>% melt(id='date')

#####################################################################################
# Death Rate Model
#####################################################################################

#Data Prepare

pos <- apply(raw.pos,2,cumsum)
death <- apply(raw.death,2,cumsum)
trate <- death/pos
death_rate <- colMeans(tail(trate))

#####################################################################################
# Confirmed
#####################################################################################

#Data Prepare
trate <- cbind(raw.trate,trate_china)
pos <- cbind(raw.pos,pos_china)
data4train <- lapply(1:(nrow(pos)-8),getpos,train=T)
data4train <- rep(data4train,ceiling((1:length(data4train))/length(data4train)))
raw.weekday <- rep(raw.weekday[-1:-8],ceiling((1:length(data4train))/length(data4train)))

x.pos <- do.call(rbind,lapply(data4train,function(x){x$x.pos}))
y.pos <- do.call(rbind,lapply(data4train,function(x){x$y.pos}))
x.trate <- do.call(rbind,lapply(data4train,function(x){cbind(trate=rowMeans(x$x.trate))}))
f.pos <- do.call(rbind,lapply(data4train,function(x){cbind(x$f.pos)}))
x.wd <- rep(raw.weekday,each=ncol(pos))
X <- cbind(x.pos,x.trate,weekday=outer(x.wd,1:7,'==')+0)
Y <- cbind(y.pos)
Y <- Y[rowSums(X[,1:8])>0,,drop=F]
X <- X[rowSums(X[,1:8])>0,,drop=F]

#Modeling

# model_positive <- lapply(1:5,function(i){print(i);model(X,Y)})
# for(i in 1:length(model_positive)){
#   save_model_hdf5(model_positive[[i]]$model,
#                          paste0('/Users/wenrurumon/Documents/posdoc/wuhan/model/positive_',i,'.model'))
# }
model_positive <- lapply(1:5,function(i){
  x <- load_model_hdf5(paste0('/Users/wenrurumon/Documents/posdoc/wuhan/model/positive_',i,'.model'))
  list(model=x)
})

#Validation

raw.date <- as.Date(rownames(raw))
raw.weekday <- weekdays(as.Date(rownames(raw)))
raw.weekday <- match(raw.weekday,c(unique(raw.weekday)[-1:-5],unique(raw.weekday)[1:5]))
pos <- raw.pos
trate <- raw.trate

X <- lapply(lapply(1:(nrow(pos)-8),getpos,train=T),function(x){
  list(x=cbind(x$x.pos,rowMeans(x$x.trate),
               wy=matrix((rep(1:7,each=51)==raw.weekday[which(raw.date==colnames(x$y.pos))]),nrow=51)+0
  ),
  f1=x$f.pos)
})
Y <- lapply(lapply(1:(nrow(pos)-8),getpos,train=T),function(x){
  list(y=cbind(x$y.pos),f1=x$f.pos)
})
pos.pred <- lapply(model_positive,function(model){
  t(sapply(X,function(x){(model$model %>% predict(x$x)) * x$f1 %>% floor}))
})
pos.pred <- sapply(pos.pred,rowSums)
pos.actual <- t(sapply(Y,function(x){x$y*x$f1})) %>% rowSums
rownames(pos.pred) <- paste(raw.date[-1:-8])
colnames(pos.pred) <- paste0('fit',1:length(model_positive))
pos.pred <- data.frame(date=as.Date(rownames(pos.pred)),
                       fit=rowMeans(pos.pred),actual=pos.actual) %>% melt(id='date')
ggplot() + geom_line(data=pos.pred,aes(x=date,y=value,colour=variable),size=1) + labs(
  x='Date',y='New Cases',colour='Legend'
)

#Prediction

rlt <- list()
for(j in 1:4){
  pos <- raw.pos
  trate <- raw.trate
  raw.date <- as.Date(rownames(raw))
  raw.weekday <- weekdays(as.Date(rownames(raw)))
  raw.weekday <- match(raw.weekday,c(unique(raw.weekday)[-1:-5],unique(raw.weekday)[1:5]))
  Xnew <- getpos(nrow(pos)-7,train=F)
  raw.date <- c(raw.date,max(raw.date)+1)
  raw.weekday <- c(raw.weekday,
                   ifelse(raw.weekday[length(raw.weekday)]+1>7,1,raw.weekday[length(raw.weekday)]+1))
  pos <- rbind(pos,rowMeans(sapply(model_positive,function(model){
    model$model %>% predict(cbind(Xnew$x.pos,rowMeans(Xnew$x.trate)
                                  ,matrix(rep((1:7)==raw.weekday[length(raw.weekday)],each=51),ncol=7)+0))
  }) * Xnew$f.pos)) %>% floor
  pos <- ifelse(pos<0,0,pos)
  trate2 <- as.numeric(getpos((nrow(trate)-2),train=F,h=3)$x.trate %*% coef(model_trate))
  trate2 <- apply(trate,2,max) + (trate2 - apply(trate,2,max)) * c(0.25,0.5,1,2)[j]
  trate <- rbind(trate,trate2)
  trate <- ifelse(trate>.5,.5,trate)
  for(i in 1:(500)){
    Xnew <- getpos(nrow(pos)-7,train=F)  
    raw.date <- c(raw.date,max(raw.date)+1)
    raw.weekday <- c(raw.weekday,
                     ifelse(raw.weekday[length(raw.weekday)]+1>7,1,raw.weekday[length(raw.weekday)]+1))
    posnew <- rowMeans(sapply(model_positive,function(model){
      model$model %>% predict(cbind(Xnew$x.pos,rowMeans(Xnew$x.trate)
                                    ,matrix(rep((1:7)==raw.weekday[length(raw.weekday)],each=51),ncol=7)+0))
    }) * Xnew$f.pos ) %>% floor
    posnew <- ifelse(posnew==pos[nrow(pos),],posnew-1,posnew)
    pos <- rbind(pos,posnew)
    pos <- ifelse(pos<0,0,pos)
    trate2 <- as.numeric(getpos((nrow(trate)-2),train=F,h=3)$x.trate %*% coef(model_trate))
    trate2 <- apply(trate,2,max) + (trate2 - apply(trate,2,max)) * c(0.25,0.5,1,2)[j]
    trate <- rbind(trate,trate2)
    trate <- ifelse(trate>.5,.5,trate)
  }
  rownames(trate) <- rownames(pos) <- paste(raw.date)
  rlt[[j]] <- list(pos=pos,trate=trate)
}
rlt.pos <- sapply(rlt,function(x){rowSums(x$pos)}) 
colnames(rlt.pos) <- paste('Scenario',1:4)
rlt.pos <- melt(rlt.pos)
rlt.trate <- sapply(rlt,function(x){colSums(t(x$trate)*raw[1,,4])/sum(raw[1,,4])})
colnames(rlt.trate) <- paste('Scenario',1:4)
rlt.trate <- melt(rlt.trate)

rlt.pos <- unique(mutate(rlt.pos,Var2=ifelse(Var1%in%rownames(raw.pos),'Actual',paste(Var2))))
rlt.trate <- unique(mutate(rlt.trate,Var2=ifelse(Var1%in%rownames(raw.pos),'Actual',paste(Var2))))

ggplot() + geom_line(data=rlt.pos,aes(x=as.Date(Var1),y=value,colour=Var2),size=1) + labs(
  x='Date',y='New Cases',colour='Legend'
)
ggplot() + geom_line(data=rlt.trate,aes(x=as.Date(Var1),y=value,colour=Var2),size=1) + labs(
  x='Date',y='New Cases',colour='Legend'
)

#Summary
rlt.pos <- acast(rlt.pos,Var1~Var2)
rlt.pos[is.na(rlt.pos[,2]),-1] <- rlt.pos[is.na(rlt.pos[,2]),1]
rlt.pos <- rlt.pos[,-1]
rownames(rlt.pos)[apply(rlt.pos,2,which.max)]
rownames(rlt.pos)[apply(apply(rlt.pos,2,cumsum),2,which.max)]
apply(rlt.pos,2,max)
apply(rlt.pos,2,sum)

#Plot
posi <- raw.pos %>% melt
posi <- posi %>% group_by(Var2) %>% summarise(value=sum(value)/sum(posi$value)) %>% arrange(desc(value))
posi <- paste(posi[1:which(cumsum(posi$value)>0.7)[1],]$Var2)

rlt2 <- lapply(rlt,function(x){
  x <- x$pos[1:365,match(posi,colnames(x$pos))]
  x <- apply(x,2,cumsum) %>% melt
  x[,1] <- as.Date(paste(x[,1]))
  x
})

grid::grid.newpage() 
grid::pushViewport(grid::viewport(layout = grid::grid.layout(2,2))) 
vplayout <- function(x,y){grid::viewport(layout.pos.row = x, layout.pos.col = y)} 
p <- ggplot() + geom_line(data=rlt2[[1]],aes(x=Var1,y=value/1000,colour=Var2),size=1) + 
  labs(x='Date',y='Accumulated Cases (K)',colour='State',title='Scenario 1') +
  theme(legend.position='top')
print(p,vp = vplayout(1,1))
p <- ggplot() + geom_line(data=rlt2[[2]],aes(x=Var1,y=value/1000,colour=Var2),size=1) + 
  labs(x='Date',y='Accumulated Cases (K)',colour='State',title='Scenario 2') +
  theme(legend.position='top')
print(p,vp = vplayout(1,2))
p <- ggplot() + geom_line(data=rlt2[[3]],aes(x=Var1,y=value/1000,colour=Var2),size=1) + 
  labs(x='Date',y='Accumulated Cases (K)',colour='State',title='Scenario 3') +
  theme(legend.position='top')
print(p,vp = vplayout(2,1))
p <- ggplot() + geom_line(data=rlt2[[4]],aes(x=Var1,y=value/1000,colour=Var2),size=1) + 
  labs(x='Date',y='Accumulated Cases (K)',colour='State',title='Scenario 4') +
  theme(legend.position='top')
print(p,vp = vplayout(2,2))

#Dead

pos <- raw.pos
death <- raw.death
drate <- apply(death,2,cumsum)/apply(pos,2,cumsum)
drate <- colMeans(tail(drate))

#####################################################################################
# Death
#####################################################################################

rlt2 <- lapply(rlt,function(x){
  x <- data.frame(
    state=c('TotalUS',colnames(x$pos)),
    date.now=max(rownames(raw.pos)),
    date.max=raw.date[apply(cbind(rowSums(x$pos),x$pos),2,which.max)],
    date.end=raw.date[apply(apply(cbind(rowSums(x$pos),x$pos),2,cumsum),2,which.max)],
    pos.now=apply(cbind(rowSums(raw.pos),raw.pos),2,sum),
    pos.max=apply(cbind(rowSums(x$pos),x$pos),2,max),
    pos.end=apply(cbind(rowSums(x$pos),x$pos),2,sum),
    dead.now=apply(cbind(rowSums(raw.death),raw.death),2,sum),
    dead.end=floor(apply(cbind(rowSums(x$pos),x$pos),2,sum)*c(NA,drate))
  )
  x$dead.end[1] <- sum(x$dead.end,na.rm=T)
  x
})
for(i in 1:4){rlt2[[i]] <- data.frame(scenario=i,rlt2[[i]])}
write.csv(do.call(rbind,rlt2),"/Users/wenrurumon/Documents/posdoc/wuhan/summary/YA.csv")
names(rlt2) <- paste0('Scenario',1:4)
filter(do.call(rbind,rlt2),state=='TotalUS')

rlt2 <- data.frame(state=rlt2[[1]]$state[-1],
                   sapply(rlt2,function(x){x$pos.end[-1]}),
                   Actual=rlt2[[1]]$pos.now[-1]) %>% mutate(
                     Scenario1 = Scenario1-Scenario2,
                     Scenario2 = Scenario2-Scenario3,
                     Scenario3 = Scenario3-Scenario4,
                     Scenario4 = Scenario4-Actual
                   ) %>% melt
rlt2$variable=factor(rlt2$variable,paste(unique(rlt2$variable))[5:1])

ggplot() +
  geom_bar(data=rlt2,aes(x=state,y=value,fill=variable),stat='identity',
           position = position_stack(reverse = TRUE)) + 
  # theme(legend.position='top') +
  labs(x='State',y='Accumulated Cases',fill='')
