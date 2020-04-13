
###################################
# Module
###################################

rm(list=ls())
library(data.table)
library(dplyr)
library(keras)

#Get data from github
setwd('/Users/wenrurumon/Documents/posdoc/wuhan/data/github')
raw <- lapply(c('china.csv','global.csv','us.csv'),function(x){
  key <- x
  x1 <- fread(x)
  for(i in (nrow(x1)-1):1){
    x1[i,] <- ifelse(x1[i,]>x1[i+1,],x1[i+1,],x1[i,])
  }
  x2 <- data.table(V1=x1$V1,apply(rbind(0,as.matrix(x1[,-1])),2,diff))
  x <- cbind(melt(x1,id='V1'),melt(x2,id='V1')$value)
  colnames(x) <- c('date','state','accum','new')
  data.frame(scope=gsub('.csv','',key),fact='confirmed',x)
})
raw <- do.call(rbind,raw)
raw <- 
  merge(raw,(raw %>% group_by(scope,state,fact) %>% 
               summarise(peak=max(new))),by=c('scope','state','fact')) %>% 
  filter(new==peak) %>% group_by(scope,state,fact) %>% summarise(peak=min(date)) %>% 
  merge(raw,by=c('scope','state','fact')) %>% mutate(d=ifelse(date>peak&(scope=='china'|state=='China'),1,0)) %>%
  mutate(key=paste(scope,fact,state),i=as.numeric((as.POSIXct(date)-as.POSIXct(min(raw$date)))/3600/24)+1) %>%
  mutate(gap=1/(as.numeric(paste(as.POSIXct(peak)-as.POSIXct(min(raw$date))+as.POSIXct(peak)-as.POSIXct(date)))/3600/24+1))
raw <- raw %>% mutate(d=ifelse(gap>0&gap!=Inf,gap,1)) %>% 
  select(-peak,-gap)%>% mutate(d=1/(1/d+i-1)*i)

#Module
getlog <- function(I,keyI,log,p=8){
  logi <- filter(log,key==keyI&i%in%(0:p+I))
  x <- logi[1:p,]
  y <- logi[-1:-p,]
  w <- y$i
  list(x=x,y=y,w=w)
}

###################################
# China Model
###################################

# log <- filter(raw,state%in%c('KoreaSouth','Japan','Singapore','China')|scope=='china')
log <- filter(raw,state%in%c('China')|scope=='china')
mfile <- do.call(c,lapply(unique(log$key),function(K){
  lapply(1:(max(log$i)-8),function(I){getlog(I,K,log,8)})
}))
X <- (t(sapply(mfile,function(x){c(x$x$new/(mean(x$x$new)+1))})))
Y <- cbind(sapply(mfile,function(x){x$y$new/mean(x$x$new+1)}))

e.input <- layer_input(shape=ncol(X))
e.layer <- layer_dense(e.input,16,activation='relu')
l.layer <- layer_dense(e.layer,4,activation='relu')
d.output <- layer_dense(units=ncol(Y),activation='relu')
model <- keras_model(e.input,d.output(l.layer))
model %>% compile(loss = "mean_squared_error", optimizer = "adam",metrics = c('mae'))

system.time(model %>% fit(x = X,y = Y,batch = 128,epochs = 2000,verbose = 0))
model %>% fit(x = X,y = Y,batch = 128,epochs = 100,verbose = 2)

model.china <- model

###################################
# Global Model
###################################

# log <- filter(raw,(!state%in%c('KoreaSouth','Japan','Singapore','China'))&scope=='global')
log <- filter(raw,(!state%in%c('China'))&scope=='global')
mfile <- do.call(c,lapply(unique(log$key),function(K){
  lapply(1:(max(log$i)-8),function(I){getlog(I,K,log,8)})
}))
X <- (t(sapply(mfile,function(x){c(x$x$new/(mean(x$x$new)+1))})))
Y <- cbind(sapply(mfile,function(x){x$y$new/mean(x$x$new+1)}))

e.input <- layer_input(shape=ncol(X))
e.layer <- layer_dense(e.input,16,activation='relu')
l.layer <- layer_dense(e.layer,4,activation='relu')
d.output <- layer_dense(units=ncol(Y),activation='relu')
model <- keras_model(e.input,d.output(l.layer))
model %>% compile(loss = "mean_squared_error", optimizer = "adam",metrics = c('mae'))

system.time(model %>% fit(x = X,y = Y,batch = 128,epochs = 2000,verbose = 0))
model %>% fit(x = X,y = Y,batch = 128,epochs = 100,verbose = 2)

model.global <- model

###################################
# Mix Model
###################################

log <- raw
mfile <- do.call(c,lapply(unique(log$key),function(K){
  lapply(1:(max(log$i)-8),function(I){getlog(I,K,log,8)})
}))
mfile.value <- sapply(mfile,function(x){
  c(x=matrix(x$x$new,nrow=1),y=x$y$new)
}) %>% t
mfile.key <- sapply(mfile,function(x){x$y$key})
mfile <- data.frame(key=mfile.key,mfile.value)
mfile <- data.frame(mfile,y.china = (model.china %>% predict(as.matrix(mfile[,2:9]/(rowMeans(mfile[,2:9]+1)))))*rowMeans(mfile[,2:9]+1)*sign(rowMeans(mfile[,2:9])),y.global=(model.global %>% predict(as.matrix(mfile[,2:9]/(rowMeans(mfile[,2:9]+1)))))*rowMeans(mfile[,2:9]+1)*sign(rowMeans(mfile[,2:9])))

test <- sapply(unique(mfile.key),function(x){
  db <- filter(mfile,key==x)
  y.china <- model.china %>% predict(as.matrix((db[,2:9])/rowMeans(db[,2:9]+1))) * 
    rowMeans(db[,2:9]+1)
  y.global <- model.global %>% predict(as.matrix((db[,2:9])/rowMeans(db[,2:9]+1))) *
    rowMeans(db[,2:9]+1)
  as.numeric(coef(lm(db$y~y.china-1))/(coef(lm(db$y~y.china-1))+coef(lm(db$y~y.global-1))))
})
test <- data.frame(key=unique(mfile.key),coef=test) %>% filter(grepl('global',key)) %>% arrange(desc(coef))


temp <- temp0 <- filter(raw,key=='global confirmed Italy')
for(i in 1:365){
  tempi <- filter(temp,i>(max(temp$i)-8))
  if(mean(tempi$new)==0){
    tempi.x <- 0
  } else {
    tempi.x <- t(matrix(tempi$new/mean(tempi$new+1)))
    temp1 <- (model.global %>% predict(tempi.x))*mean(tempi$new+1)
    tempi.x <- t(matrix(tempi$new/mean(tempi$new+1)))
    temp2 <- (model.china %>% predict(tempi.x))*mean(tempi$new+1)
    w <- filter(test,key==temp$key[1])$coef
    tempi.x = floor(temp1*(1-w)+temp2*w)
  }
  tempi <- tempi[8,] %>% mutate(d=NA,new=tempi.x,accum=accum+tempi.x,
                       i=i+1,date=paste(as.POSIXct(date)+3600*24))
  temp <- rbind(temp,tempi)
}
plot.ts(temp$new)
lines(temp0$new,col=2)

