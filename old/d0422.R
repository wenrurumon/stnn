
###################################
# Module
###################################

rm(list=ls())
library(data.table)
library(dplyr)
library(keras)
library(forecast)

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
raw <- raw %>% mutate(d=ifelse(state%in%'China'|scope=='china',1,d))

#Module
getlog <- function(I,keyI,log,p=8){
  logi <- filter(log,key==keyI&i%in%(0:p+I)) %>% arrange(i)
  x <- logi[1:p,]
  y <- logi[-1:-p,]
  list(x=x,y=y)
}
max2 <- function(x){
  x1 <- which(x>0)
  if(length(x1)==0){
    0
  } else{
    min(length(x),max(x1))
  }
}
getdate <- function(x){
  paste(as.POSIXct('2020-01-22')+3600*(x-1)*24)
}

###################################
# D Model (X9 model)
###################################

#D with accumulated case

log <- filter(raw,fact=='confirmed') 

mfile <- do.call(c,lapply(unique(log$key),function(K){
  lapply(1:(max(log$i)-8),function(I){getlog(I,K,log,8)})
}))
# X <- (t(sapply(mfile,function(x){c(c(x$x$accum,x$y$accum)/(mean(x$x$accum)+1),x$y$i)})))
X <- (t(sapply(mfile,function(x){c(c(x$x$accum,x$y$accum)/(mean(x$x$accum)+1))})))
w <- (cbind(sapply(mfile,function(x){mean(x$x$accum)+1})))
Y <- cbind(sapply(mfile,function(x){x$x$d[8]}))

e.input <- layer_input(shape=ncol(X))
e.layer <- layer_dense(e.input,16,activation='relu')
l.layer <- layer_dense(e.layer,4,activation='relu')
d.output <- layer_dense(units=ncol(Y),activation='sigmoid')
model <- keras_model(e.input,d.output(l.layer))
model %>% compile(loss = "mean_squared_error", optimizer = "adam",metrics = c('mae'))

system.time(model %>% fit(x = X,y = Y,batch = 128,epochs = 2000,verbose = 0))
model %>% fit(x = X,y = Y,batch = 128,epochs = 100,verbose = 2)
D <- model %>% predict(X)

for(i in 1:length(D)){
  if(mfile[[i]]$x$i[1]==1){
    mfile[[i]]$x$d <- D[i]
  } else {
    mfile[[i]]$x <- NULL
  }
  mfile[[i]]$y$d <- D[i]
  mfile[[i]] <- do.call(rbind,mfile[[i]])
}
raw <- do.call(rbind,mfile)

raw %>% filter(scope=='global') %>% group_by(state) %>% summarise(d=mean(d)) %>% arrange(desc(d))
raw %>% filter(scope=='global') %>% group_by(state) %>% summarise(d=mean(d)) %>% filter(
  state%in%c('China','UnitedKingdom','US')
)

#D Model

log <- filter(raw,fact=='confirmed') 

mfile <- do.call(c,lapply(unique(log$key),function(K){
  lapply(1:(max(log$i)-8),function(I){getlog(I,K,log,8)})
}))
X <- (t(sapply(mfile,function(x){c(c(x$x$accum)/(mean(x$x$accum)+1),mean(x$x$d))})))
w <- (cbind(sapply(mfile,function(x){mean(x$x$accum)+1})))
Y <- cbind(sapply(mfile,function(x){x$x$d[8]}))

e.input <- layer_input(shape=ncol(X))
e.layer <- layer_dense(e.input,16,activation='relu')
l.layer <- layer_dense(e.layer,4,activation='relu')
d.output <- layer_dense(units=ncol(Y),activation='sigmoid')
model <- keras_model(e.input,d.output(l.layer))
model %>% compile(loss = "mean_squared_error", optimizer = "adam",metrics = c('mae'))

system.time(model %>% fit(x = X,y = Y,batch = 128,epochs = 1000,verbose = 0))
model %>% fit(x = X,y = Y,batch = 128,epochs = 100,verbose = 2)

D <- model %>% predict(X)
for(i in 1:length(D)){
  if(mfile[[i]]$x$i[1]==1){
    mfile[[i]]$x$d <- D[i]
  } else {
    mfile[[i]]$x <- NULL
  }
  mfile[[i]]$y$d <- D[i]
  mfile[[i]] <- do.call(rbind,mfile[[i]])
}
raw <- do.call(rbind,mfile)

raw %>% filter(scope=='global') %>% group_by(state) %>% summarise(d=mean(d)) %>% arrange(desc(d))
raw %>% filter(scope=='global') %>% group_by(state) %>% summarise(d=mean(d)) %>% filter(
  state%in%c('China','UnitedKingdom','US')
)

plot.ts(filter(raw,state=='US')$d)
plot.ts(filter(raw,state=='China')$d)
plot.ts(filter(raw,state=='KoreaSouth')$d)
plot.ts(filter(raw,state=='Singapore')$d)

keras::save_model_hdf5(model,paste0('/Users/wenrurumon/Documents/posdoc/wuhan/model/dd_d422.model'),overwrite = TRUE,include_optimizer = TRUE)

###################################
# New Prediction Model
###################################

log <- filter(raw,fact=='confirmed') 
mfile <- do.call(c,lapply(unique(log$key),function(K){
  lapply(1:(max(log$i)-8),function(I){getlog(I,K,log,8)})
}))
X <- sapply(mfile,function(x){c(x$x$new/(mean(x$x$new+1)),mean(x$x$d))}) %>% t
Y <- cbind(sapply(mfile,function(x){c(x$y$new/(mean(x$x$new+1)))}))

e.input <- layer_input(shape=ncol(X))
e.layer <- layer_dense(e.input,16,activation='relu')
l.layer <- layer_dense(e.layer,4,activation='relu')
d.output <- layer_dense(units=ncol(Y))
model <- keras_model(e.input,d.output(l.layer))
model %>% compile(loss = "mean_squared_error", optimizer = "adam",metrics = c('mae'))

system.time(model %>% fit(x = X,y = Y,batch = 128,epochs = 2000,verbose = 0))
model %>% fit(x = X,y = Y,batch = 128,epochs = 100,verbose = 2)
system.time(model %>% fit(x = X,y = Y,batch = 128,epochs = 1000,verbose = 0))
model %>% fit(x = X,y = Y,batch = 128,epochs = 100,verbose = 2)

keras::save_model_hdf5(model,paste0('/Users/wenrurumon/Documents/posdoc/wuhan/model/dd_m0422.model'),overwrite = TRUE,include_optimizer = TRUE)

###################################
# Prediction
###################################

model <- keras::load_model_hdf5('/Users/wenrurumon/Documents/posdoc/wuhan/model/dd_m0422.model')
model.d <- keras::load_model_hdf5('/Users/wenrurumon/Documents/posdoc/wuhan/model/dd_d422.model')

# K <- 'us confirmed NewYork'

par(mfrow=c(2,2))
# rlt <- lapply(unique(filter(log,scope=='global')$key),function(K){
rlt <- lapply(unique(log$key),function(K){
  print(K)
  rlt <- list()
  for(j in 1:5){
    temp <- filter(log,key==K)
    for(i in 1:500){
      tempi <- getlog(nrow(temp)-7,temp$key[1],temp,8)
      if(sum(tempi$x$new)==0){
        tempi.new <- 0
      } else {
        tempi.new <- model %>% 
          predict(matrix(c(tempi$x$new/mean(tempi$x$new+1),mean(tempi$x$d)),nrow=1)) * 
          mean(tempi$x$new+1)
      }
      tempi.new <- floor(ifelse(tempi.new>0,tempi.new,0))
      if((mean(tempi$x$new)==tempi.new)&tempi.new>0){
        tempi.new <- tempi.new - 1
      }
      if(j %in% 1:4){
        tempi.d <- temp$d[nrow(temp)]+ ((temp$d[length(temp$d)]-temp$d[1])/nrow(temp)*c(0,0.5,1,2)[j])
        if(temp$d[nrow(temp)]>=tempi.d){tempi.d <- temp$d[nrow(temp)]}
      } else {
        tempi.d <- model.d %>% 
          predict(matrix(c(tempi$x$accum/mean(tempi$x$accum+1),mean(tempi$x$d)),nrow=1))
        tempi.d <- max(tempi.d,tempi$x$d)
      }
      tempi.d <- (ifelse(tempi.d>1,1,tempi.d))
      temp <- rbind(temp,tempi$x[8,] %>% 
                      mutate(new=tempi.new,accum=accum+tempi.new,d=tempi.d,i=i+1,
                             date=paste(as.POSIXct(date)+3600*24)))
    }
    if(j==1){
      plot.ts(temp$new,main=K,col=2)
    } else {
      lines(temp$new,col=c(1,4,6,7)[j-1])
    }
    rlt[[j]] <- temp
  }
  # sapply(rlt,function(x){x$d}) %>% plot.ts
  # sapply(rlt,function(x){x$new}) %>% plot.ts
rlt
})


rlt.mat <- lapply(rlt,function(x){
  list(new = sapply(x,function(x){x$new}),
       d = sapply(x,function(x){x$d}))
})
names(rlt.mat) <- lapply(rlt,function(x){x[[1]]$key[1]})

plot.ts(sapply(1:5,function(i){
  sapply(rlt.mat,function(x){x[[1]][,i]}) %>% rowSums
}) )

#############################################

rlt.key <- do.call(rbind,lapply(rlt,function(x){x[[1]][1,1:2]})) %>% merge(
  log %>% group_by(scope,state) %>% summarise(now.value=sum(new)),by=c('scope','state')
)
rlt.mat <- data.frame(
  date.new.max = t(sapply(rlt,function(x) sapply(x,function(x) which(x$new==max(x$new))[[1]])))
  ,new.new.max = t(sapply(rlt,function(x) sapply(x,function(x) x$new[which(x$new==max(x$new))[[1]]])))
  ,accum.new.max = t(sapply(rlt,function(x) sapply(x,function(x) x$accum[which(x$new==max(x$new))[[1]]])))
  ,date.accum.max = t(sapply(rlt,function(x) sapply(x,function(x) max2(x$new))))
  ,new.accum.max = t(sapply(rlt,function(x) sapply(x,function(x) x$new[max2(x$new)])))
  ,accum.accum.max = t(sapply(rlt,function(x) sapply(x,function(x) x$accum[max2(x$new)])))
)
rlt2 <- cbind(rlt.key,rlt.mat) %>% as.matrix
rlt2[,c(1,2,3,4,5,16,17,18,19,20)+3] <- getdate(as.numeric(rlt2[,c(1,2,3,4,5,16,17,18,19,20)+3]))

write.csv(as.matrix(rlt2),'/Users/wenrurumon/Documents/posdoc/wuhan/summary/rlt_YAmodel.csv')

#############################################

rlt.new <- do.call(cbind,lapply(rlt,function(x){sapply(x,function(x)x$new)}))
colnames(rlt.new) <- 
  paste0(rep(paste(rlt.key[,1],rlt.key[,2],'new',sep='.'),each=5),paste0('.',1:5))
rownames(rlt.new) <- getdate(1:nrow(rlt.new))

rlt.d <- do.call(cbind,lapply(rlt,function(x){sapply(x,function(x)x$d)}))
colnames(rlt.d) <- 
  paste0(rep(paste(rlt.key[,1],rlt.key[,2],'d',sep='.'),each=5),paste0('.',1:5))
rownames(rlt.d) <- getdate(1:nrow(rlt.new))

write.csv(rlt.new,'/Users/wenrurumon/Documents/posdoc/wuhan/summary/rlt_new_YAmodel.csv')
write.csv(rlt.d,'/Users/wenrurumon/Documents/posdoc/wuhan/summary/rlt_d_YAmodel.csv')

rlt.d <- rlt.d[,grep('global',colnames(rlt.d))]
rlt.new <- rlt.new[,grep('global',colnames(rlt.new))]

rlt.d <- rlt.d * apply(rlt.new,2,cumsum)
rlt.d <- apply(rlt.d,1,function(x){
  tapply(x,rep(1:5,length=ncol(rlt.new)),sum)
}) %>% t

rlt.new <- apply(rlt.new,1,function(x){
  tapply(x,rep(1:5,length=ncol(rlt.new)),sum)
}) %>% t

rlt.d <- rlt.d/apply(rlt.new,2,cumsum)
write.csv(rlt.new,'/Users/wenrurumon/Documents/posdoc/wuhan/summary/rlt_total_new_YAmodel.csv')
write.csv(rlt.d,'/Users/wenrurumon/Documents/posdoc/wuhan/summary/rlt_total_d_YAmodel.csv')

sapply(apply(rlt.new,2,function(x){
  list(date.new.max = getdate(which(x==max(x))[1]),
       new.new.max = as.numeric(x[which(x==max(x))[1]]),
       accum.new.max = sum(x[1:which(x==max(x))[1]]),
       date.accum.max = getdate(max(which(x>0))),
       new.accum.max = as.numeric(x[max(which(x>0))]),
       accum.accum.max = sum(x)
  )}),as.vector)
write.csv(sapply(apply(rlt.new,2,function(x){
  list(date.new.max = getdate(which(x==max(x))[1]),
       new.new.max = as.numeric(x[which(x==max(x))[1]]),
       accum.new.max = sum(x[1:which(x==max(x))[1]]),
       date.accum.max = getdate(max(which(x>0))),
       new.accum.max = as.numeric(x[max(which(x>0))]),
       accum.accum.max = sum(x)
  )}),as.vector),'/Users/wenrurumon/Documents/posdoc/wuhan/summary/rlt_total_YAmodel.csv')

plot.ts(rlt.new)
plot.ts(rlt.d)

#############################################

grep('Sudan',rlt.key$state)
plot.ts(sapply(rlt[[193]],function(x){x$new}))
