
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
  merge(raw,(raw %>% group_by(scope,state,fact) %>% summarise(peak=max(new))),by=c('scope','state','fact')) %>% 
  filter(new==peak) %>% group_by(scope,state,fact) %>% summarise(peak=min(date)) %>% 
  merge(raw,by=c('scope','state','fact')) %>% mutate(d=ifelse(date>peak&(scope=='china'|state=='China'),1,0)) %>%
  mutate(key=paste(scope,fact,state),i=as.numeric((as.POSIXct(date)-as.POSIXct(min(raw$date)))/3600/24)+1) %>%
  mutate(gap=1/as.numeric(paste(((as.POSIXct(peak)-as.POSIXct(date))/3600/24+1))))
raw <- raw %>% mutate(d=ifelse(gap>0&gap!=Inf,gap,d)) %>% select(-peak,-gap)
raw <- raw %>% mutate(d=ifelse(state=='China'|scope=='china',d,0))

#Module
getlog <- function(I,keyI,log,p=8){
  logi <- filter(log,key==keyI&i%in%(0:p+I))
  x <- logi[1:p,]
  y <- logi[-1:-p,]
  list(x=x,y=y)
}

###################################
# China Model
###################################

log <- filter(raw,scope%in%c('china','global')&fact=='confirmed')
p <- 8
mfile <- do.call(c,lapply(unique(log$key),function(K){
  lapply(1:(max(log$i)-p),function(I){
    getlog(I,K,log,p)
  })
}))
X <- (t(sapply(mfile,function(x){
  c(x$x$new/(mean(x$x$new)+1)
    ,mean(x$x$d))
})))
Y <- cbind(sapply(mfile,function(x){
  x$y$new/(mean(x$x$new)+1)
}))

e.input <- layer_input(shape=ncol(X))
e.layer <- layer_dense(e.input,16,activation='relu')
l.layer <- layer_dense(e.layer,4,activation='relu')
d.output <- layer_dense(units=ncol(Y))
model <- keras_model(e.input,d.output(l.layer))
model %>% compile(loss = "mean_squared_error", optimizer = "adam",metrics = c('mae'))
system.time(model %>% fit(x = X,y = Y,batch = 128,epochs = 2000,verbose = 0))
model %>% fit(x = X,y = Y,batch = 128,epochs = 100,verbose = 2)
keras::save_model_hdf5(model,paste0('/Users/wenrurumon/Documents/posdoc/wuhan/model/model1.model'),overwrite = TRUE,include_optimizer = TRUE)

temp <- filter(raw,state=='US'&fact=='confirmed') %>% arrange(i)
D <- NA
J <- 0
for(j in 1:300){
  # print(j)
  tempi <- do.call(rbind,lapply(unique(temp$key),function(K){
    logi <- getlog(max(temp$i)-7,K,temp)
    # Di <- mean(logi$x$d)
    Di <- ifelse(j>J,1,1/j)
    print(Di)
    newi <- model %>% predict(matrix(c(logi$x$new/(mean(logi$x$new)+1),Di),nrow=1))
    newi <- newi * (mean(logi$x$new)+1)
    # newi <- model %>% predict(matrix(c(logi$x$new,Di),nrow=1))
    accumi <- max(logi$x$accum)+newi
    rlti <- data.frame(
      scope=unique(logi$x$scope),state=unique(logi$x$state),fact=unique(logi$x$fact),
      date=paste(as.POSIXct(max(paste(logi$x$date)))+3600*24),
      accum = accumi,
      new = newi,
      d=round(Di),
      key=unique(logi$x$key),i=max(logi$x$i)+1
    ) %>% as.data.table
  }))
  temp <- rbind(temp,tempi)
}
plot.ts((temp %>% arrange(i))$new)
sum(temp$new)
