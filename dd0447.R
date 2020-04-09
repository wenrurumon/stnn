
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
# raw <- raw %>% mutate(d=ifelse(state%in%c('KoreaSouth','Japan','Singapore','China')|scope=='china',d,0))

#Module
getlog <- function(I,keyI,log,p=8){
  logi <- filter(log,key==keyI&i%in%(0:p+I))
  x <- logi[1:p,]
  y <- logi[-1:-p,]
  w <- y$i
  list(x=x,y=y,w=w)
}

###################################
# D Model (X9 model)
###################################

log <- filter(raw,fact=='confirmed') 
mfile <- do.call(c,lapply(unique(log$key),function(K){
  lapply(1:(max(log$i)-8),function(I){getlog(I,K,log,8)})
}))
X <- (t(sapply(mfile,function(x){c(c(x$x$accum,x$y$accum)/(mean(x$x$accum)+1))})))
Y <- cbind(sapply(mfile,function(x){x$x$d[8]}))

e.input <- layer_input(shape=ncol(X))
e.layer <- layer_dense(e.input,16,activation='relu')
l.layer <- layer_dense(e.layer,4,activation='relu')
d.output <- layer_dense(units=ncol(Y),activation='sigmoid')
model <- keras_model(e.input,d.output(l.layer))
model %>% compile(loss = "mean_squared_error", optimizer = "adam",metrics = c('mae'))

system.time(model %>% fit(x = X,y = Y,batch = 128,epochs = 1000,verbose = 0))
model %>% fit(x = X,y = Y,batch = 128,epochs = 100,verbose = 2)

model.d <- model
Y <- model %>% predict(X)

# keras::save_model_hdf5(model,paste0('/Users/wenrurumon/Documents/posdoc/wuhan/model/d9.model'),overwrite = TRUE,include_optimizer = TRUE)

###################################
# D9 Validation
###################################

log <- filter(raw,fact=='confirmed') 
mfile <- do.call(c,lapply(unique(log$key),function(K){
  lapply(1:(max(log$i)-8),function(I){getlog(I,K,log,8)})
}))
X <- sapply(mfile,function(x){c(x$x$accum/(mean(x$x$accum)+1),mean(x$x$d))}) %>% t

e.input <- layer_input(shape=ncol(X))
e.layer <- layer_dense(e.input,16,activation='relu')
l.layer <- layer_dense(e.layer,4,activation='relu')
d.output <- layer_dense(units=ncol(Y),activation='sigmoid')
model <- keras_model(e.input,d.output(l.layer))
model %>% compile(loss = "mean_squared_error", optimizer = "adam",metrics = c('mae'))

system.time(model %>% fit(x = X,y = Y,batch = 128,epochs = 1000,verbose = 0))
model %>% fit(x = X,y = Y,batch = 128,epochs = 100,verbose = 2)

model.d <- model
D <- model %>% predict(X)

# keras::save_model_hdf5(model,paste0('/Users/wenrurumon/Documents/posdoc/wuhan/model/d8.model'),overwrite = TRUE,include_optimizer = TRUE)

###################################
# New Model
###################################

log <- filter(raw,fact=='confirmed') 
mfile <- do.call(c,lapply(unique(log$key),function(K){
  lapply(1:(max(log$i)-8),function(I){getlog(I,K,log,8)})
}))
X <- sapply(mfile,function(x){c(x$x$accum/(mean(x$x$accum)+1))}) %>% t

e.input <- layer_input(shape=ncol(X))
e.layer <- layer_dense(e.input,16,activation='relu')
l.layer <- layer_dense(e.layer,4,activation='relu')
d.output <- layer_dense(units=ncol(Y))
model <- keras_model(e.input,d.output(l.layer))
model %>% compile(loss = "mean_squared_error", optimizer = "adam",metrics = c('mae'))

system.time(model %>% fit(x = X,y = Y,batch = 128,epochs = 1000,verbose = 0))
model %>% fit(x = X,y = Y,batch = 128,epochs = 100,verbose = 2)

e.input <- layer_input(shape=ncol(X))
e.layer <- layer_dense(e.input,16,activation='relu')
l.layer <- layer_dense(e.layer,4,activation='relu')
d.output <- layer_dense(units=ncol(Y))
model <- keras_model(e.input,d.output(l.layer))
model %>% compile(loss = "mean_squared_error", optimizer = "adam",metrics = c('mae'))

system.time(model %>% fit(x = X,y = Y,batch = 128,epochs = 1000,verbose = 0))
model %>% fit(x = X,y = Y,batch = 128,epochs = 100,verbose = 2)

model.f <- model

# keras::save_model_hdf5(model,paste0('/Users/wenrurumon/Documents/posdoc/wuhan/model/f.model'),overwrite = TRUE,include_optimizer = TRUE)

###################################
# Country Index
###################################

test <- do.call(cbind,sapply(unique((filter(raw,scope=='global'))$state),function(s){
  log <- filter(raw,state==s)
  x <- do.call(c,lapply(unique(log$key),function(K){
    lapply(1:(max(log$i)-8),function(I){getlog(I,K,log,8)})
  }))
  as.numeric(model.d %>% predict(t(sapply(x,function(x){x$x$accum/mean(x$x$accum+1)}))))
}))
test <- data.frame(state=unique((filter(raw,scope=='global'))$state),
                   d=colMeans(test),t(apply(test,2,function(x) quantile(x,(1:5)/5))), case=
                     (filter(raw,scope=='global') %>% group_by(state) %>% summarise(case=sum(new)))$case) %>%
  arrange(desc(X100.)) %>% select(state,factor=X100.,case)
write.csv(test,"/Users/wenrurumon/Documents/posdoc/wuhan/summary/country_index.csv")

###################################
# 
###################################
