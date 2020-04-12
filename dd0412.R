
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
# Accum to D
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
D <- model %>% predict(X)
cor(Y,D)

###################################
# raw D to accum2d
###################################

e.input <- layer_input(shape=ncol(Y))
l.layer <- layer_dense(e.input,4,activation='sigmoid')
d.output <- layer_dense(units=ncol(Y),activation='sigmoid')
model <- keras_model(e.input,d.output(l.layer))
model %>% compile(loss = "mean_squared_error", optimizer = "adam",metrics = c('mae'))

system.time(model %>% fit(x = Y,y = D,batch = 128,epochs = 1000,verbose = 0))
model %>% fit(x = Y,y = D,batch = 128,epochs = 100,verbose = 2)
D <- model %>% predict(Y)

raw <- raw %>% mutate(d=model %>% predict(select(raw,d)%>%as.matrix()))
# write.csv(raw  %>% group_by(scope,state) %>% summarise(max=max(d),mean=mean(d)),
#           "/Users/wenrurumon/Documents/posdoc/wuhan/summary/temp.csv")

###################################
# Predict D
###################################

mfile <- do.call(c,lapply(unique(log$key),function(K){
  lapply(1:(max(log$i)-8),function(I){getlog(I,K,log,8)})
}))
X <- (t(sapply(mfile,function(x){c(x$x$d)})))
Y <- cbind(sapply(mfile,function(x){x$y$d}))

e.input <- layer_input(shape=ncol(X))
e.layer <- layer_dense(e.input,16,activation='relu')
l.layer <- layer_dense(e.layer,4,activation='relu')
d.output <- layer_dense(units=ncol(Y),activation='sigmoid')
model <- keras_model(e.input,d.output(l.layer))
model %>% compile(loss = "mean_squared_error", optimizer = "adam",metrics = c('mae'))
system.time(model %>% fit(x = X,y = Y,batch = 128,epochs = 1000,verbose = 0))
model %>% fit(x = X,y = Y,batch = 128,epochs = 100,verbose = 2)

model.d <- model
# keras::save_model_hdf5(model,paste0('/Users/wenrurumon/Documents/posdoc/wuhan/model/model_D.model'),overwrite = TRUE,include_optimizer = TRUE)

###################################
# China Model
###################################

log <- filter(raw,state%in%c('KoreaSouth','Japan','Singapore','China')|scope=='china')
mfile <- do.call(c,lapply(unique(log$key),function(K){
  lapply(1:(max(log$i)-8),function(I){getlog(I,K,log,8)})
}))
X <- (t(sapply(mfile,function(x){c(x$x$new/(mean(x$x$new)+1),x$y$d)})))
Y <- cbind(sapply(mfile,function(x){x$y$new/mean(x$x$new+1)}))

e.input <- layer_input(shape=ncol(X))
e.layer <- layer_dense(e.input,16,activation='relu')
l.layer <- layer_dense(e.layer,4,activation='relu')
d.output <- layer_dense(units=ncol(Y),activation='relu')
model <- keras_model(e.input,d.output(l.layer))
model %>% compile(loss = "mean_squared_error", optimizer = "adam",metrics = c('mae'))

system.time(model %>% fit(x = X,y = Y,batch = 128,epochs = 1000,verbose = 0))
model %>% fit(x = X,y = Y,batch = 128,epochs = 100,verbose = 2)

model.china <- model
# keras::save_model_hdf5(model,paste0('/Users/wenrurumon/Documents/posdoc/wuhan/model/model_china.model'),overwrite = TRUE,include_optimizer = TRUE)

###################################
# Global Model
###################################

# log <- filter(raw,scope=='global')
log <- raw
mfile <- do.call(c,lapply(unique(log$key),function(K){
  lapply(1:(max(log$i)-8),function(I){getlog(I,K,log,8)})
}))
X <- (t(sapply(mfile,function(x){c(x$x$new/(mean(x$x$new)+1),x$y$d)})))
Y <- cbind(sapply(mfile,function(x){x$y$new/mean(x$x$new+1)}))

system.time(model %>% fit(x = X,y = Y,batch = 128,epochs = 1000,verbose = 0))
model %>% fit(x = X,y = Y,batch = 128,epochs = 100,verbose = 2)

model.global <- model
# keras::save_model_hdf5(model,paste0('/Users/wenrurumon/Documents/posdoc/wuhan/model/model_global.model'),overwrite = TRUE,include_optimizer = TRUE)


#Prediction

# rm(list=ls())
# load('raw.rda')
models <- list(
  d = load_model_hdf5('/Users/wenrurumon/Documents/posdoc/wuhan/model/model_d.model'),
  china = load_model_hdf5('/Users/wenrurumon/Documents/posdoc/wuhan/model/model_china.model'),
  global = load_model_hdf5('/Users/wenrurumon/Documents/posdoc/wuhan/model/model_global.model')
)

temp <- temp1 <- filter(raw,key=='global confirmed China')
for(i in 1:365){
  # print(i)
  tempi <- filter(temp,i>(max(temp$i)-8))
  if(i %in% 1:28){
    tempi.d <- mean(tempi$d)
  } else {
    tempi.d <- tempi.d+mean(diff(temp$d))
    # tempi.d <- max(tempi$d)+(max(tempi$d)-min(tempi$d))/8
    # tempi.d <- max(models$d %>% predict(matrix(c(tempi$d),nrow=1)),tempi$d)
    # if(tempi.d==mean(tempi$d)){tempi.d <- tempi.d+mean(diff(temp$d))}
  }
  tempi.n <- floor((models$china %>% predict(matrix(c(tempi$new/mean(tempi$new+1),tempi.d),nrow=1)))*mean(tempi$new))
  tempi <- tempi[8,] %>% mutate(d=tempi.d,new=tempi.n,accum=accum+tempi.n,i=i+1,
                                date=paste(as.POSIXct(date)+3600*24))
  temp <- rbind(temp,tempi)
}
plot.ts(temp$new)
sum(temp$new)
lines(temp1$new,col=2)


###################################
# China Model
###################################
