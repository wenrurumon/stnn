
rm(list=ls())
library(openxlsx)
library(data.table)
library(dplyr)
library(keras)

setwd('/Users/wenrurumon/Documents/posdoc/wuhan')
raw <- read.xlsx('data0208.xlsx')

colnames(raw) <- c('date','province','city','confirm','heal','dead','source')
othnat <- "新加坡,台湾,澳门,马来西亚,日本,韩国,美国,香港,澳大利亚,菲律宾,加拿大,泰国,比利时,越南,德国,印度,阿联酋,斯里兰卡,西班牙,瑞典,俄罗斯,英国,意大利,法国,芬兰,柬埔寨,尼泊尔"
othnat <- strsplit(othnat,',')[[1]]
raw <- mutate(raw,city=ifelse(is.na(city),province,city)) %>% filter(!(province%in%othnat))
raw$date <- sapply(strsplit(raw$date,'月|日'),function(x){as.numeric(x[1])*100+as.numeric(x[2])})
raw <- raw %>% arrange(date,province,city) %>% 
  group_by(date,province,city) %>% 
  summarise(confirm=sum(confirm),heal=sum(heal),dead=sum(dead)) %>% 
  mutate(citykey=paste(date,city),provkey=paste(date,province)) 
raw.date <- c(min(raw$date):131,201:max(raw$date))

raw.city <- lapply(unique(raw$province),function(i){
  x <- (raw[match(paste(raw.date,i),raw$provkey),])
  x[is.na(x)] <- 0
  apply(select(as.data.frame(x),confirm,heal,dead),2,cumsum)
})
names(raw.city) <- unique(raw$province)

data.model <- sapply(raw.city,function(x){x[,1]})
plot.ts(rowSums(data.model))
data.model.max <- max(data.model)*1.2
data.model <- data.model / data.model.max

data.model <- lapply(1:(nrow(data.model)-12),function(i){
  list(x=t(data.model[0:9+i,]),y=t(data.model[10:12+i,]))
})
X <- do.call(rbind,lapply(data.model[1:(length(data.model)-2)],function(x){x$x}))
Y <- do.call(rbind,lapply(data.model[1:(length(data.model)-2)],function(x){x$y}))

############

dims <- c(32,16,4)
e.input <- layer_input(shape=ncol(X))
e.layer <- layer_dense(e.input,dims[1],activation='sigmoid')
l.layer <- layer_dense(e.layer,dims[2],activation='sigmoid')
d.layer <- layer_dense(units=dims[3],activation='sigmoid')
d.output <- layer_dense(units=ncol(Y),activation='sigmoid')
d.input <- layer_input(shape=dims[3])
model <- keras_model(e.input,d.output(d.layer(l.layer)))
model %>% compile(
  loss = "mean_squared_error", 
  optimizer = "adam",
  metrics = c('accuracy')
)
history <- model %>% fit(
  x = X, 
  y = Y,
  epochs = 4000,
  verbose = 2
)

############

Y.fit <- model %>% predict(do.call(rbind,lapply(data.model,function(x){x$x})))
Y.actual <- do.call(rbind,lapply(data.model,function(x){x$y}))
Y.fit <- data.table(
  city = rownames(Y.actual),
  batch = rep(1:(nrow(Y.actual)/31),31),
  Y.fit
) %>% group_by(batch) %>% 
  summarise(day1=sum(V1),day2=sum(V2),day3=sum(V3))
Y.actual <- data.table(
  city = rownames(Y.actual),
  batch = rep(1:(nrow(Y.actual)/31),31),
  Y.actual
) %>% group_by(batch) %>% 
  summarise(day1=sum(V1),day2=sum(V2),day3=sum(V3))

Y.validate <- data.table(
  class = c(rep('train',(length(data.model)-2)),c(rep('test',2))),
  Y.fit[,-1]/Y.actual[,-1]-1
) %>% print
Y.validate %>% group_by(class) %>% 
  summarise(day1=mean(abs(day1)),day2=mean(abs(day2)),day3=mean(abs(day3)))
