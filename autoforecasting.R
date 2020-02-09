rm(list=ls())
library(openxlsx)
library(data.table)
library(dplyr)
library(keras)
check <- function(x){data.frame(date=datemap,raw[names(raw)==x])}

############################
# Data Processing
############################

setwd('/Users/wenrurumon/Documents/posdoc/wuhan')
raw <- read.xlsx('data0209.xlsx',sheet='shimo')
colnames(raw) <- c('date','province','city','confirm','heal','dead','source')
nat <- "湖北,上海,北京,四川,广东,云南,天津,山东,河南,浙江,重庆,黑龙江,宁夏,安徽,山西,广西,江苏,江西,河北,海南,湖南,福建,贵州,辽宁,内蒙古,吉林,新疆,甘肃,陕西,青海,西藏"#,台湾,香港,澳门"
nat <- strsplit(nat,',')[[1]]
raw <- mutate(raw,city=ifelse(is.na(city),province,city)) %>% 
  arrange(date,province,city) %>% 
  group_by(date,province,city) %>% 
  summarise(confirm=sum(confirm),heal=sum(heal),dead=sum(dead))
raw$date <- sapply(strsplit(raw$date,'月|日'),function(x){as.numeric(x[1])*100+as.numeric(x[2])})
raw <- raw %>% 
  filter(province%in%nat) %>% 
  group_by(date,province) %>% 
  summarise(confirm = sum(confirm)) %>% 
  mutate(provkey=paste(date,province)) 
datemap <- c(min(raw$date):131,201:max(raw$date))
provmap <- unique(raw$province)
raw <- data.table(date=datemap,sapply(provmap,function(i){
  x <- raw[match(paste(datemap,i),raw$provkey),]$confirm
  x[is.na(x)] <- 0
  cumsum(x)
}))

############################
# WHO data
############################

who <- read.xlsx('data0209.xlsx',sheet='who')
citymap <- read.xlsx('data0209.xlsx',sheet='citymap')
read.who <- function(x){
  x <- strsplit(x,'\ ')[[1]][-1:-3]
  x.value <- sort(which(!is.na(as.numeric(x))),decreasing=T)
  for(i in 2:(length(x.value))){
    if((x.value[i-1] - x.value[i])==1){
      x[x.value[i]] <- paste0(x[x.value[i]],x[x.value[i-1]])
      x[x.value[i-1]] <- ''
    }
  }
  x <- x[x!='']
  x.value <- sort(which(is.na(as.numeric(x))),decreasing=T)
  for(i in 2:(length(x.value))){
    if((x.value[i-1] - x.value[i])==1){
      x[x.value[i]] <- paste0(x[x.value[i]],'_',x[x.value[i-1]])
      x[x.value[i-1]] <- ''
    }
  }
  x <- t(matrix(x[x!=''],2))
  x <- data.table(city=tolower(x[,1]),confirm=as.numeric(x[,2]))
  x <- merge(x,citymap,by='city') %>% select(city=zh_name,confirm)
  rlt <- x$confirm
  names(rlt) <- x$city
  rlt
}
data.who <- sapply(who[,2],read.who)
data.who <- as.data.table(t(data.who[match(colnames(raw),rownames(data.who)),]))
colnames(data.who)[1] <- 'date'
data.who[,1] <- paste0('who',who[,1])
raw <- rbind(filter(raw,date<206),data.who)
data.model <- raw[,-1]
data.model.max <- max(data.model)*3
data.model <- data.model / data.model.max

############################
# Data Setup
############################

get_model_file <- function(x,i,p,h){
  y <- t(x[1:h-1+p+i,,drop=F])
  x <- t(x[(1:p)-1+i,,drop=F])
  y <- y/x[,ncol(x)]
  y[is.na(y)] <- 1
  y[is.infinite(y)] <- 1
  y <- log(y)
  # y <- y-1
  list(x=x,y=y)
}
process_model_file <- function(data.model){
  X <- do.call(rbind,lapply(data.model,function(x){x$x}))
  Y <- do.call(rbind,lapply(data.model,function(x){x$y}))
  list(X=X,Y=Y)
}
get_x <- function(x,i,p){
  x <- t(x[(1:p)-1+i,,drop=F])
  x
}
as.n <- function(x){
  as.numeric(paste(x))
}
pred <- function(x,model){
  x[,ncol(x)] * exp(model %>% predict(x))
}

############################
# Validation
############################

p <- 8
h <- 1
mfile <- lapply(1:(length(datemap)-p),get_model_file,h=h,p=p,x=data.model)

sel <- 1
dims <- c(32,16,6)
activations <- rep('sigmoid',4)
drops <- rep(0,3)

autoforecasting <- function(sel,p,h,dims,activations,drops,mfile){
    print(paste('Model Intialized',Sys.time()))
    args <- sapply(list(sel=sel,p=p,h=h,dims=dims,activations=activations,drops=drops),
                   paste,collapse=',') %>% rbind %>% as.data.frame
    print(t(args))
    mfile <- mfile[1:(length(mfile)-sel)]
    X <- do.call(rbind,lapply(mfile,function(x){x$x}))
    Y <- do.call(rbind,lapply(mfile,function(x){x$y}))
    model <- keras_model_sequential() 
    model %>% 
      layer_dense(units=dims[1],activation=activations[1],input_shape=ncol(X)) %>%
      layer_dropout(rate=drops[1]) %>%
      layer_dense(units=dims[2],activation=activations[2]) %>% 
      layer_dropout(rate=drops[2]) %>%
      layer_dense(units=dims[3],activation=activations[3]) %>%
      layer_dropout(rate=drops[3]) %>%
      layer_dense(units=ncol(Y),activation=activations[4])
    model %>% compile(
      loss = "mean_squared_error", 
      optimizer = "adam",
      metrics = c('mae')
    )
    system.time(history <- model %>% fit(
      x = X, 
      y = Y,
      batch = 128,
      epochs = 4000,
      verbose = 0
    ))
    rlt <- list(args=args,model=model)
    rlt
}
rlt <- lapply(rep(5:1,each=5),autoforecasting,
              p=8,h=1,
              dims=c(32,16,6),activations=rep('sigmoid',4),
              drops=rep(0,3),mfile=mfile)


rlt.sel <- sapply(rlt,function(x){paste(t(x$args))})[1,]
predi <- function(rlti,data.model=data.model){
  temp <- get_x(data.model,
                nrow(data.model)-as.n(rlti$args$p)-as.n(rlti$args$sel)+1,
                as.n(rlti$args$p))  
  pred(temp,rlti$model)
}
rlt.vali <- data.table(
  date = 1:nrow(data.model),
  actual = rowSums(data.model),
  predict = c(rep(NA,nrow(data.model)-length(unique(rlt.sel))),
              colSums(
                sapply(unique(rlt.sel),function(i){
                  rowMeans(sapply(rlt[which(rlt.sel==i)],predi))})))
) %>% mutate(gap=predict/actual-1)
rlt.vali

############################
# Modeling
############################

models <- lapply(rep(0,5),autoforecasting,
              p=8,h=1,
              dims=c(32,16,6),activations=rep('sigmoid',4),
              drops=rep(0,3),mfile=mfile)
out <- data.model %>% as.matrix
for(i in 1:10){out <- rbind(out,rowMeans(sapply(models,predi,data.model=out)))  }
out <- rowSums(out)

plot.ts(out[-1]-out[-length(out)])

