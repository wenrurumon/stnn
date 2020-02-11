
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
nat <- "湖北,上海,北京,四川,广东,云南,天津,山东,河南,浙江,重庆,黑龙江,宁夏,安徽,山西,广西,江苏,江西,河北,海南,湖南,福建,贵州,辽宁,内蒙古,吉林,新疆,甘肃,陕西,青海,西藏,台湾,香港,澳门"
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
data.who[,1] <- as.numeric(paste0(who[,1]))
raw <- rbind(filter(raw,date<206),data.who)
Y.actual <- raw[,-1]

#Process data model
Y.model <- apply(Y.actual,2,diff)

############################
# Data Setup
############################

get_model_file <- function(x,i,p,h){
  y <- t(x[1:h-1+p+i,,drop=F])
  x <- t(x[(1:p)-1+i,,drop=F])
  f <- rowMeans(x)
  y <- y/f; y[is.na(y)] <- 0; y[y==Inf] <- 1
  x <- x/f; x[is.na(x)] <- 0
  list(x=x,y=y,f=f)
}
process_model_file <- function(x){
  X <- do.call(rbind,lapply(x,function(x){x$x}))
  Y <- do.call(rbind,lapply(x,function(x){x$y}))
  list(X=X,Y=Y)
}
get_x <- function(x,i,p){
  x <- t(x[(1:p)-1+i,,drop=F])
  f <- rowMeans(x)
  x <- x/f
  x[is.na(x)] <- 0
  list(x=x,f=f)
}
as.n <- function(x){
  as.numeric(paste(x))
}
pred <- function(x,model){
  model %>% predict(x)
}

############################
# Auto Forecasting
############################

dims <- c(32,4)
drops <- rep(0,2)
activations <- c('relu','relu')
mfile <- lapply(1:22,get_model_file,h=1,p=8,x=Y.model)
w <- 1
X <- do.call(rbind,lapply(rep(mfile[1:(length(mfile)-3)],w),function(x){x$x}))
Y <- do.call(rbind,lapply(rep(mfile[1:(length(mfile)-3)],w),function(x){x$y}))

autoforecasting <- 
  function(X,Y,p=8,h=1,dims=c(32,4),activations=c('relu','relu'),drops=rep(0,2)){
    model <- keras_model_sequential() 
    model %>% 
      layer_dense(units=dims[1],activation=activations[1],input_shape=ncol(X)) %>%
      layer_dropout(rate=drops[1]) %>%
      layer_dense(units=dims[2],activation=activations[2]) %>% 
      layer_dropout(rate=drops[2]) %>%
      layer_dense(units=ncol(Y),activation=NULL)
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
    list(model = model)
  }

autoforecasting2 <- 
  function(X,Y,p=8,h=1,dims=c(32,4),activations=c('relu','relu'),drops=rep(0,2)){
    e.input <- layer_input(shape=ncol(X))
    e.layer <- layer_dense(e.input,dims[1],activation=activations[1]) %>% 
      layer_dropout(rate=drops[1])
    l.layer <- layer_dense(e.layer,dims[2],activation=activations[2]) %>% 
      layer_dropout(rate=drops[2])
    d.output <- layer_dense(units=ncol(Y),activation=NULL)
    d.input <- layer_input(shape=dims[2])
    model <- keras_model(e.input,d.output(l.layer))
    encoder <- keras_model(e.input,l.layer)
    decoder <- keras_model(d.input,d.output(d.input))
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
    list(model=model,encoder=encoder,decoder=decoder)
  }

############################
# Validation
############################  

#Generate Dummy Data

dummy_data <- function(sel,p=8,h=1,w=NULL,x=Y.model){
  # sel <- 3; p <- 8; h <- 1; w <- NULL; x <- Y.model
  mfile <- lapply(1:(nrow(x)-p-h+1),get_model_file,h=h,p=p,x=x)
  if(is.null(w)){
    w <- rep(1:2,each=6)
    w <- c(w,rep(3,length(mfile)-length(w)-sel))
  }
  X <- do.call(rbind,
               lapply(rep(mfile[1:(length(mfile)-sel)],w),
                      function(x){x$x}
               )
  )
  Y <- do.call(rbind,
               lapply(rep(mfile[1:(length(mfile)-sel)],w),
                      function(x){x$y}
               )
  )
  list(X=X,Y=Y)
}
sel <- rep(1:3,each=3)
dummies <- lapply(sel,dummy_data,p=8,h=1,x=Y.model)

#Auto Forecasting Modeling

# j <- 0
# test <- lapply(dummies,function(d){
#   print(paste(j<<-j+1,Sys.time()))
#   autoforecasting(d$X,d$Y,p=8,h=1,dims=c(32,4),activations=rep('relu',2),drops=rep(0,2))
# })

j <- 0
test2 <- lapply(dummies,function(d){
  print(paste(j<<-j+1,Sys.time()))
  autoforecasting2(d$X,d$Y,p=8,h=1,dims=c(32,4),activations=rep('relu',2),drops=rep(0,2))
})

#Validational Prediction

Xs <- lapply(1:(nrow(Y.actual)-8),get_x,x=Y.model,p=8)

# Y.fits <- sapply(test,function(rlti){
#   sapply(Xs,function(xi){
#     sum((rlti$model %>% predict(xi$x))*xi$f)
#   })
# })
# Y.fits[,colSums(Y.fits)==0] <- NA
# out <- cbind()
# for(i in 1:length(unique(sel))){
#   temp <- Y.fits[,1:(length(unique(sel)))+(i-1)*(length(sel)/length(unique(sel)))]
#   for(j in 1:((nrow(temp)-(i-1)))){
#     temp[j,] <- temp[j,] + rowSums(Y.actual)[-1:-7][j]
#   }
#   if(i>1){
#     for(j in (nrow(temp)-(i-2)):(nrow(temp))){
#       temp[j,] <- temp[j-1,]+temp[j,]
#     }
#   }
#   Y.fits[,1:(length(unique(sel)))+(i-1)*(length(sel)/length(unique(sel)))] <- temp
#   out <- cbind(out,rowMeans(temp,na.rm=T))
# }

Y.fits2 <- sapply(test2,function(rlti){
  sapply(Xs,function(xi){
    sum((rlti$model %>% predict(xi$x))*xi$f)
  })
})
Y.fits2[,colSums(Y.fits2)==0] <- NA
out2 <- cbind()
for(i in 1:length(unique(sel))){
  temp <- Y.fits2[,1:(length(unique(sel)))+(i-1)*(length(sel)/length(unique(sel)))]
  for(j in 1:((nrow(temp)-(i-1)))){
    temp[j,] <- temp[j,] + rowSums(Y.actual)[-1:-7][j]
  }
  if(i>1){
    for(j in (nrow(temp)-(i-2)):(nrow(temp))){
      temp[j,] <- temp[j-1,]+temp[j,]
    }
  }
  Y.fits2[,1:(length(unique(sel)))+(i-1)*(length(sel)/length(unique(sel)))] <- temp
  out2 <- cbind(out2,rowMeans(temp,na.rm=T))
}

# Y.fits <- cbind(rowSums(Y.actual)[-1:-8],out,out2)
Y.fits <- cbind(rowSums(Y.actual)[-1:-8],out2) 
round(abs(Y.fits / Y.fits[,1] -1)[,-1],4)*100
write.csv(Y.fits,'validation_0210.csv')
                
############################
# Prediction
############################  

#Modeling

mfile <- dummy_data(sel=0,p=8,h=1,x=Y.model)
mmodel <- lapply(1:5,function(i){
  print(paste(i,Sys.time()))
  autoforecasting2(mfile$X,mfile$Y,p=8,h=1,dims=c(32,4),activation=rep('relu',2),drops=rep(0,2))
})

#Resulting

Xs <- lapply(1:(nrow(Y.actual)-8),get_x,x=Y.model,p=8)
Y.fits <- sapply(mmodel,function(rlti){
  sapply(Xs,function(xi){
    sum((rlti$model %>% predict(xi$x))*xi$f)
  })
}) + rowSums(Y.actual[-1:-8,])

data.frame(actual = c(rowSums(Y.actual)[-1:-9],NA),
      fit = rowMeans(Y.fits))

out <- cbind()
Xi <- get_x(nrow(Y.model)-7,x=Y.model,p=8)
out <- cbind(out,sapply(mmodel,function(rlti){
  (rlti$model %>% predict(Xi$x)) * Xi$f
}) %>% rowMeans)
rbind(
  today=round(rowSums(Y.actual)[nrow(Y.actual)]),
  add = round(sum(out)),
  tomorrow = round(rowSums(Y.actual)[nrow(Y.actual)] + sum(out))
)

while(ncol(out)<10){
  temp <- cbind(Xi$x*Xi$f,out[,ncol(out)])[,-1]
  Xi <- get_x(1,x=t(temp),p=8)
  out <- cbind(out,sapply(mmodel,function(rlti){
    (rlti$model %>% predict(Xi$x)) * Xi$f
  }) %>% rowMeans)
}

#Futuring

rbind(
  data.frame(actual = c(rowSums(Y.actual)[-1:-9],NA),
           fit = rowMeans(Y.fits)),
  data.frame(actual=NA,
             fit = rowSums(Y.actual)[nrow(Y.actual)]+cumsum(colSums(out))[-1]
             )
)


############################
# Further Analysis
############################  

Xs <- lapply(1:(nrow(Y.actual)-8),get_x,x=Y.model,p=8)
Xs <- do.call(rbind,lapply(Xs,function(x){x$x}))
l.Xs <- lapply(mmodel,function(rlti){
  rlti$encoder %>% predict(Xs)
})
