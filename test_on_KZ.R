
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
raw <- raw %>% mutate(d=ifelse(gap>0&gap!=Inf,gap,1)) %>% select(-peak,-gap)
# raw <- raw %>% mutate(d=ifelse(state=='China'|scope=='china',d,0))
raw <- raw %>% mutate(d=ifelse(state%in%c('KoreaSouth','Japan','Singapore','China')|scope=='china',d,0))

#Module
getlog <- function(I,keyI,log,p=8){
  logi <- filter(log,key==keyI&i%in%(0:p+I))
  x <- logi[1:p,]
  y <- logi[-1:-p,]
  w <- y$i
  list(x=x,y=y,w=w)
}

###################################
# Test
###################################

# raw %>% filter(scope=='global') %>% group_by(state) %>% summarise(case=sum(new)) %>% arrange(desc(case))
p <- 8
log <- raw %>% filter(scope=='global'&state%in%c('KoreaSouth','Japan','Singapore','China'))
log <- do.call(c,lapply(unique(log$key),function(K){
  lapply(1:(max(log$i)-p),function(I){
    getlog(I,K,log,p)
  })
}))
raw_c1 <- t(sapply(log,function(x){c(x$x$new,x$y$new)/(mean(c(x$x$new,x$y$new)+1))}))
log <- raw %>% filter(scope=='global'&!(state%in%c('KoreaSouth','Japan','Singapore','China')))
log <- do.call(c,lapply(unique(log$key),function(K){
  lapply(1:(max(log$i)-p),function(I){
    getlog(I,K,log,p)
  })
}))
raw_c2 <- t(sapply(log,function(x){c(x$x$new,x$y$new)/(mean(c(x$x$new,x$y$new)+1))}))

qpca <- function(A,rank=0,ifscale=TRUE){
  if(ifscale){A <- scale(as.matrix(A))[,]}
  A.svd <- svd(A)
  if(rank==0){
    d <- A.svd$d
  } else {
    d <- A.svd$d-A.svd$d[min(rank+1,nrow(A),ncol(A))]
  }
  d <- d[d > 1e-8]
  r <- length(d)
  prop <- d^2; info <- sum(prop)/sum(A.svd$d^2);prop <- cumsum(prop/sum(prop))
  d <- diag(d,length(d),length(d))
  u <- A.svd$u[,1:r,drop=F]
  v <- A.svd$v[,1:r,drop=F]
  x <- u%*%sqrt(d)
  y <- sqrt(d)%*%t(v)
  z <- x %*% y
  rlt <- list(rank=r,X=x,Y=y,Z=x%*%y,prop=prop,info=info)
  return(rlt)
}
p_ginv_sq <- function(X,p){
  X.eigen = eigen(X);
  X.rank = sum(X.eigen$values>1e-8);
  X.value = X.eigen$values[1:X.rank]^(-1*p);
  if (length(X.value)==1){
    D = as.matrix(X.value);
  }else{
    D = diag(X.value);
  }
  rlt = X.eigen$vectors[,1:X.rank] %*% D %*% t(X.eigen$vectors[,1:X.rank]);
  return(rlt);
}
mrank <- function(X){
  X.svd = svd(X);
  X.rank = sum(X.svd$d>1e-6);
  return(X.rank);
}
mrank_sq <- function(X){
  X.eigen = eigen(X);
  X.rank = sum(Re(X.eigen$values)>1e-6);
  return(X.rank);
}
CCA_chisq_test <- function(rho,n,p,q){
  tstat = -1*n*sum(log(1-rho^2));
  p_value = pchisq(tstat,(p*q),lower.tail=FALSE);
  return(p_value);          
}
cca <- function(A,B){
  n = nrow(A);
  p = mrank(A);
  q = mrank(B);
  if (p <= q){
    X = A;
    Y = B;
  }else{
    X = B;
    Y = A;
  }
  R = p_ginv_sq(cov(X),0.5) %*% cov(X,Y) %*% p_ginv_sq(cov(Y),1) %*% cov(Y,X) %*% p_ginv_sq(cov(X),0.5);
  k = mrank_sq(R);
  d = Re(eigen(R)$values);
  rho = d[1:k]^(0.5);
  rho[rho >= 0.9999]=0.9;
  chisq_p = CCA_chisq_test(rho,n,p,q);
  return(c("chisq_p"=chisq_p,"df"=p*q));
}
y <- cbind(rep(1:0,c(nrow(c1),nrow(c2))))

rlt <- sapply(1:1000,function(i){
  c1 <- raw_c1
  c2 <- raw_c2[sample(nrow(raw_c2),nrow(raw_c1)),]
  # cca(qpca(rbind(c1),rank=which(qpca(scale(rbind(c1)))$prop>0.8)[1])$X,
  #     qpca(rbind(c2),rank=which(qpca(scale(rbind(c2)))$prop>0.8)[1])$X)
  X <- qpca(rbind(c1,c2),rank=which(qpca(scale(rbind(c1,c2)))$prop>0.8)[1])$X
  c(ncol(X),cca(X,y))
}) %>% t


###################################
# China Model
###################################

log <- filter(raw,scope%in%c('china')&fact=='confirmed')
p <- 8
mfile <- do.call(c,lapply(unique(log$key),function(K){
  lapply(1:(max(log$i)-p),function(I){
    getlog(I,K,log,p)
  })
}))
mfile <- rep(mfile,sapply(mfile,function(x){ceiling(x$w/14)}))
X <- (t(sapply(mfile,function(x){
  c(x$x$new/(mean(x$x$new)+1),mean(x$x$d))
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

system.time(model %>% fit(x = X,y = Y,batch = 128,epochs = 1000,verbose = 0))
model %>% fit(x = X,y = Y,batch = 128,epochs = 100,verbose = 2)

# keras::save_model_hdf5(model,paste0('/Users/wenrurumon/Documents/posdoc/wuhan/model/new1.model'),overwrite = TRUE,include_optimizer = TRUE)

###################################
# Global Model
###################################

# model <- keras::load_model_hdf5(paste0('/Users/wenrurumon/Documents/posdoc/wuhan/model/new1.model'))

log <- filter(raw,scope%in%c('china','global')&fact=='confirmed')
p <- 8
mfile <- do.call(c,lapply(unique(log$key),function(K){
  lapply(1:(max(log$i)-p),function(I){
    getlog(I,K,log,p)
  })
}))
# mfile <- rep(mfile,sapply(mfile,function(x){ceiling(x$w/14)}))
X <- (t(sapply(mfile,function(x){
  c(x$x$new/(mean(x$x$new)+1),mean(x$x$d))
})))
Y <- cbind(sapply(mfile,function(x){
  x$y$new/(mean(x$x$new)+1)
}))
model %>% compile(loss = "mean_squared_error", optimizer = "adam",metrics = c('mae'))

system.time(model %>% fit(x = X,y = Y,batch = 128,epochs = 1000,verbose = 0))
model %>% fit(x = X,y = Y,batch = 128,epochs = 100,verbose = 2)

keras::save_model_hdf5(model,paste0('/Users/wenrurumon/Documents/posdoc/wuhan/model/new2.model'),overwrite = TRUE,include_optimizer = TRUE)

###################################
# Validation
###################################

# model <- keras::load_model_hdf5(paste0('/Users/wenrurumon/Documents/posdoc/wuhan/model/new2.model'))

temp1 <- temp <- filter(raw,state=='US'&fact=='confirmed') %>% arrange(i)
D <- NA
J <- Inf
for(j in 1:100){
  # j <- j+1
  tempi <- do.call(rbind,lapply(unique(temp$key),function(K){
    logi <- getlog(max(temp$i)-7,K,temp)
    if(j==1){Di <- 1/J + max(logi$x$d)} else {
      Di <- max(logi$x$d)
    }
    Di <- 1/(1/Di-1)
    Di <- ifelse(Di>1,1,Di)
    # print(Di)
    if(mean(floor(logi$x$new))==0){
      newi<-0
    }else{
      xi <- matrix(c(logi$x$new/(mean(logi$x$new)+1),Di))
      newi <- model %>% predict(t(xi),nrow=1)
      newi <- newi * (mean(logi$x$new)+1)
      newi <- ifelse(newi<0,0,floor(newi))
    }
    accumi <- max(logi$x$accum)+newi
    rlti <- data.frame(
      scope=unique(logi$x$scope),state=unique(logi$x$state),fact=unique(logi$x$fact),
      date=paste(as.POSIXct(max(paste(logi$x$date)))+3600*24),
      accum = accumi,
      new = newi,
      d=Di,
      key=unique(logi$x$key),i=max(logi$x$i)+1
    ) %>% as.data.table
  }))
  # print(tempi)
  temp <- rbind(temp,tempi)
}
plot.ts((temp %>% arrange(i))$new);
lines((temp1 %>% arrange(i))$new,col=2)
sum(temp$new)

###################################
# Prediction2
###################################

# model <- keras::load_model_hdf5(paste0('/Users/wenrurumon/Documents/posdoc/wuhan/model/new2_0401.model'))
rlts <- list()

#1
temp <- filter(raw,scope=='us'&fact=='confirmed') %>% arrange(i)
D <- NA
J <- 7
for(j in 1:(365-(max(temp$i)))){
  tempi <- do.call(rbind,lapply(unique(temp$key),function(K){
    logi <- getlog(max(temp$i)-7,K,temp)
    Di <- ifelse(j>=J,1,1/(J-j))
    if(mean(floor(logi$x$new))==0){
      newi<-0
    }else{
      xi <- matrix(c(logi$x$new/(mean(logi$x$new)+1),Di))
      newi <- model %>% predict(t(xi),nrow=1)
      newi <- newi * (mean(logi$x$new)+1)
      newi <- ifelse(newi<0,0,floor(newi))
    }
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
rlts[[length(rlts)+1]] <- temp

#2
temp <- filter(raw,scope=='us'&fact=='confirmed') %>% arrange(i)
D <- NA
J <- 14
for(j in 1:(365-(max(temp$i)))){
  tempi <- do.call(rbind,lapply(unique(temp$key),function(K){
    logi <- getlog(max(temp$i)-7,K,temp)
    Di <- ifelse(j>=J,1,1/(J-j))
    if(mean(floor(logi$x$new))==0){
      newi<-0
    }else{
      xi <- matrix(c(logi$x$new/(mean(logi$x$new)+1),Di))
      newi <- model %>% predict(t(xi),nrow=1)
      newi <- newi * (mean(logi$x$new)+1)
      newi <- ifelse(newi<0,0,floor(newi))
    }
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
rlts[[length(rlts)+1]] <- temp

#3
temp <- filter(raw,scope=='us'&fact=='confirmed') %>% arrange(i)
D <- NA
J <- 14
for(j in 1:(365-(max(temp$i)))){
  tempi <- do.call(rbind,lapply(unique(temp$key),function(K){
    logi <- getlog(max(temp$i)-7,K,temp)
    Di <- ifelse(j>=J,1,1/(J-j))
    if(mean(floor(logi$x$new))==0){
      newi<-0
    }else{
      xi <- matrix(c(logi$x$new/(mean(logi$x$new)+1),Di))
      newi <- model %>% predict(t(xi),nrow=1)
      newi <- newi * (mean(logi$x$new)+1)
      newi <- ifelse(newi<0,0,floor(newi))
    }
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
rlts[[length(rlts)+1]] <- temp

#4
temp <- filter(raw,scope=='us'&fact=='confirmed') %>% arrange(i)
D <- NA
J <- 28
for(j in 1:(365-(max(temp$i)))){
  tempi <- do.call(rbind,lapply(unique(temp$key),function(K){
    logi <- getlog(max(temp$i)-7,K,temp)
    Di <- ifelse(j>=J,1,1/(J-j))
    if(mean(floor(logi$x$new))==0){
      newi<-0
    }else{
      xi <- matrix(c(logi$x$new/(mean(logi$x$new)+1),Di))
      newi <- model %>% predict(t(xi),nrow=1)
      newi <- newi * (mean(logi$x$new)+1)
      newi <- ifelse(newi<0,0,floor(newi))
    }
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
rlts[[length(rlts)+1]] <- temp

rlts[[1]] %>% group_by(state) %>% filter(d==1)
