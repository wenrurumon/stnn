
############################
# Module
############################

rm(list=ls())
library(plyr)
library(openxlsx)
library(data.table)
library(dplyr)
library(keras)
library(MASS)

#Get Model File
get_model_file <- function(x,i,p,gety=TRUE){
  if(gety){y <- t(x[p+i,,drop=F])}
  x <- t(x[1:p+i-1,,drop=F])
  if(gety){y[y<0] <- 0}
  x[x<0] <- 0
  f <- rowMeans(x)
  if(gety){y <- y/f; y[is.na(y)] <- 0; y[y==Inf] <- 0}
  x <- x/f; x[is.na(x)] <- 0
  if(!gety){y <- NULL}
  list(x=x,y=y,f=f,i=i)
}

get_model_xy <- function(x,p,gety,w,sel){
  out <- lapply(1:(nrow(x)-p-gety-sel),get_model_file,x=x,p=p,gety=gety)
  out <- rep(out,ceiling(sapply(out,function(x){x$i})/w))
  X <- do.call(rbind,lapply(out,function(x){x$x}))
  Y <- do.call(rbind,lapply(out,function(x){x$y}))
  list(Y=Y,X=X)
}

#MSAE
MSAE <- function(X,Y,dims,activations,batch,epochs,verbose){
  e.input <- layer_input(shape=ncol(X))
  e.layer <- layer_dense(e.input,dims[1],activation=activations[1])
  l.layer <- layer_dense(e.layer,dims[2],activation=activations[2])
  d.output <- layer_dense(units=ncol(Y),activation=NULL)
  model <- keras_model(e.input,d.output(l.layer))
  encoder <- keras_model(e.input,l.layer)
  d.input <- layer_input(shape=dims[2])
  decoder <- keras_model(d.input,d.output(d.input))
  model %>% compile(
    loss = "mean_squared_error", 
    optimizer = "adam",
    metrics = c('mae')
  )
  system.time(history <- model %>% fit(
    x = X, 
    y = Y,
    batch = batch,
    epochs = epochs,
    verbose = verbose
  ))
  list(model=model,encoder=encoder,decoder=decoder,history=history)
}

############################
# Data Processing
############################

setwd('/Users/wenrurumon/Documents/posdoc/wuhan')
raw1 <- read.csv("china_confirmed.csv")[,-1]
raw2 <- read.csv("global_confirmed.csv")[,-1:-2]
raw.date <- raw1[,1]
raw <- list(
  raw1,
  rbind(matrix(0,nrow=10,ncol=ncol(raw2)),as.matrix(raw2))
)
raw <- do.call(cbind,raw)[,1:35]
Y.actual <- raw[,-1]
Y.model <- apply(Y.actual,2,diff)

############################
# Validation
############################

mfile.vali <- lapply(1:10,function(sel){
  print(sel)
  mfile <- get_model_xy(Y.model,p=8,gety=T,w=12,sel=sel)
  models.vali <- lapply(1:5,function(i){
    print(paste(i,Sys.time()))
    MSAE(X=mfile$X,Y=mfile$Y,
         dims=c(32,4),activations=c('relu','relu'),
         batch=128,epochs=1000,verbose=0)
  })
  mfile.vali <- lapply((nrow(Y.model)-9):0,function(i){
    get_model_file(x=Y.model,i=nrow(Y.model)-8-i,p=8,gety=FALSE)
  })
  mfile.vali <- rowMeans(sapply(models.vali,function(m){
    sapply(mfile.vali,function(x){
      sum((m$model %>% predict(x$x)) * x$f)
    })
  }))
  mfile.vali <- cbind(
    actual = rowSums(Y.model)[-1:-8],
    predict = ifelse(mfile.vali<0,0,mfile.vali)
  )
  for(i in 1:(nrow(mfile.vali)-sel)){
    mfile.vali[i,] <- mfile.vali[i,] + rowSums(Y.actual)[i+8]
  }
  for(i in (-(sel-1):0)+nrow(mfile.vali)){
    mfile.vali[i,1] <- mfile.vali[i,1] + rowSums(Y.actual)[i+8]
    mfile.vali[i,2] <- mfile.vali[i,2] + mfile.vali[i-1,2]
  }
  mfile.vali <- cbind(mfile.vali,error=mfile.vali[,2]/mfile.vali[,1]-1)
  c(rep(NA,nrow(mfile.vali)-sel),mfile.vali[-(1:(nrow(mfile.vali)-sel)),2])
})
mfile.vali <- cbind(actual=rowSums(Y.actual)[-1:-9],do.call(cbind,mfile.vali))
mfile.vali <- data.table(date=as.POSIXct('2020-01-19') + (1:nrow(mfile.vali))*3600*24,
                         mfile.vali)
write.csv(mfile.vali,'summary_temp/validateion.csv')

############################
# Prediction
############################  

setwd('summary_temp')

# set.seed(4)
# mfile <- get_model_xy(Y.model,p=8,gety=T,w=12,sel=sel)
# models <- lapply(1:5,function(i){
#   print(paste(i,Sys.time()))
#   MSAE(X=mfile$X,Y=mfile$Y,
#        dims=c(32,4),activations=c('relu','relu'),
#        batch=128,epochs=4000,verbose=0)
# })
# for(i in 1:length(models)){
#   save_model_hdf5(models[[i]]$model,paste0('mmodel',i,'.model')
#                   , overwrite = TRUE,include_optimizer = TRUE)
#   save_model_hdf5(models[[i]]$encoder,paste0('mmodel',i,'.encoder')
#                   , overwrite = TRUE,include_optimizer = TRUE)
# }

models <- lapply(1:5,function(i){list()})
models[[1]]$model <- keras::load_model_hdf5('mmodel1.model')
models[[1]]$encoder <- keras::load_model_hdf5('mmodel1.encoder')
models[[2]]$model <- keras::load_model_hdf5('mmodel2.model')
models[[2]]$encoder <- keras::load_model_hdf5('mmodel2.encoder')
models[[3]]$model <- keras::load_model_hdf5('mmodel3.model')
models[[3]]$encoder <- keras::load_model_hdf5('mmodel3.encoder')
models[[4]]$model <- keras::load_model_hdf5('mmodel4.model')
models[[4]]$encoder <- keras::load_model_hdf5('mmodel4.encoder')
models[[5]]$model <- keras::load_model_hdf5('mmodel5.model')
models[[5]]$encoder <- keras::load_model_hdf5('mmodel5.encoder')

setwd('..')

x <- rbind(Y.actual[1,],Y.model)
rlt <- matrix(NA,0,ncol(x))
while(nrow(rlt)<300){
  temp <- get_model_file(x=x,i=nrow(x)-7,p=8,gety=F)
  out <- rowMeans(sapply(models,function(m){(m$model %>% predict(temp$x)) * temp$f}))
  out <- ifelse(out<0,0,out)
  x <- rbind(x,out)
  rlt <- rbind(rlt,out)
}
x$shandong[x$shandong==202] <- 0
x$fujian[-1:-45] <- 0
x$ningxia[-1:-50] <- 0
x <- x[1:(apply(x,2,function(x){
  max(which(diff(ceiling(cumsum(x)))>0))
}) %>% max+1),]
apply(x,2,sum)/colSums(Y.model)
x.date <- as.POSIXct('2020-01-10') + (1:nrow(x))*3600*24

############################
# Analyzing
############################ 

#Prediction

x.fit <- c(
  58,rowSums(Y.model)[1:8],
  sapply(lapply(1:(nrow(Y.model)-8),get_model_file,p=8,gety=F,x=Y.model),function(x){
    x <- mean(sapply(models,function(m){
      sum((m$model %>% predict(x$x)) * x$f)
    }))
    ifelse(x<0,0,x)})
  )
x.actual <- rowSums(x)
x.actual <- cumsum(x.actual)
for(i in 2:length(x.fit)){x.fit[i] <- x.fit[i] + x.actual[i-1]}
rlt.nat <- rbind(
  data.frame(date=x.date[1:length(x.fit)],actual=x.actual[1:length(x.fit)],fit=x.fit),
  data.frame(date=x.date[-1:-length(x.fit)],actual=NA,fit=x.actual[-1:-length(x.fit)]))
write.csv(rlt.nat,'summary_temp/prediction_national.csv')
write.csv(data.frame(date=x.date,ceiling(apply(x,2,cumsum))),
          'summary_temp/prediction_by_city.csv')

#Featuring

idx <- apply(apply(ceiling(apply(x,2,cumsum)),2,diff),2,function(x){
  c(now = cumsum(x)[nrow(Y.actual)],
    max = max(x),
    sum = sum(x),
    start = which(x!=0)[1],
    peak = which(x==max(x))[1],
    end = max(which(x!=0)),
    end1 = which((cumsum(x)/sum(x))>0.25)[1],
    end2 = which((cumsum(x)/sum(x))>0.50)[1],
    end3 = which((cumsum(x)/sum(x))>0.75)[1],
    end4 = which((cumsum(x)/sum(x))>0.99)[1])
}) %>% t
colnames(idx) <- c('now','max','sum','start','peak','end','end1','end2','end3','end4')
mfile <- get_model_xy(apply(ceiling(apply(x,2,cumsum)),2,diff),p=8,gety=F,w=1000,sel=0)
mfile.city <- rownames(mfile$X)
mfile <- lapply(models,function(m){m$encoder %>% predict(mfile$X)})
idx <- cbind(idx,sapply(mfile,function(x){
  sapply(unique(mfile.city),function(city){
    svd(x[mfile.city==city,])$d[1]
  })
}))
colnames(idx)[-1:-10] <- paste0('d',1:5)

#Clustering

idx <- data.frame(idx)
set.seed(1); idx_sizecluster <- kmeans(scale(idx$sum),9,iter.max=1000)
idx_sizecluster <- match(paste(idx_sizecluster$cluster),names(sort(-tapply(idx$sum,idx_sizecluster$cluster,mean))))
idx$init <- NA
idx$init[rownames(idx)%in%'hubei'] <- 1
idx$init[rownames(idx)%in%strsplit('shanghai,beijing,hong_kong_sar',',')[[1]]] <- 2
idx$init[rownames(idx)%in%strsplit('jiangxi,guangdong,zhejiang,henan,',',')[[1]]] <- 3
idx$init[rownames(idx)%in%strsplit('inner_mongolia,xizang',',')[[1]]] <- 4
idx$init[rownames(idx)%in%strsplit('heilongjiang,jilin,liaoning',',')[[1]]] <- 5
idx$init[rownames(idx)%in%strsplit('anhui,hebei,fujian,shandong',',')[[1]]] <- 6

set.seed(5);idx.kmeans <- kmeans(predict(lda(init~sum+peak+end+d1+d2+d3+d4+d5,data=filter(idx,!is.na(init))),idx)$x,9,iter.max = 1000)
lapply(unique(idx.kmeans$cluster),function(x){
  names(idx.kmeans$cluster[idx.kmeans$cluster==x])
})
idx$init <- match(paste(idx.kmeans$cluster),names(sort(tapply(-idx$sum,idx.kmeans$cluster,mean))))
idx$size <- idx_sizecluster
colnames(idx)[16] <- 'feature'
idx <- data.frame(city=rownames(idx),idx)
write.csv(idx,'summary_temp/city_idx.csv')

############################
# Charting
############################ 

#Province Clustering

library(maptools)
library(ggplot2)
library(grid)
citymap <- read.xlsx('data0209.xlsx',sheet='citymap')
str.crs <- "+proj=longlat +ellps=clrk66"
china_map <- readShapePoly("chinamap/bou2_4p.shp",proj4string=CRS(str.crs))
china_province = setDT(china_map@data)
setnames(china_province, "NAME", "province")
china_province[, province:=iconv(province, from = "GBK", to = "UTF-8")] 
china_province[, id:= .I-1] 
china_province[, table(province)]
china_province[, province:= as.factor(province)]
dt_china = setDT(fortify(china_map))
dt_china[, id:= as.numeric(id)]
setkey(china_province, id); setkey(dt_china, id)
dt_china <- china_province[dt_china]
province_CH <- china_province[, levels(province)] 
province_CH <- cbind(sort(province_CH),sort(citymap[-c(33,35),]$zh_name))[,2]
province_EN <- citymap$city[match(province_CH,citymap$zh_name)]
value <- data.frame(citych = province_CH, city = province_EN,
                    long = tapply(dt_china$long,dt_china$province,median),
                    lat = tapply(dt_china$lat,dt_china$province,median))
value <- as.data.frame(rbind(as.matrix(value),c(citych='澳门',city='macao_sar',long=113.5439,lat=22.1987)))
value <- merge(value,idx,by=c('city')) %>% mutate(city=toupper(city))
value$long <- as.numeric(paste(value$long))
value$lat <- as.numeric(paste(value$lat))
value$city[13] <- 'HONGKONG'
value$city[21] <- 'MACAO'
value$city[29] <- 'TAIWAN'
value$pos <- strsplit('s,a,a,d,w,s,a,a,d,a,
a,a,d,d,s,a,d,d,a,d,
w,w,a,s,d,d,w,w,d,d,d,
d,a,d',',|,\n')[[1]]
ggplot(data=value, aes(x=long, y=lat, color=paste(feature))) + 
  geom_point(size=3) + 
  geom_text(data=filter(value,pos=='a'),aes(x=long-nchar(city)/2.8,y=lat,label=city))+
  geom_text(data=filter(value,pos=='s'),aes(x=long,y=lat-1,label=city))+
  geom_text(data=filter(value,pos=='d'),aes(x=long+nchar(city)/3,y=lat,label=city))+
  geom_text(data=filter(value,pos=='w'),aes(x=long,y=lat+1,label=city))+
  labs(x='Latitude',y='Longitude',title = "Feature Clustering") +
  theme(plot.title = element_text(hjust = 0.5)) 

sapply(1:9,function(i){
  paste0('Cluster',i,': ',paste(filter(value,feature==i)$city,collapse=', '))
})


ggplot(data=value, aes(x=long, y=lat, color=paste(size))) + 
  geom_point(size=3) + 
  geom_text(data=filter(value,pos=='a'),aes(x=long-nchar(city)/2.8,y=lat,label=city))+
  geom_text(data=filter(value,pos=='s'),aes(x=long,y=lat-1,label=city))+
  geom_text(data=filter(value,pos=='d'),aes(x=long+nchar(city)/3,y=lat,label=city))+
  geom_text(data=filter(value,pos=='w'),aes(x=long,y=lat+1,label=city))+
  labs(x='Latitude',y='Longitude',title = "Size Clustering") +
  theme(plot.title = element_text(hjust = 0.5)) 

#National chart
rlt <- data.frame(rlt.nat[1:nrow(Y.actual),])
ggplot() + geom_line(aes(x=rlt[,1],y=rlt[,2],colour='Actual')) +
  geom_line(aes(x=rlt[,1],y=rlt[,3],colour='Fitted')) + 
  labs(x='Day',y='Accumulated Confirmed Cases',title = "Fit Chart") +
  theme(plot.title = element_text(hjust = 0.5)) 
ggplot() + geom_line(aes(x=rlt[-nrow(rlt),1],y=diff(rlt[,2]),colour='Actual')) +
  geom_line(aes(x=rlt[-nrow(rlt),1],y=diff(rlt[,3]),color='Fitted')) + 
  labs(x='Day',y='New Confirmed Cases',title = "Fit Chart") +
  theme(plot.title = element_text(hjust = 0.5))
ggplot() + geom_line(aes(x=rlt.nat[,1],y=(rlt.nat[,2]),colour='Actual')) +
  geom_line(aes(x=rlt.nat[,1],y=(rlt.nat[,3]),colour='Prediction')) + 
  labs(x='Day',y='Accumulated Confirmed Cases',title = "Prediction Chart") +
  theme(plot.title = element_text(hjust = 0.5))

#City Chart 9

grid.newpage() 
pushViewport(viewport(layout = grid.layout(3,3))) 
vplayout <- function(x,y){viewport(layout.pos.row = x, layout.pos.col = y)} 
out <- data.frame(x.date,apply(x,2,cumsum))
out.clust <- idx$size
colnames(out) <- toupper(colnames(out))
colnames(out)[8] <- 'TAIWAN'
colnames(out)[14] <- 'HONGKONG'
colnames(out)[25] <- 'MACAO' 

i <- 1
temp <- out[,c(1,which(out.clust==i)+1)]; ncol(temp); colnames(temp)[2]
p <- ggplot() + geom_line(aes(x=temp[,1],y=temp[,2],colour='HUBEI'))+ 
  labs(x='',y='',title = "") +
  theme(plot.title = element_text(hjust = 0.5))
print(p, vp = vplayout(1,1))
i <- 2
temp <- out[,c(1,which(out.clust==i)+1)]; ncol(temp); colnames(temp)[2]
p <- ggplot() + geom_line(aes(x=temp[,1],y=temp[,2],colour='GUANGDONG')) + 
  geom_line(aes(x=temp[,1],y=temp[,3],colour=colnames(temp)[3]))+ 
  geom_line(aes(x=temp[,1],y=temp[,4],colour=colnames(temp)[4]))+ 
  labs(x='',y='',title = "") +
  theme(plot.title = element_text(hjust = 0.5))
print(p, vp = vplayout(1,2))
i <- 3
temp <- out[,c(1,which(out.clust==i)+1)]; ncol(temp); colnames(temp)[2]
p <- ggplot() + geom_line(aes(x=temp[,1],y=temp[,2],colour='ANHUI')) + 
  geom_line(aes(x=temp[,1],y=temp[,3],colour=colnames(temp)[3]))+ 
  geom_line(aes(x=temp[,1],y=temp[,4],colour=colnames(temp)[4]))+ 
  labs(x='',y='',title = "") +
  theme(plot.title = element_text(hjust = 0.5))
print(p, vp = vplayout(1,3))
i <- 4
temp <- out[,c(1,which(out.clust==i)+1)]; ncol(temp); colnames(temp)[2]
p <- ggplot() + geom_line(aes(x=temp[,1],y=temp[,2],colour='SHANDONG'))+  
  geom_line(aes(x=temp[,1],y=temp[,3],colour=colnames(temp)[3]))+ 
  geom_line(aes(x=temp[,1],y=temp[,4],colour=colnames(temp)[4]))+ 
  labs(x='',y='',title = "") +
  theme(plot.title = element_text(hjust = 0.5))
print(p, vp = vplayout(2,1))
i <- 5
temp <- out[,c(1,which(out.clust==i)+1)]; ncol(temp); colnames(temp)[2]
p <- ggplot() + geom_line(aes(x=temp[,1],y=temp[,2],colour='SICHUAN'))+ 
  geom_line(aes(x=temp[,1],y=temp[,3],colour=colnames(temp)[3]))+ 
  labs(x='',y='',title = "") +
  theme(plot.title = element_text(hjust = 0.5))
print(p, vp = vplayout(2,2))
i <- 6
temp <- out[,c(1,which(out.clust==i)+1)]; ncol(temp); colnames(temp)[2]
p <- ggplot() + geom_line(aes(x=temp[,1],y=temp[,2],colour='SHANGHAI'))+ 
  geom_line(aes(x=temp[,1],y=temp[,3],colour=colnames(temp)[3]))+ 
  geom_line(aes(x=temp[,1],y=temp[,4],colour=colnames(temp)[4]))+ 
  labs(x='',y='',title = "") +
  theme(plot.title = element_text(hjust = 0.5))
print(p, vp = vplayout(2,3))
i <- 7
temp <- out[,c(1,which(out.clust==i)+1)]; ncol(temp); colnames(temp)[2]
p <- ggplot() + geom_line(aes(x=temp[,1],y=temp[,2],colour='GUANGXI'))+ 
  geom_line(aes(x=temp[,1],y=temp[,3],colour=colnames(temp)[3]))+ 
  geom_line(aes(x=temp[,1],y=temp[,4],colour=colnames(temp)[4]))+ 
  labs(x='',y='',title = "") +
  theme(plot.title = element_text(hjust = 0.5))
print(p, vp = vplayout(3,1))
i <- 8
temp <- out[,c(1,which(out.clust==i)+1)]; ncol(temp); colnames(temp)[2]
p <- ggplot() + geom_line(aes(x=temp[,1],y=temp[,2],colour='YUNNAN'))+ 
  geom_line(aes(x=temp[,1],y=temp[,3],colour=colnames(temp)[3]))+ 
  geom_line(aes(x=temp[,1],y=temp[,4],colour=colnames(temp)[4]))+ 
  geom_line(aes(x=temp[,1],y=temp[,5],colour=colnames(temp)[5]))+ 
  geom_line(aes(x=temp[,1],y=temp[,6],colour=colnames(temp)[6]))+ 
  geom_line(aes(x=temp[,1],y=temp[,7],colour=colnames(temp)[7]))+ 
  geom_line(aes(x=temp[,1],y=temp[,8],colour=colnames(temp)[8]))+ 
  labs(x='',y='',title = "") +
  theme(plot.title = element_text(hjust = 0.5))
print(p, vp = vplayout(3,2))
i <- 9
temp <- out[,c(1,which(out.clust==i)+1)]; ncol(temp); colnames(temp)[2]
p <- ggplot() + geom_line(aes(x=temp[,1],y=temp[,2],colour='TAIWAN'))+ 
  geom_line(aes(x=temp[,1],y=temp[,3],colour=colnames(temp)[3]))+ 
  geom_line(aes(x=temp[,1],y=temp[,4],colour=colnames(temp)[4]))+ 
  geom_line(aes(x=temp[,1],y=temp[,5],colour=colnames(temp)[5]))+ 
  geom_line(aes(x=temp[,1],y=temp[,6],colour=colnames(temp)[6]))+ 
  geom_line(aes(x=temp[,1],y=temp[,7],colour=colnames(temp)[7]))+ 
  geom_line(aes(x=temp[,1],y=temp[,8],colour=colnames(temp)[8]))+ 
  geom_line(aes(x=temp[,1],y=temp[,9],colour=colnames(temp)[9]))+ 
  geom_line(aes(x=temp[,1],y=temp[,10],colour=colnames(temp)[10]))+ 
  labs(x='',y='',title = "") +
  theme(plot.title = element_text(hjust = 0.5))
print(p, vp = vplayout(3,3))

