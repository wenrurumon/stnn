
rm(list=ls())
library(openxlsx)
library(data.table)
library(dplyr)
library(keras)
check <- function(x){data.frame(date=datemap,raw[names(raw)==x])}
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
qqplot <- function(p_value){
  n = length(p_value);
  exp = -log10((c(1:n)-0.5)/n);
  rgen = -log10(sort(p_value));
  plot(exp,rgen,xlab="-log10(Expect)",ylab="-log10(Real)");
  abline(0,1,col="red")
}
ccap <- function(l1,l2){
  rlt <- sapply(l2,function(x2){
    sapply(l1,function(x1){
      cca(x1,x2)[[1]]
    })
  })
  dimnames(rlt) <- list(names(l1),sapply(l2,function(x){colnames(x)[1]}))
  rlt
}
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
qpca2 <- function(x,p=0.99){
  A <- qpca(x)
  A <- qpca(x,rank=which(A$prop>p)[1])$X
  A
}
library(igraph)
plotnet <- function(x,mode='undirected'){
  diag(x) <- 0
  plot(graph_from_adjacency_matrix(t(x),mode=mode),
       edge.arrow.size=.1,
       vertex.size=3,
       vertex.label.cex=1,
       edge.width=1)
}
fc <- function(x){
  w<-as.vector(t(x))[t(x)>0]
  x <- graph_from_adjacency_matrix(x>0,mode='undirected')
  fc <- membership(fastgreedy.community(x,weight=w))
  fc[] <- match(fc,unique(fc))
  fc
}
fc2 <- function(x){
  x.mat <- (x<0.01/length(x))+0
  diag(x.mat) <- 0
  x.score <- -log(x)
  # x.score <- log(2^x)
  x.score[x.mat==0] <- 0
  x.score[x.score==Inf] <- max(x.score[x.score!=Inf]*2)
  x.score <- x.score/max(x.score)
  x.g <- graph_from_adjacency_matrix(x.mat,mode='directed')
  E(x.g)$weight <- as.vector(x.score)[x.mat>0]
  x.g <- as.undirected(x.g)
  # plotclust(x.mat,rlt <- fastgreedy.community(x.g)$membership,main=main)
  list(network = x.mat,
       cluster = fastgreedy.community(x.g)$membership)
}
plotclust <- function(x,membership=NULL,main=NULL){
  G <- graph_from_adjacency_matrix(x>0,mode='undirected')
  if(is.null(membership)){membership=rep(1,ncol(x))}
  plot(create.communities(G, membership), 
       # as.undirected(G), 
       as.directed(G),
       layout=layout.kamada.kawai(as.undirected(G)),
       edge.arrow.size=.1,
       vertex.size=.3,
       vertex.label.cex=1,
       edge.width=.1,
       main=main)
}
plotclust2 <- function(p.clust){
  library(networkD3)
  g <- p.clust$network>0
  g <- apply(g,2,function(x){
    names(which(x))
  })
  g2 <- p.clust$cluster
  names(g2) <- names(g)
  g2[] <- sapply(unique(g2),function(i){
    paste(names(which.max(sapply(g[g2==i],length))),collapse=',')
  })[match(g2,unique(g2))]
  tmp <- matrix(0,0,3)
  colnames(tmp) <- c('source','target','value')
  for(i in 1:length(g)){
    tmp <- rbind(tmp,cbind(names(g)[i],names(g)[i],TRUE))
    if(length(g[[i]])>0){tmp <- rbind(tmp,cbind(names(g)[i],g[[i]],TRUE))}
  }
  plink3 <- as.data.frame(tmp)
  pnode2 <- data.frame(name=names(g2),group=g2,size=1)
  # return(apply(pnode2,2,paste))
  plink3$source <- match(plink3$source,pnode2$name)-1
  plink3$target <- match(plink3$target,pnode2$name)-1
  forceNetwork(Links = plink3, Nodes = pnode2, Source = "source",
               Target = "target", Value = "value", NodeID = "name",
               Nodesize = "size",linkColour = "#999",
               radiusCalculation = "Math.sqrt(d.nodesize)+6",
               Group = "group", opacity =20,charge=-10, legend = T
               ,zoom=T,opacityNoHove=100) 
}


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
# WHO data - top6
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
data.who <- sapply(who[1:6,2],read.who)
data.who <- as.data.table(t(data.who[match(colnames(raw),rownames(data.who)),]))
colnames(data.who)[1] <- 'date'
data.who[1:6,1] <- as.numeric(paste0(who[1:6,1]))

#Process data model

raw <- rbind(filter(raw,date<206),data.who)
Y.actual <- raw[,-1]
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
mean2 <- function(x){
  x <- sort(x[!is.na(x)])
  x <- x[-c(1,length(x))]
  mean(x)
}

############################
# Auto Forecasting
############################

# dims <- c(32,4)
# drops <- rep(0,2)
# activations <- c('relu','relu')
# mfile <- lapply(1:22,get_model_file,h=1,p=8,x=Y.model)
# w <- 1
# X <- do.call(rbind,lapply(rep(mfile[1:(length(mfile)-3)],w),function(x){x$x}))
# Y <- do.call(rbind,lapply(rep(mfile[1:(length(mfile)-3)],w),function(x){x$y}))

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

p <- 8

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
sel <- rep(1:5,each=5)
dummies <- lapply(sel,dummy_data,p=p,h=1,x=Y.model)

#Auto Forecasting Modeling

j <- 0
test2 <- lapply(dummies,function(d){
  print(paste(j<<-j+1,Sys.time()))
  autoforecasting2(d$X,d$Y,p=p,h=1,dims=c(32,4),activations=rep('relu',2),drops=rep(0,2))
})

#Validational Prediction

Xs <- lapply(1:(nrow(Y.actual)-p),get_x,x=Y.model,p=p)
Y.fits2 <- sapply(test2,function(rlti){
  sapply(Xs,function(xi){
    sum((rlti$model %>% predict(xi$x))*xi$f)
  })
})
Y.fits2[,colSums(Y.fits2)==0] <- NA
out2 <- cbind()
for(i in 1:length(unique(sel))){
  temp <- Y.fits2[,1:(length(sel)/length(unique(sel)))+(i-1)*(length(sel)/length(unique(sel)))]
  for(j in 1:((nrow(temp)-(i-1)))){
    temp[j,] <- temp[j,] + rowSums(Y.actual)[-1:-(p-1)][j]
  }
  if(i>1){
    for(j in (nrow(temp)-(i-2)):(nrow(temp))){
      temp[j,] <- temp[j-1,]+temp[j,]
    }
  }
  Y.fits2[,1:(length(sel)/length(unique(sel)))+(i-1)*(length(sel)/length(unique(sel)))] <- temp
  out2 <- cbind(out2,rowMeans(temp,na.rm=T))
  # out2 <- cbind(out2,apply(temp,1,mean2))
}
Y.fits <- cbind(rowSums(Y.actual)[-1:-p],out2) ; Y.fits
round(abs(Y.fits / Y.fits[,1] -1)[,-1],4)*100
# write.csv(Y.fits,'summary0212/validation.csv')
                
############################
# Prediction on p = 8
############################  

p <- 8
s <- 5

#Modeling

mfile <- dummy_data(sel=0,p=p,h=1,x=Y.model)
# mmodel <- lapply(1:s,function(i){
#   print(paste(i,Sys.time()))
#   autoforecasting2(mfile$X,mfile$Y,p=p,h=1,dims=c(32,4),activation=rep('relu',2),drops=rep(0,2))
# })
# save_model_hdf5(mmodel[[1]]$model,'mmodel1.model', overwrite = TRUE,include_optimizer = TRUE)
# save_model_hdf5(mmodel[[2]]$model,'mmodel2.model', overwrite = TRUE,include_optimizer = TRUE)
# save_model_hdf5(mmodel[[3]]$model,'mmodel3.model', overwrite = TRUE,include_optimizer = TRUE)
# save_model_hdf5(mmodel[[4]]$model,'mmodel4.model', overwrite = TRUE,include_optimizer = TRUE)
# save_model_hdf5(mmodel[[5]]$model,'mmodel5.model', overwrite = TRUE,include_optimizer = TRUE)
# save_model_hdf5(mmodel[[1]]$encoder,'mmodel1.encoder', overwrite = TRUE,include_optimizer = TRUE)
# save_model_hdf5(mmodel[[2]]$encoder,'mmodel2.encoder', overwrite = TRUE,include_optimizer = TRUE)
# save_model_hdf5(mmodel[[3]]$encoder,'mmodel3.encoder', overwrite = TRUE,include_optimizer = TRUE)
# save_model_hdf5(mmodel[[4]]$encoder,'mmodel4.encoder', overwrite = TRUE,include_optimizer = TRUE)
# save_model_hdf5(mmodel[[5]]$encoder,'mmodel5.encoder', overwrite = TRUE,include_optimizer = TRUE)

mmodel <- lapply(1:s,function(i){list()})
mmodel[[1]]$model <- keras::load_model_hdf5('summary0211/mmodel1.model')
mmodel[[2]]$model <- keras::load_model_hdf5('summary0211/mmodel2.model')
mmodel[[3]]$model <- keras::load_model_hdf5('summary0211/mmodel3.model')
mmodel[[4]]$model <- keras::load_model_hdf5('summary0211/mmodel4.model')
mmodel[[5]]$model <- keras::load_model_hdf5('summary0211/mmodel5.model')
mmodel[[1]]$encoder <- keras::load_model_hdf5('summary0211/mmodel1.encoder')
mmodel[[2]]$encoder <- keras::load_model_hdf5('summary0211/mmodel2.encoder')
mmodel[[3]]$encoder <- keras::load_model_hdf5('summary0211/mmodel3.encoder')
mmodel[[4]]$encoder <- keras::load_model_hdf5('summary0211/mmodel4.encoder')
mmodel[[5]]$encoder <- keras::load_model_hdf5('summary0211/mmodel5.encoder')

#Resulting

Xs <- lapply(1:(nrow(Y.actual)-p),get_x,x=Y.model,p=p)
Y.fits <- sapply(mmodel,function(rlti){
  sapply(Xs,function(xi){
    sum((rlti$model %>% predict(xi$x))*xi$f)
  })
}) + rowSums(Y.actual[-1:-p,])

data.frame(actual = c(rowSums(Y.actual)[-1:-(p+1)],NA),
      fit = rowMeans(Y.fits))

out <- cbind()
Xi <- get_x(nrow(Y.model)-(p-1),x=Y.model,p=p)
out <- cbind(out,sapply(mmodel,function(rlti){
  (rlti$model %>% predict(Xi$x)) * Xi$f
}) %>% rowMeans)
rbind(
  today=round(rowSums(Y.actual)[nrow(Y.actual)]),
  add = round(sum(out)),
  tomorrow = round(rowSums(Y.actual)[nrow(Y.actual)] + sum(out))
)
while(ncol(out)<(200-23)){
  temp <- cbind(Xi$x*Xi$f,out[,ncol(out)])[,-1]
  Xi <- get_x(1,x=t(temp),p=p)
  out <- cbind(out,sapply(mmodel,function(rlti){
    (rlti$model %>% predict(Xi$x)) * Xi$f
  }) %>% rowMeans)
}
out[out<0] <- 0

#Futuring

par(mfrow=c(1,1))
(rlt.nat <- rbind(
  data.frame(actual = c(rowSums(Y.actual)[-1:-(p+1)],NA),
             fit = rowMeans(Y.fits)),
  data.frame(actual=NA,
             fit = rowSums(Y.actual)[nrow(Y.actual)]+cumsum(colSums(out))[-1]
  )
)) %>% plot.ts()
# write.csv(rlt.nat,'summary0212/prediction_national.csv')

#Futuring by city

out <- rbind(as.matrix(Y.actual)[1,],Y.model,t(out))
out <- apply(out,2,cumsum)
colnames(out) <- citymap$city[match(colnames(out),citymap$zh_name)]
out <- out[1:nrow(rlt.nat),]
par(mfrow=c(3,3))
for(i in 1:ncol(out)){plot.ts(out[,i],main=colnames(out)[i],ylab=NULL)}
par(mfrow=c(1,1))
# write.csv(out,'summary0212/prediction_by_city.csv')

############################
# Further Analysis
############################  

#City Featuring

out <- round(out)
out <- apply(out,2,diff)
city.pattern <- apply(out,2,function(x){
  x.sel <- cumsum(x[min(which(x!=0)):max(which(x!=0))])
  x.fun <- splinefun(x=(1:length(x.sel))/length(x.sel),y=x.sel)
  x <- x.fun((1:100)/100)
  x <- (x-min(x))/(max(x)-min(x))
  ifelse(is.na(x),0,x)
})
par(mfrow=c(1,1))
plot(hclust(dist(t(city.pattern))),main='Pattern Clustering')

city.idx <- apply(out,2,function(x){
  c(max = max(x),
    sum = sum(x),
    start = which(x!=0)[1],
    peak = which(x==max(x))[1],
    end = max(which(x!=0)))
}) %>% t


#Clustering

Xs <- lapply(1:(nrow(Y.actual)-p),get_x,x=Y.model,p=p)
Xs <- do.call(rbind,lapply(Xs,function(x){x$x}))
l.Xs <- lapply(mmodel,function(rlti){
  rlti$encoder %>% predict(Xs)
})
l.Xs <- lapply(l.Xs,function(xi){
  xi <- lapply(unique(rownames(Xs)),function(i){
    xi[rownames(Xs)==i,]
  })
  names(xi) <- unique(rownames(Xs))
  xi
})

city.idx <- cbind(city.idx,sapply(l.Xs,function(xi){
  sapply(xi,function(xi){
    svd(xi)$d[1]
  })
}))
colnames(city.idx)[-1:-5] <- paste0('d',1:5)
plot(hclust(dist(scale(city.idx))),main='Feature Clustering')
# write.csv(city.idx,'summary0212/city_idx.csv')

############################
# Plots
############################  

#Clustering CNMAP

library(maptools)
library(ggplot2)
library(plyr)

set.seed(1)
city.kmeans <- lapply(1:10000,function(i){
  (city.kmeans <- kmeans(t(city.pattern),5,iter.max=10000)) 
  outer(city.kmeans$cluster,city.kmeans$cluster,'==')
})
for(i in 2:10000){city.kmeans[[1]] <- city.kmeans[[1]] + city.kmeans[[i]]}
city.kmeans.net <- ((1-city.kmeans[[1]]/10000)<0.1)
city.kmeans <- fc(city.kmeans.net)
diag(city.kmeans.net) <- 0
lapply(unique(city.kmeans),function(i){names(which(city.kmeans==i))})
plotclust((city.kmeans.net),city.kmeans)
city.idx <- cbind(city.idx,cluster=city.kmeans) 

city.kmeans <- lapply(1:10000,function(i){
  (city.kmeans <- kmeans((city.idx[,]),7,iter.max=10000)) 
  outer(city.kmeans$cluster,city.kmeans$cluster,'==')
})
for(i in 2:10000){city.kmeans[[1]] <- city.kmeans[[1]] + city.kmeans[[i]]}
city.kmeans.net <- ((1-city.kmeans[[1]]/10000)<0.2)
city.kmeans <- fc(city.kmeans.net)
diag(city.kmeans.net) <- 0
lapply(unique(city.kmeans),function(i){names(which(city.kmeans==i))})
plotclust((city.kmeans.net),city.kmeans)
city.idx <- cbind(city=rownames(city.idx), city.idx,cluster2=city.kmeans) %>% as.data.frame

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
value <- merge(value,city.idx,by=c('city'))

ggplot(data=value, 
       aes(x=long, 
           y=lat, 
           color=cluster)
       ) + 
  geom_point(size=3) + 
  geom_text(data=value,aes(x=long+1,y=lat-1,label=toupper(city))) + 
  labs(x='Latitude',y='Longitude',title = "Pattern Clustering") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data=value, 
       aes(x=long, 
           y=lat, 
           color=cluster2)
) + 
  geom_point(size=3) + 
  geom_text(data=value,aes(x=long+1,y=lat-1,label=toupper(city))) + 
  labs(x='Latitude',y='Longitude',title = "Feature Clustering") +
  theme(plot.title = element_text(hjust = 0.5))

write.csv(rbind(
cbind(
  'Pattern Clustering',
  sapply(unique(paste(city.idx$cluster)),function(i){
  toupper(paste(paste(city.idx$city[paste(city.idx$cluster)==i]),collapse=', '))
})),
cbind(
  'Feature Clustering',
  sapply(unique(paste(city.idx$cluster2)),function(i){
    toupper(paste(paste(city.idx$city[paste(city.idx$cluster2)==i]),collapse=', '))
  }))),'summary0211/clustering_result.csv')

#Fit Chart

# rlt <- data.frame(date=1:22,apply(rlt.nat[1:23,],2,diff))
rlt <- data.frame(date=1:23,rlt.nat[1:23,])
ggplot() + geom_line(aes(x=rlt[,1],y=rlt[,2],colour='Actual')) +
  geom_line(aes(x=rlt[,1],y=rlt[,3],colour='Fitted')) + 
  labs(x='Day',y='Accumulated Confirmed Cases',title = "Fit Chart") +
  theme(plot.title = element_text(hjust = 0.5)) 
ggplot() + geom_line(aes(x=rlt[-nrow(rlt),1],y=diff(rlt[,2]),colour='Actual')) +
  geom_line(aes(x=rlt[-nrow(rlt),1],y=diff(rlt[,3]),color='Fitted')) + 
  labs(x='Day',y='New Confirmed Cases',title = "Fit Chart") +
  theme(plot.title = element_text(hjust = 0.5))
ggplot() + geom_line(aes(x=1:50,y=(rlt.nat[1:50,1]),colour='Actual')) +
  geom_line(aes(x=1:50,y=(rlt.nat[1:50,2]),colour='Prediction')) + 
  labs(x='Day',y='Accumulated Confirmed Cases',title = "Prediction Chart") +
  theme(plot.title = element_text(hjust = 0.5))

#City Chart

par(mfrow=c(3,3))
for(i in 1:ncol(out)){
  plot.ts(out[1:80,i],ylab='Cases',xlab='Days',main=toupper(colnames(out))[i])
}
  
