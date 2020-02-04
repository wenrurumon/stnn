
rm(list=ls())
library(data.table)
library(dplyr)

library(fda)
library(MASS)
library(flare)
library(corpcor)
library(ggplot2)
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

setwd('/Users/wenrurumon/Documents/posdoc/wuhan')
raw <- fread('data0203.csv')
x.date <- raw$date
x <- select(raw,-date) %>% as.data.frame
feature <- do.call(rbind,strsplit(colnames(x),'_'))[,1]
city <- do.call(rbind,strsplit(colnames(x),'_'))[,2]

x.city <- lapply(unique(city),function(i){
  x[,city==i]
})
names(x.city) <- unique(city)

x.feature <- lapply(unique(feature),function(i){
  x[,feature==i]
})
names(x.feature) <- unique(feature)

x.city.cca <- -log(ccap(x.city,x.city))
x.city.cca1 <- apply(x.city.cca,1,function(x){x>quantile(x,0.975)})+0
x.city.cca2 <- apply(x.city.cca,2,function(x){x>quantile(x,0.975)})+0
x.city.cca <- ((x.city.cca1+x.city.cca2)>0)+0

write.table(x.feature$confirm,'confirm0203.csv',row.names=F,col.names=F,sep=',')
write.table(x.city.cca,'citycca0203.csv',row.names=F,col.names=F,sep=',')

##############

# setwd('/Users/wenrurumon/Documents/posdoc/wuhan/stnn-master/data')
# x <- lapply(dir(),fread)
