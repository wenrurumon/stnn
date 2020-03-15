
rm(list=ls())
library(data.table)
library(dplyr)
setwd('/Users/wenrurumon/Documents/posdoc/wuhan/data')
raw <- lapply(dir(patter='model'),function(x){data.table::fread(x)[,-1:-2] %>% as.matrix})
names(raw) <- dir(patter='model')

#Process
raw.date <- as.POSIXct( "2020-01-10") + 3600*24*1:length(1:nrow(raw[[1]]))
raw[[2]] <- rbind(do.call(rbind,rep(list(rep(0,ncol(raw[[2]]))),10)),raw[[2]])
raw[[3]] <- rbind(do.call(rbind,rep(list(rep(0,ncol(raw[[3]]))),10)),raw[[3]])
raw[[3]] <- as.matrix(as.data.table(raw[[3]]) %>% select(-Grand,-Diamond,-Wuhan))
raw[[2]][,1] <- rowSums(raw[[1]])
i <- 0
raw <- lapply(raw,function(x){
  i <<- i+1
  rlt1 <- melt(data.frame(date=raw.date,scope=c('china','global','usa')[i],x),id=c('date','scope'))
  x <- as.data.table(rbind(0,x))
  x <- apply(x,2,diff)
  rlt2 <- melt(data.frame(date=raw.date,scope=c('china','global','usa')[i],x),id=c('date','scope'))
  list(rlt1,rlt2)
})
raw1 <- do.call(rbind,lapply(raw,function(x){x[[1]]})) %>% as.data.table
raw2 <- do.call(rbind,lapply(raw,function(x){x[[2]]})) %>% as.data.table
raw <- data.table(raw2,acase=raw1$value)
colnames(raw) <- c('date','scope','state','case','acase')
raw <- raw %>% mutate(
  dum = ifelse((state=='china'|scope=='china')&date>='2020-01-23',1,0),
  scope = toupper(scope), state = toupper(state),
  key = paste(scope,state,sep="::")
)
raw <- merge(raw,raw %>% filter(case>0) %>% group_by(key) %>% summarise(w=min(date)),by='key') %>%
  mutate(w=as.numeric((date-w)/3600/24)+1,w=ifelse(w<0,0,w))

########################
#Model Filing
########################

#Module
getx <- function(keyi,i,p){
  x <- filter(raw,date%in%(as.POSIXct("2020-01-10")+(i:(p+i-1))*3600*24)&key==keyi)
  y <- filter(raw,date%in%(as.POSIXct("2020-01-10")+(p+i)*3600*24)&key==keyi)
  list(x=x,y=y)
}
procx <- function(x){
  raw <- x
  y <- x$y %>% mutate(case=ifelse(case<0,0,case)) 
  x <- x$x %>% mutate(case=ifelse(case<0,0,case))
  x.dum <- mean(x$dum)
  f <- mean(x$case+1)
  w <- y$w
  xi <- c(x=log(x$case/f+1),d=x.dum)
  yi <- log(y$case/f+1)
  list(x=xi,y=yi,f=f,w=w,log=raw)
}
proci <- function(keyi,i,p,y=TRUE){
  x <- getx(keyi,i,p)
  return(procx(x))
}

#Filing
mfile <- do.call(c,lapply(unique(raw$key),function(keyi){
  lapply(1:(length(unique(raw$date))-8),function(i){
    proci(keyi,i,8)
  })
}))
save(raw,mfile,file='mfile.rda')
