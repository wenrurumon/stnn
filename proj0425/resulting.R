
rm(list=ls())
setwd('/Users/wenrurumon/Documents/posdoc/wuhan/data/github')
load("raw0425.rda")
library(data.table)
library(dplyr)
model.d <- lapply(1:5,function(i){keras::load_model_hdf5(paste0('/Users/wenrurumon/Documents/posdoc/wuhan/model/model0425_d_',i,'.model'))})
model.m <- lapply(1:5,function(i){keras::load_model_hdf5(paste0('/Users/wenrurumon/Documents/posdoc/wuhan/model/model0425_m_',i,'.model'))})

getlog <- function(I,keyI,log,p=8){
  logi <- filter(log,key==keyI&i%in%(0:p+I)) %>% arrange(i)
  x <- logi[1:p,]
  y <- logi[-1:-p,]
  list(x=x,y=y)
}
getdate <- function(x){
  paste(as.POSIXct('2020-01-22')+3600*(x-1)*24)
}

log <- data.frame(filter(raw,state=='US'|scope=='us'),status='historical')
# log <- data.frame(filter(raw,state=='US'),status='historical')

rlt <- lapply(unique(log$key),function(K){
  rlt <- list()
  print(K)
  for(j in 1:4){
    temp <- filter(log,key==K)
    temp$d <- temp$d *(1-0.2*(j-2))
    for(i in 1:365){
      tempi <- getlog(nrow(temp)-7,temp$key[1],temp,8)
      if(sum(tempi$x$new)==0){
        tempi.new <- 0
      } else {
        tempi.new <- matrix(c(tempi$x$new/mean(tempi$x$new+1),mean(tempi$x$d)),nrow=1)
        tempi.new <- mean(sapply(model.m,function(m){m %>% predict(tempi.new)}) * mean(tempi$x$new+1))
      }
      tempi.new <- floor(ifelse(tempi.new>0,tempi.new,0))
      if((mean(tempi$x$new)==tempi.new)&tempi.new>0){
        tempi.new <- tempi.new - 1
      }
      tempi.d <- (matrix(c(tempi$x$accum/mean(tempi$x$accum+1),mean(tempi$x$d)),nrow=1))
      tempi.d <- mean(sapply(model.d,function(m){m %>% predict(tempi.d)}))
      tempi.d <- max(tempi.d,(tempi$x$d[8]))
      tempi.d <- (ifelse(tempi.d>1,1,tempi.d))
      tempi.d <- (ifelse(tempi.d<0,0,tempi.d))
      temp <- rbind(temp,tempi$x[8,] %>% 
                      mutate(new=tempi.new,accum=accum+tempi.new,d=tempi.d,i=i+1,
                             date=paste(as.POSIXct(date)+3600*24),status='prediction'))
    }
    rlt[[j]] <- data.frame(scenario=j,temp)
  }
  rlt[[1]]$d[1:max(filter(rlt[[2]],status=='historical')$i)] <- 
    rlt[[2]]$d[1:max(filter(rlt[[2]],status=='historical')$i)]
  rlt[[3]]$d[1:max(filter(rlt[[2]],status=='historical')$i)] <- 
    rlt[[2]]$d[1:max(filter(rlt[[2]],status=='historical')$i)]
  rlt[[4]]$d[1:max(filter(rlt[[2]],status=='historical')$i)] <- 
    rlt[[2]]$d[1:max(filter(rlt[[2]],status=='historical')$i)]
  plot.ts(rlt[[4]]$new,main=K)
  lines(rlt[[3]]$new,col=2)
  lines(rlt[[2]]$new,col=3)
  lines(rlt[[1]]$new,col=4)
  rlt
})

############################################################################################
############################################################################################

setwd('/Users/wenrurumon/Documents/posdoc/wuhan/summary')

rlt <- lapply(rlt,function(x){
  x <- x[c(2,3,4,1)]
  x[[1]]$scenario <- 1
  x[[2]]$scenario <- 2
  x[[3]]$scenario <- 3
  x[[4]]$scenario <- 4
  x
})
rlt2 <- do.call(rbind,lapply(rlt,function(x){do.call(rbind,x)}))
rlt2$scenario <- paste0('Scenario_',rlt2$scenario)
rlt2$date <- as.Date(rlt2$date)
library(ggplot2)
temp <- filter(rlt2,state=='US'&i<250) 
ggplot(temp,aes(colour=scenario,y=new/1000,x=date)) + geom_line(size=1) + 
  labs(y='New Cases (K)',x = 'Date', title='New Cases - Total US', colour='Scenario') +
  scale_x_date(date_breaks = '2 week',date_labels = '%b %d')
ggplot(temp,aes(colour=scenario,y=accum/1000,x=date)) + geom_line(size=1) + 
  labs(y='Accumulated Cases (K)',x = 'Date', title='Accumulated Cases - Total US', colour='Scenario') + scale_x_date(date_breaks = '2 week',date_labels = '%b %d')
ggplot(temp,aes(colour=scenario,y=d,x=date)) + geom_line(size=1) + 
  labs(y='Degree of intervention',x = 'Date', title='Degree of intervention - Total US', colour='Scenario') + scale_x_date(date_breaks = '2 week',date_labels = '%b %d')

cor.test((filter(rlt2,scenario=='Scenario_1') %>% select(d,new))$d,
         (filter(rlt2,scenario=='Scenario_1') %>% select(d,new))$new)
cor.test((filter(rlt2,scenario=='Scenario_2') %>% select(d,new))$d,
         (filter(rlt2,scenario=='Scenario_2') %>% select(d,new))$new)
cor.test((filter(rlt2,scenario=='Scenario_3') %>% select(d,new))$d,
         (filter(rlt2,scenario=='Scenario_3') %>% select(d,new))$new)
cor.test((filter(rlt2,scenario=='Scenario_4') %>% select(d,new))$d,
         (filter(rlt2,scenario=='Scenario_4') %>% select(d,new))$new)


############################################################################################
############################################################################################

test <- merge(
  rlt2 %>% filter(status=='historical') %>% group_by(scope,state,key) %>% summarise(now.date=max(date),now.accum=max(accum)),
  merge((merge(rlt2,
      rlt2 %>% group_by(key,scenario) %>% summarise(max.new = max(new)),by=c('key','scenario')) %>% 
  filter(new==max.new) %>% group_by(key,scenario) %>% 
    summarise(max.new.date=getdate(min(i)),max.new.new=mean(max.new),max.new.accum=mean(accum))),
(merge(rlt2,
      rlt2 %>% group_by(key,scenario) %>% summarise(max.accum = max(accum)),by=c('key','scenario')) %>% 
  filter(accum==max.accum) %>% group_by(key,scenario) %>% 
   summarise(max.accum.date=getdate(min(i)),max.accum.accum=mean(max.accum))),by=c('key','scenario')),by='key') %>% select(-key)
# test %>% filter(scenario=='Scenario_1') %>% mutate(rate=max.accum.accum/now.accum) %>% arrange(desc(rate))
write.csv(test,'rlt_US_0425_summary.csv')

test <- do.call(cbind,lapply(rlt,function(x){sapply(x,function(x){x$new})}))
rownames(test) <- rlt[[1]][[1]]$date
colnames(test) <- gsub(' ','_',paste(lapply(rlt,function(x){sapply(x,function(x){x$key[1]})}) %>% unlist,'new',c(1:4)))
write.csv(test,'rlt_US_0425_newcases.csv')

test <- do.call(cbind,lapply(rlt,function(x){sapply(x,function(x){x$d})}))
rownames(test) <- rlt[[1]][[1]]$date
colnames(test) <- gsub(' ','_',paste(lapply(rlt,function(x){sapply(x,function(x){x$key[1]})}) %>% unlist,'d',c(1:4)))
write.csv(test,'rlt_US_0425_degree.csv')

test <- t(sapply(1:159,function(i){
  c(paste(strsplit(colnames(test)[i],'_')[[1]][-4],collapse='_'),
    cor=cor(
    do.call(cbind,lapply(rlt,function(x){sapply(x,function(x){x$new})}))[,i],
    do.call(cbind,lapply(rlt,function(x){sapply(x,function(x){x$d})}))[,i]
  ),
  pvalue=cor.test(
    do.call(cbind,lapply(rlt,function(x){sapply(x,function(x){x$new})}))[,i],
    do.call(cbind,lapply(rlt,function(x){sapply(x,function(x){x$d})}))[,i]
  )$p.value
  )
}))
write.csv(test,'rlt_US_0425_cor.csv')

################################################

plot.ts(filter(raw,state=='Texas')$new)
plot.ts(filter(raw,state=='Texas')$d)

plot.ts(filter(raw,state=='Arkansas')$new)
plot.ts(filter(raw,state=='Arkansas')$d)

plot.ts(filter(raw,state=='Kansas')$new)
plot.ts(filter(raw,state=='Kansas')$d)

plot.ts(filter(raw,state=='Virginia')$new)
plot.ts(filter(raw,state=='Virginia')$d)

plot.ts(filter(raw,state=='RhodeIsland')$new)
plot.ts(filter(raw,state=='RhodeIsland')$d)

plot.ts(filter(raw,state=='Washington')$new)
plot.ts(filter(raw,state=='RhodeIsland')$d)

