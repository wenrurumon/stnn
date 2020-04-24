
rm(list=ls())
setwd('/Users/wenrurumon/Documents/posdoc/wuhan/data/github')
load("raw_m0423.rda")

log <- data.frame(filter(raw,state=='US'|scope=='us'),status='historical')
model.d <- lapply(1:5,function(i){keras::load_model_hdf5(paste0('/Users/wenrurumon/Documents/posdoc/wuhan/model/model_d_',i,'.model'))})
model.m <- lapply(1:5,function(i){keras::load_model_hdf5(paste0('/Users/wenrurumon/Documents/posdoc/wuhan/model/model_m_',i,'.model'))})

getlog <- function(I,keyI,log,p=8){
  logi <- filter(log,key==keyI&i%in%(0:p+I)) %>% arrange(i)
  x <- logi[1:p,]
  y <- logi[-1:-p,]
  list(x=x,y=y)
}
getdate <- function(x){
  paste(as.POSIXct('2020-01-22')+3600*(x-1)*24)
}


rlt <- lapply(unique(log$key),function(K){
  rlt <- list()
  print(K)
  for(j in 1:3){
    temp <- filter(log,key==K)
    temp$d <- temp$d *(1-0.2*(j-1))
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
  rlt[[2]]$d[1:max(filter(rlt[[1]],status=='historical')$i)] <- 
    rlt[[1]]$d[1:max(filter(rlt[[1]],status=='historical')$i)]
  rlt[[3]]$d[1:max(filter(rlt[[1]],status=='historical')$i)] <- 
    rlt[[1]]$d[1:max(filter(rlt[[1]],status=='historical')$i)]
  plot.ts(rlt[[3]]$new,main=K)
  lines(rlt[[2]]$new,col=2)
  lines(rlt[[1]]$new,col=3)
  rlt
})

############################################################################################
############################################################################################

setwd('/Users/wenrurumon/Documents/posdoc/wuhan/summary')

rlt2 <- do.call(rbind,lapply(rlt,function(x){do.call(rbind,x)}))
rlt2$scenario <- paste0('Scenario_',rlt2$scenario)
rlt2$date <- as.POSIXct(rlt2$date)
library(ggplot2)
temp <- filter(rlt2,state=='US'&i<200) 
ggplot(temp,aes(colour=scenario,y=new/1000,x=date)) + geom_line(size=1) + 
  labs(y='New Cases (K)',x = 'Date', title='New Cases - Total US', colour='Scenario')
ggplot(temp,aes(colour=scenario,y=accum/1000,x=date)) + geom_line(size=1) + 
  labs(y='Accumulated Cases (K)',x = 'Date', title='Accumulated Cases - Total US', colour='Scenario')
ggplot(temp,aes(colour=scenario,y=d,x=date)) + geom_line(size=1) + 
  labs(y='Degree of intervention',x = 'Date', title='Degree of intervention - Total US', colour='Scenario')



cor.test((filter(rlt2,scenario=='Scenario_1') %>% select(d,new))$d,
         (filter(rlt2,scenario=='Scenario_1') %>% select(d,new))$new)
cor.test((filter(rlt2,scenario=='Scenario_2') %>% select(d,new))$d,
         (filter(rlt2,scenario=='Scenario_2') %>% select(d,new))$new)
cor.test((filter(rlt2,scenario=='Scenario_3') %>% select(d,new))$d,
         (filter(rlt2,scenario=='Scenario_3') %>% select(d,new))$new)

############################################################################################
############################################################################################

test <- merge((merge(rlt2,
      rlt2 %>% group_by(key,scenario) %>% summarise(max.new = max(new)),by=c('key','scenario')) %>% 
  filter(new==max.new) %>% group_by(key,scenario) %>% 
    summarise(max.new.i=getdate(min(i)),max.new.val=mean(max.new))),
(merge(rlt2,
      rlt2 %>% group_by(key,scenario) %>% summarise(max.accum = max(accum)),by=c('key','scenario')) %>% 
  filter(accum==max.accum) %>% group_by(key,scenario) %>% 
   summarise(max.accum.i=getdate(min(i)),max.accum.val=mean(max.accum))),by=c('key','scenario'))


write.csv(test,'rlt_US_YAmodel.csv')
