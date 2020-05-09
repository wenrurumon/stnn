
#https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv
#https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv

###################################
# Get Data
###################################

rm(list=ls())
library(data.table)
library(dplyr)
k <- paste(strsplit(paste(Sys.time()),'\\W')[[1]][1:5],collapse='')

#Get data from github
setwd('/Users/wenrurumon/Documents/posdoc/wuhan/data/github')
raw <- lapply(c('us_raw.csv','global_raw.csv'),fread)

#Global
x <- raw[[2]]
colnames(x)[1:2] <- c('state','country')
colnames(x)[-1:-4] <- sapply(strsplit(colnames(x)[-1:-4],'/'),function(x){
  as.POSIXct(
    paste((2000+as.numeric(x[3])),as.numeric(x[1]),as.numeric(x[2]),sep='/')
  )-as.POSIXct('2020/1/21')
})
x <- x %>% mutate(state=ifelse(country=='Taiwan*','Taiwan',state),
                  country=ifelse(country=='Taiwan*','China',country))
temp <- read.csv('/Users/wenrurumon/Documents/posdoc/wuhan/data/china_confirmed_315.csv')[,2:3]
x[which(x$state=='Hubei'),1:54+4] <- temp[-1:-11,2]
x.key <- x[,1:4]
x.val <- x[,-1:-4]
x.global <- apply(x.val,2,function(x){
  tapply(x,x.key$country,sum)
})
colnames(x.global) <- paste(as.numeric(colnames(x.global))*3600*24 + as.POSIXct('2020/1/21'))
rownames(x.global) <- gsub('\\W','',rownames(x.global)) 
write.csv(t(x.global),'global.csv')

x.val <- x.val[x.key$country=='China',]
x.key <- x.key[x.key$country=='China',]
x.china <- apply(x.val,2,function(x){
  tapply(x,x.key$state,sum)
})
colnames(x.china) <- paste(as.numeric(colnames(x.china))*3600*24 + as.POSIXct('2020/1/21'))
rownames(x.china) <- gsub('\\W','',rownames(x.china)) 
write.csv(t(x.china),'china.csv')

#US
#https://github.com/CSSEGISandData/COVID-19/blob/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv
x <- raw[[1]]
colnames(x)[7:8] <- c('state','country')
colnames(x)[-1:-11] <- sapply(strsplit(colnames(x)[-1:-11],'/'),function(x){
  as.POSIXct(
    paste((2000+as.numeric(x[3])),as.numeric(x[1]),as.numeric(x[2]),sep='/')
  )-as.POSIXct('2020/1/21')
})
x.key <- x[,1:11]
x.val <- x[,-1:-11]
x.us <- apply(x.val,2,function(x){
  tapply(x,x.key$state,sum)
})
colnames(x.us) <- paste(as.numeric(colnames(x.us))*3600*24 + as.POSIXct('2020/1/21'))
rownames(x.us) <- gsub('\\W','',rownames(x.us)) 
write.csv(t(x.us),'us.csv')

