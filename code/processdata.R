
rm(list=ls())
library(plyr)
library(openxlsx)
library(data.table)
library(dplyr)
rmna <- function(x){ifelse(is.na(x),0,x)}

setwd('/Users/wenrurumon/Documents/posdoc/wuhan/data')
raw1 <- read.csv('china_confirmed_314.csv')[,-1]
raw2 <- read.csv("Cov19_314.csv") %>% 
  mutate(state=tolower(state),dead=rmna(death),confirmed=rmna(confirmed)) %>% 
  select(date,state,dead,confirmed)
raw2[raw2$state=='china'&raw2$date<=213,]$confirmed <- rowSums(raw1[11:34,-1])
raw <- raw2
raw <- lapply(unique(raw$state),function(s){
  x <- filter(raw,state==s)
  list(state = s,
       confirmed = rmna(x$confirmed[match(unique(raw$date),x$date)]),
       dead = rmna(x$dead[match(unique(raw$date),x$date)]))
})
names(raw) <- sapply(raw,function(x){x$state})
raw.global <- sapply(raw,function(x){x$confirmed})
raw.china <- raw1[,-1]
raw.usa <- read.csv('usa.csv')[,-1:-2]

raw.date <- as.POSIXct( "2020-01-20") + 3600*24*1:length(1:nrow(raw.global))
raw.global <- data.frame(date=raw.date,raw.global)
raw.date <- as.POSIXct( "2020-01-20") + 3600*24*1:length(1:nrow(raw.usa))
raw.usa <- data.frame(date=raw.date,raw.usa)
raw.date <- as.POSIXct( "2020-01-10") + 3600*24*1:length(1:nrow(raw.china))
raw.china <- data.frame(date=raw.date,raw.china)

#Export

write.csv(raw.usa,'model_usa.csv')
write.csv(raw.china,'model_china.csv')
write.csv(raw.global,'model_global.csv')

