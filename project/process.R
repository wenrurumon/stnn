############################
# Data Processing
############################

setwd('/Users/wenrurumon/Documents/posdoc/wuhan')
raw1 <- read.csv("china_confirmed.csv")[,-1]
raw2 <- read.csv("global_confirmed.csv")[,-1:-2]
raw3 <- read.csv("data0308.csv") %>% 
  mutate(state=tolower(state),dead=rmna(death),confirmed=rmna(confirmed)) %>% 
  select(date,state,dead,confirmed)
raw3[raw3$state=='china'&raw3$date<=213,]$confirmed <- rowSums(raw1[11:34,-1])
raw <- raw3; rm(raw1,raw2,raw3)
raw.date <- as.POSIXct( "2020-01-20") + 3600*24*1:length(unique(raw$date))
raw <- lapply(unique(raw$state),function(s){
  x <- filter(raw,state==s)
  list(state = s,
       confirmed = rmna(x$confirmed[match(unique(raw$date),x$date)]),
       dead = rmna(x$dead[match(unique(raw$date),x$date)]))
})
names(raw) <- sapply(raw,function(x){x$state})
raw.c <- sapply(raw,function(x){x$confirmed})
raw.d <- sapply(raw,function(x){x$dead})

# rate.d <- raw.d/raw.c
# rate.d <- rbind(rate.d[,which(colSums(raw.d)>0)],
#                 ttl=apply(raw.d,2,max)[which(colSums(raw.d)>0)])
# rate.d <- cbind(total=c(rowSums(raw.d)/rowSums(raw.c),ttl=max(rowSums(raw.d))),rate.d)
# rwrite.csv(rate.d,'temp.csv')
