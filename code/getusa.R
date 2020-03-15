rm(list=ls())
library(tmcn)
library(RSelenium)
library(dplyr)
library(data.table)
geteletext <- function(...){
  x <- do.call(c,list(...))
  x <- sapply(x,function(x){
    x$getElementText()
  })
}
geteleattr <- function(...,attr,ifunlist=F){
  x <- do.call(c,list(...))
  x <- sapply(x,function(x){
    x$getElementAttribute(attr)
  })
  if(ifunlist){
    return(unlist(x))
  } else {
    return(x)
  }
}
rmspace <- function(x){
  x <- strsplit(x,' ')
  x <- sapply(x,function(x) paste(x,collapse=''))
  gsub("\\[.*?\\]|\n","",x)
}
getjson <- function(x){
  x.m <- cbind(
    paste0('\"',names(x),'\"'),
    paste0('\"',as.vector(x),'\"')
  )
  paste(apply(x.m,1,function(x){paste(x,collapse=':')}),collapse=',')
}
chrome <- remoteDriver(remoteServerAddr = "localhost" 
                       , port = 4444L
                       , browserName = "chrome")
chrome$open()
#Go

chrome$navigate('https://coronavirus.1point3acres.com/?from=groupmessage?code=043vqEkj099jbs1TtUij0gxvkj0vqEkW')
b <- chrome$findElement('class','ant-pagination-next')
p <- (chrome$findElement('class','ant-pagination') %>% geteletext)[[1]]
p <- max(as.numeric(strsplit(p,'\n')[[1]]),na.rm=T)

rlt <- list()
i <- 1
x <- chrome$findElements('class',"ant-table-row")  
Sys.sleep(1)
temp <- geteletext(x)
rlt[[i]] <- temp[which(sapply(temp,function(x){x!=''}))]
b$clickElement()
Sys.sleep(2)
while(i<=1000&length(rlt[[i]])==15){
  i <- i+1
  print(i)
  x <- chrome$findElements('class',"ant-table-row")  
  Sys.sleep(1)
  temp <- geteletext(x)
  rlt[[i]] <- temp[which(sapply(temp,function(x){x!=''}))]
  b$clickElement()
  Sys.sleep(2)
}
raw <- rlt

#Processing

rlt <- unique(unlist(raw))
test <- sapply(strsplit(rlt,' '),function(x){c(x[1:3],paste(x[-1:-3],collapse=' '))}) %>% t %>% 
  as.data.table %>% select(no=V1,date=V2,state=V3,notes=V4) 
test$notes <- gsub('\n',' ',test$notes)
test$date <- do.call(c,lapply(test$date,function(x){
  as.POSIXct(paste0('2020/',x))
}))
test$no <- gsub('NO.','',test$no)
test <- data.table(test,sapply(strsplit(test$no,'-'),function(x){range(as.numeric(x))}) %>% t)
test <- mutate(test,confirmed=V2-V1+1) %>% mutate(
  dead=ifelse(is.na(confirmed),1,0),
  confirmed=ifelse(is.na(confirmed),0,confirmed)
) %>% select(no,date,state,confirmed,dead,notes)
# write.csv(test,'usa_updated.csv')

tail(test %>% group_by(date) %>% summarise(case=sum(confirmed)))
states <- unique(test$state)
dates <- unique(test$date)
p <- range(dates)[2]-range(dates)[1]
dates <- min(dates) + (0:p)*3600*24

rlt <- sapply(states,function(s){
  x <- filter(test,state==s) %>% group_by(date) %>% summarise(confirmed=sum(confirmed))
  x[match(dates,x$date),]$confirmed
})
rlt[is.na(rlt)] <- 0
rlt <- data.frame(dates,rlt)
write.csv(rlt[,],'/Users/wenrurumon/Documents/posdoc/wuhan/data/usa.csv')

