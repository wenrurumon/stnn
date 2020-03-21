
###################################
# Get Data
###################################

rm(list=ls())
library(data.table)
library(dplyr)
k <- paste(strsplit(paste(Sys.time()),'\\W')[[1]][1:5],collapse='')

#Get data from github
setwd('/Users/wenrurumon/Documents/posdoc/wuhan/data/github')
usakey <- read.csv("usa_city_key.csv")
urls <- c(
  'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv',
  'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv',
  'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv'
)
for(i in dir(pattern='time_series_19|model')){
  system(paste('rm',i))
}
for(i in urls){
  system(paste('wget',i))
}
raw <- lapply(dir(pattern='time_series_19'),function(x){
  x <- data.table::fread(x)
  colnames(x)[1:2] <- c('state','country')
  colnames(x)[-1:-4] <- sapply(strsplit(colnames(x)[-1:-4],'/'),function(x){
    as.POSIXct(
      paste((2000+as.numeric(x[3])),as.numeric(x[1]),as.numeric(x[2]),sep='/')
    )-as.POSIXct('2020/1/21')
  })
  x %>% mutate(state=ifelse(country=='Taiwan*','Taiwan',state),
               country=ifelse(country=='Taiwan*','China',country))
})
names(raw) <- sapply(strsplit(dir(pattern='time_series_19'),'-'),function(x){gsub('.csv','',x[3])})
raw <- lapply(raw,as.data.frame)
temp <- read.csv('/Users/wenrurumon/Documents/posdoc/wuhan/data/china_confirmed_315.csv')[,2:3]
raw$Confirmed[which(raw$Confirmed$state=='Hubei'),5:27] <- temp[12:34,2]

#By Country
raw.country <- lapply(raw,function(x){
  x.key <- x[,1:4]
  x.val <- x[,-1:-4]
  x <- apply(x.val,2,function(x){
    tapply(x,x.key$country,sum)
  })
  colnames(x) <- paste(as.numeric(colnames(x))*3600*24 + as.POSIXct('2020/1/21'))
  rownames(x) <- gsub('\\W','',rownames(x)) 
  t(x)
})
raw.china <- lapply(raw,function(x){
  x.key <- x[,1:4]
  x.val <- x[x.key$country%in%c('China'),-1:-4]
  x.key <- x.key[x.key$country%in%c('China'),]
  x <- apply(x.val,2,function(x){
    tapply(x,x.key$state,sum)
  })
  colnames(x) <- paste(as.numeric(colnames(x))*3600*24 + as.POSIXct('2020/1/21'))
  rownames(x) <- gsub('\\W','',rownames(x)) 
  t(x)
})
raw.usa <- lapply(raw,function(x){
  x.key <- x[,1:4]
  x.val <- x[x.key$country%in%c('US'),-1:-4]
  x.key <- x.key[x.key$country%in%c('US'),]
  x.key <- x.key %>% mutate(state=ifelse(
    is.na(paste(usakey$code)[
      match(gsub('\\W','',toupper(x.key$state)),gsub('\\W','',toupper(usakey$ename)))
      ]),state,paste('Others,',usakey$code)[
        match(gsub('\\W','',toupper(x.key$state)),gsub('\\W','',toupper(usakey$ename)))
      ]))
  x.key$state[x.key$state=='District of Columbia'] <- 'Others, DC'
  x <- apply(x.val,2,function(x){
    tapply(x,gsub(' ','',sapply(strsplit(x.key$state,', '),function(x){x[length(x)]})),sum)
  })
  colnames(x) <- paste(as.numeric(colnames(x))*3600*24 + as.POSIXct('2020/1/21'))
  t(x)
})

#Output

for(i in 1:3){
  write.csv(raw.country[[i]],paste0('model_global_',names(raw)[i],'_',k,'.csv'))
  write.csv(raw.china[[i]],paste0('model_china_',names(raw)[i],'_',k,'.csv'))
  write.csv(raw.usa[[i]],paste0('model_usa_',names(raw)[i],'_',k,'.csv'))
}
