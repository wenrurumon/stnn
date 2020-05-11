
rm(list=ls())
# setwd('/Users/wenrurumon/Downloads/figures\ data\ and\ demo')
setwd('/Users/wenrurumon/Desktop/figures\ data\ and\ demo\ 0511')

i <- 0

i <- 1
setwd(dir()[i])
dir()
x <- fread(grep('.csv',dir(),value=T)) %>% melt %>% mutate(date=as.Date(V1))
ggplot(data=x,aes(x=date,y=value,colour=variable)) + geom_line(size=1) +
  labs(x='Date',y='Accumulated Cases',colour='Legend') + scale_x_date(date_breaks = '2 week',date_labels = '%b %d')
setwd('..')

i <- 2
setwd(dir()[i])
dir()
x <- fread(grep('.csv',dir(),value=T))[,1:3] %>% mutate(date=as.Date(V1))
ggplot() +
  geom_line(data=x,aes(x=date,accumulated/1000,colour='Accumulated'),size=1) + 
  geom_line(data=x,aes(x=date,rescale(new/1000,range(accumulated)/1000),colour='New'),size=1) +
  scale_y_continuous(breaks=pretty_breaks(5),sec.axis = sec_axis( ~rescale(.,range(x$accumulated)/1000),name='New Cases (K)')) +
  labs(x='Date',y='Accumulated Cases (K)',colour='') + scale_x_date(date_breaks = '2 week',date_labels = '%b %d') + theme(legend.position='top') 
setwd('..')

i <- 3
setwd(dir()[i])
dir()
grid::grid.newpage() 
grid::pushViewport(grid::viewport(layout = grid::grid.layout(4,2))) 
vplayout <- function(x,y){grid::viewport(layout.pos.row = x, layout.pos.col = y)} 
x <- read.csv('Table 5.csv')
x <- x[-nrow(x),] %>% melt %>% mutate(date=as.Date(X))
for(j in 1:8){
  p <- ggplot(data=filter(x,variable==unique(x$variable)[j]),aes(x=date,y=value)) +
    geom_line(size=1) +
    labs(x='Date',y='Cases',title=unique(x$variable)[j]) + 
    scale_x_date(date_breaks = '2 week',date_labels = '%b %d')
  print(p, vp = vplayout(rep(1:4,2)[j],rep(1:2,each=4)[j]))
}
setwd('..')

i <- 4
setwd(dir()[i])
dir()
x <- lapply(dir(pattern='.csv')[-1],function(x){
  x <- fread(x)[-1:-6,] %>% mutate(Country=as.Date(Country))
  x[,-1] <- apply(x[,-1],2,as.numeric)
  colnames(x)[1] <- 'Date'
  x %>% melt(id=1)
})
for(i in 1:3){x[[i]] <- cbind(scenario=paste0('Scenario ',i),x[[i]])}
x <- do.call(rbind,x)
grid::grid.newpage() 
grid::pushViewport(grid::viewport(layout = grid::grid.layout(4,3))) 
vplayout <- function(x,y){grid::viewport(layout.pos.row = x, layout.pos.col = y)} 
for(j in 1:12){
  p <- ggplot(data=filter(x,variable==unique(x$variable)[j]),aes(x=Date,y=value,colour=scenario)) +
    geom_line(size=1) +
    labs(x='Date',y='Cases',colour='Scenario',title=unique(x$variable)[j]) + 
    scale_x_date(date_breaks = '1 month',date_labels = '%b %d')
  print(p, vp = vplayout(rep(1:4,3)[j],rep(1:3,each=4)[j]))
}
x <- lapply(dir(pattern='.csv')[-1],function(x){
  x <- fread(x)[-1:-6,] %>% mutate(Country=as.Date(Country))
  x[,-1] <- apply(x[,-1],2,as.numeric)
  x[,-1] <- rbind(0,apply(x[,-1],2,diff))
  colnames(x)[1] <- 'Date'
  x %>% melt(id=1)
})
for(i in 1:3){x[[i]] <- cbind(scenario=paste0('Scenario ',i),x[[i]])}
x <- do.call(rbind,x)
grid::grid.newpage() 
grid::pushViewport(grid::viewport(layout = grid::grid.layout(4,3))) 
vplayout <- function(x,y){grid::viewport(layout.pos.row = x, layout.pos.col = y)} 
for(j in 1:12){
  p <- ggplot(data=filter(x,variable==unique(x$variable)[j]),aes(x=Date,y=value,colour=scenario)) +
    geom_line(size=1) +
    labs(x='Date',y='Cases',colour='Scenario',title=unique(x$variable)[j]) + 
    scale_x_date(date_breaks = '1 month',date_labels = '%b %d')
  print(p, vp = vplayout(rep(1:4,3)[j],rep(1:3,each=4)[j]))
}
setwd('..')

i <- 5
setwd(dir()[i])
dir()
x <- fread(dir(pattern='.csv'),header=T) %>% melt %>% mutate(
  date=as.Date(V1),variable=paste(variable)
)
ggplot(data=x,aes(x=date,y=value,colour=variable)) + labs(
  x='Date',y='Cases',colour='Strength'
) + geom_line(size=1) + scale_x_date(date_breaks = '2 week',date_labels = '%b %d')
x <- fread(dir(pattern='.csv'),header=T) %>% as.data.frame
x[,-1] <- rbind(0,apply(x[,-1],2,diff))
x <- x %>% melt %>% mutate(
  date=as.Date(V1),variable=paste(variable)
)
ggplot(data=x,aes(x=date,y=value,colour=variable)) + labs(
  x='Date',y='Cases',colour='Strength'
) + geom_line(size=1) + scale_x_date(date_breaks = '2 week',date_labels = '%b %d')
setwd('..')

i <- 6
setwd(dir()[i])
dir()
x <- fread("Table S2.csv",header=T)[-1:-6,] %>% as.data.frame
x[,-1] <- apply(x[,-1],2,as.numeric)
x <- melt(x[,-2]) %>% mutate(date=as.Date(V1))
ggplot(data=x,aes(x=date,y=value,colour=variable)) + labs(
  x='Date',y='Cases',colour='Scenario'
) + geom_line(size=1) + scale_x_date(date_breaks = '2 week',date_labels = '%b %d')




