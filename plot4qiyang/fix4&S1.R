
library(ggplot2)
library(dplyr)
library(reshape2)
library(scales)
library(data.table)

x <- openxlsx::read.xlsx('Table 4.xlsx')
x$X1 <- as.Date('2020-4-28')+1:nrow(x)
colnames(x)[1] <- 'date'

ggplot() +
  geom_line(data=x,aes(x=date,accumulated/1000,colour='Accumulated'),size=1) + 
  geom_line(data=x,aes(x=date,rescale(new/1000,range(accumulated)/1000),colour='New'),size=1) +
  scale_y_continuous(breaks=pretty_breaks(5),sec.axis = sec_axis( ~rescale(.,range(x$new,na.rm=T)/1000),name='New Cases (K)')) +
  labs(x='Date',y='Accumulated Cases (K)',colour='') + scale_x_date(date_breaks = '2 week',date_labels = '%b %d') + theme(legend.position='top') 

x <- fread(dir(pattern='Table S1.csv'),header=T)
colnames(x)[-1] <- c(0,0.7,0.5,0.3,1)
x <- x%>% melt %>% mutate(
  date=as.Date(V1),variable=paste(variable)
)
ggplot(data=x,aes(x=date,y=value/1000000,colour=variable)) + labs(
  x='Date',y='Cases (Million)',colour='Strength'
) + geom_line(size=1) + scale_x_date(date_breaks = '2 week',date_labels = '%b %d')
