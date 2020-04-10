
rm(list=ls())
library(data.table)
library(dplyr)
library(ggplot2)

setwd('/Users/wenrurumon/Downloads/Fwd_\ highlights\ scenarios\ fixed')
raw <- lapply(dir(),fread)
names(raw) <- dir()

getline <- function(i){
  x <- sapply(1:4,function(j){as.numeric(raw[[j]][-1:-5,][[i+1]])})
  colnames(x) <- paste0('Scenario_',0:3)
  x.date <- raw[[1]][-1:-5,][[1]]
  x <- data.frame(country=colnames(raw[[1]])[i+1],data.frame(date=x.date,x) %>% melt)
  x$date <- as.POSIXct(paste(x$date))
  colnames(x)[3] <- 'Legend'
  x %>% mutate(value=value/1000)
}

grid::grid.newpage() 
grid::pushViewport(grid::viewport(layout = grid::grid.layout(4,3)))

vplayout <- function(x,y){grid::viewport(layout.pos.row = x, layout.pos.col = y)} 
for(j in 1:4){
  for(i in 1:3){
    x <- getline((j-1)*3+i)
    print(ggplot(filter(x,Legend!='Scenario_0'),aes(colour=Legend,y=value,x=date))+
            geom_line(size=1)+labs(x="Date",y='Accumulated Cases(K)',title=x$country[1]),
          vp=vplayout(j,i))
  }
}

vplayout <- function(x,y){grid::viewport(layout.pos.row = x, layout.pos.col = y)} 
for(j in 1:4){
  for(i in 1:3){
    x <- getline((j-1)*3+i)
    print(ggplot(filter(x,Legend=='Scenario_0'),aes(y=value,x=date))+
            geom_line(size=1)+labs(x="Date",y='Accumulated Cases(K)',title=x$country[1]),
          vp=vplayout(j,i))
  }
}

