######################
#Summary
######################

(lapply(rlts,sum))
rlts <- lapply(rlts,function(x){cbind(total=rowSums(x),x)})

#diff2
rlts.diff <- lapply(rlts,function(x){
  x <- apply(x,2,function(x){x/max(x)})
  x <- apply(x,2,diff)
  x
})
for(i in 1:4){write.csv(rlts.diff[[i]],paste0("diff2_",i,'.csv'))}

#CSV
rlts.index <- lapply(rlts,function(x){
  x <- round(x)
  x.date_begin <- apply(x,2,function(x){which(x>0)[1]})
  x.date_max <- apply(x,2,function(x){which(x==max(x))[1]})
  x.date_end <- apply(x,2,function(x){max(which(x>0))})
  x.value_max <- sapply(1:length(x.date_max),function(i){apply(x,2,cumsum)[x.date_max[i],i]})
  x.value2_max <- apply(x,2,max)
  x.value_end <- apply(x,2,sum)
  x.value_now <- colSums(x[1:nrow(Y.model),])
  x.duration <- x.date_end-x.date_begin+1
  # x.date_begin <- todate(x.date_begin)
  # x.date_max<- todate(x.date_max)
  # x.date_end <- todate(x.date_end)
  data.table(state=colnames(x),
             date_begin=x.date_begin,date_max=x.date_max,date_end=x.date_end,
             duration=x.duration,value_max=x.value_max,value2_max=x.value2_max,
             value_now=x.value_now,value_end=x.value_end)
  })
for(i in 2:4){
  rlts.index[[i]]$value_end <- ifelse(rlts.index[[i]]$value_end>rlts.index[[i-1]]$value_end,rlts.index[[i]]$value_end,rlts.index[[i-1]]$value_end)
  rlts.index[[i]]$date_end <- ifelse(rlts.index[[i]]$date_end>rlts.index[[i-1]]$date_end,rlts.index[[i]]$date_end,rlts.index[[i-1]]$date_end)
}
rlts.index <- lapply(1:4,function(i){
  Y.raw <- cbind(total=rowSums(Y.raw),Y.raw)
  temp <- c(nrow(Y.raw),nrow(Y.raw)+7,nrow(Y.raw)+14,nrow(Y.raw)+28)
  idxi <- rlts.index[[i]]
  diffi <- rlts.diff[[i]]
  rlti <- rlts[[i]]
  data.table(idxi,sapply(1:nrow(idxi),function(j){
    # c(slopeup = mean(diffi[1:(temp[i]-1),j]),
    #   slopedown = mean(diffi[c(temp[i]:(idxi$date_end[j]-1)),j]))
    # c(slopeup = mean(diffi[1:idxi$date_max[j]]),
    #   slopedown = mean(diffi[idxi$date_max[j]:idxi$date_end[j]]))
    c(slopeup = (rlti[temp[i],j]-rlti[idxi$date_begin[j],j])/(temp[i]-idxi$date_begin[j]+1),
      slopedown = (rlti[idxi$date_end[j],j]-rlti[temp[i],j])/(idxi$date_end[j]-temp[i]+1))
    }) %>% t) %>% mutate(date_begin=todate(date_begin),
                       date_max=todate(date_max),
                       date_end=todate(date_end))
})
mean(rlts.diff[[1]][1:129,2])
sapply(rlts.index,function(x){c(x$slopeup[3],x$slopedown[3])})
write.csv(do.call(cbind,rlts.index),'temp.csv')

#Risk Calculation
# rlts.index <- do.call(cbind,rlts.index)
# rlt <- data.table(state=rlts.index[,c(1,4,8)],rlts.index[,colnames(rlts.index) %in% c('date_end','value_end')])
# for(i in c(4,6,8,10)){
#   rlt <- cbind(rlt,risk_date=rlt[[i]]-rlt[[2]])
#   rlt <- cbind(rlt,risk_value=rlt[[i+1]]-rlt[[3]])
# }
# colnames(rlt) <- c('state',
#   paste(rep(c('date_end','value_end'),4),rep(c('now','scenario1','scenario2','scenario3','scenario4'),each=2),sep='_'),
#   paste(rep(c('risk_date','risk_end'),4),rep(c('scenario1','scenario2','scenario3','scenario4'),each=2),sep='_'))
# write.csv(rlt,'temp.csv')

#Nat Plot

grid::grid.newpage() 
grid::pushViewport(grid::viewport(layout = grid::grid.layout(4,2))) 
vplayout <- function(x,y){grid::viewport(layout.pos.row = x, layout.pos.col = y)} 

p <- ggline((rlts[[1]][1:150,-1]));print(p+labs(y='New Cases'), vp = vplayout(1,1))
p <- ggline(apply(rlts[[1]][1:150,-1],2,cumsum));print(p+labs(y='Accumulated Cases'), vp = vplayout(1,2))
p <- ggline((rlts[[2]][1:150,-1]));print(p+labs(y='New Cases'), vp = vplayout(2,1))
p <- ggline(apply(rlts[[2]][1:150,-1],2,cumsum));print(p+labs(y='Accumulated Cases'), vp = vplayout(2,2))
p <- ggline((rlts[[3]][1:150,-1]));print(p+labs(y='New Cases'), vp = vplayout(3,1))
p <- ggline(apply(rlts[[3]][1:150,-1],2,cumsum));print(p+labs(y='Accumulated Cases'), vp = vplayout(3,2))
p <- ggline((rlts[[4]][1:150,-1]));print(p+labs(y='New Cases'), vp = vplayout(4,1))
p <- ggline(apply(rlts[[4]][1:150,-1],2,cumsum));print(p+labs(y='Accumulated Cases'), vp = vplayout(4,2))

write.csv(sapply(rlts,function(x){x[,1]}),'temp.csv')

grid::grid.newpage() 
grid::pushViewport(grid::viewport(layout = grid::grid.layout(2,2)))
vplayout <- function(x,y){grid::viewport(layout.pos.row = x, layout.pos.col = y)} 
print(ggstack(rlts[[1]][1:150,]),vp=vplayout(1,1))
print(ggstack(rlts[[2]][1:150,]),vp=vplayout(1,2))
print(ggstack(rlts[[3]][1:150,]),vp=vplayout(2,1))
print(ggstack(rlts[[4]][1:150,]),vp=vplayout(2,2))

######################
#ARIMA
######################

y <- raw.d
x <- raw.d/raw.c
temp <- data.frame(date=as.POSIXct("2020-01-20") + 3600*24*1:nrow(x),x[,match(names(-sort(-apply(y,2,max)))[c(1:11)],colnames(x))])
colnames(temp)
temp <- temp[,-8]
colnames(temp) <- toupper(colnames(temp))
temp <- melt(temp,id='DATE') %>% 
  filter(variable!='IRAN'&!is.na(value)) %>%
  mutate(value=value*100)
colnames(temp)[1:3] <- c('Date','Country','Death')
ggplot(temp,aes(colour=Country,y=Death,x=Date)) + geom_line(size=1) + labs(y='Case Fatality%')

y <- rowSums(raw.d)
x <- rowSums(raw.c)
y <- y/x
y1 <- c(rep(NA,length=length(y)),rep(mean(y[-1:-35]),150))[1:150]
y2 <- c(y,rep(NA,150))[1:150]
x <- data.frame(date=as.POSIXct("2020-01-20") + 3600*24*1:150,Actual=y2,Predict=y1)
x <- melt(x,id='date') %>% mutate(value=value*100,Legend=variable)
ggplot(x,aes(colour=Legend,y=value,x=date)) + geom_line(size=1) + ylim(0,5) + labs(x='Date',y='Case Fatality(%)')


x <- raw.c[,colSums(raw.d)>0]
y <- raw.d[,colSums(raw.d)>0]
write.csv(cbind(x,y),'temp.csv')
