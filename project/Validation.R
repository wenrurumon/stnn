

############################
# Validation
############################

Y.raw <- rbind(0,raw.c)
Y.model <- apply(Y.raw,2,diff)
model.vali <- lapply(1:5,function(sel){
  w <- 14
  chinaw <- 10
  mfile <- get_model_xy(Y.model,p=8,gety=T,w=w,sel=sel)
  mfile$Y <- mfile$Y[c(1:nrow(mfile$X),rep(which(rownames(mfile$X)=='china'),chinaw)),,drop=F]
  mfile$X <- mfile$X[c(1:nrow(mfile$X),rep(which(rownames(mfile$X)=='china'),chinaw)),,drop=F]
  mfile$X <- cbind(mfile$X,state=as.numeric(rownames(mfile$X)=='china'))
  set.seed(4)
  models.vali <- lapply(1:5,function(i){
    print(paste(i,Sys.time()))
    MSAE(X=mfile$X,Y=mfile$Y,
         dims=c(32,4),activations=c('relu','relu'),
         batch=128,epochs=1000,verbose=0)
  })
  mfile.vali <- lapply((nrow(Y.model)-9):0,function(i){
    temp <- get_model_file(x=Y.model,i=nrow(Y.model)-8-i,p=8,gety=FALSE)
    temp$x <- cbind(temp$x,as.numeric(rownames(temp$x)=='china'))
    return(temp)
  })
  mfile.vali <- rbind(NA,NA,NA,NA,NA,NA,NA,NA,
                      sapply(mfile.vali,function(x){
                        x <- sapply(models.vali,function(m){
                          ((m$model %>% predict(x$x)) * x$f)
                        }) %>% rowMeans
                        ifelse(x<0,0,x)
                      }) %>% t
  )
  mfile.vali
})
model.vali <- lapply(1:length(model.vali),function(i){
  x <- model.vali[[i]]
  for(j in 1:(nrow(x)-i)){x[j,] <- x[j,]+Y.raw[j,]}
  for(j in (nrow(x)-i+1):nrow(x)){x[j,] <- x[j,]+x[j-1,]}
  x
})

write.csv(cbind(rowSums(Y.raw)[-1],sapply(model.vali,rowSums)
                ,rowSums(Y.raw[,-1])[-1],sapply(model.vali,function(x){rowSums(x[,-1])})
                ,(Y.raw[,1])[-1],sapply(model.vali,function(x){(x[,1])})),'oversee/model_38/temp.csv')
