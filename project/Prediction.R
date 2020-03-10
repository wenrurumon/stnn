############################
# Prediction
############################

#Setup

Y.raw <- rbind(0,raw.c)
Y.model <- apply(Y.raw,2,diff)

#Modeling
# sel <- 0
# w <- 14
# chinaw <- 10
# set.seed(7)
# mfile <- get_model_xy(Y.model,p=8,gety=T,w=w,sel=sel)
# mfile$Y <- mfile$Y[c(1:nrow(mfile$X),rep(which(rownames(mfile$X)=='china'),chinaw)),,drop=F]
# mfile$X <- mfile$X[c(1:nrow(mfile$X),rep(which(rownames(mfile$X)=='china'),chinaw)),,drop=F]
# mfile$X <- cbind(mfile$X,state=as.numeric(rownames(mfile$X)=='china'))
# models.pred <- lapply(1:5,function(i){
#   print(paste(i,Sys.time()))
#   MSAE(X=mfile$X,Y=mfile$Y,
#        dims=c(32,4),activations=c('relu','relu'),
#        batch=128,epochs=1000,verbose=0)
# })

#models.pred
setwd("/Users/wenrurumon/Documents/posdoc/wuhan/oversee/")
setwd("model_38")
models.pred <- lapply(dir(pattern='.model'),function(x){
  list(model=keras::load_model_hdf5(x))
})

#Prediction
rlts <- list()
mfile.pred <- lapply((nrow(Y.model)-9):0,function(i){
  temp <- get_model_file(x=Y.model,i=nrow(Y.model)-8-i,p=8,gety=FALSE)
  temp$x <- cbind(temp$x,as.numeric(rownames(temp$x)=='china'))
  return(temp)
})
mfile.pred <- rbind(NA,NA,NA,NA,NA,NA,NA,NA,
                    sapply(mfile.pred,function(x){
                      x <- sapply(models.pred,function(m){
                        ((m$model %>% predict(x$x)) * x$f)
                      }) %>% rowMeans
                      ifelse(x<0,0,x)
                    }) %>% t
)
Y.actual <- (Y.model + Y.raw[-nrow(Y.raw),])
Y.fit <- (mfile.pred + Y.raw[-nrow(Y.raw),])
Y.predict <- Y.fit
