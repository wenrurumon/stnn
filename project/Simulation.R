#Simulation - defense ASAP
temp <- Y.model
for(i in 1:300){
  x1 <- x2 <- x3 <- get_model_file(temp,i=nrow(temp)-7,p=8,gety=F)
  x1$x <- cbind(x1$x,1); x1$x[rownames(x1$x)=='china',9] <- 1
  x2$x <- cbind(x2$x,0.5); x2$x[rownames(x2$x)=='china',9] <- 1
  x3$x <- cbind(x3$x,0); x3$x[rownames(x3$x)=='china',9] <- 1
  x1 <- (sapply(models.pred,function(m){m$model %>% predict(x1$x)}) * x1$f) %>% rowMeans
  x2 <- (sapply(models.pred,function(m){m$model %>% predict(x2$x)}) * x2$f) %>% rowMeans
  x3 <- (sapply(models.pred,function(m){m$model %>% predict(x3$x)}) * x3$f) %>% rowMeans
  x1 <- ifelse(x1<0,0,x1); x2 <- ifelse(x2<x1,x1,x2); x3 <- ifelse(x3<x2,x2,x3)
  if(i %in% 1:7){temp <- rbind(temp,x1)}else{temp <- rbind(temp,x1)}
}
temp[-1:-nrow(Y.actual),apply(Y.model,2,function(x){sum(x>0)<5})] <- 0
rlts[[length(rlts)+1]] <- temp
ggstack(temp[1:150,])

# for(i in 1:5){keras::save_model_hdf5(models.pred[[i]]$model,paste0('/Users/wenrurumon/Documents/posdoc/wuhan/oversee/osmodel',i,'.model'),overwrite = TRUE,include_optimizer = TRUE)}

# Other Scenarios
#0.5,1
temp <- Y.model
for(i in 1:300){
  x1 <- x2 <- x3 <- get_model_file(temp,i=nrow(temp)-7,p=8,gety=F)
  x1$x <- cbind(x1$x,1); x1$x[rownames(x1$x)=='china',9] <- 1
  x2$x <- cbind(x2$x,0.5); x2$x[rownames(x2$x)=='china',9] <- 1
  x3$x <- cbind(x3$x,0); x3$x[rownames(x3$x)=='china',9] <- 1
  x1 <- (sapply(models.pred,function(m){m$model %>% predict(x1$x)}) * x1$f) %>% rowMeans
  x2 <- (sapply(models.pred,function(m){m$model %>% predict(x2$x)}) * x2$f) %>% rowMeans
  x3 <- (sapply(models.pred,function(m){m$model %>% predict(x3$x)}) * x3$f) %>% rowMeans
  x1 <- ifelse(x1<0,0,x1); x2 <- ifelse(x2<x1,x1,x2); x3 <- ifelse(x3<x2,x2,x3)
  if(i %in% 1:7){temp <- rbind(temp,x2)}else{temp <- rbind(temp,x1)}
}
temp[-1:-nrow(Y.actual),apply(Y.model,2,function(x){sum(x>0)<5})] <- 0
rlts[[length(rlts)+1]] <- temp
ggstack(temp[1:150,])
sum(temp)

#0,0.5,1
temp <- Y.model
for(i in 1:300){
  x1 <- x2 <- x3 <- get_model_file(temp,i=nrow(temp)-7,p=8,gety=F)
  x1$x <- cbind(x1$x,1); x1$x[rownames(x1$x)=='china',9] <- 1
  x2$x <- cbind(x2$x,0.5); x2$x[rownames(x2$x)=='china',9] <- 1
  x3$x <- cbind(x3$x,0); x3$x[rownames(x3$x)=='china',9] <- 1
  x1 <- (sapply(models.pred,function(m){m$model %>% predict(x1$x)}) * x1$f) %>% rowMeans
  x2 <- (sapply(models.pred,function(m){m$model %>% predict(x2$x)}) * x2$f) %>% rowMeans
  x3 <- (sapply(models.pred,function(m){m$model %>% predict(x3$x)}) * x3$f) %>% rowMeans
  x1 <- ifelse(x1<0,0,x1); x2 <- ifelse(x2<x1,x1,x2); x3 <- ifelse(x3<x2,x2,x3)
  if(i %in% 1:7){
    temp <- rbind(temp,x3)
  }else if(i%in%8:14){
    temp <- rbind(temp,x2)
  } else {
    temp <- rbind(temp,x1)
  }
}
temp[-1:-nrow(Y.actual),apply(Y.model,2,function(x){sum(x>0)<5})] <- 0
rlts[[length(rlts)+1]] <- temp
ggstack(temp[1:150,])

#0,0.5,0.5,0.5,1
temp <- Y.model
for(i in 1:300){
  x1 <- x2 <- x3 <- get_model_file(temp,i=nrow(temp)-7,p=8,gety=F)
  x1$x <- cbind(x1$x,1); x1$x[rownames(x1$x)=='china',9] <- 1
  x2$x <- cbind(x2$x,0.5); x2$x[rownames(x2$x)=='china',9] <- 1
  x3$x <- cbind(x3$x,0); x3$x[rownames(x3$x)=='china',9] <- 1
  x1 <- (sapply(models.pred,function(m){m$model %>% predict(x1$x)}) * x1$f) %>% rowMeans
  x2 <- (sapply(models.pred,function(m){m$model %>% predict(x2$x)}) * x2$f) %>% rowMeans
  x3 <- (sapply(models.pred,function(m){m$model %>% predict(x3$x)}) * x3$f) %>% rowMeans
  x1 <- ifelse(x1<0,0,x1); x2 <- ifelse(x2<x1,x1,x2); x3 <- ifelse(x3<x2,x2,x3)
  if(i %in% 1:7){
    temp <- rbind(temp,x3)
  }else if(i%in%8:28){
    temp <- rbind(temp,x2)
  } else {
    temp <- rbind(temp,x1)
  }
}
temp[-1:-nrow(Y.actual),apply(Y.model,2,function(x){sum(x>0)<5})] <- 0
rlts[[length(rlts)+1]] <- temp
ggstack(temp[1:150,])

#Resulting
rlts <- lapply(rlts,function(x){floor(x)})
for(i in 3:1){
  for(j in 1:ncol(rlts[[1]])){
    if(sum(rlts[[i]][,j]) > sum(rlts[[i+1]][,j])){rlts[[i]][,j] <- rlts[[i+1]][,j]}
  }
}
