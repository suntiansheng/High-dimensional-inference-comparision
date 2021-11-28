################################################################################
library(glmnet)
linear.splitting.fit <- function(X, y, idx){
  n <- dim(X)[1]
  p <- dim(X)[2]
  sample_idx <- sample(1:n, size = floor(0.6*n))
  X1 <- X[sample_idx,]
  y1 <- y[sample_idx]
  X2 <- X[sample_idx,]
  y2 <- y[sample_idx]
  
  Lasso.fit <- glmnet::cv.glmnet(X1, y1,family = 'gaussian')
  opt <- which.min(Lasso.fit$cvm)
  coef <- Lasso.fit$glmnet.fit$beta[,opt]
  selection_set <- which(coef != 0)
  if(idx %in% selection_set){
    X2 <- X2[,selection_set]
    lm.fit <- lm(y2~X2-1)
    res_idx <- which(selection_set == idx)
    pvalue <- summary(lm.fit)$coefficients[res_idx,4]
  }else{
    pvalue <- 1
  }
  return(pvalue)
}
