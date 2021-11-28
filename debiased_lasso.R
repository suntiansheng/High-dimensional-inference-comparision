library(lars)
library(scalreg)
# estimate weight
weight_estimation <- function(Xj, Xnj, p, C = 1){
  lar_fit <- lars::lars(Xnj, Xj, type = 'lar', intercept = FALSE, normalize = FALSE)
  etaj = 0
  k = 1
  while (etaj <= C*log(p)) {
    stop_idx <- dim(lar_fit$beta)[1]-1
    res_coef <- lar_fit$beta[k,]
    Zj <- Xj - Xnj %*% res_coef
    Wj <- Zj/as.numeric(t(Zj)%*%Xj)
    tauj <- sqrt(as.numeric(t(Wj)%*%Wj))
    etaj <- max(t(X)%*%Wj)/tauj
    if(k < stop_idx){
      k = k + 1
    }else{
      print('C is too large')
      break()
    }
  }
  return(list(Wj = Wj, tauj = tauj))
}


# main function
#idx which coefficient should be infered
debiased_lasso.fit <- function(X, y, idx){
  n <- dim(X)[1]
  p <- dim(X)[2]
  # initial with scaled lasso
  initial_fit <- scalreg::scalreg(X, y, lam0 = 'univ')
  Beta_init <- initial_fit$coefficients
  sigma_init <- initial_fit$hsigma
  
  # Choice of weight
  Xj <- X[,idx]
  Xnj <- X[,-idx]
  weight_fit <- weight_estimation(Xj, Xnj, p, C = 1)
  wj <- weight_fit$Wj
  hbeta_j <- Beta_init[idx] + t(wj) %*% (y - X %*% Beta_init)
  return(list(hbeta_j = hbeta_j, tauj = weight_fit$tauj, hsigma = sigma_init))
}

debiased.pvalue <- function(X, y, idx){
  fit <- debiased_lasso.fit(X, y, idx)
  beta_hat <- fit$hbeta_j
  sigma_hat <- fit$hsigma
  tauj <- fit$tauj
  t <- as.numeric((beta_hat)/(tauj*sigma_hat))
  pvalue <- pnorm(abs(t), lower.tail = FALSE)
  return(pvalue)
}



  