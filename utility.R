# samping X
X.sampling <- function(n, p, rho = 0){
  Sigma <- toeplitz(rho^(0:(p-1)))
  X <- matrix(rnorm(n*p), n) %*% chol(Sigma)
  return(X)
} 

y.sampling <- function(X, Beta0, sd){
  n <- dim(X)[1]
  y <- X %*% Beta0 + rnorm(n, sd = sd)
  return(y)
}

#### post selection
post_selection <- function(X,y, idx){
  larfit <- selectiveInference::lar(X,y)
  hsigma <- selectiveInference::estimateSigma(X,y)$sigmahat
  out.seq = selectiveInference::larInf(larfit, sigma = hsigma, k = 30)
  if(idx %in% out.seq$vars){
    pvalue <- out.seq$pv[which(out.seq$vars == idx)]
  }else{
    pvalue = 1
  }
  return(pvalue)
}
