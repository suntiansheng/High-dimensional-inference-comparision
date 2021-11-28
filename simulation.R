source('./debiased_lasso.R')
source('./utility.R')
source('./splitting.R')
n <- 100
p <- 200
h_c <- c(0,0.1,0.2,0.4)
rho = 0.5
simulation_time = 100
iter_time = length(h_c)
library(foreach)
library(doParallel)

cl <- parallel::makeCluster(iter_time)
doParallel::registerDoParallel(cl)
clusterEvalQ(cl, .libPaths("../R/x86_64-pc-linux-gnu-library/3.6/"))

result_matrix <- foreach(iter = 1:iter_time, .combine = 'rbind') %dopar% {
  h <- h_c[iter]
  Beta0 <- c(2,-2,h,rep(0, p-3))
  
  debias <- NULL
  splitting <- NULL
  post <- NULL
  
  for(i in 1:simulation_time){
    
    X <- X.sampling(n,p, rho = rho)
    y <- y.sampling(X, Beta0,sd = 1)
    
    #debias[i] <- debiased.pvalue(X,y, idx = 3)
    #splitting[i] <- linear.splitting.fit(X,y, idx = 3)
    post[i] <- post_selection(X,y, idx = 3)
  }
  c(mean(post < 0.05), sd(post < 0.05), h)
  #c(mean(debias < 0.05),sd(debias < 0.05), mean(splitting < 0.05), sd(splitting < 0.05), mean(post < 0.05), sd(post < 0.05), h)
}


stopCluster(cl)
result_matrix
colnames(result_matrix) <- c('GM_fdr', 'GM_power', 'BY_fdr', 'BY_power', 'BH_fdr', 'BH_power', 'k')
