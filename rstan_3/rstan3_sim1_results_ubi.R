
source("rstan3_sim1_main.R")

rmse_vec = numeric(25)
covg_vec = numeric(25)
beta_covered = matrix(nrow=25, ncol=14)
gamma_covered = matrix(nrow=25, ncol=4)
delta_covered = matrix(nrow=25,ncol=4)
sigma_covered = matrix(nrow=25,ncol=1)
for (i in 1:25) {
  print(i)
  
  # posterior samples & y vector
  fitU <- readRDS(paste0("job_output/fit_rstan3_sim1_ubi-",i,".R.rds")) 
  drawsU <- as.matrix(fitU)
  y <- readRDS(paste0("job_output/y_rstan3_sim1_ubi-",i,".R.rds")) 
  epsilon_true <- readRDS(paste0("job_output/epsilon_rstan3_sim1_ubi-",i,".R.rds")) 
  y_train = y[!test_idxs]
  y_test = y[test_idxs]
  n_test = nrow(X_test)
  
  # rmse
  beta_draws = drawsU[,str_detect(colnames(drawsU), "^beta")]
  gamma_draws = drawsU[,str_detect(colnames(drawsU), "^gamma")]
  delta_draws = drawsU[,str_detect(colnames(drawsU), "^delta")]
  sigma_draws = drawsU[,str_detect(colnames(drawsU), "^sigma")]
  post_pred_means = U_test %*% t(beta_draws) + O_test %*% t(gamma_draws) + X_test %*% t(delta_draws)
  epsilon0 = matrix(sapply(sigma_draws, function(s) { rnorm(1,0,sd=s) }), nrow=1)
  epsilon = do.call(rbind, replicate(n_test, epsilon0, simplify=FALSE))
  post_pred = post_pred_means + epsilon
  # posterior predictive intervals on test data
  pplower = apply(post_pred, 1, function(x) quantile(x,.025))
  ppmean = apply(post_pred, 1, function(x) mean(x))
  ppupper = apply(post_pred, 1, function(x) quantile(x,.975))
  # rmse of test data
  rmse = sqrt(sum((ppmean - y_test)^2)/n_test)
  rmse_vec[i] = rmse
  # coverage of test data
  covered = pplower <= y_test & y_test <= ppupper
  covg = sum(covered)/length(covered)
  covg_vec[i] = covg
  # coverage of true beta
  beta_post <- as.matrix(drawsU[,2:15])
  beta_lower = apply(beta_post, 2, function(x) quantile(x,.025))
  beta_true = beta[1:length(beta_lower)]
  beta_upper = apply(beta_post, 2, function(x) quantile(x,.975))
  beta_covered[i,] = beta_lower <= beta_true & beta_true <= beta_upper
  # coverage of true gamma
  gamma_post <- as.matrix(drawsU[,16:19])
  gamma_lower = apply(gamma_post, 2, function(x) quantile(x,.025))
  gamma_true = gamma[1:length(gamma_lower)]
  gamma_upper = apply(gamma_post, 2, function(x) quantile(x,.975))
  gamma_covered[i,] = gamma_lower <= gamma_true & gamma_true <= gamma_upper
  # coverage of true delta
  delta_post <- as.matrix(drawsU[,(ncol(drawsU)-4):(ncol(drawsU)-1)])
  delta_lower = apply(delta_post, 2, function(x) quantile(x,.025))
  delta_true = delta
  delta_upper = apply(delta_post, 2, function(x) quantile(x,.975))
  delta_covered[i,] = delta_lower <= delta_true & delta_true <= delta_upper
  # coverage of true sigma
  sigma_post <- drawsU[,1]
  sigma_lower = quantile(sigma_post,.025)
  sigma_true = sigma
  sigma_upper = quantile(sigma_post,.975)
  sigma_covered[i,] = sigma_lower <= sigma_true & sigma_true <= sigma_upper
  
  # PLOTS
  bg_plot = plot_beta_plus_gamma_post(beta_post, gamma_post)
  #ggsave(paste0("sim1_", i, "_bgPlot.png"), bg_plot)
  delta_plot = plot_delta_post(delta_post)
  #ggsave(paste0("sim1_", i, "_deltaPlot.png"), delta_plot)
}

# average rmse for BSN model
mean(rmse_vec)
# average coverage for BSN model
mean(covg_vec)
# average parameter-coverage for BSN model
param_covered = cbind(beta_covered, gamma_covered, delta_covered, sigma_covered)
mean( rowSums(param_covered)/ncol(param_covered) )
# beta coverage for BSN model
colSums(beta_covered)/nrow(beta_covered)
# gamma coverage for BSN model
colSums(gamma_covered)/nrow(gamma_covered)
# delta coverage for BSN model
colSums(delta_covered)/nrow(delta_covered)





