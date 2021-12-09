
source("rstan3_sim1_main.R")

rmse_vec = numeric(25)
covg_vec = numeric(25)
alpha_covered = matrix(nrow=25, ncol=36)
alpha_lengths = matrix(nrow=25, ncol=36)
eta_covered = matrix(nrow=25,ncol=4)
eta_lengths = matrix(nrow=25,ncol=4)
sigma_covered = matrix(nrow=25,ncol=1)
sigma_lengths = matrix(nrow=25,ncol=1)
for (i in 1:25) {
  print(i)
  
  # posterior samples & y vector
  fitB <- readRDS(paste0("job_output/fit_rstan3_sim1_bsn-",i,".R.rds")) 
  drawsB <- as.matrix(fitB)
  y <- readRDS(paste0("job_output/y_rstan3_sim1_bsn-",i,".R.rds")) 
  epsilon_true <- readRDS(paste0("job_output/epsilon_rstan3_sim1_bsn-",i,".R.rds")) 
  y_train = y[!test_idxs]
  y_test = y[test_idxs]
  n_test = nrow(X_test)
  
  # rmse
  alpha_draws = drawsB[,str_detect(colnames(drawsB), "^alpha")]
  eta_draws = drawsB[,str_detect(colnames(drawsB), "^eta")]
  sigma_draws = drawsB[,str_detect(colnames(drawsB), "^sigma")]
  post_pred_means = S_test %*% t(alpha_draws) + X_test %*% t(eta_draws)
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
  # coverage of true alpha
  alpha_post <- as.matrix(drawsB[,2:37])
  alpha_lower = apply(alpha_post, 2, function(x) quantile(x,.025))
  alpha_true = alpha[1:length(alpha_lower)]
  alpha_upper = apply(alpha_post, 2, function(x) quantile(x,.975))
  alpha_covered[i,] = alpha_lower <= alpha_true & alpha_true <= alpha_upper
  alpha_lengths[i,] = alpha_upper - alpha_lower
  # coverage of true eta
  eta_post <- as.matrix(drawsB[,(ncol(drawsB)-4):(ncol(drawsB)-1)])
  eta_lower = apply(eta_post, 2, function(x) quantile(x,.025))
  eta_true = eta
  eta_upper = apply(eta_post, 2, function(x) quantile(x,.975))
  eta_covered[i,] = eta_lower <= eta_true & eta_true <= eta_upper
  eta_lengths[i,] = eta_upper - eta_lower
  # coverage of true sigma
  sigma_post <- drawsB[,1]
  sigma_lower = quantile(sigma_post,.025)
  sigma_true = sigma
  sigma_upper = quantile(sigma_post,.975)
  sigma_covered[i,] = sigma_lower <= sigma_true & sigma_true <= sigma_upper
  sigma_lengths[i,] = sigma_upper - sigma_lower
  
  # PLOTS
  alpha_plot = plot_alpha_post(alpha_post)
  #ggsave(paste0("sim1_", i, "_alphaPlot.png"), alpha_plot)
  eta_plot = plot_eta_post(eta_post)
  #ggsave(paste0("sim1_", i, "_etaPlot.png"), eta_plot)
}

# # save data
# saveRDS(rmse_vec, paste0("job_output/rstan3_sim1_bsn_","rmse_vec",".R.rds"))
# saveRDS(covg_vec, paste0("job_output/rstan3_sim1_bsn_","covg_vec",".R.rds"))
# saveRDS(alpha_covered, paste0("job_output/rstan3_sim1_bsn_","alpha_covered",".R.rds"))
# saveRDS(eta_covered, paste0("job_output/rstan3_sim1_bsn_","eta_covered",".R.rds"))
# saveRDS(sigma_covered, paste0("job_output/rstan3_sim1_bsn_","sigma_covered",".R.rds"))
# # rmse_vec = readRDS(paste0("job_output/rstan3_sim1_bsn_","rmse_vec",".R.rds"))
# # covg_vec = paste0("job_output/rstan3_sim1_bsn_","covg_vec",".R.rds")
# # alpha_covered = paste0("job_output/rstan3_sim1_bsn_","alpha_covered",".R.rds")
# # eta_covered = paste0("job_output/rstan3_sim1_bsn_","eta_covered",".R.rds")
# # sigma_covered = paste0("job_output/rstan3_sim1_bsn_","sigma_covered",".R.rds")


# average rmse for BSN model
mean(rmse_vec)
# average coverage for BSN model
mean(covg_vec)
# average parameter-coverage for BSN model
param_covered = cbind(alpha_covered, eta_covered, sigma_covered)
mean( rowSums(param_covered)/ncol(param_covered) )
# average p.p interval length for BSN model
param_lengths = cbind(alpha_lengths, eta_lengths, sigma_lengths)
mean( rowSums(alpha_lengths)/ncol(alpha_lengths) )
mean( rowSums(alpha_lengths[,1:27])/ncol(alpha_lengths[,1:27]) )
mean( rowSums(eta_lengths)/ncol(eta_lengths) )
mean( rowSums(sigma_lengths)/ncol(sigma_lengths) )
mean( rowSums(param_lengths)/ncol(param_lengths) )
colSums(param_lengths)/nrow(param_lengths)
# alpha coverage for BSN model
colSums(alpha_covered)/nrow(alpha_covered)
# eta coverage for BSN model
colSums(eta_covered)/nrow(eta_covered)
# sigma coverage for BSN model
colSums(sigma_covered)/nrow(sigma_covered)
#####
# all parameter coverage for BSN model
covgg = colSums(param_covered)/nrow(param_covered)
covgg_names = c(paste0("alpha_",1:36), paste0("eta_",1:4), "sigma")
covgg_df = tibble(param = covgg_names, avg_covg = covgg)
covgg_df
#View(covgg_df)
# all parameter length for BSN model
legg = colSums(param_lengths)/nrow(param_lengths)
legg_names = c(paste0("alpha_",1:36), paste0("eta_",1:4), "sigma")
legg_df = tibble(param = legg_names, avg_length = legg)
legg_df
#View(legg_df)
#library(kableExtra)

# see if it found TTO effect.








