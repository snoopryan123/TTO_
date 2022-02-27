library(tidyverse)

# load data
input_file = "../../data/TTO_dataset_510.csv"  
D <- read_csv(input_file) #; D <- D %>% drop_na() 
D <- D %>% filter(YEAR == 2019) 
X <- as.matrix(D %>% select(std_WOBA_FINAL_BAT_19, std_WOBA_FINAL_PIT_19, #FIXME 
                            HAND_MATCH, BAT_HOME_IND)) 
###OUTPUT_FILE = "rstan5_sim1-results_A.R" 

source("rstan5_sim1_bsn_main.R")


rmse_vec = numeric(25)
mae_vec = numeric(25)
covg_vec = numeric(25)
beta_covered = matrix(nrow=25, ncol=14)
beta_lengths = matrix(nrow=25, ncol=14)
gamma_covered = matrix(nrow=25, ncol=4)
gamma_lengths = matrix(nrow=25, ncol=4)
eta_covered = matrix(nrow=25,ncol=4)
eta_lengths = matrix(nrow=25,ncol=4)
sigma_covered = matrix(nrow=25,ncol=1)
sigma_lengths = matrix(nrow=25,ncol=1)
beta_plus_gamma_covered = matrix(nrow=25, ncol=27)
beta_plus_gamma_lengths = matrix(nrow=25, ncol=27)
d12_lower = matrix(nrow=25,ncol=1)
d12_upper = matrix(nrow=25,ncol=1)
d23_lower = matrix(nrow=25,ncol=1)
d23_upper = matrix(nrow=25,ncol=1)
###train_idxs = which(folds != 1); test_idxs = which(folds == 1)
test_idxs = folds == 1
for (i in 1:25) {
  ii = 60 + i
  print(ii)

  # posterior samples & y vector
  fit <- readRDS(paste0("job_output/fit_rstan5_sim1-",ii,".R.rds"))
  draws <- as.matrix(fit)
  y <- readRDS(paste0("job_output/y_rstan5_sim1-",ii,".R.rds"))
  epsilon_true <- readRDS(paste0("job_output/epsilon_rstan5_sim1-",ii,".R.rds"))
  y_train = y[!test_idxs,]
  y_test = y[test_idxs,]
  X_test = X[test_idxs,]
  U_test = U[test_idxs,]
  O_test = O[test_idxs,]
  n_test = nrow(X_test)

  # rmse
  beta_draws = draws[,str_detect(colnames(draws), "^beta")]
  gamma_draws = draws[,str_detect(colnames(draws), "^gamma")]
  eta_draws = draws[,str_detect(colnames(draws), "^eta")]
  sigma_draws = draws[,str_detect(colnames(draws), "^sigma")]
  post_pred_means = U_test %*% t(beta_draws) + O_test %*% t(gamma_draws) + X_test %*% t(eta_draws)
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
  mae = sum(abs(ppmean - y_test))/n_test 
  mae_vec[i] = mae
  # coverage of test data
  covered = pplower <= y_test & y_test <= ppupper
  covg = sum(covered)/length(covered)
  covg_vec[i] = covg
  # coverage of true beta
  beta_lower = apply(beta_draws, 2, function(x) quantile(x,.025))
  beta_true = beta[1:length(beta_lower)]
  beta_upper = apply(beta_draws, 2, function(x) quantile(x,.975))
  beta_covered[i,] = beta_lower <= beta_true & beta_true <= beta_upper
  beta_lengths[i,] = beta_upper - beta_lower
  # coverage of true gamma
  gamma_lower = apply(gamma_draws, 2, function(x) quantile(x,.025))
  gamma_true = gamma[1:length(gamma_lower)]
  gamma_upper = apply(gamma_draws, 2, function(x) quantile(x,.975))
  gamma_covered[i,] = gamma_lower <= gamma_true & gamma_true <= gamma_upper
  gamma_lengths[i,] = gamma_upper - gamma_lower
  # coverage of true eta
  eta_lower = apply(eta_draws, 2, function(x) quantile(x,.025))
  eta_true = eta
  eta_upper = apply(eta_draws, 2, function(x) quantile(x,.975))
  eta_covered[i,] = eta_lower <= eta_true & eta_true <= eta_upper
  eta_lengths[i,] = eta_upper - eta_lower
  # coverage of true sigma
  sigma_lower = quantile(sigma_draws,.025)
  sigma_true = sigma
  sigma_upper = quantile(sigma_draws,.975)
  sigma_covered[i,] = sigma_lower <= sigma_true & sigma_true <= sigma_upper
  sigma_lengths[i,] = sigma_upper - sigma_lower

  # beta_plus_gamma_lengths
  beta_plus_gamma_draws = cbind(beta_draws[,1:9], beta_draws[,1:9], beta_draws[,1:9]) +
    cbind(gamma_draws[,1],gamma_draws[,1],gamma_draws[,1],gamma_draws[,1],
          gamma_draws[,1],gamma_draws[,1],gamma_draws[,1],gamma_draws[,1],gamma_draws[,1],
          gamma_draws[,2],gamma_draws[,2],gamma_draws[,2],gamma_draws[,2],
          gamma_draws[,2],gamma_draws[,2],gamma_draws[,2],gamma_draws[,2],gamma_draws[,2],
          gamma_draws[,3],gamma_draws[,3],gamma_draws[,3],gamma_draws[,3],
          gamma_draws[,3],gamma_draws[,3],gamma_draws[,3],gamma_draws[,3],gamma_draws[,3]) 
  beta_plus_gamma_true = c(beta[1:9], beta[1:9], beta[1:9]) +
    c(gamma[1],gamma[1],gamma[1],gamma[1],
          gamma[1],gamma[1],gamma[1],gamma[1],gamma[1],
          gamma[2],gamma[2],gamma[2],gamma[2],
          gamma[2],gamma[2],gamma[2],gamma[2],gamma[2],
          gamma[3],gamma[3],gamma[3],gamma[3],
          gamma[3],gamma[3],gamma[3],gamma[3],gamma[3]) 
  beta_plus_gamma_lower = apply(beta_plus_gamma_draws, 2, function(x) quantile(x,.025))
  beta_plus_gamma_upper = apply(beta_plus_gamma_draws, 2, function(x) quantile(x,.975))
  beta_plus_gamma_covered[i,] = beta_plus_gamma_lower <= beta_plus_gamma_true & beta_plus_gamma_true <= beta_plus_gamma_upper
  beta_plus_gamma_lengths[i,] = beta_plus_gamma_upper - beta_plus_gamma_lower
  
  # detect true TTO effect
  b9_g1 = beta_draws[,"beta[9]"] + gamma_draws[,"gamma[1]"]
  b1_g2 = beta_draws[,"beta[1]"] + gamma_draws[,"gamma[2]"]
  b9_g2 = beta_draws[,"beta[9]"] + gamma_draws[,"gamma[2]"]
  b1_g3 = beta_draws[,"beta[1]"] + gamma_draws[,"gamma[3]"]
  d12 = b1_g2 - b9_g1
  d23 = b1_g3 - b9_g2
  d12_lower[i,] = quantile(d12, .025)
  d12_upper[i,] = quantile(d12, .975)
  d23_lower[i,] = quantile(d23, .025)
  d23_upper[i,] = quantile(d23, .975)
  
  # PLOTS
  # bg_plot = plot_beta_plus_gamma_post(beta_draws, gamma_draws)
  # eta_plot = plot_eta_post(eta_draws)
  #ggsave(paste0("sim1_", i, "_bgPlot.png"), bg_plot)
  ####ggsave(paste0("sim1_", i, "_etaPlot.png"), eta_plot)
}

# # save data
# saveRDS(rmse_vec, paste0("job_output/rstan5_sim1-results_A_","rmse_vec",".R.rds"))
# saveRDS(covg_vec, paste0("job_output/rstan5_sim1-results_A_","covg_vec",".R.rds"))
# saveRDS(alpha_covered, paste0("job_output/rstan5_sim1-results_A_","alpha_covered",".R.rds"))
# saveRDS(eta_covered, paste0("job_output/rstan5_sim1-results_A_","eta_covered",".R.rds"))
# saveRDS(sigma_covered, paste0("job_output/rstan5_sim1-results_A_","sigma_covered",".R.rds"))
# # rmse_vec = readRDS(paste0("job_output/rstan5_sim1-results_A_","rmse_vec",".R.rds"))
# # covg_vec = paste0("job_output/rstan5_sim1-results_A_","covg_vec",".R.rds")
# # alpha_covered = paste0("job_output/rstan5_sim1-results_A_","alpha_covered",".R.rds")
# # eta_covered = paste0("job_output/rstan5_sim1-results_A_","eta_covered",".R.rds")
# # sigma_covered = paste0("job_output/rstan5_sim1-results_A_","sigma_covered",".R.rds")

print("UBI sim results")
print("average rmse")
print(mean(rmse_vec))
print("average mae")
print(mean(mae_vec))
print("average coverage of y")
print(mean(covg_vec))
print("average proportion of parameters that are covered")
param_covered = cbind(beta_covered, gamma_covered, eta_covered, sigma_covered)
prop_params_covered = rowSums(param_covered)/ncol(param_covered)
print(mean(prop_params_covered))
print("average length of credible intervals for beta_plus_gamma")
#####mean( rowSums(beta_plus_gamma_lengths)/ncol(beta_plus_gamma_lengths) )
print(mean( rowSums(beta_plus_gamma_lengths[,1:27])/ncol(beta_plus_gamma_lengths[,1:27]) ))
##colSums(param_lengths)/nrow(param_lengths)
# see if it found TTO effect.
print("proportion of b1+g2 - b9+g1 posterior intervals which are entirely positive")
print(sum(d12_lower > 0)/length(d12_lower))
print("proportion of b1+g3 - b9+g2 posterior intervals which are entirely positive")
print(sum(d23_lower > 0)/length(d23_lower))
print("(mean,sd) of d12_lower")
print(c(mean(d12_lower), sd(d12_lower)))
print("(mean,sd) of d23_lower")
print(c(mean(d23_lower), sd(d23_lower)))
print("b,m,t_2,t_3")
print(c(b,m,t_2,t_3))
print("mean_y, sd_y")
print(c(mean(y_generated), sd(y_generated)))






# # average p.p interval length for UBI model
# param_lengths = cbind(beta_lengths, gamma_lengths, delta_lengths, sigma_lengths)
# mean( rowSums(beta_plus_gamma_lengths)/ncol(beta_plus_gamma_lengths) )
# colSums(beta_plus_gamma_lengths)/nrow(beta_plus_gamma_lengths)
# colSums(param_lengths)/nrow(param_lengths)
# # beta coverage for UBI model
# colSums(beta_covered)/nrow(beta_covered)
# # gamma coverage for UBI model
# colSums(gamma_covered)/nrow(gamma_covered)
# # delta coverage for UBI model
# colSums(delta_covered)/nrow(delta_covered)
# # sigma coverage for UBI model
# colSums(sigma_covered)/nrow(sigma_covered)
# # all parameter coverage for UBI model
# covgg = colSums(param_covered)/nrow(param_covered)
# covgg_names = c(paste0("beta_",1:14), paste0("gamma_",1:4), paste0("eta_",1:4), "sigma")
# covgg_df = tibble(param = covgg_names, avg_covg = covgg)
# #covgg_df
# #View(covgg_df)
# # all parameter length for BSN model
# legg = colSums(param_lengths)/nrow(param_lengths)
# legg_names = c(paste0("beta_",1:14), paste0("gamma_",1:4), paste0("eta_",1:4), "sigma")
# legg_df = tibble(param = legg_names, avg_length = legg)
# #legg_df
# #View(legg_df)
# 
# 
# 
# # average p.p interval length for BSN model
# param_lengths = cbind(alpha_lengths, eta_lengths, sigma_lengths)
# mean( rowSums(alpha_lengths)/ncol(alpha_lengths) )
# mean( rowSums(alpha_lengths[,1:27])/ncol(alpha_lengths[,1:27]) )
# mean( rowSums(eta_lengths)/ncol(eta_lengths) )
# mean( rowSums(sigma_lengths)/ncol(sigma_lengths) )
# mean( rowSums(param_lengths)/ncol(param_lengths) )
# colSums(param_lengths)/nrow(param_lengths)
# # alpha coverage for BSN model
# colSums(alpha_covered)/nrow(alpha_covered)
# # eta coverage for BSN model
# colSums(eta_covered)/nrow(eta_covered)
# # sigma coverage for BSN model
# colSums(sigma_covered)/nrow(sigma_covered)
# #####
# # all parameter coverage for BSN model
# covgg = colSums(param_covered)/nrow(param_covered)
# covgg_names = c(paste0("alpha_",1:36), paste0("eta_",1:4), "sigma")
# covgg_df = tibble(param = covgg_names, avg_covg = covgg)
# covgg_df
# #View(covgg_df)
# # all parameter length for BSN model
# legg = colSums(param_lengths)/nrow(param_lengths)
# legg_names = c(paste0("alpha_",1:36), paste0("eta_",1:4), "sigma")
# legg_df = tibble(param = legg_names, avg_length = legg)
# legg_df
# #View(legg_df)
# #library(kableExtra)










