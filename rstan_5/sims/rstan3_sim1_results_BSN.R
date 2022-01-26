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
covg_vec = numeric(25)
alpha_covered = matrix(nrow=25, ncol=36)
alpha_lengths = matrix(nrow=25, ncol=36)
eta_covered = matrix(nrow=25,ncol=4)
eta_lengths = matrix(nrow=25,ncol=4)
sigma_covered = matrix(nrow=25,ncol=1)
sigma_lengths = matrix(nrow=25,ncol=1)
d12_lower = matrix(nrow=25,ncol=1)
d12_upper = matrix(nrow=25,ncol=1)
d23_lower = matrix(nrow=25,ncol=1)
d23_upper = matrix(nrow=25,ncol=1)
###train_idxs = which(folds != 1); test_idxs = which(folds == 1)
test_idxs = folds == 1
for (i in 1:25) {
  print(i)

  # posterior samples & y vector
  fitB <- readRDS(paste0("job_output/fit_rstan5_sim1-",i,".R.rds"))
  drawsB <- as.matrix(fitB)
  y <- readRDS(paste0("job_output/y_rstan5_sim1-",i,".R.rds"))
  epsilon_true <- readRDS(paste0("job_output/epsilon_rstan5_sim1-",i,".R.rds"))
  y_train = y[!test_idxs,]
  y_test = y[test_idxs,]
  X_test = X[test_idxs,]
  S_test = S[test_idxs,]
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
  alpha_lower = apply(alpha_draws, 2, function(x) quantile(x,.025))
  alpha_true = alpha[1:length(alpha_lower)]
  alpha_upper = apply(alpha_draws, 2, function(x) quantile(x,.975))
  alpha_covered[i,] = alpha_lower <= alpha_true & alpha_true <= alpha_upper
  alpha_lengths[i,] = alpha_upper - alpha_lower
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

  
  # detect true TTO effect
  a9 = alpha_draws[,"alpha[9]"]
  a10 = alpha_draws[,"alpha[10]"]
  a18 = alpha_draws[,"alpha[18]"]
  a19 = alpha_draws[,"alpha[19]"]
  d12 = a10 - a9
  d23 = a19 - a18
  d12_lower[i,] = quantile(d12, .025)
  d12_upper[i,] = quantile(d12, .975)
  d23_lower[i,] = quantile(d23, .025)
  d23_upper[i,] = quantile(d23, .975)
  
  # PLOTS
  # alpha_plot = plot_alpha_post(alpha_draws)
  # eta_plot = plot_eta_post(eta_draws)
  ####ggsave(paste0("sim1_", i, "_alphaPlot.png"), alpha_plot)
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


print("average rmse")
print(mean(rmse_vec))
print("average coverage of y")
print(mean(covg_vec))
print("average proportion of parameters that are covered")
param_covered = cbind(alpha_covered, eta_covered, sigma_covered)
prop_params_covered = rowSums(param_covered)/ncol(param_covered)
print(mean(prop_params_covered))
print("average length of credible intervals for alpha")
#####mean( rowSums(alpha_lengths)/ncol(alpha_lengths) )
print(mean( rowSums(alpha_lengths[,1:27])/ncol(alpha_lengths[,1:27]) ))
##colSums(param_lengths)/nrow(param_lengths)
# see if it found TTO effect.
print("proportion of alpha_10 - alpha_9 posterior intervals which are entirely positive")
print(sum(d12_lower > 0)/length(d12_lower))
print("proportion of alpha_19 - alpha_18 posterior intervals which are entirely positive")
print(sum(d23_lower > 0)/length(d23_lower))







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
# 










