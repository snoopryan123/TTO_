source("rstan8_comp_main.R")

#########################
### HELPER FUNCTIONS ####
#########################

bsn_fit_to_posterior_probs <- function(S_test,X_test,fit) {
  draws=as.matrix(fit)
  alpha_draws = draws[,str_detect(colnames(draws), "^alpha")]
  eta_draws = draws[,str_detect(colnames(draws), "^eta")]
  
  linpreds = list()
  for (k in 1:7) {
    print(k)
    alpha_draws_k = alpha_draws[,endsWith(colnames(alpha_draws), paste0(k,"]"))]
    eta_draws_k = eta_draws[,endsWith(colnames(eta_draws), paste0(k,"]"))]
    linpred_k = S_test%*%t(alpha_draws_k) + X_test%*%t(eta_draws_k)
    linpreds[[length(linpreds)+1]] = linpred_k
  }
  linpreds = lapply(linpreds, exp)
  ## linpreds[[1]][1:10,1:10]
  sum_linpreds = Reduce("+", linpreds)
  normalize <- function(A) { A / sum_linpreds}
  probs = lapply(linpreds, normalize)
  ## probs[[1]][1,1]+probs[[2]][1,1]+probs[[3]][1,1]+probs[[4]][1,1]+probs[[5]][1,1]+probs[[6]][1,1]+probs[[7]][1,1]
  ## probs[[1]][1:1000]
  ## dim(probs[[7]])
  probs
}

ubi_fit_to_posterior_probs <- function(U_test,O_test,X_test,fit) {
  draws=as.matrix(fit)
  beta_draws = draws[,str_detect(colnames(draws), "^beta")]
  gamma_draws = draws[,str_detect(colnames(draws), "^gamma")]
  eta_draws = draws[,str_detect(colnames(draws), "^eta")]
  
  linpreds = list()
  for (k in 1:7) {
    print(k)
    beta_draws_k = beta_draws[,endsWith(colnames(beta_draws), paste0(k,"]"))]
    gamma_draws_k = gamma_draws[,endsWith(colnames(gamma_draws), paste0(k,"]"))]
    eta_draws_k = eta_draws[,endsWith(colnames(eta_draws), paste0(k,"]"))]
    linpred_k = U_test%*%t(beta_draws_k) + O_test%*%t(gamma_draws_k) + X_test%*%t(eta_draws_k)
    linpreds[[length(linpreds)+1]] = linpred_k
  }
  linpreds = lapply(linpreds, exp)
  ## linpreds[[1]][1:10,1:10]
  sum_linpreds = Reduce("+", linpreds)
  normalize <- function(A) { A / sum_linpreds}
  probs = lapply(linpreds, normalize)
  ## probs[[1]][1,1]+probs[[2]][1,1]+probs[[3]][1,1]+probs[[4]][1,1]+probs[[5]][1,1]+probs[[6]][1,1]+probs[[7]][1,1]
  ## probs[[1]][1:1000]
  ## dim(probs[[7]])
  probs
}

spline_fit_to_posterior_probs <- function(SPL_test,X_test,fit) {
  draws=as.matrix(fit)
  alpha_draws = draws[,str_detect(colnames(draws), "^alpha")]
  eta_draws = draws[,str_detect(colnames(draws), "^eta")]
  
  linpreds = list()
  for (k in 1:7) {
    print(k)
    alpha_draws_k = alpha_draws[,endsWith(colnames(alpha_draws), paste0(k,"]"))]
    eta_draws_k = eta_draws[,endsWith(colnames(eta_draws), paste0(k,"]"))]
    linpred_k = SPL_test%*%t(alpha_draws_k) + X_test%*%t(eta_draws_k)
    linpreds[[length(linpreds)+1]] = linpred_k
  }
  linpreds = lapply(linpreds, exp)
  ## linpreds[[1]][1:10,1:10]
  sum_linpreds = Reduce("+", linpreds)
  normalize <- function(A) { A / sum_linpreds}
  probs = lapply(linpreds, normalize)
  ## probs[[1]][1,1]+probs[[2]][1,1]+probs[[3]][1,1]+probs[[4]][1,1]+probs[[5]][1,1]+probs[[6]][1,1]+probs[[7]][1,1]
  ## probs[[1]][1:1000]
  ## dim(probs[[7]])
  probs
}

cross_entropy_loss_posterior_raw <- function(probs,y_test) {
  cross_entropy_losses = list()
  for (i in 1:length(y_test)) {
    entropy_i = as.matrix( probs[[y_test[i]]][i,] )
    cross_entropy_losses[[length(cross_entropy_losses) + 1]] = entropy_i
  }
  cross_entropy_loss_M = t(do.call(cbind, cross_entropy_losses))
  ## cross_entropy_loss_M[1:10,1:10]
  cross_entropy_loss_M = -log(cross_entropy_loss_M)
  cross_entropy_losses = rowMeans(cross_entropy_loss_M)
  ## mean(cross_entropy_losses)
  sum(cross_entropy_losses)
}

########################################
########### MODEL COMPARISON ###########
########################################

########### BSN #############

celr = 0 # cross entropy loss raw (prior to)
for (fold_num in 1:10) {
  print(paste("fold",fold_num))
  # testing data matrices
  test_rows = which(folds == fold_num)
  X_test = X[test_rows,]
  S_test = S[test_rows,]
  U_test = U[test_rows,]
  O_test = O[test_rows,]
  y_test = y[test_rows,]
  n = nrow(y_test)
  
  ### model fit
  fit <- readRDS(paste0("./job_output/fit_rstan8_comp-", fold_num,".R.rds")) #FIXME
  
  ### posterior probabilities for each outcome
  probs = bsn_fit_to_posterior_probs(S_test,X_test,fit)
  # probs[[1]][1:1000]
  
  ### cross entropy loss
  celr_fold = cross_entropy_loss_posterior_raw(probs,y_test)
  celr = celr + celr_fold
}
cel_bsn = celr/nrow(y) # cross entropy loss BSN

########### UBI #############

celr_ubi = 0 # cross entropy loss raw (prior to)
for (fold_num in 1:10) {
  print(paste("fold",fold_num))
  # testing data matrices
  test_rows = which(folds == fold_num)
  X_test = X[test_rows,]
  S_test = S[test_rows,]
  U_test = U[test_rows,]
  O_test = O[test_rows,]
  y_test = y[test_rows,]
  n = nrow(y_test)
  
  ### BSN model fit
  fit <- readRDS(paste0("./job_output/fit_rstan8_comp-", fold_num+10,".R.rds")) #FIXME
  
  ### posterior probabilities for each outcome
  probs = ubi_fit_to_posterior_probs(U_test,O_test,X_test,fit)
  # probs[[1]][1:1000]
  
  ### cross entropy loss
  celr_fold = cross_entropy_loss_posterior_raw(probs,y_test)
  celr_ubi = celr_ubi + celr_fold
}
cel_ubi = celr_ubi/nrow(y) # cross entropy loss BSN

########### SPLINE #############

cel_spline = 0 # cross entropy loss raw (prior to)
for (fold_num in 1:10) {
  print(paste("fold",fold_num))
  # testing data matrices
  test_rows = which(folds == fold_num)
  X_test = X[test_rows,]
  SPL_test = SPL[test_rows,]
  y_test = y[test_rows,]
  n = nrow(y_test)
  
  ### model fit
  fit <- readRDS(paste0("./job_output/fit_rstan8_comp_spline-", fold_num,".R.rds")) #FIXME
  
  ### posterior probabilities for each outcome
  probs = spline_fit_to_posterior_probs(SPL_test,X_test,fit)
  # probs[[1]][1:1000]
  
  ### cross entropy loss
  celr_fold = cross_entropy_loss_posterior_raw(probs,y_test)
  cel_spline = cel_spline + celr_fold
}
cel_spline = cel_spline/nrow(y) # cross entropy loss BSN


########### Results #############

print("cross entropy loss BSN")
cel_bsn
print("cross entropy loss UBI")
cel_ubi
print("cross entropy loss SPLINE")
cel_spline


