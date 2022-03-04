library(tidyverse)
output_folder = './job_output/'

### load data
input_file = "../../data/TTO_dataset_510.csv"  
D <- read_csv(input_file) #%>% drop_na() 
D <- D %>% filter(YEAR == 2019) %>% filter(BQ>0 & BQ<1 & PQ>0 & PQ<1 )
logit <- function(p) { log(p/(1-p)) }
X <- as.matrix(D %>% mutate(lBQ=logit(BQ), lPQ=logit(PQ)) %>% select(lBQ, lPQ, HAND_MATCH, BAT_HOME_IND)) 
source("rstan8_sim_main.R")

#########################
### HELPER FUNCTIONS ####
#########################

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

cross_entropy_loss_posterior <- function(probs,y_test) {
  cross_entropy_losses = list()
  for (i in 1:length(y_test)) {
    entropy_i = as.matrix( probs[[y_test[i]]][i,] )
    cross_entropy_losses[[length(cross_entropy_losses) + 1]] = entropy_i
  }
  cross_entropy_loss_M = t(do.call(cbind, cross_entropy_losses))
  ## cross_entropy_loss_M[1:10,1:10]
  cross_entropy_loss_M = -log(cross_entropy_loss_M)
  cross_entropy_losses = rowMeans(cross_entropy_loss_M)
  mean(cross_entropy_losses)
}

ubi_get_all_params <- function(fit) {
  draws=as.matrix(fit)
  beta_draws = draws[,str_detect(colnames(draws), "^beta")]
  gamma_draws = draws[,str_detect(colnames(draws), "^gamma")]
  eta_draws = draws[,str_detect(colnames(draws), "^eta")]
  all_params = list()
  for (k in 1:7) {
    # print(k)
    beta_draws_k = beta_draws[,endsWith(colnames(beta_draws), paste0(k,"]"))]
    gamma_draws_k = gamma_draws[,endsWith(colnames(gamma_draws), paste0(k,"]"))]
    eta_draws_k = eta_draws[,endsWith(colnames(eta_draws), paste0(k,"]"))]
    all_params_k = cbind(beta_draws_k, gamma_draws_k, eta_draws_k)
    all_params[[length(all_params)+1]] = all_params_k
  }
  all_params
}

ubi_post_means_and_ci <- function(all_params) {
  params_true = cbind(beta,gamma,eta)
  pp_df = tibble()
  for (k in 1:7) {
    all_params_k = all_params[[k]]
    pplower_k = apply(all_params_k, 2, function(x) quantile(x,.025))
    ppmeans_k = colMeans(all_params_k)
    ppupper_k = apply(all_params_k, 2, function(x) quantile(x,.975))
    p_true_k = params_true[k,]
    p_names = c(paste0("beta",1:p_u),paste0("gamma",1:p_o),paste0("eta",1:p_x))
    pp_df_k = tibble(k=k,pplower=pplower_k,ppmean=ppmeans_k,ppupper=ppupper_k,
                     var=p_names,param_true = p_true_k)
    pp_df = bind_rows(pp_df, pp_df_k)
  }
  pp_df %>% arrange(-k) %>% mutate(covered = pplower <= param_true & param_true <= ppupper,
                                   pplength = ppupper - pplower)
}

ubi_beta_plus_gamma_draws <- function(fit) {
  draws=as.matrix(fit)
  beta_draws = draws[,str_detect(colnames(draws), "^beta")]
  gamma_draws = draws[,str_detect(colnames(draws), "^gamma")]
  beta_plus_gamma_draws = list()
  for (k in 2:7) {
    # print(k)
    beta_draws_k = beta_draws[,endsWith(colnames(beta_draws), paste0(k,"]"))][,1:9]
    gamma_draws_k = gamma_draws[,endsWith(colnames(gamma_draws), paste0(k,"]"))][,1:3]
    bk = cbind(beta_draws_k,beta_draws_k,beta_draws_k)
    gk = cbind(matrix(gamma_draws_k[,1], nrow(gamma_draws_k), 9),
               matrix(gamma_draws_k[,2], nrow(gamma_draws_k), 9),
               matrix(gamma_draws_k[,3], nrow(gamma_draws_k), 9))
    bgk = bk + gk
    beta_plus_gamma_draws[[length(beta_plus_gamma_draws)+1]] = bgk
  }
  beta_plus_gamma_draws
}

ubi_tto_post_means_and_ci <- function(beta_plus_gamma_draws) {
  b_true = cbind(beta[2:7,1:9],beta[2:7,1:9],beta[2:7,1:9])
  g_true = cbind(matrix(gamma[2:7,1], nrow(gamma)-1, 9),
                 matrix(gamma[2:7,2], nrow(gamma)-1, 9),
                 matrix(gamma[2:7,3], nrow(gamma)-1, 9))
  params_true = b_true + g_true
  
  bg_df = tibble()
  for (k in 2:7) {
    bgk = beta_plus_gamma_draws[[k-1]]
    pplower_k = apply(bgk, 2, function(x) quantile(x,.025))
    ppmeans_k = colMeans(bgk)
    ppupper_k = apply(bgk, 2, function(x) quantile(x,.975))
    p_true_k = params_true[k-1,]
    p_names = paste0("bg",1:27)
    bg_df_k = tibble(k=k,pplower=pplower_k,ppmean=ppmeans_k,ppupper=ppupper_k,
                     var=p_names,param_true = p_true_k)
    bg_df = bind_rows(bg_df, bg_df_k)
  }
  bg_df %>% arrange(-k) %>% mutate(covered = pplower <= param_true & param_true <= ppupper,
                                   pplength = ppupper - pplower)
}


###############
### metrics ###
###############

NSIM = 25 #FIXME #25

## cross entropy loss 
cel_vec = numeric(NSIM)
## coverage ??
### proportion of all parameters that are covered
prop_all_params_covered = numeric(NSIM)
### proportion of TTO parameters that are covered
prop_tto_params_covered = numeric(NSIM)
## average length of credible intervals for TTO params
avg_length_ci_tto_params = numeric(NSIM)
## for each category, proportion of simulations in which a 2TTO effect is detected
tto2_detected = matrix(nrow=NSIM,ncol=6)
## for each category, proportion of simulations in which a 3TTO effect is detected
tto3_detected = matrix(nrow=NSIM,ncol=6)

##########################
### loop over all sims ###
##########################

test_rows = which(folds == 1)
for (i in 1:NSIM) {
  ii = i + 50
  print(paste0("i = ",ii))
  
  # posterior samples & y vector
  fit <- readRDS(paste0("job_output/fit_rstan8_sim-",ii,".R.rds"))
  draws <- as.matrix(fit)
  y <- readRDS(paste0("job_output/y_rstan8_sim-",ii,".R.rds"))
  
  ### test data matrices
  X_test = X[test_rows,]
  U_test = U[test_rows,]
  O_test = O[test_rows,]
  y_test = y[test_rows,]
  n_test = nrow(X_test)
  
  ### posterior probabilities for each outcome
  probs = ubi_fit_to_posterior_probs(U_test,O_test,X_test,fit)
  # probs[[1]][1:1000]
  
  ### cross entropy loss
  cel = cross_entropy_loss_posterior(probs,y_test)
  cel_vec[i] = cel
  
  ### empirical proportions of each outcome
  # as_tibble(y_test) %>% group_by(value) %>% summarise(count=n()) %>% ungroup() %>% mutate(prop = count/sum(count))
  # c(mean(probs[[1]]),mean(probs[[2]]), mean(probs[[3]]), mean(probs[[4]]), mean(probs[[5]]), mean(probs[[6]]), mean(probs[[7]]))
  
  ### posterior means & CI's for all parameters
  all_params = ubi_get_all_params(fit)
  pp_df = ubi_post_means_and_ci(all_params)
  
  ### proportion of all parameters that are covered
  prop_all_params_covered[i] = mean((pp_df %>% filter(k!=1))$covered )
  
  ### proportion of TTO parameters that are covered
  beta_plus_gamma_draws = ubi_beta_plus_gamma_draws(fit)
  bg_df = ubi_tto_post_means_and_ci(beta_plus_gamma_draws)
  prop_tto_params_covered[i] = mean(bg_df$covered)
  
  ## average length of credible intervals for TTO params
  avg_length_ci_tto_params[i] = mean(bg_df$pplength)
  
  ## for each category, was a 2TTO & 3TTO effect detected
  xxx = bg_df %>% filter(k!=1) %>% filter(var %in% c("bg9","bg10","bg18","bg19")) 
  xxx1 = xxx[seq(1,nrow(xxx),by=2),]
  names(xxx1) = paste0(names(xxx1),"1")
  xxx2 = xxx[seq(2,nrow(xxx),by=2),]
  names(xxx2) = paste0(names(xxx2),"2")
  xxxx=as_tibble(cbind(xxx1,xxx2))
  xxxx = xxxx %>% mutate(tto_effect_ = pplower2 - ppupper1, tto_detected = tto_effect_ > 0)
  xxxx2 = xxxx[seq(1,nrow(xxxx),by=2),]
  xxxx3 = xxxx[seq(2,nrow(xxxx),by=2),]
  tto2_detected[i,] = xxxx2$tto_detected
  tto3_detected[i,] = xxxx3$tto_detected
}

######################
### FINAL METRICS ####
######################

print("average cross entropy loss")
print(mean(cel_vec))
## coverage ??
print("average proportion of all parameters that are covered")
print(mean(prop_all_params_covered))
print("average proportion of TTO parameters that are covered")
print(mean(prop_tto_params_covered))
print("avg. average length of credible intervals for TTO params")
print(mean(avg_length_ci_tto_params))
print("avg. for each category, proportion of simulations in which a 2TTO effect is detected")
print(colMeans(tto2_detected))
print("for each category, proportion of simulations in which a 3TTO effect is detected")
print(colMeans(tto3_detected))


