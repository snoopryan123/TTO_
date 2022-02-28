library(tidyverse)

### load data
input_file = "../../data/TTO_dataset_510.csv"  
D <- read_csv(input_file)
D <- D %>% filter(YEAR == 2019) %>% filter(BQ>0 & BQ<1 & PQ>0 & PQ<1 )
logit <- function(p) { log(p/(1-p)) }
X <- as.matrix(D %>% mutate(lBQ=logit(BQ), lPQ=logit(PQ)) %>% select(lBQ, lPQ, HAND_MATCH, BAT_HOME_IND)) 
source("rstan8_sim_main.R")

M = 25*6
alpha_diff = matrix(0,nrow=36,ncol=7)
alpha_covered = matrix(nrow=36,ncol=7)
alpha_length= matrix(nrow=36,ncol=7)
cross_entropy_loss_bsn = numeric(25)

test_idxs = folds == 1
UBI = FALSE
for (i in 1:25) {
  ii = i + 50
  print(ii)

  # posterior samples & y vector
  fit <- readRDS(paste0("job_output/fit_rstan8_sim-",ii,".R.rds"))
  draws <- as.matrix(fit)
  y <- readRDS(paste0("job_output/y_rstan8_sim-",ii,".R.rds"))
  test_idxs =which(folds == 1) ## check
  
  y_train = y[!test_idxs,]
  y_test = y[test_idxs,]
  X_test = X[test_idxs,]
  S_test = S[test_idxs,]
  U_test = U[test_idxs,]
  O_test = O[test_idxs,]
  n_test = nrow(X_test)
  
  if (!UBI) { #BSN
    alpha_draws = draws[,str_detect(colnames(draws), "^alpha")]
    eta_draws = draws[,str_detect(colnames(draws), "^eta")]
    
    for (b in 1:36) {
      for (k in 2:7) {
        alpha_draws_bk = alpha_draws[,paste0("alpha[",b,",",k,"]")]
        alpha_true_bk = alpha[k,b]
        alpha_lower_bk = unname(quantile(alpha_draws_bk, .025))
        alpha_mean_bk = mean(alpha_draws_bk)
        alpha_upper_bk = unname(quantile(alpha_draws_bk, .975))
        # MAE of true alpha
        alpha_diff[b,k] = abs(alpha_mean_bk - alpha_true_bk)
        # coverage of true alpha
        alpha_covered[b,k] = alpha_lower_bk <= alpha_true_bk & alpha_true_bk <= alpha_upper_bk
        # lengths of true alpha
        alpha_length[b,k] = alpha_upper_bk - alpha_lower_bk
      }
    }
    
    alpha_draws_1 = alpha_draws[,endsWith(colnames(alpha_draws), "1]")]
    alpha_draws_2 = alpha_draws[,endsWith(colnames(alpha_draws), "2]")]
    alpha_draws_3 = alpha_draws[,endsWith(colnames(alpha_draws), "3]")]
    alpha_draws_4 = alpha_draws[,endsWith(colnames(alpha_draws), "4]")]
    alpha_draws_5 = alpha_draws[,endsWith(colnames(alpha_draws), "5]")]
    alpha_draws_6 = alpha_draws[,endsWith(colnames(alpha_draws), "6]")]
    alpha_draws_7 = alpha_draws[,endsWith(colnames(alpha_draws), "7]")]
    eta_draws_1 = eta_draws[,endsWith(colnames(eta_draws), "1]")]
    eta_draws_2 = eta_draws[,endsWith(colnames(eta_draws), "2]")]
    eta_draws_3 = eta_draws[,endsWith(colnames(eta_draws), "3]")]
    eta_draws_4 = eta_draws[,endsWith(colnames(eta_draws), "4]")]
    eta_draws_5 = eta_draws[,endsWith(colnames(eta_draws), "5]")]
    eta_draws_6 = eta_draws[,endsWith(colnames(eta_draws), "6]")]
    eta_draws_7 = eta_draws[,endsWith(colnames(eta_draws), "7]")]
    raw_p1 = exp( S_test %*% t(alpha_draws_1) + X_test %*% t(eta_draws_1))
    raw_p2 = exp( S_test %*% t(alpha_draws_2) + X_test %*% t(eta_draws_2))
    raw_p3 = exp( S_test %*% t(alpha_draws_3) + X_test %*% t(eta_draws_3))
    raw_p4 = exp( S_test %*% t(alpha_draws_4) + X_test %*% t(eta_draws_4))
    raw_p5 = exp( S_test %*% t(alpha_draws_5) + X_test %*% t(eta_draws_5))
    raw_p6 = exp( S_test %*% t(alpha_draws_6) + X_test %*% t(eta_draws_6))
    raw_p7 = exp( S_test %*% t(alpha_draws_7) + X_test %*% t(eta_draws_7))
    raw_summed_p = raw_p1+raw_p2+raw_p3+raw_p4+raw_p5+raw_p6+raw_p7
    p1 = 1 / raw_summed_p
    p2 = raw_p2 * p1
    p3 = raw_p3 * p1
    p4 = raw_p4 * p1
    p5 = raw_p5 * p1
    p6 = raw_p6 * p1
    p7 = raw_p7 * p1
    # aaa=p1+p2+p3+p4+p5+p6+p7; aaa[1:10,1:10]
    p1[1:10,1:10]
    
    # posterior predictive intervals on the probabilities
    pplower1 = apply(p1, 1, function(x) quantile(x,.025))
    pplower2 = apply(p2, 1, function(x) quantile(x,.025))
    pplower3 = apply(p3, 1, function(x) quantile(x,.025))
    pplower4 = apply(p4, 1, function(x) quantile(x,.025))
    pplower5 = apply(p5, 1, function(x) quantile(x,.025))
    pplower6 = apply(p6, 1, function(x) quantile(x,.025))
    pplower7 = apply(p7, 1, function(x) quantile(x,.025))
    ppmean1 = apply(p1, 1, function(x) mean(x))
    ppmean2 = apply(p2, 1, function(x) mean(x))
    ppmean3 = apply(p3, 1, function(x) mean(x))
    ppmean4 = apply(p4, 1, function(x) mean(x))
    ppmean5 = apply(p5, 1, function(x) mean(x))
    ppmean6 = apply(p6, 1, function(x) mean(x))
    ppmean7 = apply(p7, 1, function(x) mean(x))
    ppmeans = cbind(ppmean1,ppmean2,ppmean3,ppmean4,ppmean5,ppmean6,ppmean7)
    ppupper1 = apply(p1, 1, function(x) quantile(x,.975))
    ppupper2 = apply(p2, 1, function(x) quantile(x,.975))
    ppupper3 = apply(p3, 1, function(x) quantile(x,.975))
    ppupper4 = apply(p4, 1, function(x) quantile(x,.975))
    ppupper5 = apply(p5, 1, function(x) quantile(x,.975))
    ppupper6 = apply(p6, 1, function(x) quantile(x,.975))
    ppupper7 = apply(p7, 1, function(x) quantile(x,.975))
    
    y_test
    y_test_class_matrix = modelr::model_matrix(~ factor(y_test) + 0,data=as.data.frame(y_test)) 
    cross_entropy_loss_bsn[i] = sum( -1 * y_test_class_matrix * log(ppmeans) ) / length(y_test)
  }

}








