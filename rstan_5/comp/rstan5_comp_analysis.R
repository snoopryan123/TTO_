
library(tidyverse)
library(latex2exp)

# load data
input_file = "../../data/TTO_dataset_510.csv"  
D <- read_csv(input_file) #; D <- D %>% drop_na() 
D <- D %>% filter(YEAR == 2019) 
X <- as.matrix(D %>% select(std_WOBA_FINAL_BAT_19, std_WOBA_FINAL_PIT_19,  
                            HAND_MATCH, BAT_HOME_IND)) 
X2 <- as.matrix(D %>% select(std_BQ, std_PQ,  
                            HAND_MATCH, BAT_HOME_IND))
IS_SIM=FALSE
source("../rstan5_main.R")

test_tib_bsn = tibble(y=numeric(0),pplower=numeric(0),ppmean=numeric(0),ppupper=numeric(0))
test_tib_bsn2 = tibble(y=numeric(0),pplower=numeric(0),ppmean=numeric(0),ppupper=numeric(0))
test_tib_ubi = tibble(y=numeric(0),pplower=numeric(0),ppmean=numeric(0),ppupper=numeric(0))
test_tib_ubi2 = tibble(y=numeric(0),pplower=numeric(0),ppmean=numeric(0),ppupper=numeric(0))

get_bsn_df <- function(X, fold_num, ind=0) {
  # testing data matrices
  test_rows = which(folds == fold_num)
  X_test = X[test_rows,]
  S_test = S[test_rows,]
  U_test = U[test_rows,]
  O_test = O[test_rows,]
  y_test = tibble(y=y[test_rows,])
  n = nrow(y_test)
  
  ### BSN model fit
  fitB <- readRDS(paste0("./job_output/fit_rstan5_comp-", ind+fold_num,".R.rds")) #FIXME
  #plot_bsn0(fitB)
  drawsB <- as_tibble(as.matrix(fitB))
  alpha_draws = drawsB[,str_detect(colnames(drawsB), "^alpha")]
  eta_draws = drawsB[,str_detect(colnames(drawsB), "^eta")]
  sigma_draws = drawsB[,str_detect(colnames(drawsB), "^sigma")]$sigma
  post_pred_means = S_test%*%t(alpha_draws) + X_test%*%t(eta_draws)
  epsilon0 = matrix(sapply(sigma_draws, function(s) { rnorm(1,0,sd=s) }), nrow=1)
  epsilon = do.call(rbind, replicate(n, epsilon0, simplify=FALSE))
  post_pred = post_pred_means + epsilon
  
  pplower = apply(post_pred, 1, function(x) quantile(x,.025))
  ppmean = apply(post_pred, 1, function(x) mean(x))
  ppupper = apply(post_pred, 1, function(x) quantile(x,.975))
  df_bsn = bind_cols(y_test, pplower=pplower, ppmean=ppmean, ppupper=ppupper)
  df_bsn
}

get_ubi_df <- function(X, fold_num, ind=20) {
  # testing data matrices
  test_rows = which(folds == fold_num)
  X_test = X[test_rows,]
  S_test = S[test_rows,]
  U_test = U[test_rows,]
  O_test = O[test_rows,]
  y_test = tibble(y=y[test_rows,])
  n = nrow(y_test)
  
  ### UBI model fit
  fitU <- readRDS(paste0("./job_output/fit_rstan5_comp-", ind+fold_num,".R.rds")) #FIXME
  #plot_ubi0(fitU)
  drawsU <- as_tibble(as.matrix(fitU))
  beta_draws = drawsU[,str_detect(colnames(drawsU), "^beta")]
  gamma_draws = drawsU[,str_detect(colnames(drawsU), "^gamma")]
  eta_drawsU = drawsU[,str_detect(colnames(drawsU), "^eta")]
  sigma_drawsU = drawsU[,str_detect(colnames(drawsU), "^sigma")]$sigma
  post_pred_meansU = U_test%*%t(beta_draws) + O_test%*%t(gamma_draws) + X_test%*%t(eta_drawsU)
  epsilon0U = matrix(sapply(sigma_drawsU, function(s) { rnorm(1,0,sd=s) }), nrow=1)
  epsilonU = do.call(rbind, replicate(n, epsilon0U, simplify=FALSE))
  post_predU = post_pred_meansU + epsilonU
  
  pplowerU = apply(post_predU, 1, function(x) quantile(x,.025))
  ppmeanU = apply(post_predU, 1, function(x) mean(x))
  ppupperU = apply(post_predU, 1, function(x) quantile(x,.975))
  df_ubi = bind_cols(y_test, pplower=pplowerU, ppmean=ppmeanU, ppupper=ppupperU)
  df_ubi
}

for (fold_num in 1:10) {
  print(fold_num)
  
  df_bsn = get_bsn_df(X, fold_num, ind=0)
  df_bsn2 = get_bsn_df(X2, fold_num, ind=10)
  df_ubi = get_ubi_df(X, fold_num, ind=20)
  df_ubi2 = get_ubi_df(X2, fold_num, ind=30)
  
  test_tib_bsn = bind_rows(test_tib_bsn, df_bsn)
  test_tib_bsn2 = bind_rows(test_tib_bsn2, df_bsn2)
  test_tib_ubi = bind_rows(test_tib_ubi, df_ubi)
  test_tib_ubi2 = bind_rows(test_tib_ubi2, df_ubi2)
}

########################################
########### MODEL COMPARISON ###########
########################################

# rmse of posterior predictive mean of y
rmse <- function(d) {
  sqrt(sum((d$y - d$ppmean)^2)/nrow(d))
}
rmse(test_tib_bsn)
rmse(test_tib_ubi)
rmse(test_tib_bsn2)
rmse(test_tib_ubi2)

# mae (mean absolute error) of posterior predictive mean of y
mae <- function(d) {
  sum(abs(d$y - d$ppmean))/nrow(d)
}
mae(test_tib_bsn)
mae(test_tib_ubi)
mae(test_tib_bsn2)
mae(test_tib_ubi2)

# coverage of posterior predictive intervals of y
covg <- function(d) {
  sum(d$pplower <= d$y & d$y <= d$ppupper)/nrow(d)
}
covg(test_tib_bsn)
covg(test_tib_ubi)
covg(test_tib_bsn2)
covg(test_tib_ubi2)

# ratio of lengths of posterior predictive intervals of y
lengths_bsn = as_tibble(test_tib_bsn$ppupper - test_tib_bsn$pplower)
lengths_ubi = as_tibble(test_tib_ubi$ppupper - test_tib_ubi$pplower)
lengths_bsn2 = as_tibble(test_tib_bsn2$ppupper - test_tib_bsn2$pplower)
lengths_ubi2 = as_tibble(test_tib_ubi2$ppupper - test_tib_ubi2$pplower)
length_ratiosBA = lengths_ubi/lengths_bsn
length_ratiosCA = lengths_bsn2/lengths_bsn
length_ratiosDA = lengths_ubi2/lengths_bsn

mean(length_ratiosBA[,1])
mean(length_ratiosCA[,1])
mean(length_ratiosDA[,1])
sd(length_ratiosBA[,1])
sd(length_ratiosCA[,1])
sd(length_ratiosDA[,1])

########################################
########### SIGMA COMPARISON ###########
########################################

fitA <- readRDS(paste0("./job_output/fit_rstan5-1.R.rds"))
fitB <- readRDS(paste0("./job_output/fit_rstan5-4.R.rds"))
fitC <- readRDS(paste0("./job_output/fit_rstan5-2.R.rds"))
fitD <- readRDS(paste0("./job_output/fit_rstan5-5.R.rds"))
#plot_bsn0(fitA)
### draws
sigmaA <- as_tibble(as.matrix(fitA))$sigma
sigmaB <- as_tibble(as.matrix(fitB))$sigma
sigmaC <- as_tibble(as.matrix(fitC))$sigma
sigmaD <- as_tibble(as.matrix(fitD))$sigma
### sigma
mean(sigmaA)
mean(sigmaB)
mean(sigmaC)
mean(sigmaD)
sd(sigmaA)
sd(sigmaB)
sd(sigmaC)
sd(sigmaD)










# length_ratios2 = lengths_ubi2/lengths_bsn2
# mean(length_ratios)
# sd(length_ratios)
# mean(length_ratios2)
# sd(length_ratios2)
# lp = as_tibble(length_ratios) %>% ggplot(aes(x=value)) + 
#   geom_histogram(binwidth=.001) + 
#   geom_vline(xintercept = mean(length_ratios), color="firebrick") +
#   geom_vline(xintercept = 1, color="dodgerblue2") +
#   xlim(c(0.9,1.2)) + 
#   xlab("Ratio of UBI length to BSN length") +
#   annotate(x=1,y=+Inf,label="1",vjust=2,geom="label",color="dodgerblue2") +
#   annotate(x=mean(length_ratios),y=2900,label="Mean ratio of lengths",vjust=2,geom="label",color="firebrick")
# lp
# #ggsave("job_output/plot_ppLengthRatio.png", lp)


# # lengths of posterior predictive intervals of y
# lengths_bsn = as_tibble(test_tib_bsn$ppupper - test_tib_bsn$pplower)
# lengths_bsn$model = "model (A)"
# lengths_ubi = as_tibble(test_tib_ubi$ppupper - test_tib_ubi$pplower)
# lengths_ubi$model = "model (B)"
# lengths_bsn2 = as_tibble(test_tib_bsn2$ppupper - test_tib_bsn2$pplower)
# lengths_bsn2$model = "model (C)"
# lengths_ubi2 = as_tibble(test_tib_ubi2$ppupper - test_tib_ubi2$pplower)
# lengths_ubi2$model = "model (D)"
# lengths_df = bind_rows(lengths_bsn,lengths_ubi,lengths_bsn2,lengths_ubi2)
# lppi = lengths_df %>% 
#   ggplot(aes(x=value,fill=model)) +
#   #facet_wrap(~model) +
#   geom_density(alpha=0.2) +
#   xlim(c(1.75,2.05)) +
#   xlab("length") +
#   labs(title="distribution of posterior predictive interval lengths")
# lppi


