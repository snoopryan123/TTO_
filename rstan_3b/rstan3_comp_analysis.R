
source("rstan3_comp_getData.R")
library(latex2exp)

test_tib_bsn_3a = tibble(y=numeric(0),pplower=numeric(0),ppmean=numeric(0),ppupper=numeric(0))
test_tib_bsn_3b = tibble(y=numeric(0),pplower=numeric(0),ppmean=numeric(0),ppupper=numeric(0))

test_tib_ubi = tibble(y=numeric(0),pplower=numeric(0),ppmean=numeric(0),ppupper=numeric(0))

for (fold_num in 1:10) {
  print(fold_num)
  
  # testing data matrices
  test_rows = which(folds == fold_num)
  X_test = X[test_rows,]
  X0_test = X0[test_rows,]
  S_test = S[test_rows,]
  U_test = U[test_rows,]
  O_test = O[test_rows,]
  y_test = tibble(y=y[test_rows,])
  n = nrow(y_test)
  
  ### BSN model fit, rstan_3a final_woba
  fitB_3a <- readRDS(paste0("../rstan_3a/job_output/fit_rstan3_comp_bsn-",fold_num,".R.rds")) #FIXME
  #plot_bsn0(fitB_3a) 
  drawsB <- as_tibble(as.matrix(fitB_3a)) #FIXME
  alpha_draws = drawsB[,str_detect(colnames(drawsB), "^alpha")]
  eta_draws = drawsB[,str_detect(colnames(drawsB), "^eta")]
  sigma_draws = drawsB[,str_detect(colnames(drawsB), "^sigma")]$sigma
  post_pred_means = S_test%*%t(alpha_draws) + X0_test%*%t(eta_draws) #FIXME
  epsilon0 = matrix(sapply(sigma_draws, function(s) { rnorm(1,0,sd=s) }), nrow=1)
  epsilon = do.call(rbind, replicate(n, epsilon0, simplify=FALSE))
  post_pred = post_pred_means + epsilon
  pplower = apply(post_pred, 1, function(x) quantile(x,.025))
  ppmean = apply(post_pred, 1, function(x) mean(x))
  ppupper = apply(post_pred, 1, function(x) quantile(x,.975))
  df_bsn = bind_cols(y_test, pplower=pplower, ppmean=ppmean, ppupper=ppupper)
  test_tib_bsn_3a = bind_rows(test_tib_bsn_3a, df_bsn) #FIXME
  
  ### BSN model fit, rstan_3b BQ,PQ
  fitB_3b <- readRDS(paste0("./job_output/fit_rstan3_comp_bsn-",fold_num,".R.rds")) #FIXME
  #plot_bsn0(fitB_3b)
  drawsB <- as_tibble(as.matrix(fitB_3b)) #FIXME
  alpha_draws = drawsB[,str_detect(colnames(drawsB), "^alpha")]
  eta_draws = drawsB[,str_detect(colnames(drawsB), "^eta")]
  sigma_draws = drawsB[,str_detect(colnames(drawsB), "^sigma")]$sigma
  post_pred_means = S_test%*%t(alpha_draws) + X_test%*%t(eta_draws) #FIXME
  epsilon0 = matrix(sapply(sigma_draws, function(s) { rnorm(1,0,sd=s) }), nrow=1)
  epsilon = do.call(rbind, replicate(n, epsilon0, simplify=FALSE))
  post_pred = post_pred_means + epsilon
  pplower = apply(post_pred, 1, function(x) quantile(x,.025))
  ppmean = apply(post_pred, 1, function(x) mean(x))
  ppupper = apply(post_pred, 1, function(x) quantile(x,.975))
  df_bsn = bind_cols(y_test, pplower=pplower, ppmean=ppmean, ppupper=ppupper)
  test_tib_bsn_3b = bind_rows(test_tib_bsn_3b, df_bsn) #FIXME
  

  
  # ### UBI model fit
  # fitU <- readRDS(paste0("./job_output/fit_rstan3_comp_ubi-",fold_num,".R.rds"))
  # #plot_ubi0(fitU)
  # drawsU <- as_tibble(as.matrix(fitU))
  # beta_draws = drawsU[,str_detect(colnames(drawsU), "^beta")]
  # gamma_draws = drawsU[,str_detect(colnames(drawsU), "^gamma")]
  # delta_draws = drawsU[,str_detect(colnames(drawsU), "^delta")]
  # sigma_drawsU = drawsU[,str_detect(colnames(drawsU), "^sigma")]$sigma
  # post_pred_meansU = U_test%*%t(beta_draws) + O_test%*%t(gamma_draws) + X_test%*%t(delta_draws)
  # epsilon0U = matrix(sapply(sigma_drawsU, function(s) { rnorm(1,0,sd=s) }), nrow=1)
  # epsilonU = do.call(rbind, replicate(n, epsilon0U, simplify=FALSE))
  # post_predU = post_pred_meansU + epsilonU
  # 
  # pplowerU = apply(post_predU, 1, function(x) quantile(x,.025))
  # ppmeanU = apply(post_predU, 1, function(x) mean(x))
  # ppupperU = apply(post_predU, 1, function(x) quantile(x,.975))
  # df_ubi = bind_cols(y_test, pplower=pplowerU, ppmean=ppmeanU, ppupper=ppupperU)
  # test_tib_ubi = bind_rows(test_tib_ubi, df_ubi)
}

########################################
########### MODEL COMPARISON ###########
########################################

# sd of woba
sd(D$EVENT_WOBA_19)

# mae (mean absolute error) of posterior predictive mean of y
mae <- function(d) {
  sum(abs(d$y - d$ppmean))/nrow(d)
}
mae(test_tib_bsn_3a)
mae(test_tib_bsn_3b)

#mae(test_tib_ubi)

# rmse of posterior predictive mean of y
rmse <- function(d) {
  sqrt(sum((d$y - d$ppmean)^2)/nrow(d))
}
rmse(test_tib_bsn_3a)
rmse(test_tib_bsn_3b)

#rmse(test_tib_ubi)

# coverage of posterior predictive intervals of y
covg <- function(d) {
  sum(d$pplower <= d$y & d$y <= d$ppupper)/nrow(d)
}
covg(test_tib_bsn_3a)
covg(test_tib_bsn_3b)

# covg(test_tib_ubi)

# ratio of lengths of posterior predictive intervals of y
lengths_bsn_3a = test_tib_bsn_3a$ppupper - test_tib_bsn_3a$pplower
lengths_bsn_3b = test_tib_bsn_3b$ppupper - test_tib_bsn_3b$pplower
length_ratios = lengths_bsn_3a/lengths_bsn_3b
mean(length_ratios)
sd(length_ratios)
lp = as_tibble(length_ratios) %>% ggplot(aes(x=value)) + 
  geom_histogram(binwidth=.001) + 
  geom_vline(xintercept = mean(length_ratios), color="firebrick") +
  geom_vline(xintercept = 1, color="dodgerblue2") +
  xlim(c(0.9,1.2)) + 
  xlab("Ratio of BSN final_woba lengths to BQ,PQ lengths") +
  annotate(x=1,y=+Inf,label="1",vjust=2,geom="label",color="dodgerblue2") +
  annotate(x=mean(length_ratios),y=2900,label="Mean ratio of lengths",vjust=2,geom="label",color="firebrick")
lp
#ggsave("job_output/plot_ppLengthRatio.png", lp)


