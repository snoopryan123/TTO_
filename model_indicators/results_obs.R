
########################
library(tidyverse)
########################

fit_to_posterior_probs <- function(fit,S,X) {
  draws=as.matrix(fit)
  alpha_draws = draws[,str_detect(colnames(draws), "^alpha")]
  eta_draws = draws[,str_detect(colnames(draws), "^eta")]
  linpreds = list()
  for (k in 1:7) {
    print(k)
    alpha_draws_k = alpha_draws[,endsWith(colnames(alpha_draws), paste0(k,"]"))]
    eta_draws_k = eta_draws[,endsWith(colnames(eta_draws), paste0(k,"]"))]
    linpred_k = S%*%t(alpha_draws_k) + X%*%t(eta_draws_k)
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
  
  ### turn to tibble
  probs_df = tibble()
  for (k in 1:7) {
    probs_df_k0 = probs[[k]]
    probs_df_k = reshape2::melt(probs_df_k0) %>%
      as_tibble() %>%
      rename(t = Var1, iter=Var2, p=value) %>%
      arrange(t, iter) %>%
      mutate(k = k) 
    probs_df = bind_rows(probs_df, probs_df_k)
  }
  # probs_tilde_df %>% group_by(k) %>% summarise(count=n(), count2=n()/27) ## check
  return(probs_df)
}

########################
probs_checkAll = tibble()

YEEERS = 17:18 #12:19 #18:18
for (s in YEEERS)  
{
  print("*****"); print(paste0("results: 20", s)); print("*****");
  
  YRS = 2000 + s
  source("A_getData.R") ### get observed data 
  
  # Sys.sleep(5) ###
  
  ### import fit from rstan
  fit <- readRDS(paste0(output_folder, "fit_obs_model_indicators_yrs_",s,"_.rds"))
  draws <- as.matrix(fit)
  
  alpha_draws <- draws[,startsWith(colnames(draws), "alpha")]
  eta_draws <- draws[,startsWith(colnames(draws), "eta")]
  
  ############### get t -> P(y=k|t,x) ##############
  logit <- function(p) { log(p/(1-p)) }
  X_tilde = matrix( rep(c(logit(0.315), logit(0.315), 1, 0), 27), nrow=27, byrow = TRUE)
  # X_tilde = matrix( rep(c(0.315, 0.315, 1, 0), 27), nrow=27, byrow = TRUE)
  S_tilde = diag(27) ## cbind(1, 1:27)  
  probs_tilde = fit_to_posterior_probs(fit, S_tilde, X_tilde)
  x_tilde = c(logit(0.315), logit(0.315), 1, 0)
  # x_tilde = c(0.315, 0.315, 1, 0)
  
  smoothing_spl_df_0 = 6 ###
  probs_check = probs_tilde %>%
    group_by(k,t) %>%
    summarise(
      p_L95 = quantile(p, 0.025),
      p_L50 = quantile(p, 0.25),
      pM = mean(p),
      p_U50 = quantile(p, 0.75),
      p_U95 = quantile(p, 0.975),
      .groups = "drop"
    ) %>%
    arrange(t,k) %>%
    mutate(c = category_strings[k]) %>%
    relocate(c, .after = k) %>%
    # mutate(ci_50_length = p_U50 - p_L50) %>%
    # mutate(ci_95_length = p_U95 - p_L95) %>%
    filter(k != 1) %>%
    mutate(s = s) %>% relocate(s, .before=k) %>%
    ### smoothing spline
    mutate(
      p_L95_s = smooth.spline(p_L95, df=smoothing_spl_df_0)$y,
      p_L50_s = smooth.spline(p_L50, df=smoothing_spl_df_0)$y,
      pM_s = smooth.spline(pM, df=smoothing_spl_df_0)$y,
      p_U50_s = smooth.spline(p_U50, df=smoothing_spl_df_0)$y,
      p_U95_s = smooth.spline(p_U95, df=smoothing_spl_df_0)$y
    )
  
  probs_check
  probs_checkAll = bind_rows(probs_checkAll, probs_check)
}

smoothing_spl_df = 7 ###
xwoba_checkAll = probs_checkAll %>%
  mutate(w = categories[k]) %>%
  group_by(s,t) %>%
  summarise(
    xw_L95 = sum(p_L95*w*1000),
    xw_L50 = sum(p_L50*w*1000),
    xwM = sum(pM*w*1000),
    xw_U50 = sum(p_U50*w*1000),
    xw_U95 = sum(p_U95*w*1000),
    ### xwOBA on top of smoothed probs
    xw_L95_sp = sum(p_L95_s*w*1000),
    xw_L50_sp = sum(p_L50_s*w*1000),
    xwM_sp = sum(pM_s*w*1000),
    xw_U50_sp = sum(p_U50_s*w*1000),
    xw_U95_sp = sum(p_U95_s*w*1000),
  ) %>% ungroup() %>%
  ### smoothing spline
  mutate(
    xw_L95_s = smooth.spline(xw_L95, df=smoothing_spl_df)$y,
    xw_L50_s = smooth.spline(xw_L50, df=smoothing_spl_df)$y,
    xw_s = smooth.spline(xwM, df=smoothing_spl_df)$y,
    xw_U50_s = smooth.spline(xw_U50, df=smoothing_spl_df)$y,
    xw_U95_s = smooth.spline(xw_U95, df=smoothing_spl_df)$y
  ) 
  

#################### PLOTS #################### 

sig_color = "#56B4E9" # "firebrick" "#56B4E9"
sig_neg_color = "firebrick"
blue1 = "dodgerblue2"
blue2 = "#56B4E9"
### sss = 18
for(sss in YEEERS) # 12:19 # 18:18
{
  print("*****"); print(paste0("xWOBA Plots: 20", sss)); print("*****");
  
  ################
  xwoba_check_plot = xwoba_checkAll %>%
    filter(t < 26) %>%
    filter(s == sss) %>%
    ggplot(aes(x=t)) +
    geom_vline(aes(xintercept =  9), size=0.5, color="gray50") + #1.2
    geom_vline(aes(xintercept = 18), size=0.5, color="gray50") +
    
    # geom_line(aes(x=t, y = xw_s), color=blue1, size=3) +
    
    # geom_errorbar(aes(ymin=xw_L95_s, ymax=xw_U95_s), width = 0.5) +
    # geom_errorbar(aes(ymin=xw_L50_s, ymax=xw_U50_s), width = 0.25, size=1) +
    # geom_point(aes(y=xw_s), col="black", size=2, stroke=1, shape=21, fill="white") +
    geom_errorbar(aes(ymin=xw_L95, ymax=xw_U95), width = 0.5) +
    geom_errorbar(aes(ymin=xw_L50, ymax=xw_U50), width = 0.25, size=1) +
    # geom_line(aes(x=t, y = xwM), color=blue1, size=3) + 
    geom_point(aes(y=xwM), col="black", size=2, stroke=1, shape=21, fill="white") +
    scale_x_continuous(name="batter sequence number, t", breaks=seq(0,27,3)) +
    scale_y_continuous(name="wOBA", breaks=seq(0,1000,by=20))
  xwoba_check_plot
  ggsave(paste0("plots/plot_obs_results_20", sss, "_xwoba_check_indicator", ".png"),
         xwoba_check_plot, width=8, height=5)
  
  ################
  xwoba_check_plot_1 = xwoba_checkAll %>%
    filter(t < 26) %>%
    filter(s == sss) %>%
    ggplot(aes(x=t)) +
    geom_vline(aes(xintercept =  9), size=0.5, color="gray50") + #1.2
    geom_vline(aes(xintercept = 18), size=0.5, color="gray50") +
    
    geom_line(aes(x=t, y = xw_s), color=blue1, size=3) +
    
    # geom_errorbar(aes(ymin=xw_L95_s, ymax=xw_U95_s), width = 0.5) +
    # geom_errorbar(aes(ymin=xw_L50_s, ymax=xw_U50_s), width = 0.25, size=1) +
    # geom_point(aes(y=xw_s), col="black", size=2, stroke=1, shape=21, fill="white") +
    geom_errorbar(aes(ymin=xw_L95, ymax=xw_U95), width = 0.5) +
    geom_errorbar(aes(ymin=xw_L50, ymax=xw_U50), width = 0.25, size=1) +
    # geom_line(aes(x=t, y = xwM), color=blue1, size=3) + 
    geom_point(aes(y=xwM), col="black", size=2, stroke=1, shape=21, fill="white") +
    scale_x_continuous(name="batter sequence number, t", breaks=seq(0,27,3)) +
    scale_y_continuous(name="wOBA", breaks=seq(0,1000,by=20))
  xwoba_check_plot_1
  ggsave(paste0("plots/plot_obs_results_20", sss, "_xwoba_check_1_indicator", ".png"),
         xwoba_check_plot_1, width=8, height=5)

}











