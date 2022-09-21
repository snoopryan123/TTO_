
########################
library(tidyverse)
########################

fit_to_posterior_probs <- function(fit,SPL,X) {
  draws=as.matrix(fit)
  alpha_draws = draws[,str_detect(colnames(draws), "^alpha")]
  eta_draws = draws[,str_detect(colnames(draws), "^eta")]
  linpreds = list()
  for (k in 1:7) {
    print(k)
    alpha_draws_k = alpha_draws[,endsWith(colnames(alpha_draws), paste0(k,"]"))]
    eta_draws_k = eta_draws[,endsWith(colnames(eta_draws), paste0(k,"]"))]
    linpred_k = SPL%*%t(alpha_draws_k) + X%*%t(eta_draws_k)
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

YEEERS = 17:17 #12:19 #18:18
for (s in YEEERS)  
{
  print("*****"); print(paste0("results: 20", s)); print("*****");
  
  YRS = 2000 + s
  source("model10_getData.R") ### get observed data 
  
  # Sys.sleep(5) ###
  
  ### import fit from rstan
  fit <- readRDS(paste0(output_folder, "fit_obs_model_splyrs_",s,"_.rds"))
  draws <- as.matrix(fit)
  
  alpha_draws <- draws[,startsWith(colnames(draws), "alpha")]
  eta_draws <- draws[,startsWith(colnames(draws), "eta")]
  
  ############### get t -> P(y=k|t,x) ##############
  X_tilde = matrix( rep(c(logit(0.315), logit(0.315), 1, 0), 27), nrow=27, byrow = TRUE)
  S_tilde = diag(27) ## cbind(1, 1:27)  
  SPL_tilde = S_tilde %*% bbb
  probs_tilde = fit_to_posterior_probs(fit, SPL_tilde, X_tilde)
  # x_tilde = c(logit(0.315), logit(0.315), 1, 0)
  
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
    mutate(s = s) %>% relocate(s, .before=k)
  
  probs_check
  probs_checkAll = bind_rows(probs_checkAll, probs_check)
}

xwoba_checkAll = probs_checkAll %>%
  mutate(w = categories[k]) %>%
  group_by(s,t) %>%
  summarise(
    xw_L95 = sum(p_L95*w*1000),
    xw_L50 = sum(p_L50*w*1000),
    xwM = sum(pM*w*1000),
    xw_U50 = sum(p_U50*w*1000),
    xw_U95 = sum(p_U95*w*1000)
  ) %>% ungroup() # %>%
  # mutate(
  #   tto = ifelse(1 <= t & t <= 9, 1, ifelse(10 <= t & t <= 18, 2, 3)),
  #   tto1 = ifelse(tto == 1, 1, NA),
  #   tto2 = ifelse(tto == 2, 1, NA),
  #   tto3 = ifelse(tto == 3, 1, NA),
  # ) %>%
  # group_by(s, tto) %>%
  # mutate(
  #   xw_tto_L95 = mean(xw_L95),
  #   xw_tto_L50 = mean(xw_L50),
  #   xw_tto_M = mean(xwM),
  #   xw_tto_U50 = mean(xw_U50),
  #   xw_tto_U95 = mean(xw_U95),
  # )

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
    geom_errorbar(aes(ymin=xw_L95, ymax=xw_U95), width = 0.5) +
    geom_errorbar(aes(ymin=xw_L50, ymax=xw_U50), width = 0.25, size=1) +
    geom_line(aes(x=t, y = xwM), color=blue1, size=3) + 
    geom_point(aes(y=xwM), col="black", size=2, stroke=1, shape=21, fill="white") +
    scale_x_continuous(name="batter sequence number, t", breaks=seq(0,27,3)) +
    scale_y_continuous(name="wOBA", breaks=seq(0,1000,by=20))
  xwoba_check_plot
  ggsave(paste0("plots/plot_obs_results_20", sss, "_xwoba_check", ".png"),
         xwoba_check_plot, width=8, height=5)
  
  
  # xwoba_check_plot_1 = xwoba_checkAll %>%
  #   filter(s == sss) %>%
  #   mutate(
  #     xw_tto_L95_tto1 = xw_tto_L95*tto1,
  #     xw_tto_L50_tto1 = xw_tto_L50*tto1,
  #     xw_tto_M_tto1 = xw_tto_M*tto1,
  #     xw_tto_U50_tto1 = xw_tto_U50*tto1,
  #     xw_tto_U95_tto1 = xw_tto_U95*tto1,
  #     xw_tto_L95_tto2 = xw_tto_L95*tto2,
  #     xw_tto_L50_tto2 = xw_tto_L50*tto2,
  #     xw_tto_M_tto2 = xw_tto_M*tto2,
  #     xw_tto_U50_tto2 = xw_tto_U50*tto2,
  #     xw_tto_U95_tto2 = xw_tto_U95*tto2,
  #     xw_tto_L95_tto3 = xw_tto_L95*tto3,
  #     xw_tto_L50_tto3 = xw_tto_L50*tto3,
  #     xw_tto_M_tto3 = xw_tto_M*tto3,
  #     xw_tto_U50_tto3 = xw_tto_U50*tto3,
  #     xw_tto_U95_tto3 = xw_tto_U95*tto3,
  #   ) %>%
  #   ggplot(aes(x=t)) +
  #   geom_line(aes(y = xw_tto_L95_tto1), linetype=3) +
  #   geom_line(aes(y = xw_tto_L50_tto1), linetype=3, size=0.75) +
  #   geom_line(aes(y = xw_tto_M_tto1), size=1, color=blue1) +
  #   geom_line(aes(y = xw_tto_U50_tto1), linetype=3, size=0.75) +
  #   geom_line(aes(y = xw_tto_U95_tto1), linetype=3) +
  #   geom_rect(aes(ymin=xw_tto_L95_tto1, ymax=xw_tto_U95_tto1),xmin=1,xmax=9,fill="black",alpha=0.01,) +
  #   geom_rect(aes(ymin=xw_tto_L50_tto1, ymax=xw_tto_U50_tto1),xmin=1,xmax=9,fill=blue2,alpha=0.03,) +
  #   
  #   geom_line(aes(y = xw_tto_L95_tto2), linetype=3) +
  #   geom_line(aes(y = xw_tto_L50_tto2), linetype=3, size=0.75) +
  #   geom_line(aes(y = xw_tto_M_tto2), size=1, color=blue1) +
  #   geom_line(aes(y = xw_tto_U50_tto2), linetype=3, size=0.75) +
  #   geom_line(aes(y = xw_tto_U95_tto2), linetype=3) +
  #   geom_rect(aes(ymin=xw_tto_L95_tto2, ymax=xw_tto_U95_tto2),xmin=10,xmax=18,fill="black",alpha=0.01,) +
  #   geom_rect(aes(ymin=xw_tto_L50_tto2, ymax=xw_tto_U50_tto2),xmin=10,xmax=18,fill=blue2,alpha=0.03,) +
  #   
  #   geom_line(aes(y = xw_tto_L95_tto3), linetype=3) +
  #   geom_line(aes(y = xw_tto_L50_tto3), linetype=3, size=0.75) +
  #   geom_line(aes(y = xw_tto_M_tto3), size=1, color=blue1) +
  #   geom_line(aes(y = xw_tto_U50_tto3), linetype=3, size=0.75) +
  #   geom_line(aes(y = xw_tto_U95_tto3), linetype=3) +
  #   geom_rect(aes(ymin=xw_tto_L95_tto3, ymax=xw_tto_U95_tto3),xmin=19,xmax=27,fill="black",alpha=0.01,) +
  #   geom_rect(aes(ymin=xw_tto_L50_tto3, ymax=xw_tto_U50_tto3),xmin=19,xmax=27,fill=blue2,alpha=0.03,) +
  #   
  #   # geom_hline(aes(yintercept = xw_tto_M*tto1), linetype="dashed") +
  #   # geom_hline(aes(yintercept = xw_tto_U50*tto1), linetype="dashed") +
  #   
  #   
  #   geom_vline(aes(xintercept =  9), size=0.5, color="gray50") + #1.2
  #   geom_vline(aes(xintercept = 18), size=0.5, color="gray50") +
  #   geom_errorbar(aes(ymin=xw_L95, ymax=xw_U95), width = 0.5) +
  #   geom_errorbar(aes(ymin=xw_L50, ymax=xw_U50), width = 0.25, size=1) +
  #   geom_point(aes(y=xwM), col="black", size=2, stroke=1, shape=21, fill="white") +
  #   ylab("wOBA") + 
  #   scale_x_continuous(name="batter sequence number, t", breaks=seq(0,27,3))
  # xwoba_check_plot_1
  # ggsave(paste0("plots/plot_obs_results_20", sss, "_xwoba_check_1", ".png"),
  #        xwoba_check_plot_1, width=8, height=5)
}











