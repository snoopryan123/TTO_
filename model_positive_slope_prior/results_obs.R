
########################
library(tidyverse)
########################

fit_to_posterior_probs <- function(fit,INCPT,S,O,X) {
  draws=as.matrix(fit)
  alpha_incpt_draws <- draws[,startsWith(colnames(draws), "alpha_incpt")]
  alpha_slope_draws <- draws[,startsWith(colnames(draws), "alpha_slope")]
  beta_draws = draws[,str_detect(colnames(draws), "^beta")] 
  eta_draws = draws[,str_detect(colnames(draws), "^eta")]
  linpreds = list()
  for (k in 1:7) {
    print(k)
    alpha_incpt_draws_k = alpha_incpt_draws[,endsWith(colnames(alpha_incpt_draws), paste0(k,"]"))]
    alpha_slope_draws_k = alpha_slope_draws[,endsWith(colnames(alpha_slope_draws), paste0(k,"]"))]
    beta_draws_k = beta_draws[,endsWith(colnames(beta_draws), paste0(k,"]"))] 
    eta_draws_k = eta_draws[,endsWith(colnames(eta_draws), paste0(k,"]"))]
    linpred_k = INCPT%*%t(alpha_incpt_draws_k) + S%*%t(alpha_slope_draws_k) + 
                O%*%t(beta_draws_k) + X%*%t(eta_draws_k)
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
beta_allDraws = tibble()
probs_allDraws = tibble()
beta_checkAll = tibble()
eta_checkAll = tibble()
probs_checkAll = tibble()

YEEERS = 14:19 #18:18 #14:19  #12:19 #18:18
for (s in YEEERS)  
{
  print("*****"); print(paste0("results: 20", s)); print("*****");
  
  YRS = 2000 + s
  source("A_getData.R") ### get observed data
  
  # Sys.sleep(5) ###
  
  ### import fit from rstan
  fit <- readRDS(paste0(output_folder, "fit_obs_model_lineyrs_",s,"_.rds"))
  draws <- as.matrix(fit)
  
  ### Convergence Diagnostics
  library(rstan)
  
  fit_summary <- summary(fit)
  print(names(fit_summary))
  fit_summary_s = fit_summary$summary
  Rhats = na.omit(fit_summary_s[,"Rhat"])# fit_summary_s[,"Rhat"]
  n_effs = na.omit(fit_summary_s[,"n_eff"])
  print(paste0("R_hat ", c(min(Rhats), mean(Rhats), max(Rhats)) ))
  print(paste0("n_eff ", c(min(n_effs), mean(n_effs), max(n_effs))))
  print(get_elapsed_time(fit))
  mean(rowSums(get_elapsed_time(fit))) / 3600
  
  
  alpha_draws <- draws[,startsWith(colnames(draws), "alpha")]
  beta_draws <- draws[,startsWith(colnames(draws), "beta")]
  eta_draws <- draws[,startsWith(colnames(draws), "eta")]
  
  ############### get t -> P(y=k|t,x) ##############
  INCPT_tilde = cbind(rep(1,27))
  S_tilde = cbind(1:27) ## cbind(1, 1:27)  
  O_tilde = matrix(c(rep(0,9), rep(1,9), rep(0,9), rep(0,9), rep(0,9), rep(1,9)), nrow=27)
  X_tilde = matrix( rep(c(logit(0.315), logit(0.315), 1, 0), 27), nrow=27, byrow = TRUE)
  # X_tilde = matrix( rep(c(0.315, 0.315, 1, 0), 27), nrow=27, byrow = TRUE)
  probs_tilde = fit_to_posterior_probs(fit, INCPT_tilde, S_tilde, O_tilde, X_tilde)

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
  
  
  #################### check whether BETA was recovered #################### 
  beta_draws_1 <- reshape2::melt(beta_draws) %>%
    as_tibble() %>%
    rename(i = iterations, beta=value) %>%
    mutate(k = as.numeric(str_sub(parameters, 8, 8))) %>%
    mutate(tto = 1+as.numeric(str_sub(parameters, 6, 6)))
  
  beta_drawsAll = beta_draws_1 %>%
    ### transform beta_3k into beta_3k minus beta_2k
    filter(k != 1) %>%
    arrange(i,k) %>%
    group_by(i,k) %>%
    mutate(
      beta = ifelse(tto==3, beta[2] - beta[1], beta),
    ) %>% ungroup() %>% arrange(tto,k) %>%
    mutate(s=s)
  beta_allDraws = bind_rows(beta_allDraws, beta_drawsAll)
  
  probs_drawsAll = probs_tilde %>%
    mutate(tto = floor( (t-1)/9 )+1 ) %>%
    rename(i = iter) %>%
    filter(k != 1) %>%
    arrange(i,k) %>%
    group_by(i,k) %>%
    mutate(
      p = ifelse(tto==3, p[2] - p[1], p),
    ) %>% ungroup() %>% arrange(tto,k) %>%
    mutate(s=s)
  probs_allDraws = bind_rows(probs_allDraws, probs_drawsAll)
  
  beta_check <- beta_draws_1 %>% 
    group_by(k, tto) %>%
    summarise(
      beta_L95 = quantile(beta, 0.025),
      beta_L50 = quantile(beta, 0.25),
      betaM = mean(beta),
      beta_U50 = quantile(beta, 0.75),
      beta_U95 = quantile(beta, 0.975),
      .groups = "drop"
    ) %>%
    arrange(tto,k) %>%
    mutate(c = category_strings[k]) %>%
    relocate(c, .after = k) %>%
    # mutate(ci_50_length = beta_U50 - beta_L50) %>%
    # mutate(ci_95_length = beta_U95 - beta_L95) %>%
    filter(k != 1) %>%
    mutate(s = s) %>% relocate(s, .before=k)
  beta_check
  
  ### transform beta_3k into beta_3k minus beta_2k
  beta_check_1 = beta_check %>%
    arrange(s,k,tto) %>%
    group_by(s,k) %>%
    mutate(
      beta_L95 = ifelse(tto==3, beta_L95[2] - beta_L95[1], beta_L95),
      beta_L50 = ifelse(tto==3, beta_L50[2] - beta_L50[1], beta_L50),
      betaM = ifelse(tto==3, betaM[2] - betaM[1], betaM),
      beta_U50 = ifelse(tto==3, beta_U50[2] - beta_U50[1], beta_U50),
      beta_U95 = ifelse(tto==3, beta_U95[2] - beta_U95[1], beta_U95),
    ) %>% ungroup() %>% arrange(tto,k) %>%
    ### TTOP tests
    mutate(TTOP_95 = as.numeric(beta_L95 > 0),
           # TTOP_50 = as.numeric(beta_L50 > 0)
           )
  
  beta_checkAll = bind_rows(beta_checkAll, beta_check_1)
  
  # #################### get ETA #################### 
  # eta_draws_1 <- reshape2::melt(eta_draws) %>%
  #   as_tibble() %>%
  #   rename(i = iterations, eta=value) %>%
  #   mutate(k = as.numeric(str_sub(parameters, 7, 7))) %>%
  #   mutate(l = as.numeric(str_sub(parameters, 5, 5)))
  # 
  # eta_check <- eta_draws_1 %>% 
  #   group_by(k, l) %>%
  #   summarise(
  #     eta_L95 = quantile(eta, 0.025),
  #     eta_L50 = quantile(eta, 0.25),
  #     etaM = mean(eta),
  #     eta_U50 = quantile(eta, 0.75),
  #     eta_U95 = quantile(eta, 0.975),
  #     .groups = "drop"
  #   ) %>%
  #   arrange(l,k) %>%
  #   mutate(c = category_strings[k]) %>%
  #   relocate(c, .after = k) %>%
  #   # mutate(ci_50_length = eta_U50 - eta_L50) %>%
  #   # mutate(ci_95_length = eta_U95 - eta_L95) %>%
  #   filter(k != 1) %>%
  #   mutate(s = s) %>% relocate(s, .before=k) %>%
  #   # mutate(l_ = colnames(X)[l]) 
  #   mutate(l_ = c("BQ", "PQ", "HAND", "HOME")[l])
  # data.frame(eta_check)
  # 
  # eta_checkAll = bind_rows(eta_checkAll, eta_check)
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
  ) %>%
  mutate(
    tto = ifelse(1 <= t & t <= 9, 1, ifelse(10 <= t & t <= 18, 2, 3)),
    tto1 = ifelse(tto == 1, 1, NA),
    tto2 = ifelse(tto == 2, 1, NA),
    tto3 = ifelse(tto == 3, 1, NA),
  ) %>%
  group_by(s, tto) %>%
  mutate(
    xw_tto_L95 = mean(xw_L95),
    xw_tto_L50 = mean(xw_L50),
    xw_tto_M = mean(xwM),
    xw_tto_U50 = mean(xw_U50),
    xw_tto_U95 = mean(xw_U95),
  )

xwoba_allDraws = probs_allDraws %>%
  mutate(w = categories[k]) %>%
  mutate(xw = p*w*1000)

# ### save beta csv to check for TTOP:
write_csv(beta_checkAll, paste0("plots/beta_ttop_results_beta_checkAll.csv"))

#################### summmary stats over all sims #################### 

### TTOP
TTOP_proportions = beta_checkAll %>%
  group_by(k,tto) %>%
  summarise(TTOP_95 = mean(TTOP_95),
            # TTOP_50 = mean(TTOP_50),
            .groups="drop") %>%
  arrange(tto,k) %>%
  mutate(c = category_strings[k]) %>% relocate(c,.after=k)
TTOP_proportions
### save TTOP csv to check for TTOP:
write_csv(TTOP_proportions, paste0("plots/TTOP_proportions.csv"))

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
  ### beta params boxplot
  beta_boxplot = beta_allDraws %>%
    mutate(c = category_strings[k]) %>%
    filter(s == sss) %>%
    mutate(param = ifelse(tto==2, paste0("b",2,c),
                          paste0("b",3,c,"-","b",2,c)),
           ordering = paste0(k,tto), #paste0(tto,k),
           param = fct_reorder(param, desc(ordering))
    ) %>%
    ggplot() +
    geom_vline(aes(xintercept=0), color=blue1, size=1.5) +
    geom_boxplot(aes(y=param, x=beta)) +
    scale_y_discrete(labels = unname(TeX(rev(c(
      "$\\beta_{2,BB}$",
      "$\\beta_{3,BB}-\\beta_{2,BB}$",
      "$\\beta_{2,HBP}$",
      "$\\beta_{3,HBP}-\\beta_{2,HBP}$",
      "$\\beta_{2,1B}$",
      "$\\beta_{3,1B}-\\beta_{2,1B}$",
      "$\\beta_{2,2B}$",
      "$\\beta_{3,2B}-\\beta_{2,2B}$",
      "$\\beta_{2,3B}$",
      "$\\beta_{3,3B}-\\beta_{2,3B}$",
      "$\\beta_{2,HR}$",
      "$\\beta_{3,HR}-\\beta_{2,HR}$"
    ))))) +
    scale_x_continuous(breaks=seq(-5,5,by=0.2)) +
    ylab("") + xlab("log odds") 
  beta_boxplot
  ggsave(paste0("plots/beta_boxplot_20",sss,".png"), beta_boxplot, width=10, height=5)
  
  # ################
  # ### xwoba params boxplot
  # xwoba_boxplot = xwoba_allDraws %>%
  #   mutate(c = category_strings[k]) %>%
  #   filter(s == sss) %>%
  #   mutate(param = ifelse(tto==2, paste0("b",2,c),
  #                         paste0("b",3,c,"-","b",2,c)),
  #          ordering = paste0(k,tto), #paste0(tto,k),
  #          param = fct_reorder(param, desc(ordering))
  #   ) %>%
  #   ggplot() +
  #   geom_vline(aes(xintercept=0), color=blue1, size=1.5) +
  #   geom_boxplot(aes(y=param, x=xw)) +
  #   scale_y_discrete(labels = unname(TeX(rev(c(
  #     "$\\xwoba_{2,BB}$",
  #     "$\\xwoba_{3,BB}-\\xwoba_{2,BB}$",
  #     "$\\xwoba_{2,HBP}$",
  #     "$\\xwoba_{3,HBP}-\\xwoba_{2,HBP}$",
  #     "$\\xwoba_{2,1B}$",
  #     "$\\xwoba_{3,1B}-\\xwoba_{2,1B}$",
  #     "$\\xwoba_{2,2B}$",
  #     "$\\xwoba_{3,2B}-\\xwoba_{2,2B}$",
  #     "$\\xwoba_{2,3B}$",
  #     "$\\xwoba_{3,3B}-\\xwoba_{2,3B}$",
  #     "$\\xwoba_{2,HR}$",
  #     "$\\xwoba_{3,HR}-\\xwoba_{2,HR}$"
  #   ))))) +
  #   scale_x_continuous(breaks=seq(-5,5,by=0.2)) +
  #   ylab("") + xlab("log odds") 
  # xwoba_boxplot
  # ggsave(paste0("plots/xwoba_boxplot_20",sss,".png"), xwoba_boxplot, width=10, height=5)
  
  ################
  xwoba_check_plot = xwoba_checkAll %>%
    filter(s == sss) %>%
    ggplot(aes(x=t)) +
    geom_vline(aes(xintercept =  9), size=0.5, color="gray50") + #1.2
    geom_vline(aes(xintercept = 18), size=0.5, color="gray50") +
    geom_errorbar(aes(ymin=xw_L95, ymax=xw_U95), width = 0.5) +
    geom_errorbar(aes(ymin=xw_L50, ymax=xw_U50), width = 0.25, size=1) +
    geom_point(aes(y=xwM), col="black", size=2, stroke=1, shape=21, fill="white") +
    ylab("wOBA") + 
    scale_x_continuous(name="batter sequence number, t", breaks=seq(0,27,3))
  xwoba_check_plot
  ggsave(paste0("plots/plot_obs_results_20", sss, "_xwoba_check", ".png"),
         xwoba_check_plot, width=8, height=5)
  
  xwoba_check_plot_1 = xwoba_checkAll %>%
    filter(s == sss) %>%
    mutate(
      xw_tto_L95_tto1 = xw_tto_L95*tto1,
      xw_tto_L50_tto1 = xw_tto_L50*tto1,
      xw_tto_M_tto1 = xw_tto_M*tto1,
      xw_tto_U50_tto1 = xw_tto_U50*tto1,
      xw_tto_U95_tto1 = xw_tto_U95*tto1,
      xw_tto_L95_tto2 = xw_tto_L95*tto2,
      xw_tto_L50_tto2 = xw_tto_L50*tto2,
      xw_tto_M_tto2 = xw_tto_M*tto2,
      xw_tto_U50_tto2 = xw_tto_U50*tto2,
      xw_tto_U95_tto2 = xw_tto_U95*tto2,
      xw_tto_L95_tto3 = xw_tto_L95*tto3,
      xw_tto_L50_tto3 = xw_tto_L50*tto3,
      xw_tto_M_tto3 = xw_tto_M*tto3,
      xw_tto_U50_tto3 = xw_tto_U50*tto3,
      xw_tto_U95_tto3 = xw_tto_U95*tto3,
    ) %>%
    ggplot(aes(x=t)) +
    geom_line(aes(y = xw_tto_L95_tto1), linetype=3) +
    geom_line(aes(y = xw_tto_L50_tto1), linetype=3, size=0.75) +
    geom_line(aes(y = xw_tto_M_tto1), size=1, color=blue1) +
    geom_line(aes(y = xw_tto_U50_tto1), linetype=3, size=0.75) +
    geom_line(aes(y = xw_tto_U95_tto1), linetype=3) +
    geom_rect(aes(ymin=xw_tto_L95_tto1, ymax=xw_tto_U95_tto1),xmin=1,xmax=9,fill="black",alpha=0.01,) +
    geom_rect(aes(ymin=xw_tto_L50_tto1, ymax=xw_tto_U50_tto1),xmin=1,xmax=9,fill=blue2,alpha=0.03,) +
    
    geom_line(aes(y = xw_tto_L95_tto2), linetype=3) +
    geom_line(aes(y = xw_tto_L50_tto2), linetype=3, size=0.75) +
    geom_line(aes(y = xw_tto_M_tto2), size=1, color=blue1) +
    geom_line(aes(y = xw_tto_U50_tto2), linetype=3, size=0.75) +
    geom_line(aes(y = xw_tto_U95_tto2), linetype=3) +
    geom_rect(aes(ymin=xw_tto_L95_tto2, ymax=xw_tto_U95_tto2),xmin=10,xmax=18,fill="black",alpha=0.01,) +
    geom_rect(aes(ymin=xw_tto_L50_tto2, ymax=xw_tto_U50_tto2),xmin=10,xmax=18,fill=blue2,alpha=0.03,) +
    
    geom_line(aes(y = xw_tto_L95_tto3), linetype=3) +
    geom_line(aes(y = xw_tto_L50_tto3), linetype=3, size=0.75) +
    geom_line(aes(y = xw_tto_M_tto3), size=1, color=blue1) +
    geom_line(aes(y = xw_tto_U50_tto3), linetype=3, size=0.75) +
    geom_line(aes(y = xw_tto_U95_tto3), linetype=3) +
    geom_rect(aes(ymin=xw_tto_L95_tto3, ymax=xw_tto_U95_tto3),xmin=19,xmax=27,fill="black",alpha=0.01,) +
    geom_rect(aes(ymin=xw_tto_L50_tto3, ymax=xw_tto_U50_tto3),xmin=19,xmax=27,fill=blue2,alpha=0.03,) +
    
    # geom_hline(aes(yintercept = xw_tto_M*tto1), linetype="dashed") +
    # geom_hline(aes(yintercept = xw_tto_U50*tto1), linetype="dashed") +
    
    
    geom_vline(aes(xintercept =  9), size=0.5, color="gray50") + #1.2
    geom_vline(aes(xintercept = 18), size=0.5, color="gray50") +
    geom_errorbar(aes(ymin=xw_L95, ymax=xw_U95), width = 0.5) +
    geom_errorbar(aes(ymin=xw_L50, ymax=xw_U50), width = 0.25, size=1) +
    geom_point(aes(y=xwM), col="black", size=2, stroke=1, shape=21, fill="white") +
    ylab("wOBA") + 
    scale_x_continuous(name="batter sequence number, t", breaks=seq(0,27,3))
  xwoba_check_plot_1
  ggsave(paste0("plots/plot_obs_results_20", sss, "_xwoba_check_1", ".png"),
         xwoba_check_plot_1, width=8, height=5)
}


###############################################

# {
#     beta_check_plot_df = beta_checkAll %>%
#       mutate(sig = ifelse(beta_L95 > 0, 1, 0),
#              sig_neg = ifelse(beta_U95 < 0, 1, 0)) %>%
#       mutate(beta_L95_sig = ifelse(sig, beta_L95, NA),
#              beta_U95_sig = ifelse(sig, beta_U95, NA),
#              beta_L50_sig = ifelse(sig, beta_L50, NA),
#              beta_U50_sig = ifelse(sig, beta_U50, NA),
#              betaM_sig = ifelse(sig, betaM, NA),
#              beta_L95_sig_neg = ifelse(sig_neg, beta_L95, NA),
#              beta_U95_sig_neg = ifelse(sig_neg, beta_U95, NA),
#              beta_L50_sig_neg = ifelse(sig_neg, beta_L50, NA),
#              beta_U50_sig_neg = ifelse(sig_neg, beta_U50, NA),
#              betaM_sig_neg = ifelse(sig_neg, betaM, NA)
#       ) %>%
#       filter(s == sss) %>%
#       mutate(tto = paste0(tto, "TTO"))
#     beta_check_plot = beta_check_plot_df %>%
#       ggplot(aes(x=fct_reorder(c, k))) +
#       facet_wrap(~tto, nrow=1) +
#       theme(panel.spacing = unit(2, "lines")) +
#       xlab("") + ylab(TeX("$\\beta$")) +
#       geom_hline(yintercept=0, size=0.5, col="grey") +
#       geom_errorbar(aes(ymin=beta_L95, ymax=beta_U95), width = 0.5)
#     if (!all(beta_check_plot_df$sig == 0)) {
#       beta_check_plot = beta_check_plot + 
#         geom_errorbar(aes(ymin=beta_L95_sig, ymax=beta_U95_sig), width = 0.5, color=sig_color) 
#     }
#     if (!all(beta_check_plot_df$sig_neg == 0)) {
#       beta_check_plot = beta_check_plot + 
#         geom_errorbar(aes(ymin=beta_L95_sig_neg, ymax=beta_U95_sig_neg), width = 0.5, color=sig_neg_color)
#     }
#     beta_check_plot = beta_check_plot + 
#       geom_errorbar(aes(ymin=beta_L50, ymax=beta_U50), width = 0.25, size=1) 
#     if (!all(beta_check_plot_df$sig == 0)) {
#       beta_check_plot = beta_check_plot + 
#         geom_errorbar(aes(ymin=beta_L50_sig, ymax=beta_U50_sig), width = 0.25, size=1, color=sig_color) 
#     }
#     if (!all(beta_check_plot_df$sig_neg == 0)) {
#       beta_check_plot = beta_check_plot + 
#         geom_errorbar(aes(ymin=beta_L50_sig_neg, ymax=beta_U50_sig_neg), width = 0.25, size=1, color=sig_neg_color)
#     }
#     beta_check_plot = beta_check_plot + 
#       geom_point(aes(y=betaM), col="black", size=2, stroke=1, shape=21, fill="white")
#     if (!all(beta_check_plot_df$sig == 0)) {
#       beta_check_plot = beta_check_plot + 
#         geom_point(aes(y=betaM_sig), col=sig_color, size=2, stroke=1, shape=21, fill="white") 
#     }
#     if (!all(beta_check_plot_df$sig_neg == 0)) {
#       beta_check_plot = beta_check_plot + 
#         geom_point(aes(y=betaM_sig_neg), col=sig_neg_color, size=2, stroke=1, shape=21, fill="white")
#     }
# }
# beta_check_plot
# ggsave(paste0("plots/beta_plots/plot_obs_results_", 2000+sss, "_beta_check", ".png"),
#        beta_check_plot, width=9, height=5)

# eta_check_plot = eta_checkAll %>%
#   filter(s == sss) %>%
#   mutate(l_order = ifelse(l_ == "PQ", 1, ifelse(l_ == "BQ", 2, ifelse(l_ == "HAND", 3, 4)))) %>%
#   ggplot(aes(x=fct_reorder(l_,  l_order ))) +
#   # ggplot(aes(x=l_)) +
#   facet_wrap(~c, nrow=2, scales="free") +
#   theme(panel.spacing = unit(2, "lines")) +
#   xlab("") + ylab(TeX("$\\eta$")) +
#   # geom_hline(yintercept=0, size=0.5, col="grey") + 
#   geom_errorbar(aes(ymin=eta_L95, ymax=eta_U95), width = 0.5) +
#   geom_errorbar(aes(ymin=eta_L50, ymax=eta_U50), width = 0.25, size=1) +
#   geom_point(aes(y=etaM), col="black", size=2, stroke=1, shape=21, fill="white")
# eta_check_plot
# ggsave(paste0("plots/plot_obs_results_", 2000+sss, "_eta_check", ".png"),
#        eta_check_plot, width=12, height=8)

# probs_check_plot = probs_checkAll %>%
#   filter(s == sss) %>%
#   ggplot(aes(x=t)) +
#   facet_wrap(~c, nrow=3, scales="free") +
#   theme(panel.spacing = unit(2, "lines")) +
#   # xlab("") + ylab(TeX("$\\p$")) +
#   # geom_hline(yintercept=0, size=0.5, col="grey") + 
#   geom_errorbar(aes(ymin=p_L95, ymax=p_U95), width = 0.5) +
#   geom_errorbar(aes(ymin=p_L50, ymax=p_U50), width = 0.25, size=1) +
#   geom_point(aes(y=pM), col="black", size=2, stroke=1, shape=21, fill="white") +
#   geom_vline(aes(xintercept =  9), size=0.5, color="gray50") + #1.2
#   geom_vline(aes(xintercept = 18), size=0.5, color="gray50") +
#   ylab("probability") + 
#   scale_x_continuous(name="batter sequence number, t", breaks=seq(0,27,3))
# probs_check_plot
# ggsave(paste0("plots/plot_obs_results_", 2000+sss, "_probs_check", ".png"),
#        probs_check_plot, width=12, height=12)

# ################
# xwoba_checkAll %>%
#   group_by(s,tto) %>%
#   summarise(
#     xw_withinTTO_diff = max(xwM) - min(xwM),
#     xw_mean = mean(xwM)
#   ) %>%
#   mutate(xwdiff = c(0,diff(xw_mean)))

################
xwoba_check_plot_ALLYRS = xwoba_checkAll %>%
  mutate(year = s+2000) %>%
  ggplot(aes(x=t)) +
  facet_wrap(~year, nrow=2) +
  geom_vline(aes(xintercept =  9), size=0.5, color="gray50") + #1.2
  geom_vline(aes(xintercept = 18), size=0.5, color="gray50") +
  geom_errorbar(aes(ymin=xw_L95, ymax=xw_U95), width = 0.5) +
  geom_errorbar(aes(ymin=xw_L50, ymax=xw_U50), width = 0.25, size=1) +
  geom_point(aes(y=xwM), col="black", size=2, stroke=1, shape=21, fill="white") +
  ylab("wOBA") + 
  scale_x_continuous(name="batter sequence number, t", breaks=seq(0,27,3))
# xwoba_check_plot_ALLYRS
ggsave(paste0("plots/plot_obs_results", "_xwoba_check_ALLYR", ".png"),
       xwoba_check_plot_ALLYRS, width=15, height=6)


xwoba_check_plot_ALLYRS_1 = xwoba_checkAll %>%
  mutate(year = s+2000) %>%
  mutate(
    xw_tto_L95_tto1 = xw_tto_L95*tto1,
    xw_tto_L50_tto1 = xw_tto_L50*tto1,
    xw_tto_M_tto1 = xw_tto_M*tto1,
    xw_tto_U50_tto1 = xw_tto_U50*tto1,
    xw_tto_U95_tto1 = xw_tto_U95*tto1,
    xw_tto_L95_tto2 = xw_tto_L95*tto2,
    xw_tto_L50_tto2 = xw_tto_L50*tto2,
    xw_tto_M_tto2 = xw_tto_M*tto2,
    xw_tto_U50_tto2 = xw_tto_U50*tto2,
    xw_tto_U95_tto2 = xw_tto_U95*tto2,
    xw_tto_L95_tto3 = xw_tto_L95*tto3,
    xw_tto_L50_tto3 = xw_tto_L50*tto3,
    xw_tto_M_tto3 = xw_tto_M*tto3,
    xw_tto_U50_tto3 = xw_tto_U50*tto3,
    xw_tto_U95_tto3 = xw_tto_U95*tto3,
  ) %>%
  ggplot(aes(x=t)) +
  facet_wrap(~year, nrow=2) +
  geom_line(aes(y = xw_tto_L95_tto1), linetype=3) +
  geom_line(aes(y = xw_tto_L50_tto1), linetype=3, size=0.75) +
  geom_line(aes(y = xw_tto_M_tto1), size=1, color=blue1) +
  geom_line(aes(y = xw_tto_U50_tto1), linetype=3, size=0.75) +
  geom_line(aes(y = xw_tto_U95_tto1), linetype=3) +
  geom_rect(aes(ymin=xw_tto_L95_tto1, ymax=xw_tto_U95_tto1),xmin=1,xmax=9,fill="black",alpha=0.01,) +
  geom_rect(aes(ymin=xw_tto_L50_tto1, ymax=xw_tto_U50_tto1),xmin=1,xmax=9,fill=blue2,alpha=0.03,) +
  
  geom_line(aes(y = xw_tto_L95_tto2), linetype=3) +
  geom_line(aes(y = xw_tto_L50_tto2), linetype=3, size=0.75) +
  geom_line(aes(y = xw_tto_M_tto2), size=1, color=blue1) +
  geom_line(aes(y = xw_tto_U50_tto2), linetype=3, size=0.75) +
  geom_line(aes(y = xw_tto_U95_tto2), linetype=3) +
  geom_rect(aes(ymin=xw_tto_L95_tto2, ymax=xw_tto_U95_tto2),xmin=10,xmax=18,fill="black",alpha=0.01,) +
  geom_rect(aes(ymin=xw_tto_L50_tto2, ymax=xw_tto_U50_tto2),xmin=10,xmax=18,fill=blue2,alpha=0.03,) +
  
  geom_line(aes(y = xw_tto_L95_tto3), linetype=3) +
  geom_line(aes(y = xw_tto_L50_tto3), linetype=3, size=0.75) +
  geom_line(aes(y = xw_tto_M_tto3), size=1, color=blue1) +
  geom_line(aes(y = xw_tto_U50_tto3), linetype=3, size=0.75) +
  geom_line(aes(y = xw_tto_U95_tto3), linetype=3) +
  geom_rect(aes(ymin=xw_tto_L95_tto3, ymax=xw_tto_U95_tto3),xmin=19,xmax=27,fill="black",alpha=0.01,) +
  geom_rect(aes(ymin=xw_tto_L50_tto3, ymax=xw_tto_U50_tto3),xmin=19,xmax=27,fill=blue2,alpha=0.03,) +
  
  # geom_hline(aes(yintercept = xw_tto_M*tto1), linetype="dashed") +
  # geom_hline(aes(yintercept = xw_tto_U50*tto1), linetype="dashed") +
  
  
  geom_vline(aes(xintercept =  9), size=0.5, color="gray50") + #1.2
  geom_vline(aes(xintercept = 18), size=0.5, color="gray50") +
  geom_errorbar(aes(ymin=xw_L95, ymax=xw_U95), width = 0.5) +
  geom_errorbar(aes(ymin=xw_L50, ymax=xw_U50), width = 0.25, size=1) +
  geom_point(aes(y=xwM), col="black", size=2, stroke=1, shape=21, fill="white") +
  ylab("wOBA") + 
  scale_x_continuous(name="batter sequence number, t", breaks=seq(0,27,3))
# xwoba_check_plot_ALLYRS_1
ggsave(paste0("plots/plot_obs_results", "_xwoba_check_ALLYRS_1", ".png"),
       xwoba_check_plot_ALLYRS_1, width=15, height=6)


beta_boxplot_ALLYRS = beta_allDraws %>%
  mutate(year=2000+s) %>%
  mutate(c = category_strings[k]) %>%
  mutate(param = ifelse(tto==2, paste0("b",2,c),
                        paste0("b",3,c,"-","b",2,c)),
         ordering = paste0(k,tto), #paste0(tto,k),
         param = fct_reorder(param, desc(ordering))
  ) %>%
  ggplot() +
  facet_wrap(~year, nrow=2) +
  geom_vline(aes(xintercept=0), color=blue1, size=1.5) +
  geom_boxplot(aes(y=param, x=beta)) +
  scale_y_discrete(labels = unname(TeX(rev(c(
    "$\\beta_{2,BB}$",
    "$\\beta_{3,BB}-\\beta_{2,BB}$",
    "$\\beta_{2,HBP}$",
    "$\\beta_{3,HBP}-\\beta_{2,HBP}$",
    "$\\beta_{2,1B}$",
    "$\\beta_{3,1B}-\\beta_{2,1B}$",
    "$\\beta_{2,2B}$",
    "$\\beta_{3,2B}-\\beta_{2,2B}$",
    "$\\beta_{2,3B}$",
    "$\\beta_{3,3B}-\\beta_{2,3B}$",
    "$\\beta_{2,HR}$",
    "$\\beta_{3,HR}-\\beta_{2,HR}$"
  ))))) +
  scale_x_continuous(breaks=seq(-5,5,by=0.2)) +
  ylab("") + xlab("log odds") 
# beta_boxplot_ALLYRS
ggsave(paste0("plots/beta_boxplot_ALLYRS.png"), beta_boxplot_ALLYRS, width=18, height=8)




