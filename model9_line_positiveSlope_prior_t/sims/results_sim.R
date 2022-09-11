
########################
source("sim_config_2.R")
SIM_NO_PF = FALSE
# for (SIM_NUM in 1:2) {
# SIM_NUM = 2 #1 #2
# YRS = 2018
sim_noPf_str = ifelse(SIM_NO_PF, "A", "")
underlying = "line"
IS_SIM = TRUE
########################
source("../model9_getData.R") ### get observed data 

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
beta_checkAll = tibble()
eta_checkAll = tibble()
probs_checkAll = tibble()
# s = 1
for (s in 3:3) {
  print(paste0("sleeping ", s))
  
  source("sim_simulateData.R") ### get simulated outcomes and "true" params
  
  Sys.sleep(5)
  
  ### import fit from rstan
  OUTPUT_FILE = paste0("job_output/", "fit_sim",SIM_NUM,sim_noPf_str,"_model_bsnBL_", s, "_underlying_", underlying, ".rds") 
  fit <- readRDS(OUTPUT_FILE)
  draws <- as.matrix(fit)
  print("*****"); print(s); print("*****");
  
  alpha_incpt_draws <- draws[,startsWith(colnames(draws), "alpha_incpt")]
  alpha_slope_draws <- draws[,startsWith(colnames(draws), "alpha_slope")]
  beta_draws <- draws[,startsWith(colnames(draws), "beta")]
  eta_draws <- draws[,startsWith(colnames(draws), "eta")]
  
  ############### check whether t -> P(y=k|t,x) was recovered ##############
  INCPT_tilde = cbind(rep(1,27))
  S_tilde = cbind(1:27) ## cbind(1, 1:27)
  O_tilde = matrix(c(rep(0,9), rep(1,9), rep(0,9), rep(0,9), rep(0,9), rep(1,9)), nrow=27)
  X_tilde = matrix( rep(c(logit(0.315), logit(0.315), 1, 0), 27), nrow=27, byrow = TRUE)
  probs_tilde = fit_to_posterior_probs(fit, INCPT_tilde, S_tilde, O_tilde, X_tilde)
  
  ### get true t -> P(y=k|t,x)
  {
    x_tilde = c(logit(0.315), logit(0.315), 1, 0)
    raw_probs_tilde_true = tibble()
    for (kk in 1:7) {
      alpha_k = (alpha_tib %>% filter(k == kk))$alpha_line
      eta_k = (eta_tib %>% filter(k == kk))$eta
      beta_2k = (beta_tib %>% filter(k==kk & sim_num==SIM_NUM))$beta_2
      beta_3k = (beta_tib %>% filter(k==kk & sim_num==SIM_NUM))$beta_3
      beta_bumps = beta_2k * c(rep(0,9), rep(1,9), rep(0,9)) +
        beta_3k * c(rep(0,9), rep(0,9), rep(1,9))
      raw_p_k = unname(exp(alpha_k + 
                             pracma::dot(x_tilde, eta_k) +
                             beta_bumps))
      raw_probs_tilde_true = bind_rows(raw_probs_tilde_true,   
                                       tibble(p = raw_p_k, k=kk, t=1:27)
      )
    }
    probs_tilde_true = raw_probs_tilde_true %>% 
      rename(p_true = p) %>%
      group_by(t) %>% 
      mutate(p_true = p_true/sum(p_true)) %>% 
      ungroup()
  }

  probs_check = probs_tilde %>%
    group_by(k,t) %>%
    summarise(
      p_L95 = quantile(p, 0.025),
      p_L70 = quantile(p, 0.05),
      p_L50 = quantile(p, 0.25),
      pM = mean(p),
      p_U50 = quantile(p, 0.75),
      p_U70 = quantile(p, 0.95),
      p_U95 = quantile(p, 0.975),
      .groups = "drop"
    ) %>%
    arrange(t,k) %>%
    mutate(c = category_strings[k]) %>%
    relocate(c, .after = k) %>%
    left_join(probs_tilde_true) %>%
    relocate(p_true, .after = pM) %>%
    mutate(is_covered_50 = as.numeric(p_L50 <= p_true & p_true <= p_U50)) %>%
    mutate(is_covered_95 = as.numeric(p_L70 <= p_true & p_true <= p_U70)) %>%
    mutate(is_covered_95 = as.numeric(p_L95 <= p_true & p_true <= p_U95)) %>%
    mutate(abs_diff = abs(p_true - pM)) %>%
    mutate(same_sign = ifelse(p_true != 0, as.numeric(p_true*pM > 0), NA)) %>%
    mutate(ci_50_length = p_U50 - p_L50) %>%
    mutate(ci_70_length = p_U70 - p_L70) %>%
    mutate(ci_95_length = p_U95 - p_L95) %>%
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
  
  beta_check <- beta_draws_1 %>% 
    group_by(k, tto) %>%
    summarise(
      beta_L95 = quantile(beta, 0.025),
      beta_L70 = quantile(beta, 0.05),
      beta_L50 = quantile(beta, 0.25),
      betaM = mean(beta),
      beta_U50 = quantile(beta, 0.75),
      beta_U70 = quantile(beta, 0.95),
      beta_U95 = quantile(beta, 0.975),
      .groups = "drop"
    ) %>%
    arrange(tto,k) %>%
    mutate(c = category_strings[k]) %>%
    mutate(beta_true = c(beta_mat[1,], beta_mat[2,])) %>%
    relocate(c, .after = k) %>%
    relocate(beta_true, .after = betaM) %>%
    mutate(is_covered_50 = as.numeric(beta_L50 <= beta_true & beta_true <= beta_U50)) %>%
    mutate(is_covered_70 = as.numeric(beta_L70 <= beta_true & beta_true <= beta_U70)) %>%
    mutate(is_covered_95 = as.numeric(beta_L95 <= beta_true & beta_true <= beta_U95)) %>%
    mutate(abs_diff = abs(beta_true - betaM)) %>%
    mutate(same_sign = ifelse(beta_true != 0, as.numeric(beta_true*betaM > 0), NA)) %>%
    mutate(ci_50_length = beta_U50 - beta_L50) %>%
    mutate(ci_70_length = beta_U70 - beta_L70) %>%
    mutate(ci_95_length = beta_U95 - beta_L95) %>%
    filter(k != 1) %>%
    mutate(s = s) %>% relocate(s, .before=k)
  beta_check
  
  beta_checkAll = bind_rows(beta_checkAll, beta_check)
  
  
  #################### check whether ETA was recovered #################### 
  eta_draws_1 <- reshape2::melt(eta_draws) %>%
    as_tibble() %>%
    rename(i = iterations, eta=value) %>%
    mutate(k = as.numeric(str_sub(parameters, 7, 7))) %>%
    mutate(l = as.numeric(str_sub(parameters, 5, 5)))
  
  eta_check <- eta_draws_1 %>% 
    group_by(k, l) %>%
    summarise(
      eta_L95 = quantile(eta, 0.025),
      eta_L70 = quantile(eta, 0.05),
      eta_L50 = quantile(eta, 0.25),
      etaM = mean(eta),
      eta_U50 = quantile(eta, 0.75),
      eta_U70 = quantile(eta, 0.95),
      eta_U95 = quantile(eta, 0.975),
      .groups = "drop"
    ) %>%
    arrange(l,k) %>%
    mutate(c = category_strings[k]) %>%
    mutate(eta_true = c(eta_mat[1,], eta_mat[2,], eta_mat[3,], eta_mat[4,])) %>%
    relocate(c, .after = k) %>%
    relocate(eta_true, .after = etaM) %>%
    mutate(is_covered_50 = as.numeric(eta_L50 <= eta_true & eta_true <= eta_U50)) %>%
    mutate(is_covered_70 = as.numeric(eta_L70 <= eta_true & eta_true <= eta_U70)) %>%
    mutate(is_covered_95 = as.numeric(eta_L95 <= eta_true & eta_true <= eta_U95)) %>%
    mutate(abs_diff = abs(eta_true - etaM)) %>%
    mutate(same_sign = ifelse(eta_true != 0, as.numeric(eta_true*etaM > 0), NA)) %>%
    mutate(ci_50_length = eta_U50 - eta_L50) %>%
    mutate(ci_70_length = eta_U70 - eta_L70) %>%
    mutate(ci_95_length = eta_U95 - eta_L95) %>%
    filter(k != 1) %>%
    mutate(s = s) %>% relocate(s, .before=k) %>%
    # mutate(l_ = colnames(X)[l]) 
    mutate(l_ = c("BQ", "PQ", "HAND", "HOME")[l])
  data.frame(eta_check)
  
  eta_checkAll = bind_rows(eta_checkAll, eta_check)
}

# write_csv(beta_checkAll, paste0("plots/results_sim", SIM_NUM, sim_noPf_str, "_underlying_", underlying, "_beta_checkAll.csv"))
# write_csv(eta_checkAll, paste0("plots/results_sim", SIM_NUM, sim_noPf_str, "_underlying_", underlying, "_eta_checkAll.csv"))
# write_csv(probs_checkAll, paste0("plots/results_sim", SIM_NUM, sim_noPf_str, "_underlying_", underlying, "_probs_checkAll.csv"))


########################################

{
  xwoba_checkAll = probs_checkAll %>%
    mutate(w = categories[k]) %>%
    group_by(s,t) %>%
    summarise(
      xw_L95 = sum(p_L95*w*1000),
      xw_L70 = sum(p_L70*w*1000),
      xw_L50 = sum(p_L50*w*1000),
      xwM = sum(pM*w*1000),
      xw_true = sum(p_true*w*1000),
      xw_U50 = sum(p_U50*w*1000),
      xw_U70 = sum(p_U70*w*1000),
      xw_U95 = sum(p_U95*w*1000)
    ) %>%
    mutate(is_covered_50 = as.numeric(xw_L50 <= xw_true & xw_true <= xw_U50)) %>%
    mutate(is_covered_70 = as.numeric(xw_L70 <= xw_true & xw_true <= xw_U70)) %>%
    mutate(is_covered_95 = as.numeric(xw_L95 <= xw_true & xw_true <= xw_U95)) %>%
    mutate(abs_diff = abs(xw_true - xwM)) %>%
    mutate(same_sign = ifelse(xw_true != 0, as.numeric(xw_true*xwM > 0), NA)) %>%
    mutate(ci_50_length = xw_U50 - xw_L50) %>%
    mutate(ci_70_length = xw_U70 - xw_L70) %>%
    mutate(ci_95_length = xw_U95 - xw_L95) 
}

#################### summmary stats over all sims #################### 

library(gt)
# library(gridExtra)

beta_is_covered = beta_checkAll %>% 
  mutate(
    TTOP_found_95 = ifelse(beta_true > 0, as.numeric(beta_L95 > 0), NA),
    no_TTOP_found_95 = ifelse(beta_true == 0, as.numeric(beta_L95 < 0 & 0 < beta_U95), NA),
    TTOP_found_50 = ifelse(beta_true > 0, as.numeric(beta_L50 > 0), NA),
    no_TTOP_found_50 = ifelse(beta_true == 0, as.numeric(beta_L50 < 0 & 0 < beta_U50), NA),
  ) %>%
  group_by(tto,c) %>%
  summarise(
    # TTOP_found_95 = mean(TTOP_found_95),
    # no_TTOP_found_95 = mean(no_TTOP_found_95),
    # TTOP_found_50 = mean(TTOP_found_50),
    # no_TTOP_found_50 = mean(no_TTOP_found_50),
    is_covered_50 = mean(is_covered_50),
    is_covered_50 = mean(is_covered_50),
    is_covered_95 = mean(is_covered_95),
    same_sign = mean(same_sign),
    .groups = "drop"
  ) %>% relocate(is_covered_95, .after=c) %>%
  gt() %>% fmt_missing(columns=everything(), missing_text = "")
beta_is_covered
gtsave(beta_is_covered,
       paste0("plots/plot_betaStats_sim", SIM_NUM, sim_noPf_str, ".png"),
       vwidth=1500, vheight=1500)

# png(paste0("plots/plot_betaStats_sim", SIM_NUM, sim_noPf_str, "_s", sss, ".png"),
#     height=1500, width=1500)
# p<-tableGrob(beta_is_covered)
# grid.arrange(p)
# dev.off()


eta_is_covered = eta_checkAll %>% 
  group_by(c,l_) %>%
  summarise(
    is_covered_50 = mean(is_covered_50),
    is_covered_70 = mean(is_covered_70),
    is_covered_95 = mean(is_covered_95),
    .groups="drop"
  ) %>% gt()
# data.frame(eta_is_covered)
gtsave(eta_is_covered,
       paste0("plots/plot_etaStats_sim", SIM_NUM, sim_noPf_str, ".png"),
       vwidth=1500, vheight=1500)

#################### PLOTS #################### 

sss = 3 # sim2: 7, 6, 9    # sim1: 5

beta_check_plot = beta_checkAll %>%
  filter(s == sss) %>%
  # mutate(beta_true_zeros = ifelse(beta_true == 0, beta_true, NA),
  #        beta_true_nonzeros = ifelse(beta_true != 0, beta_true, NA)) %>%
  mutate(tto = paste0(tto, "TTO")) %>%
  ggplot(aes(x=fct_reorder(c, k))) +
  facet_wrap(~tto, nrow=1) +
  theme(panel.spacing = unit(2, "lines")) +
  xlab("") + ylab(TeX("$\\beta$")) +
  geom_hline(yintercept=0, size=0.5, col="grey") + 
  geom_errorbar(aes(ymin=beta_L95, ymax=beta_U95), width = 0.5) +
  geom_errorbar(aes(ymin=beta_L50, ymax=beta_U50), width = 0.25, size=1) +
  geom_point(aes(y=betaM), col="black", size=2, stroke=1, shape=21, fill="white") +
  geom_point(aes(y=beta_true), col="firebrick", size=5, shape=18)
#   geom_point(aes(y=beta_true_zeros), col="firebrick", size=5, shape=18)
# if (SIM_NUM != 1) {
#   beta_check_plot = beta_check_plot +
#     # geom_point(aes(y=beta_true_nonzeros), col="#56B4E9", size=5, shape=18)
#     geom_point(aes(y=beta_true_nonzeros), col="firebrick", size=5, shape=18)
# }
beta_check_plot
ggsave(paste0("plots/plot_sim", SIM_NUM, sim_noPf_str, "_s", sss, "_beta_check", ".png"),
       beta_check_plot, width=9, height=5)


probs_check_plot = probs_checkAll %>%
  filter(s == sss) %>%
  ggplot(aes(x=t)) +
  facet_wrap(~c, nrow=3, scales="free") +
  theme(panel.spacing = unit(2, "lines")) +
  # xlab("") + ylab(TeX("$\\p$")) +
  # geom_hline(yintercept=0, size=0.5, col="grey") + 
  geom_errorbar(aes(ymin=p_L95, ymax=p_U95), width = 0.5) +
  geom_errorbar(aes(ymin=p_L50, ymax=p_U50), width = 0.25, size=1) +
  geom_point(aes(y=pM), col="black", size=2, stroke=1, shape=21, fill="white") +
  geom_point(aes(y=p_true), col="firebrick", size=5, shape=18) +
  geom_vline(aes(xintercept =  9), size=0.5, color="gray50") + #1.2
  geom_vline(aes(xintercept = 18), size=0.5, color="gray50") +
  ylab("probability") + 
  scale_x_continuous(name="batter sequence number, t", breaks=seq(0,27,3))
probs_check_plot
ggsave(paste0("plots/plot_sim", SIM_NUM, sim_noPf_str, "_s", sss, "_probs_check", ".png"),
       probs_check_plot, width=12, height=12)


xwoba_check_plot = xwoba_checkAll %>%
  filter(s == sss) %>%
  ggplot(aes(x=t)) +
  geom_errorbar(aes(ymin=xw_L95, ymax=xw_U95), width = 0.5) +
  geom_errorbar(aes(ymin=xw_L50, ymax=xw_U50), width = 0.25, size=1) +
  geom_point(aes(y=xwM), col="black", size=2, stroke=1, shape=21, fill="white") +
  geom_point(aes(y=xw_true), col="firebrick", size=5, shape=18) +
  geom_vline(aes(xintercept =  9), size=0.5, color="gray50") + #1.2
  geom_vline(aes(xintercept = 18), size=0.5, color="gray50") +
  ylab("wOBA") + 
  scale_x_continuous(name="batter sequence number, t", breaks=seq(0,27,3))
xwoba_check_plot
ggsave(paste0("plots/plot_sim", SIM_NUM, sim_noPf_str, "_s", sss, "_xwoba_check", ".png"),
       xwoba_check_plot, width=8, height=5)

eta_check_plot = eta_checkAll %>%
  filter(s == sss) %>%
  mutate(l_order = ifelse(l_ == "PQ", 1, ifelse(l_ == "BQ", 2, ifelse(l_ == "HAND", 3, 4)))) %>%
  ggplot(aes(x=fct_reorder(l_,  l_order ))) +
  # ggplot(aes(x=l_)) +
  facet_wrap(~c, nrow=2, scales="free") +
  theme(panel.spacing = unit(2, "lines")) +
  xlab("") + ylab(TeX("$\\eta$")) +
  # geom_hline(yintercept=0, size=0.5, col="grey") + 
  geom_errorbar(aes(ymin=eta_L95, ymax=eta_U95), width = 0.5) +
  geom_errorbar(aes(ymin=eta_L50, ymax=eta_U50), width = 0.25, size=1) +
  geom_point(aes(y=etaM), col="black", size=2, stroke=1, shape=21, fill="white") +
  geom_point(aes(y=eta_true), col="firebrick", size=4, shape=18) 
eta_check_plot
ggsave(paste0("plots/plot_sim", SIM_NUM, sim_noPf_str, "_s", sss, "_eta_check", ".png"),
       eta_check_plot, width=12, height=8)

# }

############################################
############################################

# sss = s #2

xwoba_check_plot_true = xwoba_checkAll %>%
  filter(s == sss) %>%
  ggplot(aes(x=t)) +
  # geom_errorbar(aes(ymin=xw_L95, ymax=xw_U95), width = 0.5) +
  # geom_errorbar(aes(ymin=xw_L50, ymax=xw_U50), width = 0.25, size=1) +
  # geom_point(aes(y=xwM), col="black", size=2, stroke=1, shape=21, fill="white") +
  geom_point(aes(y=xw_true), col="firebrick", size=5, shape=18) +
  geom_vline(aes(xintercept =  9), size=0.5, color="gray50") + #1.2
  geom_vline(aes(xintercept = 18), size=0.5, color="gray50") +
  ylab("wOBA") + 
  scale_y_continuous(name="wOBA", breaks=seq(270,350,by=20), 
                     limits = c(min(xwoba_checkAll$xw_L95)-5, max(xwoba_checkAll$xw_U95)+5))  +
  scale_x_continuous(name="batter sequence number, t", breaks=seq(0,27,3))
xwoba_check_plot_true
ggsave(paste0("plots/plot_sim", SIM_NUM, sim_noPf_str, "_s", sss, "_xwoba_check_true", ".png"),
       xwoba_check_plot_true, width=8, height=5)

xwoba_check_plot_post = xwoba_checkAll %>%
  filter(s == sss) %>%
  ggplot(aes(x=t)) +
  geom_errorbar(aes(ymin=xw_L95, ymax=xw_U95), width = 0.5) +
  geom_errorbar(aes(ymin=xw_L50, ymax=xw_U50), width = 0.25, size=1) +
  geom_point(aes(y=xwM), col="black", size=2, stroke=1, shape=21, fill="white") +
  geom_point(aes(y=xw_true), col="firebrick", size=5, shape=18) +
  geom_vline(aes(xintercept =  9), size=0.5, color="gray50") + #1.2
  geom_vline(aes(xintercept = 18), size=0.5, color="gray50") +
  ylab("wOBA") + 
  scale_y_continuous(name="wOBA", breaks=seq(270,350,by=20), 
                     limits = c(min(xwoba_checkAll$xw_L95)-5, max(xwoba_checkAll$xw_U95)+5))  +
  scale_x_continuous(name="batter sequence number, t", breaks=seq(0,27,3))
xwoba_check_plot
ggsave(paste0("plots/plot_sim", SIM_NUM, sim_noPf_str, "_s", sss, "_xwoba_check_post", ".png"),
       xwoba_check_plot_post, width=8, height=5)

