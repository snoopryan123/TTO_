
########################
s = 1
# SIM_NUM = 2 #1 #2
# YRS = 2018
source("sim_config.R")
########################

source("../model9_getData.R") ### get observed data 
source("sim_simulateData.R") ### get simulated outcomes and "true" params

### import fit from rstan
fit <- readRDS(paste0(output_folder,"fit_sim", SIM_NUM, "_model_bsnBL_", s, ".rds"))
draws <- as.matrix(fit)

beta_draws <- draws[,startsWith(colnames(draws), "beta")]
eta_draws <- draws[,startsWith(colnames(draws), "eta")]

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
    beta_L50 = quantile(beta, 0.25),
    betaM = mean(beta),
    beta_U50 = quantile(beta, 0.75),
    beta_U95 = quantile(beta, 0.975),
    .groups = "drop"
  ) %>%
  arrange(tto,k) %>%
  mutate(c = category_strings[k]) %>%
  mutate(beta_true = c(beta_mat[1,], beta_mat[2,])) %>%
  relocate(c, .after = k) %>%
  relocate(beta_true, .after = betaM) %>%
  mutate(is_covered_50 = as.numeric(beta_L50 <= beta_true & beta_true <= beta_U50)) %>%
  mutate(is_covered_95 = as.numeric(beta_L95 <= beta_true & beta_true <= beta_U95)) %>%
  mutate(abs_diff = abs(beta_true - betaM)) %>%
  mutate(same_sign = ifelse(beta_true != 0, as.numeric(beta_true*betaM > 0), NA)) %>%
  mutate(ci_50_length = beta_U50 - beta_L50) %>%
  mutate(ci_95_length = beta_U95 - beta_L95) %>%
  filter(k != 1)
beta_check


beta_check_plot = beta_check %>%
  mutate(beta_true_zeros = ifelse(beta_true == 0, beta_true, NA),
         beta_true_nonzeros = ifelse(beta_true != 0, beta_true, NA)) %>%
  mutate(tto = paste0(tto, "TTO")) %>%
  ggplot(aes(x=fct_reorder(c, k))) +
  facet_wrap(~tto, nrow=1) +
  theme(panel.spacing = unit(2, "lines")) +
  xlab("") + ylab(TeX("$\\beta$")) +
  geom_hline(yintercept=0, size=0.5, col="grey") + 
  geom_errorbar(aes(ymin=beta_L95, ymax=beta_U95), width = 0.5) +
  geom_errorbar(aes(ymin=beta_L50, ymax=beta_U50), width = 0.25, size=1) +
  geom_point(aes(y=betaM), col="black", size=2, stroke=1, shape=21, fill="white") +
  # geom_point(aes(y=beta_true), col="firebrick", size=4, shape=18) 
  geom_point(aes(y=beta_true_zeros), col="firebrick", size=5, shape=18) +
  geom_point(aes(y=beta_true_nonzeros), col="#56B4E9", size=5, shape=18)
  # geom_point(aes(y=beta_true_nonzeros), col="dodgerblue2", size=5, shape=18) 
beta_check_plot
ggsave(paste0("plot_sim", SIM_NUM, "_s", s, "_beta_check", ".png"),
       beta_check_plot, width=9, height=5)


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
    eta_L50 = quantile(eta, 0.25),
    etaM = mean(eta),
    eta_U50 = quantile(eta, 0.75),
    eta_U95 = quantile(eta, 0.975),
    .groups = "drop"
  ) %>%
  arrange(l,k) %>%
  mutate(c = category_strings[k]) %>%
  mutate(eta_true = c(eta_mat[1,], eta_mat[2,], eta_mat[3,], eta_mat[4,])) %>%
  relocate(c, .after = k) %>%
  relocate(eta_true, .after = etaM) %>%
  mutate(is_covered_50 = as.numeric(eta_L50 <= eta_true & eta_true <= eta_U50)) %>%
  mutate(is_covered_95 = as.numeric(eta_L95 <= eta_true & eta_true <= eta_U95)) %>%
  mutate(abs_diff = abs(eta_true - etaM)) %>%
  mutate(same_sign = ifelse(eta_true != 0, as.numeric(eta_true*etaM > 0), NA)) %>%
  mutate(ci_50_length = eta_U50 - eta_L50) %>%
  mutate(ci_95_length = eta_U95 - eta_L95) %>%
  filter(k != 1) %>%
  # mutate(l_ = colnames(X)[l]) 
  mutate(l_ = c("BQ", "PQ", "HAND", "HOME")[l])
data.frame(eta_check)

eta_check_plot = eta_check %>%
  mutate(l_order = ifelse(l_ == "PQ", 1, ifelse(l_ == "BQ", 2, ifelse(l_ == "HAND", 3, 4)))) %>%
  ggplot(aes(x=fct_reorder(l_,  l_order ))) +
  # ggplot(aes(x=l_)) +
  facet_wrap(~c, nrow=2) +
  theme(panel.spacing = unit(2, "lines")) +
  xlab("") + ylab(TeX("$\\eta$")) +
  # geom_hline(yintercept=0, size=0.5, col="grey") + 
  geom_errorbar(aes(ymin=eta_L95, ymax=eta_U95), width = 0.5) +
  geom_errorbar(aes(ymin=eta_L50, ymax=eta_U50), width = 0.25, size=1) +
  geom_point(aes(y=etaM), col="black", size=2, stroke=1, shape=21, fill="white") +
  geom_point(aes(y=eta_true), col="firebrick", size=4, shape=18) 
eta_check_plot
ggsave(paste0("plot_sim", SIM_NUM, "_s", s, "_eta_check", ".png"),
       eta_check_plot, width=12, height=8)






