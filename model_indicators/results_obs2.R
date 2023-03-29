
########################
library(tidyverse)
# theme_set(theme_bw())
# theme_update(text = element_text(size=18))
# theme_update(plot.title = element_text(hjust = 0.5))
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
  # print(dim(probs[[7]]))
  
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
  probs_df = probs_df %>% mutate(c = category_strings[k]) %>%
    mutate(tto = ifelse(1 <= t & t <= 9, 1, ifelse(10 <= t & t <= 18, 2, 3)))
  return(probs_df)
}

########################
YRS = 2018
source("model9_getData.R") ### get observed data

### import fit from rstan
fit <- readRDS(paste0(output_folder, "fit_obs_model_lineyrs_",YRS-2000,"_.rds"))
draws <- as.matrix(fit)

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

###
probs_tilde_TTOavg = probs_tilde %>%
  group_by(iter, c, tto) %>%
  summarise(p = mean(p)) %>%
  group_by(iter,c) %>%
  mutate(diff_12 = p[2] - p[1],
         diff_23 = p[3] - p[2]) %>%
  filter(tto == 1 | tto == 2) %>% 
  rename(diff = tto) %>%
  ungroup()
probs_tilde_TTOavg 

plot_category_prob_hists <- function(p_diff_df, l=-0.006, u=0.015) {
  p_diff_df2 = p_diff_df %>% group_by(c) %>% summarise(mean_p=mean(diff))
  p_diff_df %>% ggplot() +
    # facet_wrap(~c,scales = "free") +
    facet_wrap(~c) +
    geom_histogram(aes(x=diff, y=..density..), fill="black", bins=50) +
    geom_vline(aes(xintercept=0), color="dodgerblue2", size=0.5) +
    geom_vline(data=p_diff_df2, aes(xintercept=mean_p), color="firebrick", size=0.5) +
    scale_x_continuous(name="difference in probability",
    breaks=seq(-0.02,0.02,by=0.01),
    limits=c(l-0.002, u+0.002)
    ) +
    xlab("difference in probability") +
    # theme_update(text = element_text(size=12)) +
    theme(panel.spacing = unit(2, "lines")) +
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank())
}

plot_probs_avgTTOdiff12 = plot_category_prob_hists(
  probs_tilde_TTOavg %>% 
    filter(diff == 1 & c != "out") %>%
    select(c, diff_12) %>%
    rename(diff=diff_12),
  u = 0.0125,
  l = -0.005
  )
plot_probs_avgTTOdiff12

plot_probs_avgTTOdiff23 = plot_category_prob_hists(
  probs_tilde_TTOavg %>% 
    filter(diff == 2 & c != "out") %>%
    select(c, diff_23) %>%
    rename(diff=diff_23),
  u = 0.0125,
  l = -0.005
)
plot_probs_avgTTOdiff23

ggsave(paste0("plots/tto_p_diff_12.png"), plot_probs_avgTTOdiff12, width=10, height=4)
ggsave(paste0("plots/tto_p_diff_23.png"), plot_probs_avgTTOdiff23, width=10, height=4)




############### get t -> xWOBA(t,x) ##############
xwoba_tilde = probs_tilde %>%
  mutate(w = categories[k]) %>%
  group_by(iter,t,  tto) %>%
  summarise(xw = sum(p*w*1000)) %>%
  ungroup()
xwoba_tilde
### 27*750*7 = 141750
### 27*750 = 20250

###
xwoba_tilde_TTOavg = xwoba_tilde %>%
  group_by(iter,tto) %>%
  summarise(xw = mean(xw)) %>%
  mutate(diff_12 = xw[2] - xw[1],
         diff_23 = xw[3] - xw[2]) %>%
  filter(tto == 1 | tto == 2) %>% 
  rename(diff = tto) %>%
  ungroup()
xwoba_tilde_TTOavg 

plot_xwoba_diff_hist <- function(xw_diff) {
  xw_diff %>% ggplot() +
    geom_histogram(aes(x=xw, y=..density..), fill="black", bins=50) +
    geom_vline(aes(xintercept=0), color="dodgerblue2", size=1) +
    geom_vline(aes(xintercept=mean(xw)), color="firebrick", size=1) +
    # scale_x_continuous(name="probability", breaks=seq(-0.03,0.03,by=0.01)) +
    scale_x_continuous(name="difference in expected wOBA points") +
    theme(panel.spacing = unit(2, "lines")) +
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank()) 
}

plot_xwoba_avgTTOdiff12 = plot_xwoba_diff_hist(
  xwoba_tilde_TTOavg %>% 
    filter(diff == 1) %>%
    select(diff_12) %>%
    rename(xw=diff_12)
)
plot_xwoba_avgTTOdiff12

plot_xwoba_avgTTOdiff23 = plot_xwoba_diff_hist(
  xwoba_tilde_TTOavg %>% 
    filter(diff == 2) %>%
    select(diff_23) %>%
    rename(xw=diff_23)
)
plot_xwoba_avgTTOdiff23

ggsave(paste0("plots/tto_xwoba_diff_12.png"), plot_xwoba_avgTTOdiff12, width=5, height=4)
ggsave(paste0("plots/tto_xwoba_diff_23.png"), plot_xwoba_avgTTOdiff23, width=5, height=4)




############### the impact of confounders! ##############

### hand vs. home
INCPT_tilde = cbind(rep(1,27))
S_tilde = cbind(1:27) ## cbind(1, 1:27)
O_tilde = matrix(c(rep(0,9), rep(1,9), rep(0,9), rep(0,9), rep(0,9), rep(1,9)), nrow=27)

hh_results = matrix(nrow=2,ncol=2)
hand_values = c(0,1)
home_values = c(0,1)
rownames(hh_results) = paste0("hand ", hand_values)
colnames(hh_results) = paste0("home ", home_values)
for (i in 1:length(hand_values)) {
  for (j in 1:length(home_values)) {
    hand = hand_values[i]
    home = home_values[j]
    X_tilde_hh = matrix( rep(c(logit(0.315), logit(0.315), hand, home), 27), nrow=27, byrow = TRUE)
    probs_tilde_hh = fit_to_posterior_probs(fit, INCPT_tilde, S_tilde, O_tilde, X_tilde_hh)
    xw_hh = probs_tilde_hh %>%
        group_by(t, iter) %>%
        mutate(w = categories[k]) %>%
        summarise(xw = sum(p*w*1000)) %>%
        summarise(xw = mean(xw)) %>%
        summarise(xw = mean(xw))
    hh_results[i,j] = xw_hh$xw
  }
}

print(hh_results)
write_csv(data.frame(hh_results), "plots/hand_home_results.csv")

### BQ vs. PQ
INCPT_tilde = cbind(rep(1,27))
S_tilde = cbind(1:27) ## cbind(1, 1:27)
O_tilde = matrix(c(rep(0,9), rep(1,9), rep(0,9), rep(0,9), rep(0,9), rep(1,9)), nrow=27)

bq_values = quantile(D$BQ, probs = c(.05, .25, .50, .75, .95))
pq_values = quantile(D$PQ, probs = c(.05, .25, .50, .75, .95))
bqpq_results = matrix(nrow=length(bq_values),ncol=length(pq_values))
rownames(bqpq_results) = paste0("BQ ", names(bq_values))
colnames(bqpq_results) = paste0("PQ ", names(pq_values))
for (i in 1:length(bq_values)) {
  for (j in 1:length(pq_values)) {
    bq = bq_values[i]
    pq = pq_values[j]
    X_tilde_bqpq = matrix( rep(c(logit(bq), logit(pq), 1, 0), 27), nrow=27, byrow = TRUE)
    probs_tilde_bqpq = fit_to_posterior_probs(fit, INCPT_tilde, S_tilde, O_tilde, X_tilde_bqpq)
    xw_bqpq = probs_tilde_bqpq %>%
      group_by(t, iter) %>%
      mutate(w = categories[k]) %>%
      summarise(xw = sum(p*w*1000)) %>%
      summarise(xw = mean(xw)) %>%
      summarise(xw = mean(xw))
    bqpq_results[i,j] = xw_bqpq$xw
  }
}

print(bqpq_results)
# print(bqpq_results[2:4,2:4])
write_csv(data.frame(bqpq_results), "plots/bq_pq_results.csv")




##########################
### understanding wOBA ###
##########################

# # number of plate appearances
# nrow(D)
# # number of batters
# length(unique(D$BAT_ID))
# # number of pitchers
# length(unique(D$PIT_ID))

# ### between-game standard deviation in wOBA for each pitcher, then take median
# D %>% 
#   group_by(PIT_ID,YEAR) %>%
#   summarise(gameByGame_woba_sd = sd(EVENT_WOBA_19), .groups = "drop") %>%
#   summarise(med_gameByGame_woba_sd = mean(gameByGame_woba_sd, na.rm=TRUE))
# 
# ### within-game standard deviation in wOBA for each pitcher, then take median
# D %>% 
#   group_by(PIT_ID,GAME_ID) %>%
#   summarise(withinGame_woba_sd = sd(EVENT_WOBA_19), .groups = "drop") %>%
#   summarise(med_withinGame_woba_sd = mean(withinGame_woba_sd, na.rm=TRUE))

### within-game sd in pitcher performance
# D %>%
#   group_by(PIT_ID,YEAR,GAME_ID) %>%
#   summarise(withinGameWobaSd = sd(EVENT_WOBA_19), .groups = "drop") %>%
#   summarise(med_withinGameWobaSd = median(withinGameWobaSd, na.rm=TRUE)) 

batters_woba = D %>% group_by(BAT_ID) %>% 
  summarise(WOBA = unique(WOBA_FINAL_BAT_19)*1000, num_pa=n()) %>%
  filter(num_pa >= 100)
batters_woba
pitchers_woba = D %>% group_by(PIT_ID) %>% 
  summarise(WOBA = unique(WOBA_FINAL_PIT_19)*1000, num_pa=n()) %>%
  filter(num_pa >= 100)
pitchers_woba

sd(batters_woba$WOBA)
sd(pitchers_woba$WOBA)

quantile(batters_woba$WOBA, c(.05,.5,.95))
quantile(pitchers_woba$WOBA, c(.05,.5,.95))

plot_woba_hist <- function(woba_df) {
  woba_df %>% ggplot() +
    geom_histogram(aes(x=WOBA), bins=50, fill="black") +
    geom_vline(aes(xintercept=mean(WOBA)), color="firebrick", size=1) +
    scale_x_continuous(name="wOBA points", limits=c(200,450))
}

plot_bat_woba = plot_woba_hist(batters_woba)
plot_bat_woba
plot_pit_woba = plot_woba_hist(pitchers_woba)
plot_pit_woba

# ggsave("plots/plot_bat_woba.png", plot_bat_woba, width=6, height=5)
# ggsave("plots/plot_pit_woba.png", plot_pit_woba, width=6, height=5)

########################################################

### effect size calculations for the intro

### mean prob effects for each TTO
mean_prob_diffs = probs_tilde %>%
  group_by(iter, c, tto) %>%
  summarise(p = mean(p)) %>%
  group_by(tto,c) %>%
  mutate(diff_12 = p[2] - p[1],
         diff_23 = p[3] - p[2]) %>%
  filter(tto == 1 | tto == 2) %>% 
  summarise(
    diff_12_U = quantile(diff_12, .95),
    diff_12_L = quantile(diff_12, .05),
    diff_23_U = quantile(diff_23, .95),
    diff_23_L = quantile(diff_23, .05),
    diff_12 = mean(diff_12),
    diff_23 = mean(diff_23)
  ) %>%
  filter(c!="out") %>%
  rename(diff = tto) %>%
  ungroup() %>%
  mutate(
    diff_12 = ifelse(diff==2, NA, diff_12),
    diff_23 = ifelse(diff==1, NA, diff_23),
    diff_12_L = ifelse(diff==2, NA, diff_12_L),
    diff_23_L = ifelse(diff==1, NA, diff_23_L),
    diff_12_U = ifelse(diff==2, NA, diff_12_U),
    diff_23_U = ifelse(diff==1, NA, diff_23_U),
  )
mean_prob_diffs
max(mean_prob_diffs$diff_12, na.rm=TRUE)
min(mean_prob_diffs$diff_23, na.rm=TRUE)
## triple from 1TTO to 2TTO increases most in prob 
## double from 2TTO to 3TTO decreases most in prob 

mean_probs_tto = probs_tilde %>%
  group_by(iter, c, tto) %>%
  summarise(p = mean(p)) %>%
  group_by(tto,c) %>%
  summarise(p = mean(p))
data.frame(mean_probs_tto)
## triple from 1TTO to 2TTO increases in prob from 0.003291545 to 0.003666526
## double from 2TTO to 3TTO decreases in prob from 0.046110114 to 0.049469223






fit_to_logOdds <- function(fit,INCPT,S,O,X) {
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
  # ## linpreds[[1]][1:10,1:10]
  # sum_linpreds = Reduce("+", linpreds)
  # normalize <- function(A) { A / sum_linpreds}
  # probs = lapply(linpreds, normalize)
  # ## probs[[1]][1,1]+probs[[2]][1,1]+probs[[3]][1,1]+probs[[4]][1,1]+probs[[5]][1,1]+probs[[6]][1,1]+probs[[7]][1,1]
  # ## probs[[1]][1:1000]
  # # print(dim(probs[[7]]))
  
  ### turn to tibble
  linpreds_df = tibble()
  for (k in 1:7) {
    linpreds_df_k0 = linpreds[[k]]
    linpreds_df_k = reshape2::melt(linpreds_df_k0) %>%
      as_tibble() %>%
      rename(t = Var1, iter=Var2, p=value) %>%
      arrange(t, iter) %>%
      mutate(k = k) 
    linpreds_df = bind_rows(linpreds_df, linpreds_df_k)
  }
  # linpreds_tilde_df %>% group_by(k) %>% summarise(count=n(), count2=n()/27) ## check
  linpreds_df = linpreds_df %>% mutate(c = category_strings[k]) %>%
    mutate(tto = ifelse(1 <= t & t <= 9, 1, ifelse(10 <= t & t <= 18, 2, 3)))
  return(linpreds_df)
}

logOdds_tilde = fit_to_logOdds(fit, INCPT_tilde, S_tilde, O_tilde, X_tilde)

### mean logOdds effects for each TTO
mean_logOdds_diffs = logOdds_tilde %>%
  group_by(iter, c, tto) %>%
  summarise(xx = mean(p)) %>%
  group_by(tto,c) %>%
  mutate(diff_12 = xx[2] - xx[1],
         diff_23 = xx[3] - xx[2]) %>%
  filter(tto == 1 | tto == 2) %>% 
  summarise(
    diff_12 = mean(diff_12),
    diff_23 = mean(diff_23)
  ) %>%
  filter(c!="out") %>%
  rename(diff = tto) %>%
  ungroup() %>%
  mutate(
    diff_12 = ifelse(diff==2, NA, diff_12),
    diff_23 = ifelse(diff==1, NA, diff_23),
  )
mean_logOdds_diffs
max(mean_logOdds_diffs[,3:4], na.rm=TRUE)
min(mean_logOdds_diffs[,3:4], na.rm=TRUE)
## triple from 1TTO to 2TTO increase in logOdds of 0.0006742307
## double from 2TTO to 3TTO decrease in logOdds of -0.002506579

mean_logOdds_tto = logOdds_tilde %>%
  group_by(iter, c, tto) %>%
  summarise(p = mean(p)) %>%
  group_by(tto,c) %>%
  summarise(p = mean(p))
data.frame(mean_logOdds_tto)
## triple from 1TTO to 2TTO increases in logOdd from 0.004696125 to 0.005312095
## double from 2TTO to 3TTO decreases in logOdd from 0.066795279 to 0.072317538




### 
xwoba_tilde_TTOavg %>% 
  filter(diff == 1) %>%
  select(diff_12) %>%
  rename(xw=diff_12) %>%
  summarise(xwL = quantile(xw, 0.05),
            xwM = mean(xw),
            xwU = quantile(xw, 0.95),)

### 
xwoba_tilde_TTOavg %>% 
  filter(diff == 2) %>%
  select(diff_23) %>%
  rename(xw=diff_23) %>%
  summarise(xwL = quantile(xw, 0.05),
            xwM = mean(xw),
            xwU = quantile(xw, 0.95),)


