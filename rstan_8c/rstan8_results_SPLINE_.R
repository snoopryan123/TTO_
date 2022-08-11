library(tidyverse)
library(grid)
library(splines)
library(ggridges)
library(latex2exp)
theme_set(theme_bw())
theme_update(text = element_text(size=18))
theme_update(plot.title = element_text(hjust = 0.5))
output_folder = './job_output/'

###############
### RESULTS ###
###############

### posterior samples of SPLINE model
# fit <- readRDS("job_output/fit_rstan8-3.R.rds")
# fit <- readRDS("job_output/fit_rstan8-3_noPitAsBat.R.rds")
year = 2018 # 2020
fit <- readRDS(paste0("job_output/fit_rstan8-",year-2000,".R.rds"))
draws <- as.matrix(fit)

### load data
input_file = "../data/TTO_dataset_510.csv"  
D0 <- read_csv(input_file) #%>% drop_na() 
D <- D0 %>% filter(YEAR == year) %>% filter(BQ>0 & BQ<1 & PQ>0 & PQ<1 ) %>% filter(ORDER_CT <= 3)
logit <- function(p) { log(p/(1-p)) }
X <- as.matrix(D %>% mutate(lBQ=logit(BQ), lPQ=logit(PQ)) %>% select(lBQ, lPQ, HAND_MATCH, BAT_HOME_IND)) 
source("rstan8_main.R")

##############################
### dataset summary (2018) ###
##############################

D0_2018 = D0 %>% filter(YEAR == year) %>% filter(ORDER_CT <= 3)

# number of plate appearances
nrow(D0_2018)
# number of batters
length(unique(D0_2018$BAT_ID))
# number of pitchers
length(unique(D0_2018$PIT_ID))

names(D0_2018)

###########################################
### base rate cross entropy loss (2019) ###

# year=2019
# y_tib = as_tibble(y) %>% rename(y=V1)
# base_rate_df = y_tib %>% group_by(y) %>% summarise(count=n()) %>% mutate(p = count/sum(count))
# base_rate_df
# base_rate_df$p
# -mean(log(as.numeric(as.matrix(modelr::model_matrix(~ factor(y) + 0, data=y_tib)) %*% matrix(base_rate_df$p, ncol=1))))

##########################
### understanding wOBA ###
##########################

### game-by-game standard deviation in wOBA for each pitcher, then take median
D %>% 
  group_by(PIT_ID,YEAR) %>%
  summarise(gameByGame_woba_sd = sd(EVENT_WOBA_19), .groups = "drop") %>%
  summarise(med_gameByGame_woba_sd = median(gameByGame_woba_sd, na.rm=TRUE))

D %>% group_by(PIT_ID,YEAR) %>%
  summarise(gameByGame_woba_sd = sd(EVENT_WOBA_19), .groups = "drop")

data.frame(
D %>% group_by(PIT_ID,YEAR) %>%
  summarise(gameByGame_woba_mean = mean(EVENT_WOBA_19), .groups = "drop")
)

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

ggsave("plots/plot_bat_woba.png", plot_bat_woba, width=6, height=5)
ggsave("plots/plot_pit_woba.png", plot_pit_woba, width=6, height=5)






#########################
### HELPER FUNCTIONS ####
#########################

get_bat_seq_draws <- function(draws) {
  alpha_draws = draws[,str_detect(colnames(draws), "^alpha")]
  bat_seq_draws = list()
  for (k in 1:7) {
    alpha_draws_k = alpha_draws[,endsWith(colnames(alpha_draws), paste0(k,"]"))]
    bat_seq_draws_k = alpha_draws_k %*% t(bbb)
    bat_seq_draws[[length(bat_seq_draws) + 1]] = bat_seq_draws_k
  }
  bat_seq_draws
}

spline_fit_to_posterior_probs <- function(SPL_test,X_test,fit) {
  draws=as.matrix(fit)
  alpha_draws = draws[,str_detect(colnames(draws), "^alpha")]
  eta_draws = draws[,str_detect(colnames(draws), "^eta")]
  
  linpreds = list()
  for (k in 1:7) {
    print(k)
    alpha_draws_k = alpha_draws[,endsWith(colnames(alpha_draws), paste0(k,"]"))]
    eta_draws_k = eta_draws[,endsWith(colnames(eta_draws), paste0(k,"]"))]
    linpred_k = SPL_test%*%t(alpha_draws_k) + X_test%*%t(eta_draws_k)
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

get_eta_draws <- function(fit) {
  draws=as.matrix(fit)
  eta_draws = draws[,str_detect(colnames(draws), "^eta")]
  all_params = list()
  for (k in 1:7) {
    # print(k)
    eta_draws_k = eta_draws[,endsWith(colnames(eta_draws), paste0(k,"]"))]
    all_params[[length(all_params)+1]] = eta_draws_k
  }
  all_params
}


###########################################################################

prob_ktx <- function(k,t,x,bat_seq_draws,eta_draws) {
  # batter sequence number t \in {1,...,27}
  # confounder vector x^T = (logit(BQ), logit(PQ), hand, home)
  # return posterior distribution of P(y = k| t, x)
  # bat_seq_draws === alpha_{tk}
  log_numerator = bat_seq_draws[[k]][,t] + eta_draws[[k]] %*% matrix(x)
  denominator = 1
  for (j in 2:7) {
    denominator = denominator + exp(bat_seq_draws[[j]][,t] + eta_draws[[j]] %*% matrix(x))
  }
  log_p = log_numerator - log(denominator)
  p = exp(log_p)
  p
}

get_prob_tibble <- function(x, bat_seq_draws, eta_draws) {
  all_prob_ktx = tibble()
  for (t in 1:27) {
    print(t)
    for (k in 2:7) {
      pktx = as.numeric(prob_ktx(k,t,x, bat_seq_draws,eta_draws))
      tib_pktx = tibble(p = pktx, k=k, t=t)
      all_prob_ktx = bind_rows(all_prob_ktx, tib_pktx)
    }
  }
  all_prob_ktx
}

########################################
### RESULTS on the PROBABILITY SCALE ###
########################################

### batter sequence too draws 1,...,27
bat_seq_draws = get_bat_seq_draws(draws) #bat_seq_draws[[7]][1:10,1:10]
eta_draws = get_eta_draws(fit)

x1 = c(logit(median(batters_woba$WOBA)/1000), logit(median(pitchers_woba$WOBA)/1000), 1, 0); subfolder = "x1/";
# x1 = c(logit(quantile(batters_woba$WOBA,.95)/1000), logit(quantile(pitchers_woba$WOBA,.05)/1000), 1, 0);  subfolder = "x2/";
# x1 = c(logit(quantile(batters_woba$WOBA,.05)/1000), logit(quantile(pitchers_woba$WOBA,.95)/1000), 1, 0);  subfolder = "x3/";
# x1 = c(logit(median(batters_woba$WOBA)/1000), logit(median(pitchers_woba$WOBA)/1000), 1, 1);  subfolder = "x4/";
# x1 = c(logit(median(batters_woba$WOBA)/1000), logit(median(pitchers_woba$WOBA)/1000), 0, 0);  subfolder = "x5/";
probs1 = get_prob_tibble(x1, bat_seq_draws, eta_draws)
probs1

plot_category_prob_hists <- function(p_diff_df, l=-0.02, u=0.02) {
  p_diff_df2 = p_diff_df %>% group_by(k) %>% summarise(mean_p=mean(p))
  p_diff_df %>% ggplot() +
    facet_wrap(~k) +
    geom_histogram(aes(x=p, y=..density..), fill="black", bins=50) +
    geom_vline(aes(xintercept=0), color="dodgerblue2", size=0.5) +
    geom_vline(data=p_diff_df2, aes(xintercept=mean_p), color="firebrick", size=0.5) +
    scale_x_continuous(name="difference in probability", 
                       breaks=seq(-0.03,0.03,by=0.01), limits=c(l-0.007, u+0.007)) +
    # scale_x_continuous(name="probability") +
    theme_update(text = element_text(size=12)) +
    theme(panel.spacing = unit(.25, "lines")) +
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank()) 
}

# get_diff_plot_2_batters(5,14,probs1)

get_avg_tto_diff_df <- function(tto1, tto2, probs) {
  ts1 = 1:9 + (tto1-1)*9
  ts2 = 1:9 + (tto2-1)*9
  p_tto1 = probs %>% filter(t %in% ts1) 
  p_tto2 = probs %>% filter(t %in% ts2) 
  
  p_avg_tto1 = numeric( nrow(p_tto1)/9 )
  p_avg_tto2 = numeric( nrow(p_tto1)/9 )
  for (i in 1:length(ts1)) {
    t1 = ts1[i]
    t2 = ts2[i]
    p_avg_tto1 = p_avg_tto1 + (p_tto1 %>% filter(t == t1))$p
    p_avg_tto2 = p_avg_tto2 + (p_tto2 %>% filter(t == t2))$p
  } 
  p_avg_tto1 = p_avg_tto1/9
  p_avg_tto2 = p_avg_tto2/9
  p_diff = p_avg_tto2 - p_avg_tto1
  p_diff_tib = tibble(p=p_diff, k=(p_tto1 %>% filter(t == ts1[1]))$k)
  p_diff_tib$k = factor(p_diff_tib$k, labels = category_strings[2:7])
  p_diff_tib
}

get_avg_tto_diff_plot <- function(tto1, tto2, probs) {
  p_diff_tib = get_avg_tto_diff_df(tto1, tto2, probs)
  plot_category_prob_hists(p_diff_tib, l=-0.01, u=0.01)
}

get_diff_plot_2_batters <- function(t1, t2, probs) {
  p_t1 = probs %>% filter(t == t1) 
  p_t2 = probs %>% filter(t == t2) 
  p_diff = tibble(p = p_t2$p - p_t1$p, k = p_t1$k)
  p_diff$k = factor(p_diff$k, labels = category_strings[2:7])
  plot_category_prob_hists(p_diff)
}

# get_diff_plot_2_batters(5,14,probs1)

get_avg_tto_coeffs_df <- function(bat_seq_draws) {
  alpha_df = tibble()
  for (k in 1:7) {
    alphas = colMeans(bat_seq_draws[[k]])
    alpha_df = bind_rows(alpha_df,
        tibble(alpha=c(mean(alphas[1:9]), mean(alphas[10:18]), mean(alphas[19:27])), tto=1:3, k=k)
    )
  }
  alpha_df
}

get_avg_tto_p_df <- function(probs) {
  ts1 = 1:9
  ts2 = 10:18
  ts3 = 19:27
  p_tto1 = probs %>% filter(t %in% ts1) 
  p_tto2 = probs %>% filter(t %in% ts2) 
  p_tto3 = probs %>% filter(t %in% ts3) 
  
  p_avg_tto1 = numeric( nrow(p_tto1)/9 )
  p_avg_tto2 = numeric( nrow(p_tto1)/9 )
  p_avg_tto3 = numeric( nrow(p_tto1)/9 )
  for (i in 1:9) {
    t1 = ts1[i]
    t2 = ts2[i]
    t3 = ts3[i]
    p_avg_tto1 = p_avg_tto1 + (p_tto1 %>% filter(t == t1))$p
    p_avg_tto2 = p_avg_tto2 + (p_tto2 %>% filter(t == t2))$p
    p_avg_tto3 = p_avg_tto3 + (p_tto3 %>% filter(t == t3))$p
  } 
  p_avg_tto1 = tibble(p = p_avg_tto1/9, k=(p_tto1 %>% filter(t == ts1[1]))$k, tto=1)
  p_avg_tto2 = tibble(p = p_avg_tto2/9, k=(p_tto2 %>% filter(t == ts2[1]))$k, tto=2)
  p_avg_tto3 = tibble(p = p_avg_tto3/9, k=(p_tto3 %>% filter(t == ts3[1]))$k, tto=3)
  p_tib = bind_rows(p_avg_tto1, p_avg_tto2, p_avg_tto3)
  p_tib$k = factor(p_tib$k, labels = category_strings[2:7])
  p_tib
}

t_pairs = tibble(
  # t1 = c(1, 2, 3, 4, 5, 10,11,12,13,9, 18),
  # t2 = c(10,11,12,13,14,19,20,21,22,10,19)
  t1 = c(1:9,   10:18, 9, 18),
  t2 = c(10:18, 19:27, 10,19)
)

save_all_t1t2_plots <- function(probs) {
  for (i in 1:nrow(t_pairs)) {
    t1 = t_pairs[i,]$t1
    t2 = t_pairs[i,]$t2
    plot_t1t2 = get_diff_plot_2_batters(t1,t2,probs)
    ggsave(paste0("plots/prob_scale/",subfolder,"plot_pdiff_",t1,"_",t2,".png"), plot_t1t2, width=8, height=4)
  }
}

{
  save_all_t1t2_plots(probs1)
  
  atto_df = get_avg_tto_coeffs_df(bat_seq_draws)
  data.frame(atto_df)
  
  ptto_df = get_avg_tto_p_df(probs1)
  ptto_df2 = ptto_df %>% group_by(k, tto) %>% summarise(p=mean(p)) %>%
    mutate(across(where(is.numeric), ~ num(., digits = 5)))
  ptto_df2
  ptto_df2 %>% group_by(k) %>% mutate(d = c(NA,diff(p))) %>% arrange(-d)
  
  
  ptto12 = get_avg_tto_diff_plot(1, 2, probs1)
  ptto23 = get_avg_tto_diff_plot(2, 3, probs1)
  ggsave(paste0("plots/prob_scale/",subfolder,"tto_diff_12.png"), ptto12, width=8, height=4)
  ggsave(paste0("plots/prob_scale/",subfolder,"tto_diff_23.png"), ptto23, width=8, height=4)
}


##########################################
### RESULTS on the EXPECTED WOBA SCALE ###
##########################################

xWOBA_dists <- function(probs) {
  xws = tibble()
  for (t1 in 1:27) {
    pt = probs %>% filter(t == t1)
    ptk2 = (pt %>% filter(k==2))$p
    ptk3 = (pt %>% filter(k==3))$p
    ptk4 = (pt %>% filter(k==4))$p
    ptk5 = (pt %>% filter(k==5))$p
    ptk6 = (pt %>% filter(k==6))$p
    ptk7 = (pt %>% filter(k==7))$p
    xw = ptk2*categories[2] + ptk3*categories[3] + ptk4*categories[4] + 
      ptk5*categories[5] + ptk6*categories[6] + ptk7*categories[7] 
    xws = bind_rows(xws, tibble(xWOBA = xw*1000, t = t1))
  }
  xws
}

plot_xwoba_diff_hist <- function(xw_diff) {
  xw_diff %>% ggplot() +
    geom_histogram(aes(x=xWOBA, y=..density..), fill="black", bins=50) +
    geom_vline(aes(xintercept=0), color="dodgerblue2", size=1) +
    geom_vline(aes(xintercept=mean(xWOBA)), color="firebrick", size=1) +
    # scale_x_continuous(name="probability", breaks=seq(-0.03,0.03,by=0.01)) +
    scale_x_continuous(name="difference in expected wOBA points") +
    theme(panel.spacing = unit(2, "lines")) +
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank()) 
}

get_avg_tto_xwoba_diff_df <- function(tto1, tto2, xw) {
  ts1 = 1:9 + (tto1-1)*9
  ts2 = 1:9 + (tto2-1)*9
  p_tto1 = xw %>% filter(t %in% ts1) 
  p_tto2 = xw %>% filter(t %in% ts2) 
  
  p_avg_tto1 = numeric( nrow(p_tto1)/9 )
  p_avg_tto2 = numeric( nrow(p_tto1)/9 )
  for (i in 1:length(ts1)) {
    t1 = ts1[i]
    t2 = ts2[i]
    p_avg_tto1 = p_avg_tto1 + (p_tto1 %>% filter(t == t1))$xWOBA
    p_avg_tto2 = p_avg_tto2 + (p_tto2 %>% filter(t == t2))$xWOBA
  } 
  p_avg_tto1 = p_avg_tto1/9
  p_avg_tto2 = p_avg_tto2/9
  p_diff = p_avg_tto2 - p_avg_tto1
  p_diff = tibble(xWOBA = p_diff)
  
  p_diff
}

get_avg_tto_xwoba_diff_plot <- function(tto1, tto2, xw) {
  p_diff = get_avg_tto_xwoba_diff_df(tto1, tto2, xw)
  plot_xwoba_diff_hist(p_diff)
}


get_xwoba_diff_plot_2_batters <- function(t1,t2,xw) {
  xw_t1 = xw %>% filter(t == t1) 
  xw_t2 = xw %>% filter(t == t2) 
  xw_diff = tibble(xWOBA = xw_t2$xWOBA - xw_t1$xWOBA)
  plot_xwoba_diff_hist(xw_diff)
}

save_all_t1t2_xwoba_plots <- function(xw) {
  for (i in 1:nrow(t_pairs)) {
    t1 = t_pairs[i,]$t1
    t2 = t_pairs[i,]$t2
    plot_t1t2 = get_xwoba_diff_plot_2_batters(t1,t2,xw)
    ggsave(paste0("plots/xwoba_scale/",subfolder,"plot_xwoba_diff_",t1,"_",t2,".png"), plot_t1t2, width=5, height=4)
  }
}

plot_xWOBA_over_time <- function(xw) {
  xw %>% group_by(t) %>%
    summarise(xwL2 = quantile(xWOBA,.05),
              xwL1 = quantile(xWOBA,.25),
              xwU1 = quantile(xWOBA,.75),
              xwU2 = quantile(xWOBA,.95),
              xWOBA = mean(xWOBA)) %>% 
    filter(t <= 26) %>%
    ggplot(aes(x=t, y=xWOBA)) +
    geom_vline(aes(xintercept =  9), size=0.5, color="gray50") + #1.2
    geom_vline(aes(xintercept = 18), size=0.5, color="gray50") +
    geom_errorbar(aes(ymin = xwL2, ymax = xwU2), fill = "black", width = .4) +
    geom_errorbar(aes(ymin = xwL1, ymax = xwU1), fill = "black", width = .6, size=1.25) +
    geom_point(color="dodgerblue2", shape=21, size=2, fill="white") + 
    geom_line(aes(y = c(xWOBA[1:9], rep(NA,17))), color="dodgerblue2", size=0.5) +
    geom_line(aes(y = c(rep(NA,9), xWOBA[10:18], rep(NA,8))), color="dodgerblue2", size=0.5) +
    geom_line(aes(y = c(rep(NA,18), xWOBA[19:26])), color="dodgerblue2", size=0.5) +
    # geom_line(aes(y = c(xWOBA[1:9], rep(NA,18))), color="dodgerblue2", size=0.5) +
    # geom_line(aes(y = c(rep(NA,9), xWOBA[10:18], rep(NA,9))), color="dodgerblue2", size=0.5) +
    # geom_line(aes(y = c(rep(NA,18), xWOBA[19:27])), color="dodgerblue2", size=0.5) +
    # geom_vline(aes(xintercept = 9.5), size=1.2) +
    # geom_vline(aes(xintercept = 18.5), size=1.2) +
    # labs(title = "Trend in Expected wOBA over the Course of a Game") + 
    theme(legend.position="none") +
    scale_x_continuous(name=TeX("Batter Sequence Number $t$"), 
                       # limits = c(0,28),
                       limits = c(0,27), #c(0,27),
                       # breaks = c(0,5,10,15,20,25),
                       breaks = seq(3,24,by=3)) +
    scale_y_continuous(name="Expected wOBA", 
                       # limits = c(.2, .4),
                       # breaks = seq(-1, 1, .025)
    ) 
}
plot_xWOBA_over_time(xw1)

# get_xwoba_diff_plot_2_batters(1,10,xw1)

# AVG TTO XWOBA


{
  xw1 = xWOBA_dists(probs1)
  save_all_t1t2_xwoba_plots(xw1)
  
  xwtto12_df = get_avg_tto_xwoba_diff_df(1, 2, xw1)
  xwtto23_df = get_avg_tto_xwoba_diff_df(2, 3, xw1)
  
  mean(xwtto12_df$xWOBA)/1000
  quantile(xwtto12_df$xWOBA, c(.025,.975))/1000
  mean(xwtto23_df$xWOBA)/1000
  quantile(xwtto23_df$xWOBA, c(.025,.975))/1000
  
  xwtto12 = get_avg_tto_xwoba_diff_plot(1, 2, xw1)
  xwtto23 = get_avg_tto_xwoba_diff_plot(2, 3, xw1)
  ggsave(paste0("plots/xwoba_scale/",subfolder,"tto_diff_12.png"), xwtto12, width=5, height=4)
  ggsave(paste0("plots/xwoba_scale/",subfolder,"tto_diff_32.png"), xwtto23, width=5, height=4)
  
  plot_xwt = plot_xWOBA_over_time(xw1)
  ggsave(paste0("plots/xwoba_scale/",subfolder,"plot_xwoba_over_time.png"), plot_xwt, width=9.44, height=4.89)
}



##########################################
### Different X's ###
##########################################

quantiles = c(.05,0.25,0.5,0.75,0.95)
nq = length(quantiles)
PQs = quantile(pitchers_woba$WOBA, quantiles)
PQs
BQs = quantile(batters_woba$WOBA, quantiles)
BQs

XX = matrix(nrow=nq, ncol=nq)
rownames(XX) = round(BQs,3)
colnames(XX) = round(PQs,3)
for (i in 1:nq) {
  for (j in 1:nq) {
    print(c("*****", i, j, "*****"))
    x_ij = c(logit(BQs[i]/1000), logit(PQs[j]/1000), 1, 0)
    probs_ij = get_prob_tibble(x_ij, bat_seq_draws, eta_draws)
    xw_ij = xWOBA_dists(probs_ij)
    XX[i,j] = (xw_ij %>% group_by(t) %>% summarise(xWOBA = mean(xWOBA)) %>% summarise(xWOBA = mean(xWOBA)))$xWOBA
  }
}
XX

##########################################
hands = c(0,1)
homes = c(0,1)
HH = matrix(nrow=2,ncol=2)
rownames(HH) = hands
colnames(HH) = homes
for (i in 1:2) {
  for (j in 1:2) {
    print(c("*****", i, j, "*****"))
    x_ij = c(logit(median(batters_woba$WOBA)/1000), logit(median(pitchers_woba$WOBA)/1000), hands[i], homes[j])
    probs_ij = get_prob_tibble(x_ij, bat_seq_draws, eta_draws)
    xw_ij = xWOBA_dists(probs_ij)
    HH[i,j] = (xw_ij %>% group_by(t) %>% summarise(xWOBA = mean(xWOBA)) %>% summarise(xWOBA = mean(xWOBA)))$xWOBA
  }
}
HH

###########################################


prob_df_forPlot = probs1 %>%
  group_by(k,t) %>%
  summarise(
    p_L = quantile(p, 0.05),
    p_M = quantile(p, 0.5),
    p_U = quantile(p, 0.95),
    .groups = "drop"
  ) %>% mutate(label = case_when(
    k == 2 ~ "uBB",
    k == 3 ~ "HBP",
    k == 4 ~ "1B",
    k == 5 ~ "2B",
    k == 6 ~ "3B",
    k == 7 ~ "HR"
  )) 

plot_probScale = prob_df_forPlot %>%
  # mutate(label = fct_reorder(label,
  #                            c("1B", "2B", "HR", "uBB", "HBP", "3B")
  #                            )) %>%
  ggplot(aes(x=t)) +
  facet_wrap(~label, nrow=3, scales = "free") +
  geom_errorbar(aes(ymin=p_L, ymax=p_U)) +
  geom_point(aes(y=p_M), color="dodgerblue2", fill="white", shape=4) +
  scale_x_continuous(name=TeX("Batter Sequence Number $t$"), 
                     limits = c(0,26.5), 
                     breaks = seq(3,24,by=3)) +
  ylab("Probability")
plot_probScale
ggsave(paste0("plots/prob_scale/",subfolder,"plot_prob_over_time.png"), plot_probScale, width=9.44, height=4.89)




