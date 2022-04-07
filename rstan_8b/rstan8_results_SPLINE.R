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
year = 2018
fit <- readRDS(paste0("job_output/fit_rstan8-",year-2000,".R.rds"))
draws <- as.matrix(fit)

### load data
input_file = "../data/TTO_dataset_510.csv"  
D <- read_csv(input_file) #%>% drop_na() 
D <- D %>% filter(YEAR == year) %>% filter(BQ>0 & BQ<1 & PQ>0 & PQ<1 ) %>% filter(ORDER_CT <= 3)
logit <- function(p) { log(p/(1-p)) }
X <- as.matrix(D %>% mutate(lBQ=logit(BQ), lPQ=logit(PQ)) %>% select(lBQ, lPQ, HAND_MATCH, BAT_HOME_IND)) 
source("rstan8_main.R")

##########################
### understanding wOBA ###
##########################

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

### confounder vector examples
mean(D$BQ)
mean(D$PQ)
quantile(D$BQ,.95)
quantile(D$PQ,.05)
quantile(D$BQ,.05)
quantile(D$PQ,.95)
sd(D$EVENT_WOBA_19)


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


get_avg_tto_diff_plot <- function(tto1, tto2, probs) {
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
  
  ptto12 = get_avg_tto_diff_plot(1, 2, probs1)
  ptto23 = get_avg_tto_diff_plot(2, 3, probs1)
  ggsave(paste0("plots/prob_scale/",subfolder,"tto_diff_12.png"), ptto12, width=8, height=4)
  ggsave(paste0("plots/prob_scale/",subfolder,"tto_diff_32.png"), ptto23, width=8, height=4)
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

get_avg_tto_xwoba_diff_plot <- function(tto1, tto2, xw) {
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
    geom_errorbar(aes(ymin = xwL2, ymax = xwU2), fill = "black", width = .4) +
    geom_errorbar(aes(ymin = xwL1, ymax = xwU1), fill = "black", width = .6, size=1.25) +
    geom_point(color="dodgerblue2", shape=21, size=2, fill="white") + 
    geom_line(aes(y = c(xWOBA[1:9], rep(NA,17))), color="dodgerblue2", size=0.5) +
    geom_line(aes(y = c(rep(NA,9), xWOBA[10:18], rep(NA,8))), color="dodgerblue2", size=0.5) +
    geom_line(aes(y = c(rep(NA,18), xWOBA[19:26])), color="dodgerblue2", size=0.5) +
    # geom_line(aes(y = c(xWOBA[1:9], rep(NA,18))), color="dodgerblue2", size=0.5) +
    # geom_line(aes(y = c(rep(NA,9), xWOBA[10:18], rep(NA,9))), color="dodgerblue2", size=0.5) +
    # geom_line(aes(y = c(rep(NA,18), xWOBA[19:27])), color="dodgerblue2", size=0.5) +
    geom_vline(aes(xintercept = 9.5), size=1.2) +
    geom_vline(aes(xintercept = 18.5), size=1.2) +
    # labs(title = "Trend in Expected wOBA over the Course of a Game") + 
    theme(legend.position="none") +
    scale_x_continuous(name=TeX("Batter Sequence Number $t$"), 
                       # limits = c(0,28),
                       limits = c(0,27),
                       breaks = c(0,5,10,15,20,25)) +
    scale_y_continuous(name="Expected wOBA", 
                       # limits = c(.2, .4),
                       # breaks = seq(-1, 1, .025)
    ) 
}

# get_xwoba_diff_plot_2_batters(1,10,xw1)

# AVG TTO XWOBA


{
  xw1 = xWOBA_dists(probs1)
  save_all_t1t2_xwoba_plots(xw1)
  
  xwtto12 = get_avg_tto_xwoba_diff_plot(1, 2, xw1)
  xwtto23 = get_avg_tto_xwoba_diff_plot(2, 3, xw1)
  ggsave(paste0("plots/xwoba_scale/",subfolder,"tto_diff_12.png"), xwtto12, width=5, height=4)
  ggsave(paste0("plots/xwoba_scale/",subfolder,"tto_diff_32.png"), xwtto23, width=5, height=4)
  
  plot_xwt = plot_xWOBA_over_time(xw1)
  ggsave(paste0("plots/xwoba_scale/",subfolder,"plot_xwoba_over_time.png"), plot_xwt, width=9.44, height=4.89)
}










