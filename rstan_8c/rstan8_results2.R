library(tidyverse)
library(grid)
library(splines)
library(ggridges)
library(latex2exp)
theme_set(theme_bw())
theme_update(text = element_text(size=18))
theme_update(plot.title = element_text(hjust = 0.5))
output_folder = './job_output/'

#########################
### HELPER FUNCTIONS ####
#########################

get_bat_seq_draws <- function(draws) {
  alpha_draws = draws[,str_detect(colnames(draws), "^alpha")]
  bat_seq_draws = list()
  for (k in 1:7) {
    alpha_draws_k = alpha_draws[,endsWith(colnames(alpha_draws), paste0(k,"]"))]
    # bat_seq_draws_k = alpha_draws_k %*% t(bbb)
    bat_seq_draws_k = alpha_draws_k ##%*% t(bbb)
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




###############
### RESULTS ###
###############

### load data
input_file = "../data/TTO_dataset_510.csv"  
D0 <- read_csv(input_file) #%>% drop_na() 

probs_allyrs = tibble()
xw_allyrs = tibble()
for (year in 2010:2019) {
  print(paste0("*** ", year, " ***"))
  ### posterior samples of model
  # year = 2019 #2018 # 2020
  fit <- readRDS(paste0("job_output/fit_rstan8-",year-2000,".R.rds"))
  draws <- as.matrix(fit)
  
  ### load data
  D <- D0 %>% filter(YEAR == year) %>% filter(BQ>0 & BQ<1 & PQ>0 & PQ<1 ) %>% filter(ORDER_CT <= 3)
  logit <- function(p) { log(p/(1-p)) }
  X <- as.matrix(D %>% mutate(lBQ=logit(BQ), lPQ=logit(PQ)) %>% select(lBQ, lPQ, HAND_MATCH, BAT_HOME_IND)) 
  source("rstan8_main.R")
  
  
  D0_2018 = D0 %>% filter(YEAR == year) %>% filter(ORDER_CT <= 3)
  
  # number of plate appearances
  nrow(D0_2018)
  # number of batters
  length(unique(D0_2018$BAT_ID))
  # number of pitchers
  length(unique(D0_2018$PIT_ID))
  
  batters_woba = D %>% group_by(BAT_ID) %>% 
    summarise(WOBA = unique(WOBA_FINAL_BAT_19)*1000, num_pa=n()) %>%
    filter(num_pa >= 100)
  batters_woba
  pitchers_woba = D %>% group_by(PIT_ID) %>% 
    summarise(WOBA = unique(WOBA_FINAL_PIT_19)*1000, num_pa=n()) %>%
    filter(num_pa >= 100)
  pitchers_woba
  
  
  
  ### batter sequence too draws 1,...,27
  bat_seq_draws = get_bat_seq_draws(draws) #bat_seq_draws[[7]][1:10,1:10]
  eta_draws = get_eta_draws(fit)
  
  x1 = c(logit(median(batters_woba$WOBA)/1000), logit(median(pitchers_woba$WOBA)/1000), 1, 0); subfolder = "x1/";
  probs1 = get_prob_tibble(x1, bat_seq_draws, eta_draws)
  probs1$year = year
  
  probs_allyrs = bind_rows(probs_allyrs, probs1)
  
  
  xw1 = xWOBA_dists(probs1)
  xw1$year = year
  xw_allyrs = bind_rows(xw_allyrs, xw1)
}

### plot xwoba over time
xwoba_over_time_all_years = xw_allyrs %>% 
  group_by(t, year) %>%
  summarise(xwL2 = quantile(xWOBA,.05),
            xwL1 = quantile(xWOBA,.25),
            xwU1 = quantile(xWOBA,.75),
            xwU2 = quantile(xWOBA,.95),
            xWOBA = mean(xWOBA)) %>% 
  filter(t <= 26) %>%
  filter(year >= 2012) %>%
  ggplot(aes()) +
  facet_wrap(~year, scales="free", nrow=4) +
  geom_vline(aes(xintercept =  9), size=0.5, color="gray50") + #1.2
  geom_vline(aes(xintercept = 18), size=0.5, color="gray50") +
  geom_errorbar(aes(y=xWOBA, ymin = xwL2, ymax = xwU2), fill = "black", width = .4) +
  geom_errorbar(aes(y=xWOBA, ymin = xwL1, ymax = xwU1), fill = "black", width = .6, size=1.25) +
  geom_point(color="dodgerblue2", shape=21, size=2, fill="white") + 
  # geom_line(aes(y = c(xWOBA[1:9], rep(NA,17))), color="dodgerblue2", size=0.5) +
  # geom_line(aes(y = c(rep(NA,9), xWOBA[10:18], rep(NA,8))), color="dodgerblue2", size=0.5) +
  # geom_line(aes(y = c(rep(NA,18), xWOBA[19:26])), color="dodgerblue2", size=0.5) +
  theme(legend.position="none") +
  scale_x_continuous(name=TeX("Batter Sequence Number $t$"), 
                     # limits = c(0,28),
                     limits = c(0,27), #c(0,27),
                     # breaks = c(0,5,10,15,20,25),
                     breaks = seq(3,24,by=3)) +
  scale_y_continuous(name="Expected wOBA", 
                     breaks=seq(0,1000,by=20)
                     # limits = c(.2, .4),
                     # breaks = seq(-1, 1, .025)
  ) 
xwoba_over_time_all_years
ggsave("plot_xwoba_over_time_all_years.png",
       xwoba_over_time_all_years, width=11, height=11)



####################################################
plot_xWOBA_over_time <- function(xw) {
  xw %>% group_by(t) %>%
      summarise(xwL2 = quantile(xWOBA,.05),
                xwL1 = quantile(xWOBA,.25),
                xwU1 = quantile(xWOBA,.75),
                xwU2 = quantile(xWOBA,.95),
                xWOBA = mean(xWOBA),
      ) %>%
    filter(t <= 26) %>%
    ggplot() +
    geom_vline(aes(xintercept =  9), size=0.5, color="gray50") + #1.2
    geom_vline(aes(xintercept = 18), size=0.5, color="gray50") +
    geom_errorbar(aes(x=t, y=xWOBA, ymin = xwL2, ymax = xwU2), fill = "black", width = .4) +
    geom_errorbar(aes(x=t, y=xWOBA, ymin = xwL1, ymax = xwU1), fill = "black", width = .6, size=1.25) +
    geom_point(aes(x=t, y=xWOBA), color="dodgerblue2", shape=21, size=2, fill="white") +
    geom_line(aes(x=t, y = c(xWOBA[1:9], rep(NA,17))), color="dodgerblue2", size=0.5) +
    geom_line(aes(x=t, y = c(rep(NA,9), xWOBA[10:18], rep(NA,8))), color="dodgerblue2", size=0.5) +
    geom_line(aes(x=t, y = c(rep(NA,18), xWOBA[19:26])), color="dodgerblue2", size=0.5) +
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
                       breaks=seq(0,1000,by=10)
                       # limits = c(.2, .4),
                       # breaks = seq(-1, 1, .025)
    ) 
  
}

#########################################################
plot_xWOBA_over_time_smooth <- function(xws, v="1", allyrs=FALSE) {
  if (v == "0") {
    # do nothing
    xws_ = xws %>%
      mutate(xWOBA0 = xWOBA)
  }
  if (v == "1") {
    xws_ = xws %>% 
      select(-c(xwL2, xwL1, xWOBA, xwU1, xwU2)) %>%
      rename(
        xwL2 = xwL2_s1,
        xwL1 = xwL1_s1,
        xWOBA = xWOBA_s1,
        xwU1 = xwU1_s1,
        xwU2 = xwU2_s1,
      ) %>% mutate(
        xWOBA0 = xWOBA
      )
  } else if (v == "1b") {
    xws_ = xws %>% 
      rename(
        xWOBA0 = xWOBA,
        xWOBA = xWOBA_s1
      ) 
  } else if (v == "2") {
    xws_ = xws %>% 
      select(-c(xwL2, xwL1, xWOBA, xwU1, xwU2)) %>% 
      rename(
        xwL2 = xwL2_s2,
        xwL1 = xwL1_s2,
        xWOBA = xWOBA_s2,
        xwU1 = xwU1_s2,
        xwU2 = xwU2_s2,
      ) %>% mutate(
        xWOBA0 = xWOBA
      )
  }  else if (v == "2b") {
    xws_ = xws %>% 
      rename(
        xWOBA0 = xWOBA,
        xWOBA = xWOBA_s2,
      ) 
  }
  p = xws_ %>%
    filter(t <= 26) %>%
    ggplot() 
  
  if (allyrs) {
    p = p +
      facet_wrap(~year, ncol=2, scales="free") 
  }
  
  p = p +
    geom_vline(aes(xintercept =  9), size=0.5, color="gray50") + #1.2
    geom_vline(aes(xintercept = 18), size=0.5, color="gray50") +
    geom_errorbar(aes(x=t, ymin = xwL2, ymax = xwU2), fill = "black", width = .4) +
    geom_errorbar(aes(x=t, ymin = xwL1, ymax = xwU1), fill = "black", width = .6, size=1.25) +
    geom_point(aes(x=t, y=xWOBA0), color="dodgerblue2", shape=21, size=2, fill="white") 
  
  if (v=="2" | v=="2b") {
    
    if (!allyrs) {
      p = p +
        geom_line(aes(x=t, y = c(xWOBA[1:9], rep(NA,17))), color="dodgerblue2", size=0.5) +
        geom_line(aes(x=t, y = c(rep(NA,9), xWOBA[10:18], rep(NA,8))), color="dodgerblue2", size=0.5) +
        geom_line(aes(x=t, y = c(rep(NA,18), xWOBA[19:26])), color="dodgerblue2", size=0.5) 
    } else {
      # ???
      # p = p +
      #   geom_line(aes(x=t, y = xWOBA), color="dodgerblue2", size=0.5)
      
      p = p +
        geom_line(  
                  aes(x=t, y=xWOBA), color="dodgerblue2", size=0.5)
      
    }
  } else if (v=="1" | v=="1b") {
    p = p +
      geom_line(aes(x=t, y = xWOBA), color="dodgerblue2", size=0.5)
  }
  p = p +
    theme(legend.position="none") +
    scale_x_continuous(name=TeX("Batter Sequence Number $t$"), 
                       # limits = c(0,28),
                       limits = c(0,27), #c(0,27),
                       # breaks = c(0,5,10,15,20,25),
                       breaks = seq(3,24,by=3)) 
  
  if (!allyrs) {
    p = p + 
      scale_y_continuous(name="Expected wOBA", 
                       breaks=seq(0,1000,by=10)
                       # limits = c(.2, .4),
                       # breaks = seq(-1, 1, .025)
    ) 
  } else {
    p = p + 
    scale_y_continuous(name="Expected wOBA", 
                       breaks=seq(0,1000,by=20)
                       # limits = c(.2, .4),
                       # breaks = seq(-1, 1, .025)
    ) 
  }
    
  p
  
}
plot_xwta_yr_2 = plot_xWOBA_over_time_smooth(xw_allyrs_smoothedMeans, v="2", allyrs=TRUE)


# xw_2018 = xw_allyrs %>% filter(year == 2018)
# xwoba_over_time_2018 = plot_xWOBA_over_time(xw_2018)
# xwoba_over_time_2018
# ggsave("plot_xwoba_over_time_2018.png",
#        xwoba_over_time_2018, width=9, height=5)



####################################################
s_d1 = 7
s_d2 = 4

for (yr in unique(xw_allyrs$year)) {
  print(yr)
  xw_yr = xw_allyrs %>% filter(year == yr)
  
  {
    xw_yr_smoothedMeans =
      xw_yr %>%
      group_by(t) %>%
      summarise(
        xwL2 = quantile(xWOBA,.05),
        xwL1 = quantile(xWOBA,.25),
        xwU1 = quantile(xWOBA,.75),
        xwU2 = quantile(xWOBA,.95),
        xWOBA = mean(xWOBA),
      ) %>%
      mutate(
        xwL2_s1 = smooth.spline(1:27, xwL2, df=s_d1)$y,
        xwL1_s1 = smooth.spline(1:27, xwL1, df=s_d1)$y,
        xWOBA_s1 = smooth.spline(1:27, xWOBA, df=s_d1)$y,
        xwU1_s1 = smooth.spline(1:27, xwU1, df=s_d1)$y,
        xwU2_s1 = smooth.spline(1:27, xwU2, df=s_d1)$y,
      ) %>%
      mutate(
        xwL2_s2 = 
          ifelse(1 <= t & t <= 9,   smooth.spline(1:9,   xwL2[1:9], df=s_d2)$y,
                 ifelse(10 <= t & t <= 18, smooth.spline(10:18, xwL2[10:18], df=s_d2)$y,
                        smooth.spline(19:27, xwL2[19:27], df=s_d2)$y)),
        xwL1_s2 = 
          ifelse(1 <= t & t <= 9,   smooth.spline(1:9,   xwL1[1:9], df=s_d2)$y,
                 ifelse(10 <= t & t <= 18, smooth.spline(10:18, xwL1[10:18], df=s_d2)$y,
                        smooth.spline(19:27, xwL1[19:27], df=s_d2)$y)),
        xWOBA_s2 = 
          ifelse(1 <= t & t <= 9,   smooth.spline(1:9,   xWOBA[1:9], df=s_d2)$y,
                 ifelse(10 <= t & t <= 18, smooth.spline(10:18, xWOBA[10:18], df=s_d2)$y,
                        smooth.spline(19:27, xWOBA[19:27], df=s_d2)$y)),
        xwU1_s2 = 
          ifelse(1 <= t & t <= 9,   smooth.spline(1:9,   xwU1[1:9], df=s_d2)$y,
                 ifelse(10 <= t & t <= 18, smooth.spline(10:18, xwU1[10:18], df=s_d2)$y,
                        smooth.spline(19:27, xwU1[19:27], df=s_d2)$y)),
        xwU2_s2 = 
          ifelse(1 <= t & t <= 9,   smooth.spline(1:9,   xwU2[1:9], df=s_d2)$y,
                 ifelse(10 <= t & t <= 18, smooth.spline(10:18, xwU2[10:18], df=s_d2)$y,
                        smooth.spline(19:27, xwU2[19:27], df=s_d2)$y))
      )
  }
  
  plot_xwt_yr_0 = plot_xWOBA_over_time_smooth(xw_yr_smoothedMeans, v="0")
  plot_xwt_yr_1 = plot_xWOBA_over_time_smooth(xw_yr_smoothedMeans, v="1")
  plot_xwt_yr_1b = plot_xWOBA_over_time_smooth(xw_yr_smoothedMeans, v="1b")
  plot_xwt_yr_2 = plot_xWOBA_over_time_smooth(xw_yr_smoothedMeans, v="2")
  plot_xwt_yr_2b = plot_xWOBA_over_time_smooth(xw_yr_smoothedMeans, v="2b")
  
  # ggsave(paste0("plots/plot_xwt_", "0", "_", yr, ".png"), plot_xwt_yr_0, width=9, height=5)
  # ggsave(paste0("plots/plot_xwt_", "1", "_", yr, ".png"), plot_xwt_yr_1, width=9, height=5)
  # ggsave(paste0("plots/plot_xwt_", "1b", "_", yr, ".png"), plot_xwt_yr_1b, width=9, height=5)
  # ggsave(paste0("plots/plot_xwt_", "2", "_", yr, ".png"), plot_xwt_yr_2, width=9, height=5)
  # ggsave(paste0("plots/plot_xwt_", "2b", "_", yr, ".png"), plot_xwt_yr_2b, width=9, height=5)
}

####################################################
s_d1 = 7
s_d2 = 4

{
xw_allyrs_smoothedMeans =
  xw_allyrs %>%
  filter(2012 <= year & year <= 2019) %>%
  group_by(year, t) %>%
  summarise(
    xwL2 = quantile(xWOBA,.05),
    xwL1 = quantile(xWOBA,.25),
    xwU1 = quantile(xWOBA,.75),
    xwU2 = quantile(xWOBA,.95),
    xWOBA = mean(xWOBA),
  ) %>%
  mutate(
    xwL2_s1 = smooth.spline(1:27, xwL2, df=s_d1)$y,
    xwL1_s1 = smooth.spline(1:27, xwL1, df=s_d1)$y,
    xWOBA_s1 = smooth.spline(1:27, xWOBA, df=s_d1)$y,
    xwU1_s1 = smooth.spline(1:27, xwU1, df=s_d1)$y,
    xwU2_s1 = smooth.spline(1:27, xwU2, df=s_d1)$y,
  ) %>%
  mutate(
    xwL2_s2 = 
      ifelse(1 <= t & t <= 9,   smooth.spline(1:9,   xwL2[1:9], df=s_d2)$y,
             ifelse(10 <= t & t <= 18, smooth.spline(10:18, xwL2[10:18], df=s_d2)$y,
                    smooth.spline(19:27, xwL2[19:27], df=s_d2)$y)),
    xwL1_s2 = 
      ifelse(1 <= t & t <= 9,   smooth.spline(1:9,   xwL1[1:9], df=s_d2)$y,
             ifelse(10 <= t & t <= 18, smooth.spline(10:18, xwL1[10:18], df=s_d2)$y,
                    smooth.spline(19:27, xwL1[19:27], df=s_d2)$y)),
    xWOBA_s2 = 
      ifelse(1 <= t & t <= 9,   smooth.spline(1:9,   xWOBA[1:9], df=s_d2)$y,
             ifelse(10 <= t & t <= 18, smooth.spline(10:18, xWOBA[10:18], df=s_d2)$y,
                    smooth.spline(19:27, xWOBA[19:27], df=s_d2)$y)),
    xwU1_s2 = 
      ifelse(1 <= t & t <= 9,   smooth.spline(1:9,   xwU1[1:9], df=s_d2)$y,
             ifelse(10 <= t & t <= 18, smooth.spline(10:18, xwU1[10:18], df=s_d2)$y,
                    smooth.spline(19:27, xwU1[19:27], df=s_d2)$y)),
    xwU2_s2 = 
      ifelse(1 <= t & t <= 9,   smooth.spline(1:9,   xwU2[1:9], df=s_d2)$y,
             ifelse(10 <= t & t <= 18, smooth.spline(10:18, xwU2[10:18], df=s_d2)$y,
                    smooth.spline(19:27, xwU2[19:27], df=s_d2)$y))
  )
}


plot_xwta_yr_0 = plot_xWOBA_over_time_smooth(xw_allyrs_smoothedMeans, v="0", allyrs=TRUE)
plot_xwta_yr_1 = plot_xWOBA_over_time_smooth(xw_allyrs_smoothedMeans, v="1", allyrs=TRUE)
plot_xwta_yr_1b = plot_xWOBA_over_time_smooth(xw_allyrs_smoothedMeans, v="1b", allyrs=TRUE)
plot_xwta_yr_2 = plot_xWOBA_over_time_smooth(xw_allyrs_smoothedMeans, v="2", allyrs=TRUE)
plot_xwta_yr_2b = plot_xWOBA_over_time_smooth(xw_allyrs_smoothedMeans, v="2b", allyrs=TRUE)

w1 = 12
h1 = 10
ggsave(paste0("plots/plot_xwta_", "0", "_", ".png"), plot_xwta_yr_0, width=w1, height=h1)
ggsave(paste0("plots/plot_xwta_", "1", "_", ".png"), plot_xwta_yr_1, width=w1, height=h1)
ggsave(paste0("plots/plot_xwta_", "1b", "_", ".png"), plot_xwta_yr_1b, width=w1, height=h1)
ggsave(paste0("plots/plot_xwta_", "2", "_", ".png"), plot_xwta_yr_2, width=w1, height=h1)
ggsave(paste0("plots/plot_xwta_", "2b", "_", ".png"), plot_xwta_yr_2b, width=w1, height=h1)





# ###########################################

prob_df_forPlot = probs_allyrs %>%
  filter(year==2018) %>%
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

plot_probScale_2018 = prob_df_forPlot %>%
  # mutate(label = fct_reorder(label,
  #                            c("1B", "2B", "HR", "uBB", "HBP", "3B")
  #                            )) %>%
  ggplot(aes(x=t)) +
  facet_wrap(~label, nrow=3, scales = "free") +
  geom_errorbar(aes(ymin=p_L, ymax=p_U)) +
  geom_point(aes(y=p_M), color="dodgerblue2", fill="white", shape=4) +
  # geom_line(aes(x=t, y = c(p_M[1:9], rep(NA,17))), color="dodgerblue2", size=0.5) +
  # geom_line(aes(x=t, y = c(rep(NA,9), p_M[10:18], rep(NA,8))), color="dodgerblue2", size=0.5) +
  # geom_line(aes(x=t, y = c(rep(NA,18), p_M[19:26])), color="dodgerblue2", size=0.5) +
  # geom_line(aes(x=t, y = p_M), color="dodgerblue2", size=0.5) +
  scale_x_continuous(name=TeX("Batter Sequence Number $t$"),
                     limits = c(0,26.5),
                     breaks = seq(3,24,by=3)) +
  
  ylab("Probability")
plot_probScale_2018
ggsave("plot_probScale_2018.png",
       plot_probScale_2018, width=11, height=9)



