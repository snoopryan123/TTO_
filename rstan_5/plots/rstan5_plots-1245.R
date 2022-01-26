library(tidyverse)
library(cowplot)
library(grid)
 
OUTPUT_FILE = "rstan5_plots-1245.R"
### load data
input_file = "../../data/TTO_dataset_510.csv"  
D <- read_csv(input_file)
#D <- D %>% drop_na() 
D <- D %>% filter(YEAR == 2019) 
#### X <- as.matrix(D %>% select(std_BQ, std_PQ, HAND_MATCH, BAT_HOME_IND)) 


fit1 <- readRDS("../job_output/fit_rstan5-1.R.rds") 
fit2 <- readRDS("../job_output/fit_rstan5-2.R.rds") 
fit4 <- readRDS("../job_output/fit_rstan5-4.R.rds") 
fit5 <- readRDS("../job_output/fit_rstan5-5.R.rds") 
source("rstan5_plots_main.R")

#############################
########### PLOTS ###########
#############################

# for each of the 4 models,  plot 3 panel histogram B1->B2, B9->B10, B18->B19
{
  d1 = getDf_3hist_bsn1(fit1)
  d2 = getDf_3hist_bsn1(fit2)
  d4 = getDf_3hist_ubi1(fit4)
  d5 = getDf_3hist_ubi1(fit5)
  d1$model = "model (A)"
  d2$model = "model (C)"
  d4$model = "model (B)"
  d5$model = "model (D)"
  dh = bind_rows(d1,d2,d4,d5)
  # labs <- c(bquote(paste("posterior dist. of ", alpha[2] - alpha[1])), 
  #           bquote(paste("posterior dist. of ", alpha[10] - alpha[9])),
  #           bquote(paste("posterior dist. of ", alpha[19] - alpha[18])))
  labs <- ""
  ph = dh %>% 
    ggplot() + 
    facet_wrap(~ model, ncol=2) +
    geom_density(aes(value, fill = name), alpha = 0.2) +
    #scale_fill_discrete(labels=labs, name = "") +
    theme(legend.position = "none") +
    theme(#axis.title.y=element_blank(),
      axis.text.y=element_blank(),
      axis.ticks.y=element_blank()) +
    theme(panel.spacing = unit(2, "lines")) +
    ylab("posterior density") +
    xlab("change in wOBA") +
    labs(title="Comparing the TTO penalties to the difference between the second and first batters")
  ph
  ggsave(paste0("./plot_4_3hists_",OUTPUT_FILE,".png"), ph)
}

# for each of the 4 models,  plot 3 panel histogram avg TTO1, TTO2, TTO3
{
  d1 = getDf_3hist_bsn2(fit1)
  d2 = getDf_3hist_bsn2(fit2)
  d4 = getDf_3hist_ubi2(fit4)
  d5 = getDf_3hist_ubi2(fit5)
  d1$model = "model (A)"
  d2$model = "model (C)"
  d4$model = "model (B)"
  d5$model = "model (D)"
  tto_means = bind_rows(d1,d2,d4,d5)

  ph2 = tto_means %>% 
    ggplot(aes(tto_means, fill = tto)) + 
    facet_wrap(~ model, ncol=2) +
    geom_density(alpha = 0.2) +
    #scale_fill_discrete(labels=labs, name = "") +
    theme(legend.position = "none") +
    theme(#axis.title.y=element_blank(),
      axis.text.y=element_blank(),
      axis.ticks.y=element_blank()) +
    theme(panel.spacing = unit(2, "lines")) +
    xlab("change in wOBA") +
    ylab("posterior density") +
    labs(title = "Pitcher performance averaged over each TTO, after adjustments")
  #labs(title=bquote(paste("Average ", alpha,  " posterior dists. in each TTO")))
  ph2
  ggsave(paste0("./plot_4_3hists2_",OUTPUT_FILE,".png"), ph2)
}

# for each of the 4 models, plot the 1-27 wOBA trend over the course of a game
{
  d1 = getDf_bsn0(fit1)
  d2 = getDf_bsn0(fit2)
  d4 = getDf_ubi0(fit4)
  d5 = getDf_ubi0(fit5)
  d1$model = "model (A)"
  d2$model = "model (C)"
  d4$model = "model (B)"
  d5$model = "model (D)"

  BREAKS = c(1,5,9,14,18,23,27)
  
  ps1 =  bind_rows(d1,d2) %>% #bind_rows(d1,d2,d4,d5) %>% 
    ggplot(aes(x=bn, y=avg)) +
    facet_wrap(~ model, ncol=2) +
    geom_errorbar(aes(ymin = lower, ymax = upper), fill = "black", width = .4) +
    geom_point(color="dodgerblue2", shape=21, size=2, fill="white") + 
    # geom_line(aes(y = c(avg[1:9], rep(NA,18))), color="firebrick", size=1) +
    # geom_line(aes(y = c(rep(NA,9), avg[10:18], rep(NA,9))), color="firebrick", size=1) +
    # geom_line(aes(y = c(rep(NA,18), avg[19:27])), color="firebrick", size=1) +
    geom_vline(aes(xintercept = 9.5), size=1.2) +
    geom_vline(aes(xintercept = 18.5), size=1.2) +
    theme(legend.position="none") +
    scale_x_continuous(name=TeX("Batter sequence number $k$"), 
                       limits = c(0,28),
                       breaks = BREAKS) +
    labs(title = TeX("Trend in wOBA over the course of a game")) + 
    scale_y_continuous(name=TeX("95% credible interval for $\\alpha_k$"), 
                       limits = c(-.05, .06),
                       breaks = seq(-.1, .1, .01)
    ) 
  ps1
  
  XLABS = c("", paste0("(",1,",",1:9,")"), paste0("(",2,",",1:9,")"), paste0("(",3,",",1:9,")"))
  ps2 = bind_rows(d4,d5) %>% 
    ggplot(aes(x=bn, y=avg)) +
    facet_wrap(~ model, ncol=2) +
    geom_errorbar(aes(ymin = lower, ymax = upper), fill = "black", width = .4) +
    geom_point(color="dodgerblue2", shape=21, size=2, fill="white") + 
    # geom_line(aes(y = c(avg[1:9], rep(NA,18))), color="firebrick", size=1) +
    # geom_line(aes(y = c(rep(NA,9), avg[10:18], rep(NA,9))), color="firebrick", size=1) +
    # geom_line(aes(y = c(rep(NA,18), avg[19:27])), color="firebrick", size=1) +
    geom_vline(aes(xintercept = 9.5), size=1.2) +
    geom_vline(aes(xintercept = 18.5), size=1.2) +
    #labs(title = TeX("Trend in wOBA over the course of a game")) + 
    theme(legend.position="none") +
    scale_x_continuous(name=TeX("(order Count $l$, unique batter index $m$)"), 
                       limits = c(0,28),
                       breaks = BREAKS,
                       labels =  XLABS[BREAKS+1]) +
    scale_y_continuous(name=TeX("95% credible interval for $\\beta_{m} + \\gamma_{l}$"), 
                       limits = c(-.05, .06),
                       breaks = seq(-.1, .1, .01)
    ) 
  ps2
  
  pss = plot_grid(ps1,ps2, ncol=1); 
  pss
  ggsave(paste0("./plot_4_series_2019_",OUTPUT_FILE,".png"), pss)
}


# # plot spline trajectory 
# p3 = plot_bsn_spline(fit)
# p3
# ggsave(paste0("./plot_spline_",OUTPUT_FILE,".png"), p)





