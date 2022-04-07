library(tidyverse)
library(grid)
library(splines)
library(ggridges)
library(latex2exp)
theme_set(theme_bw())
theme_update(text = element_text(size=18))
theme_update(plot.title = element_text(hjust = 0.5))
output_folder = './job_output/'

### load data
input_file = "./../data/TTO_dataset_510.csv"  
D <- read_csv(input_file) %>% filter(!PIT_IS_BAT) # %>% drop_na() 
D <- D %>% filter(YEAR == 2019) %>% filter(BQ>0 & BQ<1 & PQ>0 & PQ<1)
D <- D %>% filter(ORDER_CT <= 3) # keep only 1TTO, 2TTO, 3TTO
logit <- function(p) { log(p/(1-p)) }
X <- as.matrix(D %>% mutate(lBQ=logit(BQ), lPQ=logit(PQ)) %>% select(lBQ, lPQ, HAND_MATCH, BAT_HOME_IND))
source("rstan8_main.R")

#########################
### HELPER FUNCTIONS ####
#########################

get_bat_seq_draws <- function(draws) {
  beta_draws = draws[,str_detect(colnames(draws), "^beta")]
  gamma_draws = draws[,str_detect(colnames(draws), "^gamma")]
  eta_draws = draws[,str_detect(colnames(draws), "^eta")]
  bat_seq_draws = list()
  for (k in 1:7) {
    beta_draws_k = beta_draws[,endsWith(colnames(beta_draws), paste0(k,"]"))]
    bk = beta_draws_k %*% t(bbb2) 
    gamma_draws_k = gamma_draws[,endsWith(colnames(gamma_draws), paste0(k,"]"))]
    gk = cbind(matrix(gamma_draws_k[,1], nrow(gamma_draws_k), 9),
               matrix(gamma_draws_k[,2], nrow(gamma_draws_k), 9),
               matrix(gamma_draws_k[,3], nrow(gamma_draws_k), 9))
    bgk = bk + gk
    bat_seq_draws[[length(bat_seq_draws)+1]] = bgk
  }
  bat_seq_draws
}

cws_fit_to_posterior_probs <- function(S_cws_test,O_test,X_test,fit) {
  draws=as.matrix(fit)
  beta_draws = draws[,str_detect(colnames(draws), "^beta")]
  gamma_draws = draws[,str_detect(colnames(draws), "^gamma")]
  eta_draws = draws[,str_detect(colnames(draws), "^eta")]
  
  linpreds = list()
  for (k in 1:7) {
    print(k)
    beta_draws_k = beta_draws[,endsWith(colnames(beta_draws), paste0(k,"]"))]
    gamma_draws_k = gamma_draws[,endsWith(colnames(gamma_draws), paste0(k,"]"))]
    eta_draws_k = eta_draws[,endsWith(colnames(eta_draws), paste0(k,"]"))]
    linpred_k = S_cws_test%*%t(beta_draws_k) + O_test%*%t(gamma_draws_k) + X_test%*%t(eta_draws_k)
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

###########################################################################

spline_xWoba_post <- function() {
  S_cws1 = bbb2
  O1 = matrix(0, nrow=27, ncol=3)
  for (i in 1:9) {O1[i,1] = 1}
  for (i in 10:18) {O1[i,2] = 1}
  for (i in 19:27) {O1[i,3] = 1}
  X1 = matrix( c(logit(mean(D$BQ)),logit(mean(D$PQ)),1,1), nrow=27, ncol=4, byrow=TRUE)
  probs1 = cws_fit_to_posterior_probs(S_cws1,O1,X1,fit)
  # probs1[[1]][1:10,1:10]
  xw = matrix(0, ncol = dim(probs1[[1]])[1], nrow = dim(probs1[[1]])[2])
  for (k in 1:7) {
    xw = xw + t(probs1[[k]]) * categories[k]
  }
  xw
}

get_tto_means_and_ci <- function(xw) {
  # compute mean and 2.5%, 97.5% quantiles of posterior samples
  p = dim(xw)[2] #27 
  lower <- numeric(p)
  avg <- numeric(p)
  upper <- numeric(p)
  for (i in 1:p) {
    x = xw[,i]
    lower[i] = quantile(x,.025)
    avg[i] = mean(x)
    upper[i] = quantile(x,.975)
  }
  A = (data.frame(lower = lower,avg = avg,upper= upper,bn = 1:p))[1:27,]
  A
}

get_tto_draws <- function(xw) {
  xwt = reshape::melt(xw) %>% rename(bn=X2)
  xwt = as_tibble(xwt) %>% filter(bn <= 27)
  xwtR = tibble()
  for (b in 1:27) {
    xwtd = density( (xwt %>% filter(bn==b))$value)
    xwtd_bn = tibble(value=xwtd$x, density=xwtd$y, bn=b)
    xwtR = bind_rows(xwtR,xwtd_bn)
  }
  list(xwt, xwtR)
}

plot_xWOBA_over_time <- function(A) {
  pxw = A %>% 
    ggplot(aes(x=bn, y=avg)) +
    geom_errorbar(aes(ymin = lower, ymax = upper), fill = "black", width = .4) +
    geom_point(color="dodgerblue2", shape=21, size=2, fill="white") + 
    geom_line(aes(y = c(avg[1:9], rep(NA,18))), color="dodgerblue2", size=0.5) +
    geom_line(aes(y = c(rep(NA,9), avg[10:18], rep(NA,9))), color="dodgerblue2", size=0.5) +
    geom_line(aes(y = c(rep(NA,18), avg[19:27])), color="dodgerblue2", size=0.5) +
    geom_vline(aes(xintercept = 9.5), size=1.2) +
    geom_vline(aes(xintercept = 18.5), size=1.2) +
    labs(title = "Trend in Expected wOBA over the Course of a Game") + 
    theme(legend.position="none") +
    scale_x_continuous(name=TeX("Batter Sequence Number $m$"), 
                       limits = c(0,28),
                       breaks = c(0,5,10,15,20,25)) +
    scale_y_continuous(name="Expected wOBA", 
                       # limits = c(.2, .4),
                       breaks = seq(-1, 1, .025)
    ) 
  pxw
}

plot_xWOBA_over_time_bayes <- function(A,AAA) {
  line_size=1
  PP = AAA %>% ggplot() + 
    geom_hline(aes(yintercept = 9.5), size=1.2) +
    geom_hline(aes(yintercept = 18.5), size=1.2) +
    # geom_density_ridges(aes(x=value, y=bn, height = density, group = bn),
    #                     stat = "identity", scale = 1, fill="dodgerblue2") +
    geom_density_ridges_gradient(scale = 2, rel_min_height = 0.01, gradient_lwd = 1,
                                 aes(x=value, y=bn, height = density, group = bn, fill = stat(x)), #fill="dodgerblue2"
                                 stat = "identity", scale = 1, ) +
    scale_fill_viridis_c(name = "Expected wOBA", option = "C") +
    labs(title = 'Trend in Expected wOBA over the Course of a Game') +
    theme(legend.position="none") +
    # theme(axis.title.y = element_blank()) +
    scale_y_continuous(name="Batter Sequence Number", #TeX("Batter Sequence Number $m$"), 
                       limits = c(0,28),
                       breaks = c(0,5,10,15,20,25)) +
    scale_x_continuous(name="Expected wOBA", 
                       # limits = c(.2, .4),
                       breaks = seq(-1, 1, .05)) +
    coord_flip()
  PP
  for (b in c(1:8, 10:17, 19:26)) {
    # print(b)
    PP = PP + geom_segment(aes_string(x=A$avg[b], y=A$bn[b],
                                      xend=A$avg[b+1],yend=A$bn[b+1]),
                           data=A,color="lightskyblue1", size=line_size) 
    #aquamarine2
  }
  PP = PP + geom_point(aes(x=avg, y=bn),data=A,color="black", shape=21, size=2, fill="white") 
  PP
}

###############
### RESULTS ###
###############

### posterior samples of SPLINE model
fit <- readRDS("job_output/fit_rstan8-4.R.rds")
draws <- as.matrix(fit)
### batter sequence draws 1,...,27 for each category
bat_seq_draws = get_bat_seq_draws(draws) 
#bat_seq_draws[[7]][1:10,1:10]


### plot trend in expected wOBA over the course of a game
xw = spline_xWoba_post()
A = get_tto_means_and_ci(xw)
pxw = plot_xWOBA_over_time(A)
pxw
# ggsave("plots_spline/plot_xwoba_cws.png", pxw)
get_tto_draws_xw = get_tto_draws(xw) 
# A$avg <- factor(A$avg, levels = A$avg) ## make A$avg an ordered factor
AA = get_tto_draws_xw[[1]]
AAA = get_tto_draws_xw[[2]]
pxwb = plot_xWOBA_over_time_bayes(A,AAA)
pxwb
# ggsave(paste0("plots_spline/plot_xwoba_bayes_",year,".png"), pxwb)




