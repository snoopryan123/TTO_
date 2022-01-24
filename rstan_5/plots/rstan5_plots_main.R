
library(tidyverse)
library(rstan)
library(ggthemes)
library(latex2exp)
theme_set(theme_bw())
theme_update(plot.title = element_text(hjust = 0.5))
if(!interactive()) pdf(NULL)

#####################################
########### OBSERVED DATA ###########
#####################################

### HERE get `D` from rstan_5x.R

# NO INTERCEPT and INCLUDE FIRST COLUMN
change_factor_names <- function(s) {
  s <- str_remove(s, "factor")
  s <- str_remove_all(s, "\\(")
  s <- str_remove_all(s, "\\)")
  s
}
# categorical dummies for BATTER_SEQ_NUM
BATTER_SEQ_dummies <- D %>% modelr::model_matrix(~ factor(BATTER_SEQ_NUM) + 0)
names(BATTER_SEQ_dummies) <- change_factor_names(names(BATTER_SEQ_dummies))
# categorical dummies for BATTER_IDX
BATTER_IDX_dummies <- D %>% modelr::model_matrix(~ factor(BATTER_IDX) + 0) 
names(BATTER_IDX_dummies) <- change_factor_names(names(BATTER_IDX_dummies))
# categorical dummies for ORDER_CT
ORDER_CT_dummies <- D %>% modelr::model_matrix(~ factor(ORDER_CT) + 0) 
names(ORDER_CT_dummies) <- change_factor_names(names(ORDER_CT_dummies))
# Observed data matrices 
S <- as.matrix(BATTER_SEQ_dummies)
U <- as.matrix(BATTER_IDX_dummies)
O <- as.matrix(ORDER_CT_dummies)
### X is loaded in another file
y <- matrix(D$std_EVENT_WOBA_19, ncol=1)
# 10 Fold CV folds
set.seed(12345) # make sure to have the same folds each time!
folds <- loo::kfold_split_random(K=10,N=nrow(y))

######################################
########### PLOT FUNCTIONS ###########
######################################

# RESCALE the coefficients back to un-standardized form
#mu_y = mean(D$EVENT_WOBA_19)
sd_y = sd(D$EVENT_WOBA_19)

transform_back <- function(x) {
  2*sd_y*x # +mu_y
}

plot_bsn0 <- function(fit) {
  draws <- as_tibble(as.matrix(fit))

  # due to autocorrelation, keep every other posterior sample
  #draws <- draws[seq(1,nrow(draws),2),]
  
  # compute mean and 2.5%, 97.5% quantiles of posterior samples
  p = 27 #dim(BATTER_SEQ_dummies)[2]
  bsn <- paste0("alpha[",1:p,"]")
  lower <- numeric(p)
  avg <- numeric(p)
  upper <- numeric(p)
  for (i in 1:length(bsn)) {
    b = bsn[i]
    x = transform_back(draws[[bsn[i]]])
    lower[i] = quantile(x,.025)
    avg[i] = mean(x)
    upper[i] = quantile(x,.975)
  }
  
  # plot
  A4 = data.frame(
    lower = lower,
    avg = avg,
    upper= upper,
    bn = 1:p
  )
  
  # PRODUCTION PLOT
  production_plot = A4 %>% 
    ggplot(aes(x=bn, y=avg)) +
    geom_errorbar(aes(ymin = lower, ymax = upper), fill = "black", width = .4) +
    geom_point(color="dodgerblue2", shape=21, size=2, fill="white") + 
    # geom_line(aes(y = c(avg[1:9], rep(NA,18))), color="firebrick", size=1) +
    # geom_line(aes(y = c(rep(NA,9), avg[10:18], rep(NA,9))), color="firebrick", size=1) +
    # geom_line(aes(y = c(rep(NA,18), avg[19:27])), color="firebrick", size=1) +
    geom_vline(aes(xintercept = 9.5), size=1.2) +
    geom_vline(aes(xintercept = 18.5), size=1.2) +
    labs(title = TeX("Posterior distribution of $\\alpha$")) + 
    theme(legend.position="none") +
    scale_x_continuous(name=TeX("Batter sequence number $k$"), 
                       limits = c(0,28),
                       breaks = c(0,5,10,15,20,25)) +
    scale_y_continuous(name=TeX("$\\alpha_k$"), 
                       #limits = c(-.02, .03),
                       breaks = seq(-.1, .1, .005)
    ) 
  production_plot
}

plot_ubi0 <- function(fit) {
  draws <- as_tibble(as.matrix(fit))

  # compute mean and 2.5%, 97.5% quantiles of posterior samples
  p = 27 #dim(BATTER_SEQ_dummies)[2]
  bidx <- paste0("beta[",1:9,"]")
  oc <- paste0("gamma[",1:3,"]")
  lower <- numeric(p)
  avg <- numeric(p)
  upper <- numeric(p)
  for (i in 1:length(oc)) {
    o = oc[i]
    x0 = transform_back(draws[[o]])
    for (j in 1:length(bidx)) {
      b = bidx[j]
      xb = transform_back(draws[[b]])
      x = x0 + xb
      lower[(i-1)*length(bidx) + j] = quantile(x,.025)
      avg[(i-1)*length(bidx) + j] = mean(x)
      upper[(i-1)*length(bidx) + j] = quantile(x,.975)
    }
  }
  
  # plot
  A4 = data.frame(
    lower = lower,
    avg = avg,
    upper= upper,
    bn = 1:p
  )
  
  XLABS = c("", paste0("(",1,",",1:9,")"), paste0("(",2,",",1:9,")"), paste0("(",3,",",1:9,")"))
  BREAKS = seq(1,28,by=2)#c(1,6,11,16,21,26)#c(0,5,10,15,20,25)
  
  # PRODUCTION PLOT
  theme_update(plot.title = element_text(hjust = 0.5))
  production_plot = A4 %>% 
    ggplot(aes(x=bn, y=avg)) +
    geom_errorbar(aes(ymin = lower, ymax = upper), fill = "black", width = .4) +
    geom_point(color="dodgerblue2", shape=21, size=2, fill="white") + 
    # geom_line(aes(y = c(avg[1:9], rep(NA,18))), color="firebrick", size=1) +
    # geom_line(aes(y = c(rep(NA,9), avg[10:18], rep(NA,9))), color="firebrick", size=1) +
    # geom_line(aes(y = c(rep(NA,18), avg[19:27])), color="firebrick", size=1) +
    geom_vline(aes(xintercept = 9.5), size=1.2) +
    geom_vline(aes(xintercept = 18.5), size=1.2) +
    labs(title = TeX("Posterior distribution of $\\beta +\\gamma$")) + 
    theme(legend.position="none") +
    scale_x_continuous(name=TeX("(order Count $l$, unique batter index $k$)"), 
                       limits = c(0,28),
                       breaks = BREAKS,
                       labels =  XLABS[BREAKS+1]) +
    scale_y_continuous(name=TeX("$\\beta_{k} + \\gamma_{l}$"), 
                       #limits = c(-.015, .03),
                       breaks = seq(-.1, .1, .005)
    ) 
  production_plot
}


plot_3hist_bsn1 <- function(fit) {
  draws <- as_tibble(as.matrix(fit))
  draws <- transform_back(draws)
  d1 = draws[["alpha[1]"]]
  d2 = draws[["alpha[2]"]]
  d9 = draws[["alpha[9]"]]
  d10 = draws[["alpha[10]"]]
  d18 = draws[["alpha[18]"]]
  d19 = draws[["alpha[19]"]]
  diff1 = as_tibble(d2 - d1)
  diff2 = as_tibble(d10 - d9)
  diff3 = as_tibble(d19 - d18)
  diff1$name = "diff1"
  diff2$name = "diff2"
  diff3$name = "diff3"
  diffs = bind_rows(diff1,diff2,diff3)
  labs <- c(bquote(paste("posterior dist. of ", alpha[2] - alpha[1])), 
            bquote(paste("posterior dist. of ", alpha[10] - alpha[9])),
            bquote(paste("posterior dist. of ", alpha[19] - alpha[18])))
  p1 = diffs %>% 
    ggplot(aes(value, fill = name)) + 
    geom_density(alpha = 0.2) +
    scale_fill_discrete(labels=labs,
                        name = "") +
    theme(#axis.title.y=element_blank(),
      axis.text.y=element_blank(),
      axis.ticks.y=element_blank()) +
    xlab("") +
    labs(title="Comparing the TTO penalties to the \n difference between the second and first batters")
  p1
}

plot_3hist_ubi1 <- function(fit) {
  draws <- as_tibble(as.matrix(fit))
  draws <- transform_back(draws)
  d1 = draws[["beta[1]"]] + draws[["gamma[1]"]]
  d2 = draws[["beta[2]"]] + draws[["gamma[1]"]]
  d9 = draws[["beta[9]"]] + draws[["gamma[1]"]]
  d10 = draws[["beta[1]"]] + draws[["gamma[2]"]]
  d18 = draws[["beta[9]"]] + draws[["gamma[2]"]]
  d19 = draws[["beta[1]"]] + draws[["gamma[3]"]]
  diff1 = as_tibble(d2 - d1)
  diff2 = as_tibble(d10 - d9)
  diff3 = as_tibble(d19 - d18)
  diff1$name = "diff1"
  diff2$name = "diff2"
  diff3$name = "diff3"
  diffs = bind_rows(diff1,diff2,diff3)
  labs <- c(bquote(paste("posterior dist. of ", (beta[2] + gamma[1]) - (beta[1] + gamma[1]))), 
            bquote(paste("posterior dist. of ", (beta[1] + gamma[2]) - (beta[9] + gamma[1]))),
            bquote(paste("posterior dist. of ", (beta[1] + gamma[3]) - (beta[9] + gamma[2]))))
  p1 = diffs %>% 
    ggplot(aes(value, fill = name)) + 
    geom_density(alpha = 0.2) +
    scale_fill_discrete(labels=labs,
                        name = "") +
    theme(#axis.title.y=element_blank(),
      axis.text.y=element_blank(),
      axis.ticks.y=element_blank()) +
    xlab("") +
    labs(title="Comparing the TTO penalties to the \n difference between the second and first batters")
  p1
}


plot_3hist_bsn2 <- function(fit) {
  draws <- as_tibble(as.matrix(fit))
  draws <- as_tibble(transform_back(draws))
  tto1 = draws[paste0("alpha[",1:9,"]")]
  tto2 = draws[paste0("alpha[",10:18,"]")]
  tto3 = draws[paste0("alpha[",19:27,"]")]
  tto1_means = tto1 %>% mutate(tto_means = rowMeans(.)) %>% select(tto_means)
  tto2_means = tto2 %>% mutate(tto_means = rowMeans(.)) %>% select(tto_means)
  tto3_means = tto3 %>% mutate(tto_means = rowMeans(.)) %>% select(tto_means)
  tto1_means$tto = "1"
  tto2_means$tto = "2"
  tto3_means$tto = "3"
  tto_means = bind_rows(tto1_means,tto2_means,tto3_means)
  # labs <- c("first TTO",
  #           "second TTO",
  #           "third TTO")
  labs <- c(bquote(paste("1st TTO: mean posterior dist. of {", alpha[k], ": k=1,...,9}"  )), 
            bquote(paste("2nd TTO: mean posterior dist. of {", alpha[k], ": k=10,...,18}"  )), 
            bquote(paste("3rd TTO: mean posterior dist. of {", alpha[k], ": k=19,...,27}"  )) )
  p2 = tto_means %>% 
    ggplot(aes(tto_means, fill = tto)) + 
    geom_density(alpha = 0.2) +
    scale_fill_discrete(labels=labs, name="") +
    theme(#axis.title.y=element_blank(),
      axis.text.y=element_blank(),
      axis.ticks.y=element_blank()) +
    xlab("") +
    labs(title = "Posterior dists. of pitcher performance averaged \n over each TTO, after adjustments")
    #labs(title=bquote(paste("Average ", alpha,  " posterior dists. in each TTO")))
  p2
}

plot_3hist_ubi2 <- function(fit) {
  draws <- as_tibble(as.matrix(fit))
  draws <- as_tibble(transform_back(draws))
  betas = draws[paste0("beta[",1:9,"]")]
  gamma1 = as_tibble(do.call(cbind, rep(draws[paste0("gamma[",1,"]")], 9) ))
  gamma2 = as_tibble(do.call(cbind, rep(draws[paste0("gamma[",2,"]")], 9) ))
  gamma3 = as_tibble(do.call(cbind, rep(draws[paste0("gamma[",3,"]")], 9) ))
  tto1 = as_tibble(betas + gamma1)
  tto2 = as_tibble(betas + gamma2)
  tto3 = as_tibble(betas + gamma3)
  tto1_means = tto1 %>% mutate(tto_means = rowMeans(.)) %>% select(tto_means)
  tto2_means = tto2 %>% mutate(tto_means = rowMeans(.)) %>% select(tto_means)
  tto3_means = tto3 %>% mutate(tto_means = rowMeans(.)) %>% select(tto_means)
  tto1_means$tto = "1"
  tto2_means$tto = "2"
  tto3_means$tto = "3"
  tto_means = bind_rows(tto1_means,tto2_means,tto3_means)
  # labs <- c("first TTO",
  #           "second TTO",
  #           "third TTO")
  labs <- c(bquote(paste("1st TTO: mean posterior dist. of {", beta[k]+gamma[1], ": k=1,...,9}"  )), 
            bquote(paste("2nd TTO: mean posterior dist. of {", beta[k]+gamma[2], ": k=10,...,18}"  )), 
            bquote(paste("3rd TTO: mean posterior dist. of {", beta[k]+gamma[3], ": k=19,...,27}"  )) )
  p2 = tto_means %>% 
    ggplot(aes(tto_means, fill = tto)) + 
    geom_density(alpha = 0.2) +
    scale_fill_discrete(labels=labs, name="") +
    theme(#axis.title.y=element_blank(),
      axis.text.y=element_blank(),
      axis.ticks.y=element_blank()) +
    xlab("") +
    labs(title = "Posterior dists. of pitcher performance averaged \n over each TTO, after adjustments")
    #labs(title=bquote(paste("Average ", alpha,  " posterior dists. in each TTO")))
  p2
}

