########################################
### 10 FOLD CV: COMPARE THE 2 MODELS ###
########################################

library(tidyverse)
library(rstan)
library(ggthemes)
library(latex2exp)
theme_set(theme_bw())
theme_update(plot.title = element_text(hjust = 0.5))
if(!interactive()) pdf(NULL)
cores = strtoi(Sys.getenv('OMP_NUM_THREADS')) ### for HPCC
options(mc.cores = cores) ### for HPCC
# options(mc.cores = parallel::detectCores()) # use this on my computer
rstan_options(auto_write = TRUE)

#####################################
########### OBSERVED DATA ###########
#####################################

### HERE get `D` from rstan_6x.R

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
y <- matrix(D$OB, ncol=1)
# 10 Fold CV folds
set.seed(12345) # make sure to have the same folds each time!
kk = if (exists("IS_SIM")) { if (IS_SIM) 5 else 10 } else 10
folds <- loo::kfold_split_random(K=kk,N=nrow(y))

############################################
########### BATTER_SEQ_NUM MODEL ###########
############################################

file_bsn = 'tto6_bsn.stan'
model_bsn <- stan_model(file = file_bsn, model_name = file_bsn)

fit_model_bsn <- function(fold_num=NA) {
  # training data - exclude FOLD_NUM, unless FOLD_NUM is NA 
  train_rows = if (is.na(fold_num)) TRUE else which(folds != fold_num)
  #train_rows = which(folds != fold_num)
  y_train = y[train_rows,]
  X_train = X[train_rows,]
  S_train = S[train_rows,]
  data_train <- list(
    y=y_train,S=S_train,X=X_train,
    n=nrow(X_train),p_x=ncol(X_train),p_s=ncol(S_train)
  )
  # Train the models
  seed = 12345
  set.seed(seed)
  NUM_ITERS_IN_CHAIN = 1500 #FIXME #10 
  fit <- sampling(model_bsn,
                  data = data_train,
                  iter = NUM_ITERS_IN_CHAIN,
                  chains = cores, #1 #cores, 
                  cores = cores, # HPCC
                  seed = seed)
  fit
}

############################################
########### UNIQUE_BAT_IDX MODEL ###########
############################################

file_ubi = 'tto6_ubi.stan'
model_ubi <- stan_model(file = file_ubi, model_name = file_ubi)

fit_model_ubi <- function(fold_num=NA) {
  # training data - exclude FOLD_NUM, unless FOLD_NUM is NA 
  train_rows = if (is.na(fold_num)) TRUE else which(folds != fold_num)
  #train_rows = which(folds != fold_num)
  y_train = y[train_rows,]
  X_train = X[train_rows,]
  U_train = U[train_rows,]
  O_train = O[train_rows,]
  data_train <- list(
    y=y_train,X=X_train,U=U_train,O=O_train,
    n=nrow(X_train),p_x=ncol(X_train),p_u=ncol(U_train),p_o=ncol(O_train)
  )
  # Train the models
  NUM_ITERS_IN_CHAIN = 1500
  seed = 12345
  set.seed(seed)
  fit <- sampling(model_ubi,
                  data = data_train,
                  iter = NUM_ITERS_IN_CHAIN,
                  chains = cores, #1 #cores, 
                  cores = cores, # HPCC
                  seed = seed)
  fit
}

######################################
########### PLOT FUNCTIONS ###########
######################################

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
    x = draws[[bsn[i]]]
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
                       breaks = seq(-2, 2, .05)
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
    x0 = draws[[o]]
    for (j in 1:length(bidx)) {
      b = bidx[j]
      xb = draws[[b]]
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
    scale_x_continuous(name=TeX("(order Count $l$, unique batter index $m$)"), 
                       limits = c(0,28),
                       breaks = BREAKS,
                       labels =  XLABS[BREAKS+1]) +
    scale_y_continuous(name=TeX("$\\beta_{m} + \\gamma_{l}$"), 
                       #limits = c(-.015, .03),
                       breaks = seq(-2, 2, .05)
    ) 
  production_plot
}

plot_bsn_spline <- function(fit) {
  draws <- as_tibble(as.matrix(fit))
  p = dim(B)[2]
  names(draws)[2:(1+p)] = names(B)
  
  bsn <- paste0("B", 1:p)
  lower <- numeric(p)
  avg <- numeric(p)
  upper <- numeric(p)
  for (i in 1:length(bsn)) {
    b = bsn[i]
    x = draws[[bsn[i]]]
    lower[i] = quantile(x,.025)
    avg[i] = mean(x)
    upper[i] = quantile(x,.975)
  }
  
  # spline basis matrix 
  aa = unique(D$BATTER_SEQ_NUM) 
  BB_ <- bs(aa, knots=knots, degree=3, intercept = TRUE) # creating the B-splines
  colnames(BB_) = paste0("B",1:ncol(BB_))
  BB = as_tibble(BB_)
  bbb = as.matrix(BB)
  
  # quantiles of each batter sequence number
  lower_ = bbb %*% lower
  avg_ = bbb %*% avg
  upper_ = bbb %*% upper
  
  # plot
  A = data.frame(
    lower = lower_[1:27],
    avg = avg_[1:27],
    upper= upper_[1:27],
    bn = 1:27
  )
  
  # PRODUCTION PLOT
  production_plot = A %>% 
    ggplot(aes(x=bn, y=avg)) +
    geom_errorbar(aes(ymin = lower, ymax = upper), fill = "black", width = .4) +
    geom_point(color="dodgerblue2", shape=21, size=2, fill="white") + 
    # geom_line(aes(y = c(avg[1:9], rep(NA,18))), color="firebrick", size=1) +
    # geom_line(aes(y = c(rep(NA,9), avg[10:18], rep(NA,9))), color="firebrick", size=1) +
    # geom_line(aes(y = c(rep(NA,18), avg[19:27])), color="firebrick", size=1) +
    geom_vline(aes(xintercept = 9.5), size=1.2) +
    geom_vline(aes(xintercept = 18.5), size=1.2) +
    labs(title = TeX("Pitcher effectiveness over the course of a game")) + # "Pitcher Effectiveness"
    theme(legend.position="none") +
    scale_x_continuous(name=TeX("Batter sequence number"), # $k$ 
                       limits = c(0,28),
                       breaks = c(0,5,10,15,20,25)) +
    scale_y_continuous(name=TeX("Posterior change in wOBA"), # "Posterior change in wOBA" "$\\alpha_k$"
                       #limits = c(-.02, .03),
                       breaks = seq(-.1, .1, .005)
    ) 
  production_plot
}




