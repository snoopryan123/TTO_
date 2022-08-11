########################################
### 10 FOLD CV: COMPARE THE 2 MODELS ###
########################################

library(tidyverse)
library(rstan)
library(ggthemes)
library(latex2exp)
library(splines)
theme_set(theme_bw())
theme_update(text = element_text(size=18))
theme_update(plot.title = element_text(hjust = 0.5))
if(!interactive()) pdf(NULL)
rstan_options(auto_write = TRUE)
##### uncomment these if working on my computer #####
# cores = 1
# NUM_ITS = 10
# # #### options(mc.cores = parallel::detectCores())
#####################################################
####### uncomment these if working on HPCC ##########
cores=1#strtoi(Sys.getenv('OMP_NUM_THREADS')) 
options(mc.cores = cores) ### for HPCC
NUM_ITS = 5000 #2500 #1500 #5000
#####################################################

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
# categorical dummies for YEAR
if (exists("YEAR_EFFECTS")) { 
  if (YEAR_EFFECTS) {
    YEAR_dummies <- D %>% modelr::model_matrix(~ factor(YEAR) + 0) 
    names(YEAR_dummies) <- change_factor_names(names(YEAR_dummies))
    # ### sum(YEAR_dummies$YEAR2019) 
    YEAR_dummies_woLastCol = YEAR_dummies[,1:(ncol(YEAR_dummies)-1)]
    ### Fixed Effect for Year
    YR = as.matrix(YEAR_dummies_woLastCol)
    X_yr = cbind(X, YR)
  }
}
# BSN data matrices 
S <- as.matrix(BATTER_SEQ_dummies)
# SPLINE data matrices
knots = c(rep(9.5,4), rep(18.5,4), rep(27.5,4))  
aa = unique(D$BATTER_SEQ_NUM) 
BB_ <- bs(aa, knots=knots, degree=3, intercept = TRUE) # creating the B-splines
colnames(BB_) = paste0("B",1:ncol(BB_))
BB = as_tibble(BB_)
bbb = as.matrix(BB)
SPL = S ### not actually a spline, but using spline code because lazy... it's just indicators now...
###SPL = S %*% bbb ### splined data matrix
# UBI data matrices 
U <- as.matrix(BATTER_IDX_dummies)
O <- as.matrix(ORDER_CT_dummies)
# CUBIC WITH SHIFTS data matrices
# bbb2 = outer(1:27, seq(0, 3), `^`)
# ###bbb2 = outer(1:36, seq(0, 3), `^`)
# S_cws = S %*% bbb2
### X is loaded in another file
y_og <- D$EVENT_WOBA_19
categories = sort(unique(y_og))
num_categories = length(categories)
y_to_category <- function(y_ij) { which(y_ij == categories)[1] }
y <- matrix( sapply(y_og, y_to_category), ncol=1)
category_strings <- c("out","BB","HBP","1B","2B","3B","HR")
# 10 Fold CV folds
set.seed(12345) # make sure to have the same folds each time!
kk = if (exists("IS_SIM")) { if (IS_SIM) 5 else 10 } else 10
folds <- loo::kfold_split_random(K=kk,N=nrow(y))

############################################
########### BATTER_SEQ_NUM MODEL ###########
############################################

file_bsn = "tto8_bsn.stan"
CHANGE_DIR = if (exists("IS_SIM")) { IS_SIM } else if (exists("IS_COMP")) { IS_COMP } else { FALSE }
og_dir = getwd()
if (CHANGE_DIR) { setwd("..") }
model_bsn <- stan_model(file = file_bsn, model_name = file_bsn)
if (CHANGE_DIR) { setwd(og_dir) }

# fit_model_bsn <- function(fold_num=NA) {
#   # training data - exclude FOLD_NUM, unless FOLD_NUM is NA 
#   train_rows = if (is.na(fold_num)) TRUE else which(folds != fold_num)
#   #train_rows = which(folds != fold_num)
#   y_train = y[train_rows,]
#   X_train = X[train_rows,]
#   S_train = S[train_rows,]
#   data_train <- list(
#     y=y_train,S=S_train,X=X_train,
#     n=nrow(X_train),p_x=ncol(X_train),p_s=ncol(S_train),K=num_categories
#   )
#   # Train the models
#   seed = 12345
#   set.seed(seed)
#   NUM_ITERS_IN_CHAIN = NUM_ITS
#   fit <- sampling(model_bsn,
#                   data = data_train,
#                   iter = NUM_ITERS_IN_CHAIN,
#                   pars=c("linpred","alpha_raw","eta_raw"), include=FALSE,
#                   chains = cores, #1 #cores, 
#                   cores = cores, # HPCC
#                   seed = seed)
#   fit
# }

########### SPLINE MODEL ###########

fit_model_spline <- function(fold_num=NA) {
  # training data - exclude FOLD_NUM, unless FOLD_NUM is NA 
  train_rows = if (is.na(fold_num)) TRUE else which(folds != fold_num)
  #train_rows = which(folds != fold_num)
  y_train = y[train_rows,]
  X_train = X[train_rows,]
  S_train = SPL[train_rows,] ### this is where fit_model_spline differs from fit_model_bsn
  data_train <- list(
    y=y_train,S=S_train,X=X_train,
    n=nrow(X_train),p_x=ncol(X_train),p_s=ncol(S_train),K=num_categories
  )
  # Train the models
  seed = 12345
  set.seed(seed)
  NUM_ITERS_IN_CHAIN = NUM_ITS
  fit <- sampling(model_bsn,
                  data = data_train,
                  iter = NUM_ITERS_IN_CHAIN,
                  pars=c("linpred","alpha_raw","eta_raw"), include=FALSE,
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
  A = tibble()
  for (k in 2:num_categories) {
    # compute mean and 2.5%, 97.5% quantiles of posterior samples
    p = 27 #dim(BATTER_SEQ_dummies)[2]
    bsn <- paste0("alpha[",1:p,",",k,"]")
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
    A4 = tibble(
      lower = lower,
      avg = avg,
      upper= upper,
      bn = 1:p,
      k=paste0("wOBA_19 = ", categories[k]," (",category_strings[k],")")
    )
    A = bind_rows(A,A4)
  }
  # PLOT
  production_plot = A %>% 
    ggplot(aes(x=bn, y=avg)) +
    facet_wrap(~k) +
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
                       #breaks = seq(-2, 2, .05)
    ) 
  production_plot
}

plot_ubi0 <- function(fit) {
  draws <- as_tibble(as.matrix(fit))
  A = tibble()
  for (k in 2:num_categories) {
    # compute mean and 2.5%, 97.5% quantiles of posterior samples
    p = 27 #dim(BATTER_SEQ_dummies)[2]
    bidx <- paste0("beta[",1:9,",",k,"]")
    oc <- paste0("gamma[",1:3,",",k,"]")
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
      bn = 1:p,
      k=paste0("wOBA_19 = ", categories[k])
    )
    A = bind_rows(A,A4)
  }
  
  XLABS = c("", paste0("(",1,",",1:9,")"), paste0("(",2,",",1:9,")"), paste0("(",3,",",1:9,")"))
  BREAKS = seq(1,28,by=2)#c(1,6,11,16,21,26)#c(0,5,10,15,20,25)
  
  # PRODUCTION PLOT
  theme_update(plot.title = element_text(hjust = 0.5))
  production_plot = A %>% 
    ggplot(aes(x=bn, y=avg)) +
    facet_wrap(~k) +
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
                       # breaks = seq(-2, 2, .05)
    ) 
  production_plot
}




