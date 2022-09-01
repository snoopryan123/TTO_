
### import RStan models
### make sure to run `model9_main.R` first

library(rstan)
rstan_options(auto_write = TRUE)
##### uncomment these if working on my computer #####
# cores = 1
# NUM_ITS = 10
# #### options(mc.cores = parallel::detectCores())
#####################################################
####### uncomment these if working on HPCC ##########
cores=strtoi(Sys.getenv('OMP_NUM_THREADS'))
options(mc.cores = cores) ### for HPCC
NUM_ITS = 1500 #1500 #2000 #2500 #5000

#########################################################################
### RSTAN: pitcher fatigue with positive slop + batter learning bumps ###
#########################################################################

### rstan model with pitcher fatigue spline and batter learning bumps

file_bsnBL = "tto9_PF_BL_linePosSlope.stan"
CHANGE_DIR = if (exists("IS_SIM")) { IS_SIM } else if (exists("IS_COMP")) { IS_COMP } else { FALSE }
og_dir = getwd()
if (CHANGE_DIR) { setwd("..") }
model_bsnBL <- stan_model(file = file_bsnBL, model_name = file_bsnBL)
if (CHANGE_DIR) { setwd(og_dir) }

fit_model_bsnBL <- function(fold_num=NA) {
  # training data - exclude FOLD_NUM, unless FOLD_NUM is NA
  train_rows = if (is.na(fold_num)) TRUE else which(folds != fold_num)
  y_train = y[train_rows,]
  S_train = matrix(SPL[train_rows,], ncol=1) ### SPL, not S
  O_train = O[train_rows,]
  X_train = X[train_rows,]
  INCPT_train = matrix(INCPT[train_rows,], ncol=1)
  data_train <- list(
    y=y_train,S=S_train,O=O_train,X=X_train,INCPT=INCPT_train,
    p_o=ncol(O_train),p_x=ncol(X_train),
    n=nrow(X_train),K=num_categories
  )
  # Train the model
  seed = 12345
  set.seed(seed)
  NUM_ITERS_IN_CHAIN = NUM_ITS
  fit <- sampling(model_bsnBL,
                  data = data_train,
                  iter = NUM_ITERS_IN_CHAIN,
                  pars=c("linpred","alpha_incpt_raw","alpha_slope_raw","beta_raw","eta_raw"), include=FALSE,
                  chains = cores, #1 #cores,
                  cores = cores, # HPCC
                  seed = seed)
  fit
}




