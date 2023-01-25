
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

filename = "tto11.stan"
CHANGE_DIR = if (exists("IS_SIM")) { IS_SIM } else if (exists("IS_COMP")) { IS_COMP } else { FALSE }
og_dir = getwd()
if (CHANGE_DIR) { setwd("..") }
MODEL <- stan_model(file = filename, model_name = filename)
if (CHANGE_DIR) { setwd(og_dir) }

fit_MODEL <- function(fold_num=NA) {
  # training data - exclude FOLD_NUM, unless FOLD_NUM is NA
  train_rows = if (is.na(fold_num)) TRUE else which(folds != fold_num)
  y_train = y[train_rows,]
  BSN_train = BSN[train_rows,]
  O2_train = O2[train_rows]
  O3_train = O3[train_rows]
  X_train = X[train_rows,]
  INCPT_train = matrix(INCPT[train_rows,], ncol=1)
  data_train <- list(
    K=num_categories, n=nrow(X_train), p_x=ncol(X_train),
    INCPT=INCPT_train, t=BSN_train, O2=O2_train, O3=O3_train,
    X=X_train, y=y_train, 
    num_bat=num_bat, bat_idxs=bat_idxs,
    num_pit=num_pit, pit_idxs=pit_idxs
  )

  # Train the model
  seed = 12345
  set.seed(seed)
  NUM_ITERS_IN_CHAIN = NUM_ITS
  fit <- sampling(MODEL,
                  data = data_train,
                  iter = NUM_ITERS_IN_CHAIN,
                  pars=c(
                    "alpha_0j_raw", "alpha_1j_raw", "beta_2l_raw", "beta_3l_raw",
                    "Z_alpha_0j_raw", "Z_alpha_1j_raw", "Z_beta_2l_raw", "Z_beta_3l_raw",
                    # "alpha_0j", "alpha_1j", "beta_2l", "beta_3l",
                    # "sig_sq_0", "sig_sq_1", "sig_sq_2", "sig_sq_3", 
                    "linpred", "eta_raw"
                  ), 
                  include=FALSE,
                  chains = cores, #1 #cores,
                  cores = cores, # HPCC
                  seed = seed)
  fit
}

# fit = fit_MODEL() ### fit the model



