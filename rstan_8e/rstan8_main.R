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
NUM_ITS = 2500 #2500 #1500 #5000
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
# # categorical dummies for YEAR
# if (exists("YEAR_EFFECTS")) { 
#   if (YEAR_EFFECTS) {
#     YEAR_dummies <- D %>% modelr::model_matrix(~ factor(YEAR) + 0) 
#     names(YEAR_dummies) <- change_factor_names(names(YEAR_dummies))
#     # ### sum(YEAR_dummies$YEAR2019) 
#     YEAR_dummies_woLastCol = YEAR_dummies[,1:(ncol(YEAR_dummies)-1)]
#     ### Fixed Effect for Year
#     YR = as.matrix(YEAR_dummies_woLastCol)
#     X_yr = cbind(X, YR)
#   }
# }
# BSN data matrices 
S <- as.matrix(BATTER_SEQ_dummies)
# SPLINE data matrices
knots = c(rep(9.5,4), rep(18.5,4), rep(27.5,4))  
aa = unique(D$BATTER_SEQ_NUM) 
BB_ <- bs(aa, knots=knots, degree=3, intercept = TRUE) 
colnames(BB_) = paste0("B",1:ncol(BB_))
BB = as_tibble(BB_)
bbb = as.matrix(BB)

### Spline matrix SPL
SPL1 = S ### indicator model
SPL2 = S %*% bbb ### cubic within each TTO, discontinuity between each TTO

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

########### BSN & SPLINE MODEL ###########

fit_model_bsnSpl <- function(fold_num=NA, SPL) {
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




