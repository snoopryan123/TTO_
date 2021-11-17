########################################
### 10 FOLD CV: COMPARE THE 2 MODELS ###
########################################

output_folder = "./job_output/"
#OUTPUT_FILE = "rstan3_comp.R" #FIXME
NUM_ITERS_IN_CHAIN = 1500 #FIXME #10 

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

# USE THE ACTUAL X DATA MATRIX FROM 2019 
# read data
input_file = "./../data/design_matrix2_3.csv" #FIXME
D <- read_csv(input_file)
D <- D %>% drop_na() %>% filter(YEAR >= 2015 & YEAR <= 2019) #FIXME
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
X <- as.matrix(D %>% select(std_WOBA_FINAL_BAT_19, std_WOBA_FINAL_PIT_19, HAND_MATCH, BAT_HOME_IND))
y <- matrix(D$std_EVENT_WOBA_19, ncol=1)
# 10 Fold CV folds
set.seed(12345) # make sure to have the same folds each time!
folds <- loo::kfold_split_random(K=10,N=nrow(y))

############################################
########### BATTER_SEQ_NUM MODEL ###########
############################################

file_bsn = 'tto3_bsn.stan'
model_bsn <- stan_model(file = file_bsn, model_name = file_bsn)

fit_model_bsn <- function(fold_num) {
  # training data - exclude FOLD_NUM
  train_rows = which(folds != fold_num)
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

file_ubi = 'tto3_ubi.stan'
model_ubi <- stan_model(file = file_ubi, model_name = file_ubi)

fit_model_ubi <- function(fold_num) {
  # training data - exclude FOLD_NUM
  train_rows = which(folds != fold_num)
  y_train = y[train_rows,]
  X_train = X[train_rows,]
  U_train = U[train_rows,]
  O_train = O[train_rows,]
  data_train <- list(
    y=y_train,X=X_train,U=U_train,O=O_train,
    n=nrow(X_train),p_x=ncol(X_train),p_u=ncol(U_train),p_o=ncol(O_train)
  )
  # Train the models
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

