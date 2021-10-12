###############
#### SETUP ####
###############

OUTPUT_FILE = str_remove(sub('.*/', '', rstudioapi::getSourceEditorContext()$path), ".R")
OUTPUT_FILE
NUM_ITERS_IN_CHAIN = 1000 #FIXME #500 #1000 #10 
K = 10 #FIXME #K-fold cross validation #2

library(tidyverse)
library(stringr)
library(rstan)
library(loo)
cores = strtoi(Sys.getenv('OMP_NUM_THREADS')) ### for HPCC
options(mc.cores = cores) ### for HPCC
# options(mc.cores = parallel::detectCores()) # use this on my computer
rstan_options(auto_write = TRUE)

############################
########### DATA ###########
############################

# read data
input_file = "design_matrix1_2.csv" #FIXME
output_folder = "./job_output/"
D <- read_csv(input_file) 
D <- D %>% drop_na()
# create dummy variables for the categorical variables
# NO INTERCEPT and INCLUDE FIRST COLUMN 
change_factor_names <- function(s) {
  s <- str_remove(s, "factor")
  s <- str_remove_all(s, "\\(")
  s <- str_remove_all(s, "\\)")
  s
}
#
BATTER_IDX_dummies <- D %>% modelr::model_matrix(~ factor(BATTER_IDX) + 0) 
names(BATTER_IDX_dummies) <- change_factor_names(names(BATTER_IDX_dummies))
#
ORDER_CT_dummies <- D %>% modelr::model_matrix(~ factor(ORDER_CT) + 0) 
names(ORDER_CT_dummies) <- change_factor_names(names(ORDER_CT_dummies))
#
D$fold = kfold_split_random(K = K, N = nrow(D))
y <- D %>% select(fold, std_EVENT_WOBA_19)
X1 <- D %>% select(fold) %>%
            bind_cols(BATTER_IDX_dummies, ORDER_CT_dummies)
X2 <- bind_cols(X1, D %>% select(std_WOBA_FINAL_BAT_19, std_WOBA_FINAL_PIT_19))
X3 <- bind_cols(X2, D %>% select(HAND_MATCH))
X4 <- bind_cols(X3, D %>% select(IN_DIV))
X5 <- bind_cols(X4, D %>% select(BAT_HOME_IND))

# Show number of obs for each fold:
D %>% group_by(fold) %>% count()

#############################
########### RSTAN ###########
#############################

# compile rstan models
seed = 12345
set.seed(seed)
file = 'tto1_1.stan' #FIXME
model <- stan_model(file = file, model_name = file)
#model2 <- stan_model(file = file, model_name = file)

# Loop over the models
Xs = list(X1, X2, X3, X4, X5)
ees = list()
log_pd_kfolds = list()
elpd_kfolds = list()

for (i in 1:length(Xs)) {
  X = Xs[[i]]

  # Loop over the folds
  for(k in 1:K){
    y_train <- y %>% filter(fold != k) %>% select(-fold)
    y_test <- y %>% filter(fold == k) %>% select(-fold)
    X_train <- X %>% filter(fold != k) %>% select(-fold)
    X_test <- X %>% filter(fold == k) %>% select(-fold)
    # Training sets for k
    data_train <- list(
      y = y_train[[1]], X = X_train, n = nrow(X_train), p = ncol(X_train)
    )
    # Testing sets for k
    data_test <- list(
      y = y_test[[1]], X = X_test, n = nrow(X_test), p = ncol(X_test)
    )
    # Train the models
    fit_train <- sampling(model,
                          data = data_train,
                          iter = NUM_ITERS_IN_CHAIN,
                          chains = cores, #1 #cores, 
                          cores = cores, # HPCC
                          seed = seed)
    # save the stan objects
    #saveRDS(fit, file = paste0(output_folder, "fit_", OUTPUT_FILE, ".rds"))
    
    # Generated quantities based on the posterior from the training set
    # and the data from the held out set 
    gq <- gqs(model,
                draws = as.matrix(fit_train),
                data = data_test)
    
    # Extract log likelihood which represents
    # the pointwise predictive density
    e = extract_log_lik(gq)
    ees[[length(ees) + 1]] = e
  }
  
  log_pd_kfold <- matrix(nrow = dim(ees[[2*(i-1)+1]])[[1]], ncol = nrow(X))
  for(k in 1:K){
    e = ees[[K*(i-1)+k]]
    de = dim(e)[1]
    log_pd_kfold[, X$fold == k] <- e
  }
  
  log_pd_kfolds[[length(log_pd_kfolds) + 1]] = log_pd_kfold
  elpd_kfold <- elpd(log_pd_kfold)
  elpd_kfolds[[length(elpd_kfolds) + 1]] = elpd_kfold
}

L = loo_compare(elpd_kfolds[[1]], elpd_kfolds[[2]], elpd_kfolds[[3]], elpd_kfolds[[4]], elpd_kfolds[[5]])
L
L1 = as_tibble(L) %>% mutate(frac = round(abs(elpd_diff)/se_diff,2))
L1

write_csv(L1, paste0("./job_output/","OUTPUT_FILE","_L1.txt"))


