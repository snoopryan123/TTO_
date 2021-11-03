runnit <- function(YEARS, OUTPUT_FILE) {
  # SETUP
  NUM_ITERS_IN_CHAIN = 1000 #FIXME #10 
  library(tidyverse)
  library(rstan)
  library(ggthemes)
  theme_set(theme_bw())
  if(!interactive()) pdf(NULL)
  cores = strtoi(Sys.getenv('OMP_NUM_THREADS')) ### for HPCC
  options(mc.cores = cores) ### for HPCC
  # options(mc.cores = parallel::detectCores()) # use this on my computer
  rstan_options(auto_write = TRUE)
  
  # read data
  input_file = "./../data/design_matrix2_3.csv" #FIXME
  output_folder = "./job_output/"
  D <- read_csv(input_file) %>% filter(YEAR %in% YEARS)
  D <- D %>% drop_na() 
  # create dummy variables for the categorical variables
  # NO INTERCEPT and INCLUDE FIRST COLUMN 
  change_factor_names <- function(s) {
    s <- str_remove(s, "factor")
    s <- str_remove_all(s, "\\(")
    s <- str_remove_all(s, "\\)")
    s
  }
  
  # categorical dummies for BATTER_IDX
  BATTER_IDX_dummies <- D %>% modelr::model_matrix(~ factor(BATTER_IDX) + 0) 
  names(BATTER_IDX_dummies) <- change_factor_names(names(BATTER_IDX_dummies))
  # categorical dummies for ORDER_CT
  ORDER_CT_dummies <- D %>% modelr::model_matrix(~ factor(ORDER_CT) + 0) 
  names(ORDER_CT_dummies) <- change_factor_names(names(ORDER_CT_dummies))
  # data 
  y <- D %>% select(std_EVENT_WOBA_19)
  X <- bind_cols(BATTER_IDX_dummies, 
                 ORDER_CT_dummies,
                 D %>% select(std_WOBA_FINAL_BAT_19, std_WOBA_FINAL_PIT_19, HAND_MATCH, BAT_HOME_IND))
  
  # compile rstan models
  seed = 12345
  set.seed(seed)
  file = 'tto2_1.stan' #FIXME
  model <- stan_model(file = file, model_name = file)
  
  # training data
  data_train <- list(y = y[[1]], X = X, n = nrow(X), p = ncol(X))
  
  # Train the models
  fit <- sampling(model,
                  data = data_train,
                  iter = NUM_ITERS_IN_CHAIN,
                  chains = cores, #1 #cores, 
                  cores = cores, # HPCC
                  seed = seed)
  
  # save the stan objects
  saveRDS(fit, file = paste0(output_folder, "fit_", OUTPUT_FILE, ".rds"))
  
  #NAMES <- c("sigma", names(X), "lp__")
  #NAMES
}




