###############
#### SETUP ####
###############

OUTPUT_FILE = "rstan1_4.R" #FIXME
#OUTPUT_FILE = str_remove(sub('.*/', '', rstudioapi::getSourceEditorContext()$path), ".R")
OUTPUT_FILE
NUM_ITERS_IN_CHAIN = 3000 #FIXME #500 #1000 #10 

library(tidyverse)
library(rstan)
library(loo)
library(ggthemes)
theme_set(theme_classic())
cores = strtoi(Sys.getenv('OMP_NUM_THREADS')) ### for HPCC
options(mc.cores = cores) ### for HPCC
# options(mc.cores = parallel::detectCores()) # use this on my computer
rstan_options(auto_write = TRUE)

############################
########### DATA ###########
############################

# read data
input_file = "design_matrix1_3.csv" #FIXME
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
PARK_dummies <- D %>% modelr::model_matrix(~ factor(PARK) + 0) 
names(PARK_dummies) <- change_factor_names(names(PARK_dummies))
# Split Data into Training and Testing in R 
sample_size = floor(0.9*nrow(D))
set.seed(12345)
# randomly split data in r
picked = sample(seq_len(nrow(D)), size = sample_size)
D = D %>% mutate(train = row_number() %in% picked)
y <- D %>% select(std_EVENT_WOBA_19, train)
X <- bind_cols(BATTER_IDX_dummies, ORDER_CT_dummies)
X <- bind_cols(X, D %>% select(std_WOBA_FINAL_BAT_19, 
                               std_WOBA_FINAL_PIT_19,
                               HAND_MATCH, 
                               BAT_HOME_IND, 
                               train))
X <- bind_cols(X, PARK_dummies)

#############################
########### RSTAN ###########
#############################

# compile rstan models
seed = 12345
set.seed(seed)
file = 'tto1_2.stan' #FIXME
model <- stan_model(file = file, model_name = file)

# y_train <- y %>% filter(train %>% select(-train)
# y_test <- y %>% filter(!train) %>% select(-train)
# X_train <- X %>% filter(train) %>% select(-train)
# X_test <- X %>% filter(!train) %>% select(-train)
y_train <- y %>% select(-train)
X_train <- X %>% select(-train)

# Training sets for k
data_train <- list(
  y = y_train[[1]], X = X_train, n = nrow(X_train), p = ncol(X_train)
)
# Testing sets for k
# data_test <- list(
#   y = y_test[[1]], X = X_test, n = nrow(X_test), p = ncol(X_test)
# )
# Train the models
fit <- sampling(model,
                data = data_train,
                iter = NUM_ITERS_IN_CHAIN,
                chains = cores, #1 #cores, 
                cores = cores, # HPCC
                seed = seed)
# save the stan objects
saveRDS(fit, file = paste0(output_folder, "fit_", OUTPUT_FILE, ".rds"))

#fit <- readRDS("job_output/fit_rstan1_4.R.rds") 

#############################
########### PLOTS ###########
#############################

# draws and fit summary
NAMES <- c("sigma", names(X %>% select(-train)), "lp__")
s <- summary(fit)$summary
rownames(s) <- NAMES
draws <- as_tibble(as.matrix(fit))
names(draws) <- NAMES
# write.csv(data.frame(ss), file = paste0(output_folder, "fit_ss", OUTPUT_FILE, ".csv"), row.names=TRUE)

# RESCALE the coefficients back to un-standardized form
mu_y = mean(D$EVENT_WOBA_19)
sd_y = sd(D$EVENT_WOBA_19)
transform_back <- function(x) {
  mu_y + 2*sd_y*x
}

# 
A0 = draws %>%
     mutate(b11 = transform_back(BATTER_IDX1 + ORDER_CT1),
            b12 = transform_back(BATTER_IDX2 + ORDER_CT1),
            b13 = transform_back(BATTER_IDX3 + ORDER_CT1),
            b14 = transform_back(BATTER_IDX4 + ORDER_CT1),
            b15 = transform_back(BATTER_IDX5 + ORDER_CT1),
            b16 = transform_back(BATTER_IDX6 + ORDER_CT1),
            b17 = transform_back(BATTER_IDX7 + ORDER_CT1),
            b18 = transform_back(BATTER_IDX8 + ORDER_CT1),
            b19 = transform_back(BATTER_IDX9 + ORDER_CT1),
            b21 = transform_back(BATTER_IDX1 + ORDER_CT2),
            b22 = transform_back(BATTER_IDX2 + ORDER_CT2),
            b23 = transform_back(BATTER_IDX3 + ORDER_CT2),
            b24 = transform_back(BATTER_IDX4 + ORDER_CT2),
            b25 = transform_back(BATTER_IDX5 + ORDER_CT2),
            b26 = transform_back(BATTER_IDX6 + ORDER_CT2),
            b27 = transform_back(BATTER_IDX7 + ORDER_CT2),
            b28 = transform_back(BATTER_IDX8 + ORDER_CT2),
            b29 = transform_back(BATTER_IDX9 + ORDER_CT2),
            b31 = transform_back(BATTER_IDX1 + ORDER_CT3),
            b32 = transform_back(BATTER_IDX2 + ORDER_CT3),
            b33 = transform_back(BATTER_IDX3 + ORDER_CT3),
            b34 = transform_back(BATTER_IDX4 + ORDER_CT3),
            b35 = transform_back(BATTER_IDX5 + ORDER_CT3),
            b36 = transform_back(BATTER_IDX6 + ORDER_CT3),
            b37 = transform_back(BATTER_IDX7 + ORDER_CT3),
            b38 = transform_back(BATTER_IDX8 + ORDER_CT3),
            b39 = transform_back(BATTER_IDX9 + ORDER_CT3)) %>%
  select(b11,b12,b13,b14,b15,b16,b17,b18,b19,
         b21,b22,b23,b24,b25,b26,b27,b28,b29,
         b31,b32,b33,b34,b35,b36,b37,b38,b39)

A1 = reshape2::melt(A0)
plot1 = A1 %>% ggplot(aes(x=variable, y=value)) +
               geom_boxplot() +
               labs(y="wOBA", 
                    x = "time thru order number + batter index number",
                    title = OUTPUT_FILE)
plot1

#
ggsave(paste0(output_folder, "plot_", OUTPUT_FILE, ".png"), plot1)


