########################################
#### GLOBAL CONSTANTS TO BE CHANGED ####
########################################
# - add tto8.stan
# - fix options mccores 
# - fix num_iters_in_chain
# - fix output_file

OUTPUT_FILE = "_10_2015-2020b" 
NUM_ITERS_IN_CHAIN = 500 #500 #1000 #10

library(tidyverse)
library(rstan)
cores = strtoi(Sys.getenv('OMP_NUM_THREADS')) ### for HPCC
options(mc.cores = cores) ### for HPCC
# options(mc.cores = parallel::detectCores()) # use this on my computer
rstan_options(auto_write = TRUE)

############################
########### DATA ###########
############################

# read data
input_file = "design_matrix_2015-2020b.csv"
D <- read_csv(input_file, col_types = "ddddddddddddddddcccc")
spec(D)
output_folder = "./job_output/"

D <- D %>% drop_na() #FIXME

# create dummy variables for the categorical variables
# NO INTERCEPT and INCLUDE FIRST COLUMN 
BATTER_IDX_dummies <- D %>% modelr::model_matrix(~ BATTER_IDX + 0) 
BATTER_IDX_dummies <- BATTER_IDX_dummies %>% 
                      relocate(BATTER_IDX2, .after = BATTER_IDX1) %>%
                      relocate(BATTER_IDX3, .after = BATTER_IDX2) %>%
                      relocate(BATTER_IDX4, .after = BATTER_IDX3) %>%
                      relocate(BATTER_IDX5, .after = BATTER_IDX4) %>%
                      relocate(BATTER_IDX6, .after = BATTER_IDX5) %>%
                      relocate(BATTER_IDX7, .after = BATTER_IDX6) %>%
                      relocate(BATTER_IDX8, .after = BATTER_IDX7) %>%
                      relocate(BATTER_IDX9, .after = BATTER_IDX8) 

# NO INTERCEPT and INCLUDE FIRST COLUMN 
ORDER_CT_dummies <- D %>% modelr::model_matrix(~ ORDER_CT + 0)

# design matrices
X_b = as.matrix(BATTER_IDX_dummies)
X_o = as.matrix(ORDER_CT_dummies)
# response variable
y = D$EVENT_WOBA_19

# confounders
# DATA BLEED: 
X_c = D %>% select(
                   #WOBA_AVG_BAT_19, WOBA_AVG_PIT_19, 
                   WOBA_FINAL_BAT_19, WOBA_FINAL_PIT_19,
                   HAND_MATCH, BAT_HOME_IND)
                   #PIT_REST, DAYS_SINCE_SZN_START, IN_DIV, IN_LEAGUE, PITCH_COUNT_CUMU)
# FIELD_POS <chr>, OUTS_CT <chr> --> categorical too

# 
#X_n = D %>% select(NUM_WOBA_APP_BAT, NUM_WOBA_APP_PIT)

#############################
########### RSTAN ###########
#############################

tto_dat <- list(n = length(y),
                p_b = ncol(X_b),
                p_o = ncol(X_o),
                p_c = ncol(X_c),
                X_b = X_b,
                X_o = X_o,
                X_c = X_c,
                y = y) 

# compile .stan file
file = 'tto10.stan'
model <- stan_model(file = file, model_name = "tto10")
# obtain posterior samples of the parameters
fit <- sampling(model, 
                data = tto_dat, 
                include = FALSE,
                pars = c("linpred_b", "linpred_o", "linpred_c"), 
                iter = NUM_ITERS_IN_CHAIN, 
                chains=cores, cores=cores, ### for HPCC
                #chains = 1, 
                seed = 12345)

# save the stan object
saveRDS(fit, file = paste0(output_folder, "fit", OUTPUT_FILE, ".rds"))
#fit <- readRDS("job_output/fit_07_2015-2020a_1.rds") 

#############################
########### PLOTS ###########
#############################

NAMES <- c("beta_b0", "beta_b1", 
           colnames(X_b), colnames(X_o), colnames(X_c), 
           "tau_b", "tau_o", "sigma")
P <- length(NAMES)
draws <- as_tibble(as.matrix(fit))
draws <- draws[,1:P]
names(draws) <- NAMES

#################
s <- summary(fit)
ss <- s$summary[1:P,]
rownames(ss) <- names(draws)
##ss
write.csv(data.frame(ss), file = paste0(output_folder, "fit_ss", OUTPUT_FILE, ".csv"), 
          row.names=TRUE)
#################

#############################
########### PLOTS ###########
#############################

#m1=lm(data=D, EVENT_WOBA_19 ~ BATTER_IDX + ORDER_CT + WOBA_AVG_BAT_19 + WOBA_AVG_PIT_19 + HAND_MATCH + BAT_HOME_IND + PIT_REST)
#m2 = m1$coefficients

A0 = tibble(draws) %>%  
                       mutate(b11 = BATTER_IDX1 + ORDER_CT1,
                              b12 = BATTER_IDX2 + ORDER_CT1,
                              b13 = BATTER_IDX3 + ORDER_CT1,
                              b14 = BATTER_IDX4 + ORDER_CT1,
                              b15 = BATTER_IDX5 + ORDER_CT1,
                              b16 = BATTER_IDX6 + ORDER_CT1,
                              b17 = BATTER_IDX7 + ORDER_CT1,
                              b18 = BATTER_IDX8 + ORDER_CT1,
                              b19 = BATTER_IDX9 + ORDER_CT1,
                              b21 = BATTER_IDX1 + ORDER_CT2,
                              b22 = BATTER_IDX2 + ORDER_CT2,
                              b23 = BATTER_IDX3 + ORDER_CT2,
                              b24 = BATTER_IDX4 + ORDER_CT2,
                              b25 = BATTER_IDX5 + ORDER_CT2,
                              b26 = BATTER_IDX6 + ORDER_CT2,
                              b27 = BATTER_IDX7 + ORDER_CT2,
                              b28 = BATTER_IDX8 + ORDER_CT2,
                              b29 = BATTER_IDX9 + ORDER_CT2,
                              b31 = BATTER_IDX1 + ORDER_CT3,
                              b32 = BATTER_IDX2 + ORDER_CT3,
                              b33 = BATTER_IDX3 + ORDER_CT3,
                              b34 = BATTER_IDX4 + ORDER_CT3,
                              b35 = BATTER_IDX5 + ORDER_CT3,
                              b36 = BATTER_IDX6 + ORDER_CT3,
                              b37 = BATTER_IDX7 + ORDER_CT3,
                              b38 = BATTER_IDX8 + ORDER_CT3,
                              b39 = BATTER_IDX9 + ORDER_CT3) %>%
  select(b11,b12,b13,b14,b15,b16,b17,b18,b19,b21,b22,b23,b24,b25,b26,b27,b28,b29,b31,b32,b33,b34,b35,b36,b37,b38,b39)


A1 = A0 + 
    mean(draws$WOBA_CUMU_BAT) * (.3) +
    mean(draws$WOBA_CUMU_PIT) * (.3) +
    mean(draws$HAND_MATCH) * 0 +
    mean(draws$BAT_HOME_IND) * 1 +
    mean(draws$PIT_REST) * 5 +
    mean(draws$DAYS_SINCE_SZN_START) * 30 +
    mean(draws$IN_DIV) * 1 +
    mean(draws$IN_LEAGUE) * 1 +
    mean(draws$PITCH_COUNT_CUMU) * 0

A2 = reshape2::melt(A0) ##FIXME

library(ggthemes)
theme_set(theme_classic())
plot1 = A2 %>% ggplot(aes(x=variable, y=value)) + 
  geom_boxplot() +
  labs(y="wOBA", x = "time thru order number + batter index number",
       title = OUTPUT_FILE)

ggsave(paste0(output_folder, "plot", OUTPUT_FILE, ".png"), plot1)



####################################
########### CHECK IT OUT ###########
####################################

fit <- readRDS("job_output/fit_10_2015-2020b.rds") 
ss <- read_csv("job_output/fit_ss_10_2015-2020b.csv")
draws <- as_tibble(as.matrix(fit))

stan_hist(fit, pars="tau_b", include=FALSE)
stan_trace(fit, pars="tau_b", include=FALSE)

hist(draws$`beta_b[2]`)
