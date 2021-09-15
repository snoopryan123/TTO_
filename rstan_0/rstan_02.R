library(tidyverse)
library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

################################
#### COMPILER OPTIMIZATIONS ####
################################

# dotR <- file.path(Sys.getenv("HOME"), ".R")
# if (!file.exists(dotR)) dir.create(dotR)
# M <- file.path(dotR, "Makevars")
# if (!file.exists(M)) file.create(M)
# cat("\nCXX14FLAGS += -O3 -mtune=native -arch x86_64 -ftemplate-depth-256",
#     file = M, sep = "\n", append = FALSE)


################################
########### THE DATA ###########
################################

# read data
#D <- read_csv("design_matrix_0.csv")
D <- read_csv("design_matrix_0.csv", col_types = "ddddddddddcccc")
names(D)


X0 = D %>% select(EVENT_WOBA, WOBA_CUMU_BAT, WOBA_CUMU_PIT) %>% drop_na()
y = X0$EVENT_WOBA # response variable
X0 = X0 %>% select(-EVENT_WOBA)
X = as.matrix(X0) # design matrix

############################
########## RSTAN ###########
############################

tto2_dat <- list(n = nrow(X),
                 p = ncol(X),
                 X = X,
                 y = y) 

fit <- stan(file = 'tto2.stan', data = tto2_dat, iter = 1000, chains = 3)

print(fit)
colnames(X)
stan_hist(fit)

############################
######### RSTANARM #########
############################

library(rstanarm)

post1 <- stan_glm(y ~ .  ,# + WOBA_CUMU_BAT + WOBA_CUMU_PIT + DAYS_SINCE_SZN_START, 
                  data = as.data.frame(X,y),
                  family = gaussian(link = "identity"),
                  seed = 12345)
post1
stan_hist(post1)
