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

# create dummy variables for the categorical variables
BATTER_IDX_dummies <- D %>% modelr::model_matrix(~ BATTER_IDX)
BATTER_IDX_dummies <- BATTER_IDX_dummies %>% 
                      relocate(BATTER_IDX2, .after = `(Intercept)`) %>%
                      relocate(BATTER_IDX3, .after = BATTER_IDX2) %>%
                      relocate(BATTER_IDX4, .after = BATTER_IDX3) %>%
                      relocate(BATTER_IDX5, .after = BATTER_IDX4) %>%
                      relocate(BATTER_IDX6, .after = BATTER_IDX5) %>%
                      relocate(BATTER_IDX7, .after = BATTER_IDX6) %>%
                      relocate(BATTER_IDX8, .after = BATTER_IDX7) %>%
                      relocate(BATTER_IDX9, .after = BATTER_IDX8) %>%
                      relocate(BATTER_IDX10, .after = BATTER_IDX9) %>%
                      relocate(BATTER_IDX11, .after = BATTER_IDX10) %>%
                      relocate(BATTER_IDX12, .after = BATTER_IDX11) %>%
                      relocate(BATTER_IDX13, .after = BATTER_IDX12) 
ORDER_CT_dummies <- D %>% modelr::model_matrix(~ ORDER_CT) %>% select(-`(Intercept)`)

# design matrix
X0 = bind_cols(BATTER_IDX_dummies, ORDER_CT_dummies)
X = as.matrix(X0)

# response variable
y = D$EVENT_WOBA

############################
########### STAN ###########
############################

tto3_dat <- list(n = nrow(X),
                 p = ncol(X),
                 X = X,
                 y = y) 

fit <- stan(file = 'tto3.stan', data = tto3_dat, iter = 1000, chains = 3)

print(fit)
stan_diag(fit)

pars = c("alpha", 
         paste(rep("BATTER_IDX_",12), 2:13, sep = ""),
         paste(rep("ORDER_CT_",3), 2:4, sep = "")) #,"sigma")

k = ncol(X)
pars <- c("alpha", paste(rep("beta[",k), 1:k, rep("]",k), sep = ""), "sigma")
stan_hist(fit, pars=pars)

stan_hist(fit, include=TRUE)

plot(fit)
pairs(fit, pars = c("mu", "tau", "lp__"))

la <- rstan::extract(fit, permuted = TRUE) # return a list of arrays 
mu <- la$mu 

### return an array of three dimensions: iterations, chains, parameters 
#a <- rstan::extract(fit, permuted = FALSE) 

### use S3 functions on stanfit objects
a2 <- as.array(fit)
m <- as.matrix(fit)
d <- as.data.frame(fit)

