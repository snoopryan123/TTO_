library(tidyverse)
library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

# stan tutorial
# http://webpages.math.luc.edu/~ebalderama/bayes_resources/code/mlr_stan.html

load("stan_ex_gop_data.RData")

# data clean
junk <- is.na(Y + rowSums(X))
Y <- Y[!junk]
X <- X[!junk,]
X <- scale(X)
n <- length(Y)
k <- ncol(X)
dim(X)

############################
########## RSTAN ###########
############################

dat <- list(n = n, k = k, X = X, Y = Y)
#fit <- stan(file = 'mlr_flat.stan', data = dat)
fit <- stan(file = 'stan_ex_gop.stan', data = dat, iter = 1000, chains = 3) 
print(fit)

pars <- c("alpha", paste(rep("beta[",k), 1:k, rep("]",k), sep = ""), "sigma")
stan_hist(fit, pars = pars)
stan_dens(fit, pars = pars)
stan_trace(fit, pars = pars)
stan_ac(fit, pars = pars)
stan_scat(fit, pars = c("beta[6]", "beta[8]"))

############################
######### RSTANARM #########
############################

library(rstanarm)

post1 <- stan_glm(Y ~ .  ,# + WOBA_CUMU_BAT + WOBA_CUMU_PIT + DAYS_SINCE_SZN_START, 
                  data = as.data.frame(X,Y),
                  family = gaussian(link = "identity"),
                  seed = 12345)
post1
