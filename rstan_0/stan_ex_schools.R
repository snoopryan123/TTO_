library(tidyverse)
library(rstan)

library(rstanarm)
library(ggplot2)
library(ggthemes)
theme_set(theme_classic())
#theme_set(theme_wsj())


################################
#### COMPILER OPTIMIZATIONS ####
################################

dotR <- file.path(Sys.getenv("HOME"), ".R")
if (!file.exists(dotR)) dir.create(dotR)
M <- file.path(dotR, "Makevars")
if (!file.exists(M)) file.create(M)
cat("\nCXX14FLAGS += -O3 -mtune=native -arch x86_64 -ftemplate-depth-256",
    file = M, sep = "\n", append = FALSE)

# use multiple cores
options(mc.cores = parallel::detectCores())
# automatically save a bare version of a compiled Stan program to the hard disk
rstan_options(auto_write = TRUE)

################################
########### STAN EXAMPLE ###########
################################

schools_dat <- list(J = 8, 
                    y = c(28,  8, -3,  7, -1,  1, 18, 12),
                    sigma = c(15, 10, 16, 11,  9, 11, 10, 18))

fit <- stan(file = 'stan_ex_schools.stan', data = schools_dat)

print(fit)
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

