# source("sim_config.R")
IS_SIM=TRUE
SIM_NUM=1 #1 #2
# YRS=2018
YRS=2017:2019
s=1

OUTPUT_FILE = paste0("sim",SIM_NUM,"simple_model_bsnBL_", s) 
source("../model9_getData.R") ### get observed data 
source("sim_simulateData.R") ### get simulated outcomes and "true" params

### fit simple model here
library(nnet)

#########################################
### play around with the spline basis ###
aa = unique(D$BATTER_SEQ_NUM) 

# ### spline model with discontinuities between each TTO
# knots = c(rep(9.5,4), rep(18.5,4), rep(27.5,4))  
# BB_ <- bs(aa, knots=knots, degree=3, intercept = TRUE) # creating the B-splines
# colnames(BB_) = paste0("B",1:ncol(BB_))
# BB = as_tibble(BB_)
# bbb = as.matrix(BB)
# SPL = S %*% bbb 

# ### one cubic spline over batters 1...27 with 4 df
# BB_ <- bs(aa, df=4)
# colnames(BB_) = paste0("B",1:ncol(BB_))
# BB = as_tibble(BB_)
# bbb = as.matrix(BB)
# SPL = S %*% bbb ### spline model: spline basis with 4 degrees of freedom

### spline model with discontinuities between each TTO
knots = c(5,14,23)
BB_ <- bs(aa, knots=knots, degree=3, intercept = TRUE) # creating the B-splines
colnames(BB_) = paste0("B",1:ncol(BB_))
BB = as_tibble(BB_)
bbb = as.matrix(BB)
SPL = S %*% bbb 

# ### indicator model: 27 b.s.n. indicators
# SPL = S 

#########################################

m1 <- multinom(y ~ SPL + X + 0)
# m1 <- multinom(y ~ SPL + O + X)
# m1 <- multinom(y ~ SPL + O[,2:ncol(O)] + X)
# m1 <- multinom(y ~ SPL + O[,2:ncol(O)] + X + 0)

### quick view
m1
cbind(
  t(beta_mat)[2:7,],
  t(eta_mat)[2:7,]
)

### the ETA coefficients are recovered, except for the HBP and 3B ones since too little data
eta_hat = coefficients(m1)[,startsWith(colnames(coefficients(m1)), "X")]
eta_true = t(eta_mat)[2:7,]
rownames(eta_hat) = rownames(eta_true)
rowMeans(abs(eta_hat - eta_true))


