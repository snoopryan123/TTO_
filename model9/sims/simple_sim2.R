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

### spline model with discontinuities between each TTO
knots = c(rep(9.5,4), rep(18.5,4), rep(27.5,4))
BB_ <- bs(aa, knots=knots, degree=3, intercept = TRUE) # creating the B-splines
colnames(BB_) = paste0("B",1:ncol(BB_))
BB = as_tibble(BB_)
bbb1 = as.matrix(BB)
SPL1 = S %*% bbb1

### one cubic spline over batters 1...27 with 3 df
BB_ <- bs(aa, df=3)
colnames(BB_) = paste0("B",1:ncol(BB_))
BB = as_tibble(BB_)
bbb3 = as.matrix(BB)
SPL3 = S %*% bbb3 ### spline model: spline basis with 4 degrees of freedom

### one cubic spline over batters 1...27 with 4 df
BB_ <- bs(aa, df=4)
colnames(BB_) = paste0("B",1:ncol(BB_))
BB = as_tibble(BB_)
bbb4 = as.matrix(BB)
SPL4 = S %*% bbb4 ### spline model: spline basis with 4 degrees of freedom

# ### one  spline model with knots
knots = c(5,14,23)
BB_ <- bs(aa, knots=knots, degree=3, intercept = TRUE) # creating the B-splines
colnames(BB_) = paste0("B",1:ncol(BB_))
BB = as_tibble(BB_)
bbb5 = as.matrix(BB)
SPL5 = S %*% bbb5

#########################################

### assess parameter fit of ETA
assess_eta_fit <- function(m, byCol=TRUE) {
  eta_hat = coefficients(m)[,startsWith(colnames(coefficients(m)), "X")]
  eta_true = t(eta_mat)[2:7,]
  if (byCol) {
    colnames(eta_true) = colnames(eta_hat)
    colMeans(abs(eta_hat - eta_true))
  } else {
    rownames(eta_hat) = rownames(eta_true)
    rowMeans(abs(eta_hat - eta_true))
  }
}

### m1
m1 <- multinom(y ~ S + X + 0)
assess_eta_fit(m3b)
alpha_27_m3b = t( coefficients(m3b)[,startsWith(colnames(coefficients(m3b)), "SPL")] %*% t(bbb3) )
rowMeans( abs(alpha_27_m3b - alpha_mat[,2:ncol(alpha_mat)]) )










#########################################

m2 <- multinom(y ~ S + O + X + 0)

m3 <- multinom(y ~ SPL3 + O + X + 0)
m3b <- multinom(y ~ SPL3 + X + 0)

m4 <- multinom(y ~ SPL4 + O + X + 0)
m5 <- multinom(y ~ SPL5 + O + X + 0)
m6 <- multinom(y ~ SPL1 + O + X + 0)

# m1 <- multinom(y ~ SPL + O + X + 0)
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
assess_eta_fit(m3)
assess_eta_fit(m3, byCol=FALSE)

### assess fit of batter_seq_num params
alpha_mat[,2:ncol(alpha_mat)]

### assess m3 fit of batter_seq_num params
alpha_27_m3 = t( coefficients(m3)[,startsWith(colnames(coefficients(m3)), "SPL")] %*% t(bbb3) )
rowMeans( abs(alpha_27_m3 - alpha_mat[,2:ncol(alpha_mat)]) )

### assess m3b fit of batter_seq_num params
assess_eta_fit(m3b)
alpha_27_m3b = t( coefficients(m3b)[,startsWith(colnames(coefficients(m3b)), "SPL")] %*% t(bbb3) )
rowMeans( abs(alpha_27_m3b - alpha_mat[,2:ncol(alpha_mat)]) )

