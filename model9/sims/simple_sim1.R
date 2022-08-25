# source("sim_config.R")
IS_SIM=TRUE
SIM_NUM=2 #1 #2
YRS=2018
# YRS=2017:2019
s=1

OUTPUT_FILE = paste0("sim",SIM_NUM,"simple_model_bsnBL_", s) 
source("../model9_getData.R") ### get observed data 
source("sim_simulateData.R") ### get simulated outcomes and "true" params

### fit simple model here
library(nnet)

###########################################################

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

### assess parameter fit of BETA
assess_beta_fit <- function(m, byCol=TRUE) {
  beta_hat = coefficients(m)[,startsWith(colnames(coefficients(m)), "O")]
  beta_true = t(beta_mat)[2:7,]
  if (byCol) {
    colnames(beta_true) = colnames(beta_hat)
    colMeans(abs(beta_hat - beta_true))
  } else {
    rownames(beta_hat) = rownames(beta_true)
    rowMeans(abs(beta_hat - beta_true))
  }
}

################################################################
### models using BSN directly and without beta_2k or beta_3k ###

###
m1a <- multinom(y ~ D$BATTER_SEQ_NUM + X)

### sim1: m1a recovered eta
assess_eta_fit(m1a)

### sim1: m1a recovered alpha_seq_27
alpha_incpt_m1a = t( coefficients(m1a)[,startsWith(colnames(coefficients(m1a)), "(Int")] )
alpha_slope_m1a = t( coefficients(m1a)[,startsWith(colnames(coefficients(m1a)), "D$B")] )
alpha_27_m1a = t(alpha_slope_m1a)%*%t(matrix(1:27)) + t(alpha_incpt_m1a)%*%t(matrix(rep(1,27)))
rowMeans( abs(t(alpha_27_m1a) - alpha_mat[,2:ncol(alpha_mat)]) )


###
m2a <- multinom(y ~ D$BATTER_SEQ_NUM + I(D$BATTER_SEQ_NUM^2) + I(D$BATTER_SEQ_NUM^3) + X)

### sim1: m2a recovered eta
assess_eta_fit(m2a)

### sim1: m2a recovered alpha_seq_27
alpha_hat_m2a = coefficients(m2a)[,1:4]
alpha_27_m2a = alpha_hat_m2a %*% t(outer(1:27, seq(0, 3), `^`))
rowMeans( abs(t(alpha_27_m2a) - alpha_mat[,2:ncol(alpha_mat)]) )


### same BSN cubic as m2a, but this time with matrices
S_cubic = S %*% outer(1:27, seq(0, 3), `^`)
m3a <- multinom(y ~ S_cubic + X + 0)

### sim1: m3a recovered eta
assess_eta_fit(m3a)

### sim1: m3a recovered alpha_seq_27
alpha_hat_m3a = coefficients(m3a)[,1:4]
alpha_27_m3a = alpha_hat_m3a %*% t(outer(1:27, seq(0, 3), `^`))
rowMeans( abs(t(alpha_27_m3a) - alpha_mat[,2:ncol(alpha_mat)]) )


##############################################################
### models using BSN spline and without beta_2k or beta_3k ###
aa = unique(D$BATTER_SEQ_NUM) 

### one cubic spline over batters 1...27 with 4 df, directly with matrices
bbb_1b = as.matrix(as_tibble(bs(aa, df=4)))
colnames(bbb_1b) = paste0("B",1:ncol(bbb_1b))
SPL_1b = S %*% bbb_1b 

m1b <- multinom(y ~ 1 + SPL_1b + X)

### sim1: m1b recovered eta
assess_eta_fit(m1b)

### sim1: m1b recovered alpha_seq_27
alpha_incpt_m1b = t( coefficients(m1b)[,startsWith(colnames(coefficients(m1b)), "(Int")] )
alpha_spline_m1b = coefficients(m1b)[,2:5]
alpha_27_m1b =   alpha_spline_m1b %*% t(bbb_1b)  +  t(alpha_incpt_m1b) %*% t(matrix(rep(1,27)))
rowMeans( abs(t(alpha_27_m1b) - alpha_mat[,2:ncol(alpha_mat)]) )


### one cubic spline over batters 1...27 with 4 df, directly with matrices, with intercept in spline
bbb_2b = as.matrix(as_tibble(bs(aa, df=4, intercept = TRUE)))
colnames(bbb_2b) = paste0("B",1:ncol(bbb_2b))
SPL_2b = S %*% bbb_2b 

m2b <- multinom(y ~ 0 + SPL_2b + X)

### sim1: m2b recovered eta
assess_eta_fit(m2b)

### sim1: m2b recovered alpha_seq_27
alpha_spline_m2b = coefficients(m2b)[,1:4]
alpha_27_m2b = alpha_spline_m2b %*% t(bbb_2b)  
rowMeans( abs(t(alpha_27_m2b) - alpha_mat[,2:ncol(alpha_mat)]) )


### one cubic spline over batters 1...27 with knots at the start/end of each TTO
knots_3b = c(1,9,18,27)
bbb_3b = as.matrix(as_tibble(bs(aa, knots=knots_3b, degree=3, intercept=TRUE))) 
colnames(bbb_3b) = paste0("B",1:ncol(bbb_3b))
SPL_3b = S %*% bbb_3b 

m3b <- multinom(y ~ 0 + SPL_3b + X)

### sim1: m3b recovered eta
assess_eta_fit(m3b)

### sim1: m3b recovered alpha_seq_27
alpha_spline_m3b = coefficients(m3b)[,1:8]
alpha_27_m3b =   alpha_spline_m3b %*% t(bbb_3b) 
rowMeans( abs(t(alpha_27_m3b) - alpha_mat[,2:ncol(alpha_mat)]) )


### one cubic spline over batters 1...27 with knots just between each TTO
knots_4b = c(9,18)
bbb_4b = as.matrix(as_tibble(bs(aa, knots=knots_4b, degree=3, intercept=TRUE))) 
colnames(bbb_4b) = paste0("B",1:ncol(bbb_4b))
SPL_4b = S %*% bbb_4b 

m4b <- multinom(y ~ 0 + SPL_4b + X)

### sim1: m4b recovered eta
assess_eta_fit(m4b)

### sim1: m4b recovered alpha_seq_27
alpha_spline_m4b = coefficients(m4b)[,1:6]
alpha_27_m4b =   alpha_spline_m4b %*% t(bbb_4b) 
rowMeans( abs(t(alpha_27_m4b) - alpha_mat[,2:ncol(alpha_mat)]) )


### one cubic spline over batters 1...27 with knots just in the middle of each TTO
knots_5b = c(5,14,23)
bbb_5b = as.matrix(as_tibble(bs(aa, knots=knots_5b, degree=3, intercept=TRUE))) 
colnames(bbb_5b) = paste0("B",1:ncol(bbb_5b))
SPL_5b = S %*% bbb_5b 

m5b <- multinom(y ~ 0 + SPL_5b + X)

### sim1: m5b recovered eta
assess_eta_fit(m5b)

### sim1: m5b recovered alpha_seq_27
alpha_spline_m5b = coefficients(m5b)[,1:7]
alpha_27_m5b =   alpha_spline_m5b %*% t(bbb_5b) 
rowMeans( abs(t(alpha_27_m5b) - alpha_mat[,2:ncol(alpha_mat)]) )


### spline model with discontinuities between each TTO (3 cubics)
knots_6b = c(rep(9.5,4), rep(18.5,4)) #c(rep(9.5,4), rep(18.5,4), rep(27.5,4))
bbb_6b = as.matrix(as_tibble(bs(aa, knots=knots_6b, degree=3, intercept=TRUE))) 
colnames(bbb_6b) = paste0("B",1:ncol(bbb_6b))
SPL_6b = S %*% bbb_6b 

m6b <- multinom(y ~ 0 + SPL_6b + X)

### sim1: m6b recovered eta
assess_eta_fit(m6b)

### sim1: m6b recovered alpha_seq_27 (albeit less well)
alpha_spline_m6b = coefficients(m6b)[,1:12]
alpha_27_m6b =   alpha_spline_m6b %*% t(bbb_6b) 
rowMeans( abs(t(alpha_27_m6b) - alpha_mat[,2:ncol(alpha_mat)]) )


#########################################################
### models using BSN directly with beta_2k or beta_3k ###

###
m1c <- multinom(y ~ D$BATTER_SEQ_NUM + O + X)

### sim1 and sim2: m1c recovered eta
assess_eta_fit(m1c)

### sim1: hard to tell if beta is significantly nonzero without confidence intervals
### sim1: probably a false positive for TTOP
### sim2: for the nonzero coefficients, it finds the TTOP. for the zero coeffs, it finds a false positive for TTOP
m1c
assess_beta_fit(m1c)

### sim1: m1c recovers the overall alpha_seq_27 trend
### sim1: m1c did NOT recover the specific alpha_seq_27 in 3TTO
alpha_incpt_m1c = t( coefficients(m1c)[,startsWith(colnames(coefficients(m1c)), "(Int")] )
alpha_slope_m1c = t( coefficients(m1c)[,startsWith(colnames(coefficients(m1c)), "D$B")] )
beta_hat_m1c = coefficients(m1c)[,startsWith(colnames(coefficients(m1c)), "O")]
alpha_27_m1c_0 = t(alpha_slope_m1c)%*%t(matrix(1:27)) + t(alpha_incpt_m1c)%*%t(matrix(rep(1,27)))
alpha_27_m1c = alpha_27_m1c_0 +
  matrix(beta_hat_m1c[,1]) %*% c(rep(0,9),rep(1,9),rep(0,9)) +
  matrix(beta_hat_m1c[,2]) %*% c(rep(0,9),rep(0,9),rep(1,9))
rowMeans( abs(t(alpha_27_m1c) - alpha_mat[,2:ncol(alpha_mat)]) )
rowMeans( abs(t(alpha_27_m1c_0) - alpha_mat[,2:ncol(alpha_mat)]) )


### 
S_cubic = S %*% outer(1:27, seq(0, 3), `^`)
m2c <- multinom(y ~ S_cubic + O + X + 0)

### sim1 and sim2: m2c recovered eta
assess_eta_fit(m2c)

### sim1: hard to tell if beta is significantly nonzero without confidence intervals
### sim1: probably a false positive for TTOP
### sim2: for the nonzero coefficients, it finds the TTOP. for the zero coeffs, it finds a false positive for TTOP
print(m2c); print(beta_mat);
assess_beta_fit(m2c)

### sim1: m2c recovers the overall alpha_seq_27 trend
### sim1: m2c did NOT recover the specific alpha_seq_27 in 3TTO
alpha_hat_m2c = coefficients(m2c)[,startsWith(colnames(coefficients(m2c)), "S")]
beta_hat_m2c = coefficients(m2c)[,startsWith(colnames(coefficients(m2c)), "O")]
alpha_27_m2c_0 = alpha_hat_m2c %*% t(outer(1:27, seq(0, 3), `^`))
alpha_27_m2c = alpha_27_m2c_0 +
  matrix(beta_hat_m2c[,1]) %*% c(rep(0,9),rep(1,9),rep(0,9)) +
  matrix(beta_hat_m2c[,2]) %*% c(rep(0,9),rep(0,9),rep(1,9))
rowMeans( abs(t(alpha_27_m2c) - alpha_mat[,2:ncol(alpha_mat)]) )
rowMeans( abs(t(alpha_27_m2c_0) - alpha_mat[,2:ncol(alpha_mat)]) )


##############################################################
### models using BSN spline with beta_2k or beta_3k ###
aa = unique(D$BATTER_SEQ_NUM) 

### one cubic spline over batters 1...27 with 4 df, directly with matrices, with intercept in spline
bbb_1d = as.matrix(as_tibble(bs(aa, df=4, intercept = TRUE)))
colnames(bbb_1d) = paste0("B",1:ncol(bbb_1d))
SPL_1d = S %*% bbb_1d 

m1d <- multinom(y ~ 0 + SPL_1d + O + X)

### sim1 and sim2: m1d recovered eta
assess_eta_fit(m1d)

### sim1: hard to tell if beta is significantly nonzero without confidence intervals
### sim1: probably a false positive for TTOP
### sim2: for the nonzero coefficients, it finds the TTOP. for the zero coeffs, it finds a false positive for TTOP
m1d
assess_beta_fit(m1d)

### sim1: m1d recovers the overall alpha_seq_27 trend
### sim1: m1d did NOT recover the specific alpha_seq_27 in 3TTO
alpha_spline_m1d = coefficients(m1d)[,startsWith(colnames(coefficients(m1d)), "SPL")]
beta_hat_m1d = coefficients(m1d)[,startsWith(colnames(coefficients(m1d)), "O")]
alpha_27_m1d_0 = alpha_spline_m1d %*% t(bbb_1d)  
alpha_27_m1d = alpha_27_m1d_0 +
  matrix(beta_hat_m1d[,1]) %*% c(rep(0,9),rep(1,9),rep(0,9)) +
  matrix(beta_hat_m1d[,2]) %*% c(rep(0,9),rep(0,9),rep(1,9))
rowMeans( abs(t(alpha_27_m1d) - alpha_mat[,2:ncol(alpha_mat)]) )
rowMeans( abs(t(alpha_27_m1d_0) - alpha_mat[,2:ncol(alpha_mat)]) )










