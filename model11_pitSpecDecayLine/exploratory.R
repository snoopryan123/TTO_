# # source("sim_config.R")
# IS_SIM=TRUE
# SIM_NUM=2 #1 #2
# YRS=2018
# # YRS=2017:2019
# s=1

# OUTPUT_FILE = paste0("sim",SIM_NUM,"_simple_model_bsnBL_", s) 
source("model9_getData.R") ### get observed data 

##############################################################

fold_num = 1
train_rows = which(folds != fold_num)
test_rows = which(folds == fold_num)
D_train = D[train_rows,]
S_train = S[train_rows,]
O_train = O[train_rows,]
X_train = X[train_rows,]
woba_train = y_og[train_rows]
D_test = D[test_rows,]
S_test = S[test_rows,]
O_test = O[test_rows,]
X_test = X[test_rows,]
woba_test = y_og[test_rows]
bsn_test = D_test$BATTER_SEQ_NUM

##############################################################

rmse <- function(x,y) {
  sqrt(mean( (x-y)**2 ))
}

rmse_BLOnly <- function(m, O_test, X_test) {
  rmse(cbind(1, O_test, X_test) %*% coefficients(m), woba_test) 
}

rmse_pfOnlySpl <- function(m, bbb, S_test, X_test) {
  SPL_test = S_test %*% bbb 
  m_preds = cbind(SPL_test, X_test) %*% coefficients(m)
  rmse(m_preds, woba_test)
}

rmse_pfBLSpl <- function(m, bbb, S_test, O_test, X_test) {
  SPL_test = S_test %*% bbb 
  m_preds = cbind(SPL_test, O_test, X_test) %*% coefficients(m)
  rmse(m_preds, woba_test)
}

rmse_indicators <- function(m, S_test, X_test) {
  rmse(cbind(S_test, X_test) %*% coefficients(m), woba_test)
}

rmse_eco_BLOnly <- function(m, O_test, X_test) {
  (tibble(t = bsn_test, w = woba_test, 
          pred = (cbind(1, O_test, X_test) %*% coefficients(m))[,1] ) %>%
    group_by(t) %>%
    summarise(rmse = rmse(w, pred)) %>%
    summarise(mean(rmse)))[[1]]
}

rmse_eco_pfOnlySpl <- function(m, bbb, S_test, X_test) {
  SPL_test = S_test %*% bbb 
  m_preds = cbind(SPL_test, X_test) %*% coefficients(m)
  (tibble(t = bsn_test, w = woba_test, pred = m_preds ) %>%
      group_by(t) %>%
      summarise(rmse = rmse(w, pred)) %>%
      summarise(mean(rmse)))[[1]]
}

rmse_eco_pfBLSpl <- function(m, bbb, S_test, O_test, X_test) {
  SPL_test = S_test %*% bbb 
  m_preds = cbind(SPL_test, O_test, X_test) %*% coefficients(m)
  (tibble(t = bsn_test, w = woba_test, pred = m_preds ) %>%
      group_by(t) %>%
      summarise(rmse = rmse(w, pred)) %>%
      summarise(mean(rmse)))[[1]]
}

rmse_eco_indicators <- function(m, S_test, X_test) {
  (tibble(t = bsn_test, w = woba_test, 
          pred = (cbind(S_test, X_test) %*% coefficients(m))[,1] ) %>%
     group_by(t) %>%
     summarise(rmse = rmse(w, pred)) %>%
     summarise(mean(rmse)))[[1]]
}

##############################################################

### Tango: batter learning only
m1 <- lm(woba_train ~ 1 + O_train + X_train)

### pitcher fatigue only: one SPL_trainine over t=1,...,27 of degree 3
aa = unique(D$BATTER_SEQ_NUM) 
bbb_3 = as.matrix(as_tibble(bs(aa, df=3)))
colnames(bbb_3) = paste0("B",1:ncol(bbb_3))
SPL_train_3 = S_train %*% bbb_3 
m3 <- lm(woba_train ~ 0 + SPL_train_3 + X_train)

### pitcher fatigue only: one SPL_trainine over t=1,...,27 of degree 4
aa = unique(D$BATTER_SEQ_NUM) 
bbb_4 = as.matrix(as_tibble(bs(aa, df=4, intercept = TRUE)))
colnames(bbb_4) = paste0("B",1:ncol(bbb_4))
SPL_train_4 = S_train %*% bbb_4 
m4 <- lm(woba_train ~ 0 + SPL_train_4 + X_train)

### pitcher fatigue only: one SPL_trainine over t=1,...,27 of degree 5
aa = unique(D$BATTER_SEQ_NUM) 
bbb_5 = as.matrix(as_tibble(bs(aa, df=5, intercept = TRUE)))
colnames(bbb_5) = paste0("B",1:ncol(bbb_5))
SPL_train_5 = S_train %*% bbb_5 
m5 <- lm(woba_train ~ 0 + SPL_train_5 + X_train)

### pitcher fatigue only: one SPL_trainine over t=1,...,27 of degree 6
aa = unique(D$BATTER_SEQ_NUM) 
bbb_6 = as.matrix(as_tibble(bs(aa, df=6, intercept = TRUE)))
colnames(bbb_6) = paste0("B",1:ncol(bbb_6))
SPL_train_6 = S_train %*% bbb_6 
m6 <- lm(woba_train ~ 0 + SPL_train_6 + X_train)

### pitcher fatigue only: one SPL_trainine over t=1,...,27 of degree 7
aa = unique(D$BATTER_SEQ_NUM) 
bbb_7 = as.matrix(as_tibble(bs(aa, df=7, intercept = TRUE)))
colnames(bbb_7) = paste0("B",1:ncol(bbb_7))
SPL_train_7 = S_train %*% bbb_7 
m7 <- lm(woba_train ~ 0 + SPL_train_7 + X_train)

### pitcher fatigue only: one SPL_trainine over t=1,...,27 of degree 8
aa = unique(D$BATTER_SEQ_NUM) 
bbb_8 = as.matrix(as_tibble(bs(aa, df=8, intercept = TRUE)))
colnames(bbb_8) = paste0("B",1:ncol(bbb_8))
SPL_train_8 = S_train %*% bbb_8 
m8 <- lm(woba_train ~ 0 + SPL_train_8 + X_train)

### pitcher fatigue only: one SPL_trainine over t=1,...,27 of degree 9
aa = unique(D$BATTER_SEQ_NUM) 
bbb_9 = as.matrix(as_tibble(bs(aa, df=9, intercept = TRUE)))
colnames(bbb_9) = paste0("B",1:ncol(bbb_9))
SPL_train_9 = S_train %*% bbb_9 
m9 <- lm(woba_train ~ 0 + SPL_train_9 + X_train)

### pitcher fatigue only: one SPL_trainine over t=1,...,27 of degree 10
aa = unique(D$BATTER_SEQ_NUM) 
bbb_10 = as.matrix(as_tibble(bs(aa, df=10, intercept = TRUE)))
colnames(bbb_10) = paste0("B",1:ncol(bbb_10))
SPL_train_10 = S_train %*% bbb_10 
m10 <- lm(woba_train ~ 0 + SPL_train_10 + X_train)

### pitcher fatigue only: one SPL_trainine over t=1,...,27 of degree 11
aa = unique(D$BATTER_SEQ_NUM) 
bbb_11 = as.matrix(as_tibble(bs(aa, df=11, intercept = TRUE)))
colnames(bbb_11) = paste0("B",1:ncol(bbb_11))
SPL_train_11 = S_train %*% bbb_11 
m11 <- lm(woba_train ~ 0 + SPL_train_11 + X_train)

### pitcher fatigue only: one SPL_trainine over t=1,...,27 of degree 12
aa = unique(D$BATTER_SEQ_NUM) 
bbb_12 = as.matrix(as_tibble(bs(aa, df=12, intercept = TRUE)))
colnames(bbb_12) = paste0("B",1:ncol(bbb_12))
SPL_train_12 = S_train %*% bbb_12 
m12 <- lm(woba_train ~ 0 + SPL_train_12 + X_train)

### pitcher fatigue only: one SPL_trainine over t=1,...,27 of degree 13
aa = unique(D$BATTER_SEQ_NUM) 
bbb_13 = as.matrix(as_tibble(bs(aa, df=13, intercept = TRUE)))
colnames(bbb_13) = paste0("B",1:ncol(bbb_13))
SPL_train_13 = S_train %*% bbb_13 
m13 <- lm(woba_train ~ 0 + SPL_train_13 + X_train)

### pitcher fatigue and batter learning: one SPL_trainine over t=1,...,27 of degree 4 + BL bumps
m4a <- lm(woba_train ~ 0 + SPL_train_4 + O_train + X_train)
m5a <- lm(woba_train ~ 0 + SPL_train_5 + O_train + X_train)
m6a <- lm(woba_train ~ 0 + SPL_train_6 + O_train + X_train)
m7a <- lm(woba_train ~ 0 + SPL_train_7 + O_train + X_train)
m8a <- lm(woba_train ~ 0 + SPL_train_8 + O_train + X_train)
m9a <- lm(woba_train ~ 0 + SPL_train_9 + O_train + X_train)
m10a <- lm(woba_train ~ 0 + SPL_train_10 + O_train + X_train)
m11a <- lm(woba_train ~ 0 + SPL_train_11 + O_train + X_train)

### indicator models
mi <- lm(woba_train ~ 0 + S_train + X_train)

### smoothing spline over indicator model
mi1 <- mi
mi1$coefficients[1:27] <- smooth.spline(coefficients(mi1)[1:27])$y

### test: pitcher fatigue only models
rmse_pfOnlySpl(m4, bbb_4, S_test, X_test)
rmse_pfOnlySpl(m5, bbb_5, S_test, X_test)
rmse_pfOnlySpl(m6, bbb_6, S_test, X_test)
rmse_pfOnlySpl(m7, bbb_7, S_test, X_test)
rmse_pfOnlySpl(m8, bbb_8, S_test, X_test)
rmse_pfOnlySpl(m9, bbb_9, S_test, X_test)
rmse_pfOnlySpl(m10, bbb_10, S_test, X_test) ### best spline
rmse_pfOnlySpl(m11, bbb_11, S_test, X_test)

### test: pitcher fatigue and batter learning models
rmse_pfBLSpl(m4a, bbb_4, S_test, O_test, X_test)
rmse_pfBLSpl(m5a, bbb_5, S_test, O_test, X_test)
rmse_pfBLSpl(m6a, bbb_6, S_test, O_test, X_test)
rmse_pfBLSpl(m7a, bbb_7, S_test, O_test, X_test)
rmse_pfBLSpl(m8a, bbb_8, S_test, O_test, X_test)
rmse_pfBLSpl(m9a, bbb_9, S_test, O_test, X_test)
rmse_pfBLSpl(m10a, bbb_10, S_test, O_test, X_test) ### best spline
rmse_pfBLSpl(m11a, bbb_11, S_test, O_test, X_test)

### test: out-of-sample rmse
rmse_BLOnly(m1, O_test, X_test) ### batter learning only
rmse_pfOnlySpl(m4, bbb_4, S_test, X_test) ### pitcher fatigue only
rmse_pfBLSpl(m4a, bbb_4, S_test, O_test, X_test) ### best spline
rmse_indicators(mi, S_test, X_test)
rmse_indicators(mi1, S_test, X_test) ### best!

### test: out-of-sample ecological rmse
rmse_eco_BLOnly(m1, O_test, X_test) ### batter learning only
rmse_eco_pfOnlySpl(m4, bbb_4, S_test, X_test) ### pitcher fatigue only
rmse_eco_pfBLSpl(m4a, bbb_4, S_test, O_test, X_test) ### best spline
rmse_eco_indicators(mi, S_test, X_test)
rmse_eco_indicators(mi1, S_test, X_test) ### best!

##############################################################

### plot
S_plot = diag(27)
O_plot = matrix( c(rep(0,9),rep(1,9),rep(0,9),rep(0,9),rep(0,9),rep(1,9)), nrow=27 )
X_plot = matrix(rep(c(logit(0.315), logit(0.315), 1, 0), 27), nrow=27, byrow=TRUE)

plot_xwOverTime <- function(preds) {
  tibble(t=1:27, xw=preds) %>%
    ggplot(aes(x=t,y=xw)) +
    geom_point()
}

plot_xwOverTime(cbind(1, O_plot, X_plot) %*% coefficients(m1))

plot_xwOverTime(cbind(S_plot%*%bbb_4, X_plot) %*% coefficients(m4))
plot_xwOverTime(cbind(S_plot%*%bbb_10, X_plot) %*% coefficients(m10))

plot_xwOverTime(cbind(S_plot%*%bbb_4, O_plot, X_plot) %*% coefficients(m4a))
plot_xwOverTime(cbind(S_plot%*%bbb_10, O_plot, X_plot) %*% coefficients(m10a))

plot_xwOverTime(cbind(S_plot, X_plot) %*% coefficients(mi1))

### smooth pitcher fatigue effect looks right...
plot_xwOverTime(cbind(S_plot, X_plot) %*% coefficients(mi)) +
  geom_line(aes(x=t, y=smooth.spline(xw)$y))

