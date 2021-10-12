
data {
  int<lower=0> n;       // number of datapoints (rows)
  int<lower=0> p_b;     // number of predictors in BATTER_IDX matrix
  int<lower=0> p_o;     // number of predictors in ORDER_CT matrix
  int<lower=0> p_c;     // number of predictors in other-covariates matrix
  matrix[n, p_b] X_b;   // BATTER_IDX predictor matrix
  matrix[n, p_o] X_o;   // ORDER_CT predictor matrix
  matrix[n, p_c] X_c;   // other-covariates matrix
  vector[n] y;          // outcome vector
}
parameters {
  // sd of normal likelihood
  real<lower=0> sigma;      
  // important: NO INTERCEPT because of the dummy matrices
  // BATTER_IDX, ORDER_CT prior parameters
  vector[p_b] beta_b;       // coefficients for BATTER_IDX predictors
  vector[p_o] beta_o;       // coefficients for ORDER_CT predictors
  vector[p_c] beta_c;       // coefficients for other-covariates 
}
transformed parameters {
  vector[n] linpred_b;
  vector[n] linpred_o;
  vector[n] linpred_c;
  linpred_b = X_b * beta_b;
  linpred_o = X_o * beta_o;
  linpred_c = X_c * beta_c;
}
model {
  // std. normal priors for standardized covariates
  to_vector(beta_b) ~ normal(0,1);
  to_vector(beta_o) ~ normal(0,1);
  to_vector(beta_c) ~ normal(0,1);
  
  // likelihood
  y ~ normal(linpred_b + linpred_o + linpred_c, sigma); 
}
generated quantities {
  vector[n] log_lik;
  for (i in 1:n)
    log_lik[i] = normal_lpdf(y[i] | linpred_b[i] + linpred_o[i] + linpred_c[i], sigma);
}



