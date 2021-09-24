
data {
  int<lower=0> n;   // number of data items
  int<lower=0> p_b;   // number of predictors in BATTER_IDX matrix
  int<lower=0> p_o;   // number of predictors in ORDER_CT matrix
  matrix[n, p_b] X_b;   // BATTER_IDX predictor matrix
  matrix[n, p_o] X_o;   // ORDER_CT predictor matrix
  vector[n] y;      // outcome vector
}
parameters {
  // important: NO INTERCEPT because of the dummy matrices
  real beta_b0;             // prior mean for BATTER_IDX1
  real beta_o0;             // prior mean for ORDER_CT1
  vector[p_b] beta_b;       // coefficients for BATTER_IDX predictors
  vector[p_o] beta_o;       // coefficients for ORDER_CT predictors
  real<lower=0> tau_b;      // sd of normal prior for BATTER_IDX
  real<lower=0> tau_o;      // sd of normal prior for ORDER_CT
  real<lower=0> sigma;      // sd of normal likelihood
}
transformed parameters {
  vector[n] linpred_b;
  vector[n] linpred_o;
  linpred_b = X_b * beta_b;
  linpred_o = X_o * beta_o;
}
model {
  // beta_b prior
  beta_b[1] ~ normal(beta_b0, tau_b);
  {
    for (k in 2:p_b)
    // beta_b[k] ~ normal(beta_b[k-1], tau_b); 
    beta_b[k] ~ normal(beta_b[k-1], tau_b / k); 
  }
  // beta_o prior
  beta_o[1] ~ normal(beta_o0, tau_o);
  {
    for (k in 2:p_o)
    // beta_o[k] ~ normal(beta_o[k-1], tau_o); 
    beta_o[k] ~ normal(beta_o[k-1], tau_o / k); 
  }
  // likelihood
  y ~ normal(linpred_b + linpred_o, sigma); 
}



