data {
  int<lower=2> K;             // number of categories
  int<lower=0> n;             // number of datapoints
  int<lower=0> p_u;           // number of predictors of U matrix
  int<lower=0> p_o;           // number of predictors of O matrix
  int<lower=0> p_x;           // number of predictors of X matrix
  matrix[n,p_u] U;            // batter sequence number matrix
  matrix[n,p_o] O;            // batter sequence number matrix
  matrix[n,p_x] X;            // adjustments matrix
  int<lower=1, upper=K> y[n]; // outcome vector
}
transformed data {
  vector[p_u] zeros_beta = rep_vector(0, p_u);    // category 1 is 0
  vector[p_x] zeros_eta = rep_vector(0, p_x);     // category 1 is 0
  vector[p_o - 1] zeros_gamma = rep_vector(0, p_o - 1);   // category 1 is 0 (without gamma_1)
  row_vector[K] zero_gamma1 = rep_vector(0, K)';   // force gamma_1 to be 0
}
parameters {
  matrix[p_u,K-1] beta_raw;           // unique batter index parameters 
  matrix[p_o - 1,K-1] gamma_raw;          // order count parameters (without gamma_1)
  matrix[p_x,K-1] eta_raw;            // adjustment parameters
}
transformed parameters {
  matrix[p_u,K] beta;            
  matrix[p_o - 1,K] gamma_wo_g1;    // gamma without gamma_1
  matrix[p_o,K] gamma;  
  matrix[p_x,K] eta;            
  matrix[n, K] linpred;
  eta = append_col(zeros_eta, eta_raw); // category 1 (out) (col 1) is base category
  beta = append_col(zeros_beta, beta_raw); // category 1 (out) (col 1) is base category
  gamma_wo_g1 = append_col(zeros_gamma, gamma_raw); // category 1 (out) (col 1) is base category
  gamma = append_row(zero_gamma1, gamma_wo_g1); // category 1 (out) (col 1) is base category
  linpred = U*beta + O*gamma + X*eta;  // linear prediction
}
model {
  // // std. normal priors
  to_vector(beta_raw) ~ normal(0,1);
  to_vector(gamma_raw) ~ normal(0,1);
  to_vector(eta_raw) ~ normal(0,1);

  for (i in 1:n) {
    // likelihood
    y[i] ~ categorical_logit(linpred[i]');
  }
}




