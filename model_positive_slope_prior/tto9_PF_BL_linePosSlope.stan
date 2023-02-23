data {
  int<lower=2> K;             // number of categories
  int<lower=0> n;             // number of datapoints
  ////int<lower=0> p_s;           // number of predictors of S matrix == 1
  int<lower=0> p_o;           // number of predictors of O matrix
  int<lower=0> p_x;           // number of predictors of X matrix
  matrix[n,1] INCPT;          // intercept matrix
  matrix[n,1] S;              // batter sequence number matrix
  matrix[n,p_o] O;            // batter learning (order count) matrix
  matrix[n,p_x] X;            // adjustments matrix
  int<lower=1, upper=K> y[n]; // outcome vector
  real<lower=0> row_weights[n];  // row weights
}
transformed data {
  vector[1] zeros_alpha_incpt = rep_vector(0, 1);
  vector[1] zeros_alpha_slope = rep_vector(0, 1);
  vector[p_o] zeros_beta = rep_vector(0, p_o);
  vector[p_x] zeros_eta = rep_vector(0, p_x);
}
parameters {
  matrix[1,K-1] alpha_incpt_raw;            // intercept parameters 
  matrix<lower=0>[1,K-1] alpha_slope_raw;   // batter sequence number slope parameters 
  matrix[p_o,K-1] beta_raw;                 // batter learning parameters 
  matrix[p_x,K-1] eta_raw;                  // adjustment parameters
}
transformed parameters {
  matrix[1,K] alpha_incpt;
  matrix<lower=0>[1,K] alpha_slope;
  matrix[p_o,K] beta;
  matrix[p_x,K] eta;
  matrix[n, K] linpred;
  alpha_incpt = append_col(zeros_alpha_incpt, alpha_incpt_raw); // category 1 (out) (col 1) is base category
  alpha_slope = append_col(zeros_alpha_slope, alpha_slope_raw); // category 1 (out) (col 1) is base category
  beta = append_col(zeros_beta, beta_raw);    // category 1 (out) (col 1) is base category
  eta = append_col(zeros_eta, eta_raw);       // category 1 (out) (col 1) is base category
  linpred = INCPT*alpha_incpt + S*alpha_slope + O*beta + X*eta; 
}
model {
  // positive slope prior
  // for (j in 1:(K-1)) {
  //   alpha_slope_raw[1,j] ~ gamma(1,1);
  // }
  to_vector(alpha_slope_raw) ~ student_t(7,0,1);

  // std. normal priors
  to_vector(alpha_incpt_raw) ~ normal(0,1);
  to_vector(beta_raw) ~ normal(0,1);
  to_vector(eta_raw) ~ normal(0,1);
  
  for (i in 1:n) {
    // likelihood
    // y[i] ~ categorical_logit(linpred[i]');
    target += categorical_logit_lpmf(y[i] | linpred[i]') * row_weights[i];
  }
}
// generated quantities {
//    simplex[K] probs[n];
//    for (i in 1:n) probs[i] = softmax(linpred[i]');
// }


