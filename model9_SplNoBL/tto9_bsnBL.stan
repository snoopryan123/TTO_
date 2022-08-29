data {
  int<lower=2> K;             // number of categories
  int<lower=0> n;             // number of datapoints
  int<lower=0> p_s;           // number of predictors of S matrix
  int<lower=0> p_o;           // number of predictors of O matrix
  int<lower=0> p_x;           // number of predictors of X matrix
  matrix[n,p_s] S;            // batter sequence number matrix
  matrix[n,p_o] O;            // batter learning (order count) matrix
  matrix[n,p_x] X;            // adjustments matrix
  int<lower=1, upper=K> y[n]; // outcome vector
}
transformed data {
  vector[p_s] zeros_alpha = rep_vector(0, p_s);
  vector[p_o] zeros_beta = rep_vector(0, p_o);
  vector[p_x] zeros_eta = rep_vector(0, p_x);
}
parameters {
  matrix[p_s,K-1] alpha_raw;          // batter sequence number parameters 
  matrix[p_o,K-1] beta_raw;           // batter learning parameters 
  matrix[p_x,K-1] eta_raw;            // adjustment parameters
}
transformed parameters {
  matrix[p_s,K] alpha;
  matrix[p_o,K] beta;
  matrix[p_x,K] eta;
  matrix[n, K] linpred;
  alpha = append_col(zeros_alpha, alpha_raw); // category 1 (out) (col 1) is base category
  beta = append_col(zeros_beta, beta_raw);    // category 1 (out) (col 1) is base category
  eta = append_col(zeros_eta, eta_raw);       // category 1 (out) (col 1) is base category
  linpred = S*alpha + O*beta + X*eta; 
}
model {
  // std. normal priors
  to_vector(alpha_raw) ~ normal(0,1);
  to_vector(beta_raw) ~ normal(0,1);
  to_vector(eta_raw) ~ normal(0,1);
  
  for (i in 1:n) {
    // likelihood
    y[i] ~ categorical_logit(linpred[i]');
  }
}
// generated quantities {
//    simplex[K] probs[n];
//    for (i in 1:n) probs[i] = softmax(linpred[i]');
// }


