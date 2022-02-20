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
parameters {
  matrix[p_u,K] beta;           // unique batter index parameters 
  matrix[p_o,K] gamma;          // order count parameters 
  matrix[p_x,K] eta;            // adjustment parameters
}
transformed parameters {
  matrix[n, K] linpred;
  linpred = U*beta + O*gamma + X*eta;
}
model {
  // // std. normal priors
  to_vector(beta) ~ normal(0,1);
  to_vector(gamma) ~ normal(0,1);
  to_vector(eta) ~ normal(0,1);

  for (i in 1:n) {
    // likelihood
    y[i] ~ categorical_logit(linpred[i]');
  }
}



