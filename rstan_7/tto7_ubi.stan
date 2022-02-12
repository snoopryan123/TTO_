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
  // real<lower=0> sigma;        // sd of normal likelihood
  vector[p_u] beta;           // unique batter index parameters 
  vector[p_o] gamma;          // order count parameters 
  vector[p_x] eta;            // adjustment parameters
  ordered[K - 1] c;           // cutpoints for ordered logistic regression
}
// transformed parameters {
//   vector[n] linpred;
//   linpred = U*beta + O*gamma + X*eta;
// }
model {
  // // std. normal priors
  to_vector(beta) ~ normal(0,1);
  to_vector(gamma) ~ normal(0,1);
  to_vector(eta) ~ normal(0,1);
  
  // likelihood
  y ~ ordered_logistic(U*beta + O*gamma + X*eta, c);
  //y ~ bernoulli_logit(U*beta + O*gamma + X*eta); //, sigma
}



