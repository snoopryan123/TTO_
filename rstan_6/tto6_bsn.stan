data {
  int<lower=0> n;             // number of datapoints
  int<lower=0> p_s;           // number of predictors of S matrix
  int<lower=0> p_x;           // number of predictors of X matrix
  matrix[n,p_s] S;            // batter sequence number matrix
  matrix[n,p_x] X;            // adjustments matrix
  int y[n];                // outcome vector
}
parameters {
  // real<lower=0> sigma;        // sd of normal likelihood
  vector[p_s] alpha;          // batter sequence number parameters 
  vector[p_x] eta;            // adjustment parameters
}
// transformed parameters {
//   vector[n] linpred;
//   linpred = S*alpha + X*eta;
// }
model {
  // // std. normal priors
  to_vector(alpha) ~ normal(0,1);
  to_vector(eta) ~ normal(0,1);
  
  // likelihood
  y ~ bernoulli_logit(S*alpha + X*eta); //, sigma
}



