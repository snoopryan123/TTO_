data {
  int<lower=2> K;             // number of categories
  int<lower=0> n;             // number of datapoints
  int<lower=0> p_s;           // number of predictors of S matrix
  int<lower=0> p_x;           // number of predictors of X matrix
  matrix[n,p_s] S;            // batter sequence number matrix
  matrix[n,p_x] X;            // adjustments matrix
  int<lower=1, upper=K> y[n]; // outcome vector
}
parameters {
  ///real<lower=0> sigma;        // sd of normal likelihood
  vector[p_s] alpha;          // batter sequence number parameters 
  vector[p_x] eta;            // adjustment parameters
  ordered[K - 1] c;           // cutpoints for ordered logistic regression
  vector[n] epsilon;          // noise
}
// transformed parameters {
//   vector[n] linpred;
//   linpred = S*alpha + X*eta;
// }
model {
  // // std. normal priors
  to_vector(alpha) ~ normal(0,1);
  to_vector(eta) ~ normal(0,1);
  
  // noise
  ///  to_vector(epsilon) ~ normal(0, sigma);
  to_vector(epsilon) ~ normal(0, 1);
  
  // likelihood
  y ~ ordered_logistic(S*alpha + X*eta + epsilon, c);
}



