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
  matrix[p_s,K] alpha;          // batter sequence number parameters 
  matrix[p_x,K] eta;            // adjustment parameters
}
transformed parameters {
  matrix[n, K] linpred;
  linpred = S*alpha + X*eta;
}
model {
  // // std. normal priors
  to_vector(alpha) ~ normal(0,1);
  to_vector(eta) ~ normal(0,1);
  
  for (i in 1:n) {
    // likelihood
    y[i] ~ categorical_logit(linpred[i]');
  }
}



