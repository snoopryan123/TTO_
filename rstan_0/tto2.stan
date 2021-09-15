
// tto1.stan

data {
  int<lower=0> n;   // number of data items
  int<lower=0> p;   // number of predictors
  matrix[n, p] X;   // predictor matrix
  vector[n] y;      // outcome vector
}
parameters {
  real alpha;           // intercept
  vector[p] beta;       // coefficients for predictors
  real<lower=0> sigma;  // error scale
}
model {
  // priors
  alpha ~ normal(.32, 1.3);
  to_vector(beta) ~ normal(0, 11.57);
  sigma ~ exponential(1.9);
  //alpha ~ normal(.3, .03);
  //to_vector(beta) ~ normal(.3, .03);
  // likelihood
  y ~ normal(alpha + X * beta, sigma); // likelihood
  //y ~ normal(X * beta + alpha, sigma);  
}



