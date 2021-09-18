
data {
  int<lower=0> n;   // number of data items
  int<lower=0> p;   // number of predictors
  matrix[n, p] X;   // predictor matrix
  vector[n] y;      // outcome vector
}
parameters {
  //real alpha;           // intercept
  vector[p] beta;       // coefficients for predictors
  real<lower=0> sigma;  // error scale
}
transformed parameters {
  vector[n] linpred;
  linpred = X * beta;
}
model {
  to_vector(beta) ~ normal(.3, .03); // prior
  y ~ normal(linpred, sigma); // likelihood
}



