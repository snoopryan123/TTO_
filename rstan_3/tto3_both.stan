data {
  int<lower=0> n;             // number of datapoints
  int<lower=0> p;             // number of predictors
  matrix[n,p] D;              // model matrix
  vector[n] y;                // outcome vector
}
parameters {
  real<lower=0> sigma;        // sd of normal likelihood
  vector[p] w;                // params
}
model {
  // std. normal priors for standardized covariates
  to_vector(w) ~ normal(0,1);
  // likelihood
  y ~ normal(D * w, sigma); 
}



