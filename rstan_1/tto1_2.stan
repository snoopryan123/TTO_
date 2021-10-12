data {
  int<lower=0> n;             // number of datapoints
  int<lower=0> p;             // number of predictors
  matrix[n,p] X;              // model matrix
  vector[n] y;                // outcome vector
}
parameters {
  real<lower=0> sigma;        // sd of normal likelihood
  vector[p] beta;
}
// transformed parameters {
//   vector[n] linpred;
//   linpred_b = X * beta;
// }
model {
  //beta ~ normal(0, 1);
  // std. normal priors for standardized covariates
  to_vector(beta) ~ normal(0,1);
  // likelihood
  y ~ normal(X * beta, sigma); 
}



