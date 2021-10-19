data {
  int<lower=0> n;               // number of datapoints  
  int<lower=0> p_c;             // number of predictors of other covariates
  int<lower=0> p_b;             // number of predictors of spline
  matrix[n,p_c] X;              // matrix of other covariates
  matrix[n,p_b] B;              // spline matrix
  vector[n] y;                  // outcome vector
}
parameters {
  real<lower=0> sigma;        // sd of normal likelihood
  real<lower=0> tau;          // 
  vector[p_c] gamma;
  //vector[p_b] beta_raw;
  vector[p_b] beta;
}
transformed parameters {
  vector[n] linpred_b;
  vector[n] linpred_c;
  vector[n] linpred;
  //vector[p_b] beta;
  //beta = beta_raw * tau;
  linpred_b = B * beta;
  linpred_c = X * gamma;
  linpred = linpred_b + linpred_c;
}
model {
  // std. normal priors for params of standardized covariates
  to_vector(gamma) ~ normal(0,1);
  // priors for spline params
  //to_vector(beta_raw) ~ normal(0,1);
  //tau ~ cauchy(0,1);
  to_vector(beta) ~ normal(0,1);
  // likelihood
  y ~ normal(linpred, sigma); 
}



