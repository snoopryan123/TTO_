data {
  int<lower=0> N;             // number of data points
  int<lower=0> P;             // number of predictors (including intercept)
  matrix[N,P] X;              // predictors (including 1s for intercept)
  int<lower=0,upper=1> y[N];  // binary outcome
}
parameters {
  vector[P] beta;
}
model {
  beta ~ normal(0, 1);
  y ~ bernoulli_logit(X * beta);
}
generated quantities {
  vector[N] log_lik;
  for (n in 1:N) {
    log_lik[n] = bernoulli_logit_lpmf(y[n] | X[n] * beta);
  }
}
