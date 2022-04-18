data {
  int<lower=2> K;             // number of categories
  int<lower=0> n;             // number of plate appearances == num rows == num datapoints
  int<lower=0> d;             // number of predictors of X matrix
  
  int<lower=0> num_pit;
  int<lower=0> num_pit_games;
  int<lower=0> num_bat;
  int<lower=0> num_bat_games;
  
  int<lower=1, upper=num_pit> p[n];          // pitcher index p[i] for each plate appearance i
  int<lower=1, upper=num_pit_games> pg[n];   // pitcher-game index pg[i] for each plate appearance i
  int<lower=1, upper=num_bat> b[n];          // batter index b[i] for each plate appearance i
  int<lower=1, upper=num_bat_games> bg[n];   // batter-game index bg[i] for each plate appearance i
  
  matrix[n,d] X;                             // all-other-data matrix
  int<lower=1, upper=K> y[n];                // outcome vector
}
transformed data {
  ///vector[1] zeros_alpha = rep_vector(0, 1);
  vector[n] zeros_linpred = rep_vector(0, n);
  vector[d] zeros_beta = rep_vector(0, d);
  real zero_P1 = 0;
  real zero_B1 = 0;
}
parameters {
  real mu_p;
  real mu_b;
  real<lower=0> sigma_p;
  real<lower=0> sigma_b;
  
  vector[num_pit-1] P_raw;
  vector[num_bat-1] B_raw;
  
  real<lower=0> tau_p[num_pit]; ///vector[num_pit] tau_p;
  real<lower=0> tau_b[num_bat]; ///vector[num_bat] tau_b;
  matrix[num_pit,num_pit_games] PQ;
  matrix[num_bat,num_bat_games] BQ;
  
  //.matrix[1,K-1] alpha_raw;     
  ///matrix[d,1] beta;
  matrix[d,K-1] beta_raw;
}
transformed parameters {
  ///matrix[1,K] alpha; 
  matrix[d,K] beta;
  vector[num_pit] P;
  vector[num_bat] B;
  matrix[n, K-1] linpred_raw;
  matrix[n, K] linpred;
  
  P = append_row(zero_P1, P_raw);
  B = append_row(zero_B1, B_raw);

  for (i in 1:n) {
    // linpred_raw[i] = PQ[p[i], pg[i]]; 
    // linpred_raw[i] += BQ[b[i], bg[i]]; 
    // linpred_raw[i] += to_row_vector(X[i]) * beta_raw;
    
    linpred_raw[i] = PQ[p[i], pg[i]] + BQ[b[i], bg[i]] + to_row_vector(X[i]) * beta_raw;
    
    ///linpred_raw[i] = to_row_vector(alpha_raw);
    ///linpred_raw[i] += to_row_vector(X[i]'*beta);
  }
  
  // category 1 (out) (col 1) is base category
  ///alpha = append_col(zeros_alpha, alpha_raw); 
  linpred = append_col(zeros_linpred, linpred_raw);
  beta = append_col(zeros_beta, beta_raw);
}
// generated quantities {
//     matrix[n, K] linpred;
// 
//    // simplex[K] probs[n];
//    // for (i in 1:n) probs[i] = softmax(linpred[i]');
// }
model {
  // priors
  mu_p ~ normal(0,2);
  mu_b ~ normal(0,2);
  sigma_p ~ inv_gamma(0.5, 0.5);
  sigma_b ~ inv_gamma(0.5, 0.5);
  to_vector(beta_raw) ~ normal(0,2);
  ///to_vector(beta) ~ normal(0,2);
  ///to_vector(alpha_raw) ~ normal(0,2);

  // 1st Level of the Hierarchy
  to_vector(P_raw) ~ normal(mu_p, sigma_p);
  to_vector(B_raw) ~ normal(mu_b, sigma_b);
  to_vector(tau_p) ~ inv_gamma(0.5, 0.5);
  to_vector(tau_b) ~ inv_gamma(0.5, 0.5);
  
  for (i in 1:n) {
    // 2nd Level of the Hierarchy
    PQ[p[i], pg[i]] ~ normal( P[p[i]], tau_p[p[i]] );
    BQ[b[i], bg[i]] ~ normal( B[b[i]], tau_b[b[i]] );

    // 3rd Level of the Hierarchy
    // linpred[i] = alpha + PQ[p[i], pg[i]] + BQ[b[i], bg[i]] + X[i]'*beta;
    y[i] ~ categorical_logit(linpred[i]');
  }
}



