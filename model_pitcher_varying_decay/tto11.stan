data {
  int<lower=2> K;             // number of categories
  int<lower=0> n;             // number of datapoints
  int<lower=0> p_x;           // number of predictors of X matrix
  matrix[n,1] INCPT;          // intercept matrix
  vector[n] t;              // batter sequence number matrix
  vector[n] O2;            // batter learning (order count) matrix of indicators for 2TTO
  vector[n] O3;            // batter learning (order count) matrix of indicators for 3TTO
  matrix[n,p_x] X;            // adjustments matrix
  int<lower=1, upper=K> y[n]; // outcome vector

  int num_bat;
  int<lower=1, upper=num_bat> bat_idxs[n]; // batter index vector
  int num_pit;
  int<lower=1, upper=num_pit> pit_idxs[n]; // batter index vector
}
transformed data {
  vector[num_pit] zeros_alpha_0j = rep_vector(0, num_pit);
  vector[num_pit] zeros_alpha_1j = rep_vector(0, num_pit);
  vector[num_bat] zeros_beta_2l = rep_vector(0, num_bat);
  vector[num_bat] zeros_beta_3l = rep_vector(0, num_bat);
  vector[p_x] zeros_eta = rep_vector(0, p_x);
}
parameters {
  vector[K-1] alpha_0;            // pitcher-specific intercept parameters 
  vector<lower=0>[K-1] alpha_1;   // pitcher-specific slope parameters 
  vector[K-1] beta_2;                 // batter-specific batter learning parameters 
  vector[K-1] beta_3;                 // batter-specific batter learning parameters 
  
  vector<lower=0>[K-1] sig_sq_0;            // pitcher-specific intercept parameters 
  vector<lower=0>[K-1] sig_sq_1;            // pitcher-specific slope parameters 
  vector<lower=0>[K-1] sig_sq_2;                 // batter-specific batter learning parameters 
  vector<lower=0>[K-1] sig_sq_3;                 // batter-specific batter learning parameters 
  
  matrix[num_pit,K-1] Z_alpha_0j_raw;            
  matrix[num_pit,K-1] Z_alpha_1j_raw;   
  matrix[num_bat,K-1] Z_beta_2l_raw;                 
  matrix[num_bat,K-1] Z_beta_3l_raw;                 

  matrix[p_x,K-1] eta_raw;                  // adjustment parameters
}
transformed parameters {
  matrix[num_pit,K] alpha_0j;
  matrix[num_pit,K] alpha_1j;
  matrix[num_bat,K] beta_2l;
  matrix[num_bat,K] beta_3l;
  
  matrix[num_pit,K-1] alpha_0j_raw;                // pitcher-specific intercept parameters 
  matrix[num_pit,K-1] alpha_1j_raw;                // pitcher-specific slope parameters 
  matrix[num_bat,K-1] beta_2l_raw;                 // batter-specific batter learning parameters 
  matrix[num_bat,K-1] beta_3l_raw;                 // batter-specific batter learning parameters 
  
  matrix[p_x,K] eta;
  matrix[n, K] linpred;

  alpha_0j_raw = rep_matrix(alpha_0', num_pit)  + diag_post_multiply(Z_alpha_0j_raw, sig_sq_0);
  alpha_1j_raw = rep_matrix(alpha_1', num_pit)  + diag_post_multiply(Z_alpha_1j_raw, sig_sq_1);
  beta_2l_raw = rep_matrix(beta_2', num_bat)  + diag_post_multiply(Z_beta_2l_raw, sig_sq_2);
  beta_3l_raw = rep_matrix(beta_3', num_bat)  + diag_post_multiply(Z_beta_3l_raw, sig_sq_3);

  alpha_0j = append_col(zeros_alpha_0j, alpha_0j_raw);  // category 1 (out) (col 1) is base category
  alpha_1j = append_col(zeros_alpha_1j, alpha_1j_raw);  // category 1 (out) (col 1) is base category
  beta_2l =  append_col(zeros_beta_2l, beta_2l_raw);    // category 1 (out) (col 1) is base category
  beta_3l =  append_col(zeros_beta_3l, beta_3l_raw);    // category 1 (out) (col 1) is base category

  eta = append_col(zeros_eta, eta_raw);                 // category 1 (out) (col 1) is base category
  linpred = alpha_0j[pit_idxs] + diag_pre_multiply(t, alpha_1j[pit_idxs]) + diag_pre_multiply(O2, beta_2l[bat_idxs]) + diag_pre_multiply(O3, beta_3l[bat_idxs]) + X*eta; 
}
model {
  // diffuse prior on top-level means
  alpha_0 ~ normal(0, 25); 
  alpha_1 ~ normal(0, 25);
  beta_2 ~ normal(0, 25);
  beta_3 ~ normal(0, 25);
  // half std. normal prior on top-level variances
  sig_sq_0 ~ std_normal();  //inv_gamma(0.5, 0.5);
  sig_sq_1 ~ std_normal();
  sig_sq_2 ~ std_normal();
  sig_sq_3 ~ std_normal();
  
  // prior for adjustments vector
  to_vector(eta_raw) ~ normal(0, 25);
  
  // std. normal draws
  to_vector(Z_alpha_0j_raw) ~ std_normal();   // Z is a MATRIX not a vector
  to_vector(Z_alpha_1j_raw) ~ std_normal();
  to_vector(Z_beta_2l_raw) ~ std_normal();
  to_vector(Z_beta_3l_raw) ~ std_normal();

  for (i in 1:n) {
    // likelihood
    y[i] ~ categorical_logit(linpred[i]');
  }
}



