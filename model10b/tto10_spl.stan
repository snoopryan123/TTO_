data {
  int<lower=2> K;             // number of categories
  int<lower=0> n;             // number of datapoints
  int<lower=0> p_s;           // number of predictors of S matrix
  int<lower=0> p_x;           // number of predictors of X matrix
  matrix[n,p_s] S;            // batter sequence number matrix
  matrix[n,p_x] X;            // adjustments matrix
  matrix[n,1] INCPT;          // intercept matrix
  int<lower=1, upper=K> y[n]; // outcome vector
}
transformed data {
  vector[1] zeros_alpha_incpt = rep_vector(0, 1);
  vector[p_s] zeros_alpha = rep_vector(0, p_s);
  vector[p_x] zeros_eta = rep_vector(0, p_x);
}
parameters {
  matrix[1,K-1] alpha_incpt_raw;       // intercept parameters  
  matrix[p_s,1] alpha_;  // batter sequence number parameters       
  matrix[p_x,1] eta_;    // adjustment parameters
}
transformed parameters {
  matrix[p_s,K-1] alpha_raw;        
  matrix[p_x,K-1] eta_raw;    
  matrix[1,K] alpha_incpt;
  matrix[p_s,K] alpha;
  matrix[p_x,K] eta;
  matrix[n, K] linpred;
  
  // enforce the same parameter in each non-1 category, for non-intercept params
  for (j in 1:(K-1)) {
    for (i in 1:p_s) {
      alpha_raw[i,j] = alpha_[i,1];
    }
    for (i in 1:p_x) {
      eta_raw[i,j] = eta_[i,1];
    }
  }
  
  // enforce that the parameter is 0 in each 
  alpha_incpt = append_col(zeros_alpha_incpt, alpha_incpt_raw); // category 1 (out) (col 1) is base category
  alpha = append_col(zeros_alpha, alpha_raw); // category 1 (out) (col 1) is base category
  eta = append_col(zeros_eta, eta_raw);       // category 1 (out) (col 1) is base category
  
  linpred = INCPT*alpha_incpt + S*alpha + X*eta; 
}
model {
  // // std. normal priors
  to_vector(alpha_incpt_raw) ~ normal(0,1);
  to_vector(alpha_raw) ~ normal(0,1);
  to_vector(eta_raw) ~ normal(0,1);
  
  for (i in 1:n) {
    // likelihood
    y[i] ~ categorical_logit(linpred[i]');
  }
}
// generated quantities {
//    simplex[K] probs[n];
//    for (i in 1:n) probs[i] = softmax(linpred[i]');
// }


