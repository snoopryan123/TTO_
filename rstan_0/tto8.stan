
data {
  int<lower=0> n;   // number of data items
  int<lower=0> p_b;   // number of predictors in BATTER_IDX matrix
  int<lower=0> p_o;   // number of predictors in ORDER_CT matrix
  int<lower=0> p_c;   // number of predictors in other-covariates matrix
  matrix[n, p_b] X_b;   // BATTER_IDX predictor matrix
  matrix[n, p_o] X_o;   // ORDER_CT predictor matrix
  matrix[n, p_c] X_c;   // other-covariates matrix
  vector[n] y;      // outcome vector
  
  // vector[n] X_wb; // WOBA_AVG_BAT_19
  // vector[n] X_wp; // WOBA_AVG_PIT_19
  // vector[n] n_wb; // NUM_WOBA_APP_BAT
  // vector[n] n_wp; // NUM_WOBA_APP_PIT
}
parameters {
  // sd of normal likelihood
  real<lower=0> sigma;      
  
  // important: NO INTERCEPT because of the dummy matrices
  // BATTER_IDX, ORDER_CT prior parameters
  real beta_b0;             // prior mean for BATTER_IDX1
  real beta_o0;             // prior mean for ORDER_CT1
  vector[p_b] beta_b;       // coefficients for BATTER_IDX predictors
  vector[p_o] beta_o;       // coefficients for ORDER_CT predictors
  vector[p_c] beta_c;       // coefficients for other-covariates 
  real<lower=0> tau_b;      // sd of normal prior for BATTER_IDX
  real<lower=0> tau_o;      // sd of normal prior for ORDER_CT
  
  
  // WOBA_AVG_BAT_19, WOBA_AVG_PIT_19 prior parameters
  // real beta_wb;
  // real beta_wp;
  // real mu_wb;  
  // real<lower=0> sigma_wb; 
  // real mu_wp; 
  // real<lower=0> sigma_wp;
}
transformed parameters {
  vector[n] linpred_b;
  vector[n] linpred_o;
  //vector[n] linpred_wb;
  //vector[n] linpred_wp;
  vector[n] linpred_c;
  linpred_b = X_b * beta_b;
  linpred_o = X_o * beta_o;
  // linpred_wb = X_wb * beta_wb;
  // linpred_wp = X_wp * beta_wp;
  linpred_c = X_c * beta_c;
  
  // WOBA_AVG_BAT_19, WOBA_AVG_PIT_19 prior transformed parameters
  
}
model {
  // beta_b prior
  beta_b[1] ~ normal(beta_b0, tau_b);
  {
    for (k in 2:p_b)
    // beta_b[k] ~ normal(beta_b[k-1], tau_b); 
    beta_b[k] ~ normal(beta_b[k-1], tau_b / sqrt(k)); 
  }
  // beta_o prior
  beta_o[1] ~ normal(beta_o0, tau_o);
  {
    for (k in 2:p_o)
    // beta_o[k] ~ normal(beta_o[k-1], tau_o); 
    beta_o[k] ~ normal(beta_o[k-1], tau_o / sqrt(k)); 
  }
  
  
  
  
  // WOBA_AVG_BAT_19, WOBA_AVG_PIT_19 priors
  // beta_wb ~ normal(mu_wb, sigma_wb / );
  // beta_wp ~ normal(mu_wp, sigma_wp / );
  
  // beta_c prior
  //to_vector(beta_c) ~ normal(.5, .25); //FIXME
  
  // likelihood
  y ~ normal(linpred_b + linpred_o + linpred_c, sigma); 
}



