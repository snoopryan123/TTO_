
########################################
### get "true" simulation parameters ###
########################################

if (underlying == "line") {
  param_creation_folder = "param_creation"
} else if (underlying == "cubic") {
  param_creation_folder = "param_creation_cubic"
} 

alpha_tib = read_csv(paste0(param_creation_folder, "/params_sim_alpha.csv"))
beta_tib = read_csv(paste0(param_creation_folder,"/params_sim_beta.csv"))
eta_tib = read_csv(paste0(param_creation_folder,"/params_sim_eta.csv"))

alpha_mat = matrix(nrow=dim(S)[2], ncol=num_categories)
for (kk in 1:7) {
  alpha_mat[,kk] = (alpha_tib %>% filter(k==kk))$alpha_line
}
colnames(alpha_mat) = category_strings

beta_mat = matrix(
  c((beta_tib %>% filter(sim_num == SIM_NUM))$beta_2,
  (beta_tib %>% filter(sim_num == SIM_NUM))$beta_3),
  byrow = TRUE, nrow=2, ncol=num_categories
)
colnames(beta_mat) = category_strings

eta_mat = matrix(nrow=dim(X)[2], ncol=num_categories)
for (kk in 1:7) {
  eta_mat[,kk] = (eta_tib %>% filter(k==kk))$eta
}
colnames(eta_mat) = category_strings

##########################################
### SIMULATE PLATE APPEARANCE OUTCOMES ###
##########################################

### generate categorical outcome vector y
linpred = S %*% alpha_mat + O %*% beta_mat + X %*% eta_mat
P = exp(linpred) / rowSums( exp(linpred) )
get_outcome <- function(i) { # get the categorical outcome in {1,2,...,7} of row i
  which(rmultinom(1, 1, P[i,]) == 1)
}
set.seed(s) 
y = matrix( sapply(1:nrow(linpred), get_outcome), ncol=1)

# saveRDS(y, file = paste0(output_folder, "y_", OUTPUT_FILE, ".rds"))
#y <- readRDS("./job_output/y_sim_model_bsnBL_1.rds") 


