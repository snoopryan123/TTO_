
source("rstan3_sim1_main.R")

aa = 15 #FIXME
output_folder = "./job_output/"
OUTPUT_FILE = paste0("rstan3_sim1_ubi-",aa,".R")

# generate new epsilon and y vector
set.seed(aa)
epsilon = rnorm(N, mean=0, sd=sigma)
y = U%*%beta + O%*%gamma + X%*%delta + epsilon 
y = as.numeric(y)
# save epsilon and y
saveRDS(epsilon, file = paste0(output_folder, "epsilon_", OUTPUT_FILE, ".rds"))
#epsilon <- readRDS("job_output/epsilon_rstan3_sim1_bsn-1.R.rds") 
saveRDS(y, file = paste0(output_folder, "y_", OUTPUT_FILE, ".rds"))
#epsilon <- readRDS("job_output/y_rstan3_sim1_bsn-1.R.rds") 
# y train and test
y_train = y[!test_idxs]
y_test = y[test_idxs]

# fit the model
fit = fit_model_ubi(y_train)
# save the stan objects
saveRDS(fit, file = paste0(output_folder, "fit_", OUTPUT_FILE, ".rds"))
#fit <- readRDS("job_output/fit_rstan3_sim1_ubi-1.R.rds") 

# PLOTS
draws <- as_tibble(as.matrix(fit))
beta_post <- as.matrix(draws[,2:15])
gamma_post <- as.matrix(draws[,(ncol(draws)-8):(ncol(draws)-5)])
delta_post <- as.matrix(draws[,(ncol(draws)-4):(ncol(draws)-1)])
beta_plus_gamma_plot = plot_beta_plus_gamma_post(beta_post, gamma_post)
beta_plus_gamma_plot
#ggsave(paste0(output_folder, "plot_", OUTPUT_FILE, "_betaGammePlot.png"), beta_plus_gamma_plot)
delta_plot = plot_delta_post(delta_post)
delta_plot
#ggsave(paste0(output_folder, "plot_", OUTPUT_FILE, "_etaPlot.png"), delta_plot)

