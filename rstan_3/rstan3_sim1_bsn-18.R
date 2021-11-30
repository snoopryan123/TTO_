
source("rstan3_sim1_main.R")

output_folder = "./job_output/"
OUTPUT_FILE = "rstan3_sim1_bsn-18.R" #FIXME

# generate new epsilon and y vector
epsilon = rnorm(N, mean=0, sd=sigma)
y = S%*%alpha + X%*%eta + epsilon 
y = as.numeric(y)
# save epsilon and y
saveRDS(epsilon, file = paste0(output_folder, "epsilon_", OUTPUT_FILE, ".rds"))
#epsilon <- readRDS("job_output/epsilon_rstan3_sim1_bsn-1.R.rds") 
saveRDS(y, file = paste0(output_folder, "y_", OUTPUT_FILE, ".rds"))
#epsilon <- readRDS("job_output/y_rstan3_sim1_bsn-1.R.rds") 


# fit the model
fit = fit_model_bsn(y)
# save the stan objects
saveRDS(fit, file = paste0(output_folder, "fit_", OUTPUT_FILE, ".rds"))
#fit <- readRDS("job_output/fit_rstan3_sim1_bsn-18.R.rds") 

# PLOTS
draws <- as_tibble(as.matrix(fit))
alpha_post <- as.matrix(draws[,2:28])
eta_post <- as.matrix(draws[,(ncol(draws)-4):(ncol(draws)-1)])
alpha_plot = plot_alpha_post(alpha_post)
alpha_plot
#ggsave(paste0(output_folder, "plot_", OUTPUT_FILE, "_alphaPlot.png"), alpha_plot)
plot_eta_post(eta_post)
eta_post
#ggsave(paste0(output_folder, "plot_", OUTPUT_FILE, "_etaPlot.png"), eta_plot)

