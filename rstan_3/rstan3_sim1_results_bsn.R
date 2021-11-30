
source("rstan3_sim1_main.R")

rmse_vec = numeric(25)
covg_vec = numeric(25)

for (i in 1:25) {
  fit <- readRDS(paste0("job_output/fit_rstan3_sim1_bsn-",i,".R.rds")) 
  
  # rmse
  
  
  # coverage
  
  
  # PLOTS
  draws <- as_tibble(as.matrix(fit))
  alpha_post <- as.matrix(draws[,2:28])
  eta_post <- as.matrix(draws[,(ncol(draws)-4):(ncol(draws)-1)])
  alpha_plot = plot_alpha_post(alpha_post)
  eta_plot = plot_eta_post(eta_post)
  #alpha_plot
  ###ggsave(paste0(output_folder, "plot_", OUTPUT_FILE, "_alphaPlot.png"), alpha_plot)
  #eta_plot
  ###ggsave(paste0(output_folder, "plot_", OUTPUT_FILE, "_etaPlot.png"), eta_plot)
  
}





