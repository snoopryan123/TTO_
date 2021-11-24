
source("rstan3_getData2.R")

OUTPUT_FILE = "rstan3_overall_ubi2.R" 
fit = fit_model(NA, "ubi") 
saveRDS(fit, file = paste0(output_folder, "fit_", OUTPUT_FILE, ".rds"))
#fit <- readRDS("./job_output/fit_rstan3_overall_ubi2.R.rds") 

p = plot_ubi0(fit)
p
ggsave(paste0("./job_output/plot_",OUTPUT_FILE,".png"), p)

