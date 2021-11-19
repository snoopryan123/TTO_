
source("rstan3_comp_getData.R")

OUTPUT_FILE = "rstan3_overall_ubi1.R" 
fit = fit_model_ubi(NA) 
saveRDS(fit, file = paste0(output_folder, "fit_", OUTPUT_FILE, ".rds"))
#fit <- readRDS("./job_output/fit_rstan3_overall_ubi1.R.rds") 

p = plot_ubi_(fit)
p
ggsave(paste0("./job_output/plot_",OUTPUT_FILE,".png"), p)
