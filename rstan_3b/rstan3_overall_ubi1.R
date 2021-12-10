
source("rstan3_comp_getData.R")

OUTPUT_FILE = "rstan3_overall_ubi1.R" 
fit = fit_model_ubi(NA) 
saveRDS(fit, file = paste0(output_folder, "fit_", OUTPUT_FILE, ".rds"))
#fit <- readRDS("./job_output/fit_rstan3_overall_ubi1.R.rds") 

################################
########### ANALYSIS ###########
################################

fit_3a <- readRDS("../rstan_3a/job_output/fit_rstan3_overall_ubi1.R.rds")
fit_3b <- readRDS("./job_output/fit_rstan3_overall_ubi1.R.rds")

p_3a = plot_ubi0(fit_3a)
p_3b = plot_ubi0(fit_3b)
p_3a
p_3b
ggsave(paste0("./plot_3a_",OUTPUT_FILE,".png"), p_3a)
ggsave(paste0("./plot_3b_",OUTPUT_FILE,".png"), p_3b)
