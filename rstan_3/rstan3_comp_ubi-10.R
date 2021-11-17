
source("rstan3_comp_getData.R")

fold_num = 10 #FIXME

OUTPUT_FILE = paste0("rstan3_comp_ubi-",fold_num,".R") 
fit = fit_model_ubi(fold_num) 
saveRDS(fit, file = paste0(output_folder, "fit_", OUTPUT_FILE, ".rds"))
#fit <- readRDS("./job_output/fit_rstan3_comp_ubi-1.R.rds") 