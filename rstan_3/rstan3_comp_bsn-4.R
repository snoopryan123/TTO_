
source("rstan3_comp_getData.R")

fold_num = 4 #FIXME

OUTPUT_FILE = paste0("rstan3_comp_bsn-",fold_num,".R") 
fit = fit_model_bsn(fold_num) 
saveRDS(fit, file = paste0(output_folder, "fit_", OUTPUT_FILE, ".rds"))
#fit <- readRDS("./job_output/fit_rstan3_comp_bsn-1.R.rds") 