source("rstan8_comp_main.R")

### get FOLD_NUM from other file
OUTPUT_FILE = paste0("rstan8_comp-",fold_num,".R")
fit = fit_model_bsn(fold_num) 
saveRDS(fit, file = paste0(output_folder, "fit_", OUTPUT_FILE, ".rds"))
#fit <- readRDS("./job_output/fit_rstan8_comp-1.R.rds") 

