source("rstan8_comp_main.R")

### get FOLD_NUM from other file

OUTPUT_FILE = paste0("rstan8_comp-",fold_num)

fit1 = fit_model_bsnSpl(fold_num, SPL1) 
saveRDS(fit1, file = paste0(output_folder, "fit_", OUTPUT_FILE, "_1", ".rds"))

fit2 = fit_model_bsnSpl(fold_num, SPL2) 
saveRDS(fit2, file = paste0(output_folder, "fit_", OUTPUT_FILE, "_2", ".rds"))

#fit1 <- readRDS("./job_output/fit_rstan8_comp-1_1.rds") 
#fit2 <- readRDS("./job_output/fit_rstan8_comp-1_2.rds") 

