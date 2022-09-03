
source("../model9_importRstan.R") ### import the RStan model

fit = fit_model_bsnBL(fold_num=1) ### fit the model

saveRDS(fit, file = paste0(output_folder, "fit_", OUTPUT_FILE, ".rds"))
#fit <- readRDS("./job_output/fit_sim_model_bsnBL_1.rds") 

