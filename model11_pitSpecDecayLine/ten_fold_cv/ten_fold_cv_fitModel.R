
source("../model9_importRstan.R") ### import the RStan model

fit = fit_model_bsnBL(fold_num=fold_num) ### fit the model

saveRDS(fit, file = paste0(output_folder, "fit_", OUTPUT_FILE, ".rds"))
#fit <- readRDS("./job_output/fit_ten_fold_cv_1_model_bsnBL.rds") 

