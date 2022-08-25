
source("../model9_importRstan.R") ### import the RStan model

if (model_num == 1) { ### model with pitcher fatigue spline and batter learning bumps
  fit = fit_model_bsnBL(fold_num=fold_num) 
} else if (model_num == 2) { ### model with only pitcher fatigue spline
  fit = fit_model_bsn(fold_num=fold_num) 
} else if (model_num == 3) { ### model with only batter learning bumps
  fit = fit_model_BL(fold_num=fold_num) 
}

saveRDS(fit, file = paste0(output_folder, "fit_", OUTPUT_FILE, ".rds"))
#fit <- readRDS("./job_output/fit_sim_model_bsnBL_1.rds") 

