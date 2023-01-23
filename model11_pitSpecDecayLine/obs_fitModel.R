
# YRS = 2018
args <- commandArgs(trailingOnly = TRUE)
YRS <- as.numeric(args[1]) 

YRS = as.
model = "line"
OUTPUT_FILE = paste0("obs","_model_", model, 
                     "yrs_", tidyverse::str_remove_all(paste0(YRS, collapse=''), "20"),
                     "_") 

source("A_getData.R") ### get observed data 
source("A_importRstan.R") ### import the RStan model

fit = fit_MODEL() ### fit the model

saveRDS(fit, file = paste0(output_folder, "fit_", OUTPUT_FILE, ".rds"))
#fit <- readRDS("./job_output/fit_sim_model_bsnBL_1.rds") 


