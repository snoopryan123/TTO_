source("sim_config.R")
OUTPUT_FILE = paste0("sim",SIM_NUM,"_model_bsnBL_", s) 
source("../model9_getData.R") ### get observed data 
source("sim_simulateData.R") ### get simulated outcomes and "true" params
source("sim_fitModel.R") ### fit the model on our simulated data
