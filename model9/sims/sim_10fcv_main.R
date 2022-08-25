source("sim_10fcv_config.R")
OUTPUT_FILE = paste0("sim",SIM_NUM,"_model",model_num,"_", s) 
source("../model9_getData.R") ### get observed data 
source("sim_simulateData.R") ### get simulated outcomes and "true" params
source("sim_10fcv_fitModel.R") ### fit the model on our simulated data
