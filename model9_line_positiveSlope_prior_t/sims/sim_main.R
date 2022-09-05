source("sim_config.R")

underlying = "line" ## simulation: "true" pitcher fatigue is linear
### underlying = "cubic" ## simulation: "true" pitcher fatigue is cubic
YRS = 2018
IS_SIM = TRUE
sim_noPf_str = ifelse(SIM_NO_PF, "A", "")
OUTPUT_FILE = paste0("sim",SIM_NUM,sim_noPf_str,"_model_bsnBL_", s, "_underlying_", underlying) 

source("../model9_getData.R") ### get observed data 
source("sim_simulateData.R") ### get simulated outcomes and "true" params
source("sim_fitModel.R") ### fit the model on our simulated data
