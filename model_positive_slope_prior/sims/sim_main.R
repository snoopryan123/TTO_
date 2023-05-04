
### get command line args 
args = commandArgs(trailingOnly=TRUE)
s = as.numeric(args[1]) ### random seed for data generation
SIM_NUM = as.numeric(args[2])
# SIM_NUM = 1 ### no TTOP batter learning bumps
# SIM_NUM = 2 ### TTOP batter learning bumps consistent with Tango's effect size
# SIM_NUM = 3 ### absurdly huge 3TTOP batter learning bump

underlying = "line" ## simulation: "true" pitcher fatigue is linear
YRS = 2018
IS_SIM = TRUE
if (!exists("SIM_NO_PF")) { SIM_NO_PF = FALSE } ### sim with Pitcher Fatigue
sim_noPf_str = ifelse(SIM_NO_PF, "A", "")

OUTPUT_FILE = paste0("sim",SIM_NUM,sim_noPf_str,"_model_bsnBL_", s, "_underlying_", underlying) 

source("../A_getData.R") ### get observed data 
source("sim_simulateData.R") ### get simulated outcomes and "true" params
source("sim_fitModel.R") ### fit the model on our simulated data
