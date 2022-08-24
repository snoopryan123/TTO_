# source("sim_config.R")
IS_SIM=TRUE
SIM_NUM=2
YRS=2018
s=1

OUTPUT_FILE = paste0("sim",SIM_NUM,"simple_model_bsnBL_", s) 
source("../model9_getData.R") ### get observed data 
source("sim_simulateData.R") ### get simulated outcomes and "true" params

### fit simple model here
library(nnet)

m1 <- multinom(y ~ SPL + O + X + 0)

m1

y
X
SPL
O
