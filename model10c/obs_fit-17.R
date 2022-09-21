
library(tidyverse)
YRS = 2017
model = "spl"

OUTPUT_FILE = paste0("obs","_model_", model, 
                     "yrs_", str_remove_all(paste0(YRS, collapse=''), "20"),
                     "_") 

source("model10_getData.R") ### get observed data 
source("obs_fitModel.R") ### fit the model on our observed data
