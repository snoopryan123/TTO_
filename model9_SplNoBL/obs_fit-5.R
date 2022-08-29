
library(tidyverse)
YRS = 2018 #2018 #2017:2019
model = "SingleSplOnly"
spl_df = 5

OUTPUT_FILE = paste0("obs","_model_", model, 
                     "yrs_", str_remove_all(paste0(YRS, collapse=''), "20"),
                     "_df", spl_df, "_") 

source("model9_getData.R") ### get observed data 
source("obs_fitModel.R") ### fit the model on our observed data
