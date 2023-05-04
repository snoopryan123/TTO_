
YRS = 2018
IS_SIM = TRUE ### even though this is 10 fold cv and not a sim, we need this to be TRUE to get the code to work!
OUTPUT_FILE = paste0("ten_fold_cv_",fold_num,"_model_bsnBL") 

source("../A_getData.R") ### get observed data 
source("ten_fold_cv_fitModel.R") ### fit the model on our simulated data
