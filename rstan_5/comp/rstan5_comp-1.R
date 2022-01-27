library(tidyverse)
output_folder = './job_output/'

# load data
input_file = "../../data/TTO_dataset_510.csv"  
D <- read_csv(input_file) #; D <- D %>% drop_na() 
D <- D %>% filter(YEAR == 2019) 
X <- as.matrix(D %>% select(std_WOBA_FINAL_BAT_19, std_WOBA_FINAL_PIT_19, #FIXME 
                            HAND_MATCH, BAT_HOME_IND)) 
source("../rstan5_main.R")

fold_num = 1 #FIXME
OUTPUT_FILE = paste0("rstan5_comp-",0 + fold_num,".R")
fit = fit_model_bsn(fold_num) 
saveRDS(fit, file = paste0(output_folder, "fit_", OUTPUT_FILE, ".rds"))
#fit <- readRDS("./job_output/fit_rstan5_comp-1.R.rds") 