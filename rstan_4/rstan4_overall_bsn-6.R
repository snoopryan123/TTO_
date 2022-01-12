library(tidyverse)
output_folder = './job_output/'

### load data
input_file = "./../data/TTO_dataset_412.csv"  
D <- read_csv(input_file)
D <- D %>% drop_na() 
#FIXME 
D <- D %>% filter(YEAR == 2019) 
D <- D %>% filter(!FIRST_SZN_PIT) # remove rookies
X <- as.matrix(D %>% select(std_WOBA_FINAL_BAT_19, std_WOBA_FINAL_PIT_19, HAND_MATCH, BAT_HOME_IND)) 
OUTPUT_FILE = "rstan4_overall_bsn-6.R"

### rstan
source("rstan4_main.R")
fit = fit_model_bsn(NA) 
saveRDS(fit, file = paste0(output_folder, "fit_", OUTPUT_FILE, ".rds"))

### plot
# fit <- readRDS("./job_output/fit_rstan4b_overall_bsn.R.rds") 
p = plot_bsn0(fit)
p
ggsave(paste0("./plot_",OUTPUT_FILE,".png"), p)

