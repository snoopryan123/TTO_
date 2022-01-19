library(tidyverse)
output_folder = './job_output/'

### load data
input_file = "./../data/TTO_dataset_411.csv"  
D <- read_csv(input_file)
D <- D %>% drop_na() 
#FIXME 
D <- D %>% filter(YEAR == 2019) 
X <- as.matrix(D %>% select(std_WOBA_CURR_PIT_19, HAND_MATCH, BAT_HOME_IND)) 
OUTPUT_FILE = "rstan4f-11.R"

### rstan
source("rstan5_main.R")
fit = fit_model_ubi(NA) 
saveRDS(fit, file = paste0(output_folder, "fit_", OUTPUT_FILE, ".rds"))

### plot
# fit <- readRDS("./job_output/fit_rstan4f-1.R.rds") 
p = plot_ubi0(fit)
p
ggsave(paste0("./plot_",OUTPUT_FILE,".png"), p)

