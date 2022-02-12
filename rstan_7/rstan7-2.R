library(tidyverse)
output_folder = './job_output/'

### load data
input_file = "./../data/TTO_dataset_510.csv"  
D <- read_csv(input_file)
##D <- D %>% drop_na() 
#FIXME 
D <- D %>% filter(YEAR == 2019) 
X <- as.matrix(D %>% select(BQ, PQ, HAND_MATCH, BAT_HOME_IND)) 
OUTPUT_FILE = "rstan7-2.R"

### rstan
source("rstan7_main.R")
fit = fit_model_ubi(NA) 
saveRDS(fit, file = paste0(output_folder, "fit_", OUTPUT_FILE, ".rds"))

### plot
# fit <- readRDS("./job_output/fit_rstan6-2.R.rds") 
p = plot_ubi0(fit)
p
ggsave(paste0("./plot_",OUTPUT_FILE,".png"), p)

