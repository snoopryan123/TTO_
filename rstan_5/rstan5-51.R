library(tidyverse)
output_folder = './job_output/'

### load data
input_file = "./../data/TTO_dataset_510.csv"  
D <- read_csv(input_file)
D <- D %>% filter(ORDER_CT <= 3) # keep only 1TTO, 2TTO, 3TTO
D <- D %>% filter(YEAR == 2019) 
X <- as.matrix(D %>% select(std_BQ, std_PQ, HAND_MATCH, BAT_HOME_IND)) 
OUTPUT_FILE = "rstan5-50.R"

### rstan
source("rstan5_main.R")
fit = fit_model_spline(NA) 
saveRDS(fit, file = paste0(output_folder, "fit_", OUTPUT_FILE, ".rds"))

# ### plot
# # fit <- readRDS("./job_output/fit_rstan5-51.R.rds") 
bat_seq_draws = spl_to_bat_seq_draws(fit)
p = plot_bat_seq_draws(bat_seq_draws)
p
ggsave(paste0("./plot_",OUTPUT_FILE,".png"), p)

