library(tidyverse)
output_folder = './job_output/'

### load data
input_file = "./../data/TTO_dataset_411a.csv"  
D <- read_csv(input_file)
#D <- D %>% drop_na() 
#FIXME 
D <- D %>% filter(YEAR == 2019) 
D <- D %>% filter(!first_szn_pit & !first_szn_bat & DATE > "2019-07-09") # remove rookies & 2nd half of season
X <- as.matrix(D %>% select(std_BQ12, std_PQ12, HAND_MATCH, BAT_HOME_IND)) 
OUTPUT_FILE = "rstan4a_overall_bsn-25.R"

### rstan
source("rstan4a_main.R")
fit = fit_model_bsn(NA) 
saveRDS(fit, file = paste0(output_folder, "fit_", OUTPUT_FILE, ".rds"))

### plot
# fit <- readRDS("./job_output/fit_rstan4a_overall_bsn-1.R.rds") 
p = plot_bsn0(fit)
p
ggsave(paste0("./plot_",OUTPUT_FILE,".png"), p)

