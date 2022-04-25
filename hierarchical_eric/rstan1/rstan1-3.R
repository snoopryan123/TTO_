library(tidyverse)
output_folder = './job_output/'

### load data
input_file = "../../data/TTO_dataset_510.csv"  
D <- read_csv(input_file) ###%>% filter(!PIT_IS_BAT) # %>% drop_na() 
D <- D %>% filter(YEAR == 2019) %>% filter(BQ>0 & BQ<1 & PQ>0 & PQ<1 ) %>% filter(BATTER_SEQ_NUM <= 27)
# logit <- function(p) { log(p/(1-p)) }
# X <- as.matrix(D %>% mutate(lBQ=logit(BQ), lPQ=logit(PQ)) %>% select(lBQ, lPQ, HAND_MATCH, BAT_HOME_IND)) 
OUTPUT_FILE = "rstan1-3.R"

### rstan
source("rstan1_main.R")
fit = fit_model_hbp1(NA, model_type="1c") 
saveRDS(fit, file = paste0(output_folder, "fit_", OUTPUT_FILE, ".rds"))

# draws = as.matrix(fit)

