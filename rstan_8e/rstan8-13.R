library(tidyverse)
output_folder = './job_output/'

### load data
input_file = "./../data/TTO_dataset_510.csv"  
D <- read_csv(input_file) %>% filter(!PIT_IS_BAT) # %>% drop_na() 
D <- D %>% filter(YEAR == 2013) %>% filter(BQ>0 & BQ<1 & PQ>0 & PQ<1 )
D <- D %>% filter(ORDER_CT <= 3) # keep only 1TTO, 2TTO, 3TTO
logit <- function(p) { log(p/(1-p)) }
X <- as.matrix(D %>% mutate(lBQ=logit(BQ), lPQ=logit(PQ)) %>% select(lBQ, lPQ, HAND_MATCH, BAT_HOME_IND)) 
OUTPUT_FILE = "rstan8-13"

### rstan
source("rstan8_main.R")

fit1 = fit_model_bsnSpl(NA, SPL1) 
saveRDS(fit1, file = paste0(output_folder, "fit_", OUTPUT_FILE, "_1", ".rds"))

fit2 = fit_model_bsnSpl(NA, SPL2) 
saveRDS(fit2, file = paste0(output_folder, "fit_", OUTPUT_FILE, "_2", ".rds"))