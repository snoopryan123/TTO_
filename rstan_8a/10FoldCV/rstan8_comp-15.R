library(tidyverse)
output_folder = './job_output/'

### load data
input_file = "../../data/TTO_dataset_510.csv"  
D <- read_csv(input_file) #; D <- D %>% drop_na() 
D <- D %>% filter(YEAR == 2019) %>% filter(BQ>0 & BQ<1 & PQ>0 & PQ<1 )
logit <- function(p) { log(p/(1-p)) }
X <- as.matrix(D %>% mutate(lBQ=logit(BQ), lPQ=logit(PQ)) %>% select(lBQ, lPQ, HAND_MATCH, BAT_HOME_IND)) 
source("../rstan8_main.R")

fold_num = 15 #FIXME
OUTPUT_FILE = paste0("rstan8_comp-",10 + fold_num,".R")
fit = fit_model_bsn(fold_num) 
saveRDS(fit, file = paste0(output_folder, "fit_", OUTPUT_FILE, ".rds"))
#fit <- readRDS("./job_output/fit_rstan8_comp-1.R.rds") 