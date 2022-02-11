library(tidyverse)
output_folder = './job_output/'

### load data
input_file = "./../data/TTO_dataset_610.csv"  
D <- read_csv(input_file)
##D <- D %>% drop_na() 
#FIXME 
D <- D %>% filter(YEAR == 2019) %>% filter(BQ>0 & BQ<1 & PQ>0 & PQ<1 )
logit <- function(p) { log(p/(1-p)) }
X <- as.matrix(D %>% mutate(lBQ=logit(BQ), lPQ=logit(PQ)) %>% select(lBQ, lPQ, HAND_MATCH, BAT_HOME_IND)) 
OUTPUT_FILE = "rstan6-1.R"

### rstan
source("rstan6_main.R")
fit = fit_model_bsn(NA) 
saveRDS(fit, file = paste0(output_folder, "fit_", OUTPUT_FILE, ".rds"))

### plot
# fit <- readRDS("./job_output/fit_rstan5-1.R.rds") 
p = plot_bsn0(fit)
p
ggsave(paste0("./plot_",OUTPUT_FILE,".png"), p)

