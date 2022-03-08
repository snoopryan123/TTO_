library(tidyverse)
output_folder = './job_output/'
IS_COMP = TRUE

### load data
input_file = "../../data/TTO_dataset_510.csv"  
D <- read_csv(input_file) 
D <- D %>% filter(YEAR == 2019) %>% filter(BQ>0 & BQ<1 & PQ>0 & PQ<1 )
logit <- function(p) { log(p/(1-p)) }
X <- as.matrix(D %>% mutate(lBQ=logit(BQ), lPQ=logit(PQ)) %>% select(lBQ, lPQ, HAND_MATCH, BAT_HOME_IND)) 
source("../rstan8_main.R")



