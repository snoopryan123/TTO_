
source("rstan2_yrs_main.R")

YEARS = c(2017) #FIXME
OUTPUT_FILE = "rstan2_yrs-8.R" #FIXME

runnit(YEARS, OUTPUT_FILE)
#fit <- readRDS("job_output/rstan2_yrs-8.R.rds") 

