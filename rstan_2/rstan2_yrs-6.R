
source("rstan2_yrs_main.R")

YEARS = c(2015) #FIXME
OUTPUT_FILE = "rstan2_yrs-6.R" #FIXME

runnit(YEARS, OUTPUT_FILE)
#fit <- readRDS("job_output/rstan2_yrs-6.R.rds") 

