
source("rstan2_yrs_main.R")

YEARS = c(2014) #FIXME
OUTPUT_FILE = "rstan2_yrs2-5.R" #FIXME

runnit(YEARS, OUTPUT_FILE)
#fit <- readRDS("job_output/rstan2_yrs-5.R.rds") 

