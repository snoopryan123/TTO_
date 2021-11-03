
source("rstan2_yrs_main.R")

YEARS = c(2011) #FIXME
OUTPUT_FILE = "rstan2_yrs-2.R" #FIXME

runnit(YEARS, OUTPUT_FILE)
#fit <- readRDS("job_output/rstan2_yrs-1.R.rds") 

