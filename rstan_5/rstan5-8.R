library(tidyverse)
library(splines)
output_folder = './job_output/'

### load data
input_file = "./../data/TTO_dataset_510.csv"  
D <- read_csv(input_file)
D <- D %>% drop_na() 
#FIXME 
D <- D %>% filter(YEAR == 2019) 
X <- as.matrix(D %>% select(std_BQ, std_PQ, HAND_MATCH, BAT_HOME_IND)) 
OUTPUT_FILE = "rstan5-8.R"

### rstan
source("rstan5_main.R")
#FIXME
# cubic spline over BATTER_SEQ_NUM
# https://mc-stan.org/users/documentation/case-studies/splines_in_stan.html
#knots = c(9,9,9,9,18,18,18,18,27,27,27,27,36,36,36,36)
knots = c(9,9,9,18,18,18,27,27,27,36,36,36)
a = D$BATTER_SEQ_NUM 
B <- bs(a, knots=knots, degree=3, intercept = TRUE) # creating the B-splines
colnames(B) = paste0("B",1:ncol(B))
B = as_tibble(B)
S <- B
fit = fit_model_bsn(NA) 
saveRDS(fit, file = paste0(output_folder, "fit_", OUTPUT_FILE, ".rds"))

### plot
# fit <- readRDS("./job_output/fit_rstan5-1.R.rds") 
p = plot_bsn_spline(fit)
p
ggsave(paste0("./plot_",OUTPUT_FILE,".png"), p)

