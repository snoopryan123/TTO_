library(tidyverse)
library(splines)
output_folder = './' #'./job_output/'

### load data
input_file = "../../data/TTO_dataset_510.csv"  
D <- read_csv(input_file)
D <- D %>% drop_na() 
#FIXME 
D <- D %>% filter(YEAR == 2019) 
X <- as.matrix(D %>% select(std_WOBA_FINAL_BAT_19, std_WOBA_FINAL_PIT_19, HAND_MATCH, BAT_HOME_IND)) 
OUTPUT_FILE = "rstan5_plots-7.R"
fit <- readRDS("../job_output/fit_rstan5-7.R.rds") 
# cubic spline over BATTER_SEQ_NUM
# https://mc-stan.org/users/documentation/case-studies/splines_in_stan.html
#knots = c(9,9,9,9,18,18,18,18,27,27,27,27,36,36,36,36)
knots = c(9,9,9,18,18,18,27,27,27,36,36,36)
a = D$BATTER_SEQ_NUM 
B <- bs(a, knots=knots, degree=3, intercept = TRUE) # creating the B-splines
colnames(B) = paste0("B",1:ncol(B))
B = as_tibble(B)

source("rstan5_plots_main.R")

#############################
########### PLOTS ###########
#############################

# plot spline trajectory 
p3 = plot_bsn_spline(fit)
p3
ggsave(paste0("./plot_spline_",OUTPUT_FILE,".png"), p3)





