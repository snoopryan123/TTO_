library(tidyverse)
output_folder = './' #'./job_output/'

### load data
input_file = "../../data/TTO_dataset_510.csv"  
D <- read_csv(input_file)
D <- D %>% drop_na() 
#FIXME 
D <- D %>% filter(YEAR == 2019) 
X <- as.matrix(D %>% select(std_WOBA_FINAL_BAT_19, std_WOBA_FINAL_PIT_19, HAND_MATCH, BAT_HOME_IND)) 
OUTPUT_FILE = "rstan5_plots-1.R"
fit <- readRDS("../job_output/fit_rstan5-1.R.rds") 

source("rstan5_plots_main.R")

#############################
########### PLOTS ###########
#############################

# plot full trajectory 
p = plot_bsn0(fit)
p
ggsave(paste0("./plot_series_",OUTPUT_FILE,".png"), p)

# plot 3 panel histogram B1->B2, B9->B10, B18->B19
p1 = plot_3hist_bsn1(fit)
p1
ggsave(paste0("./plot_3hist1_",OUTPUT_FILE,".png"), p1)

# plot 3 panel histogram avg TTO1, TTO2, TTO3
p2 = plot_3hist_bsn2(fit)
p2
ggsave(paste0("./plot_3hist2_",OUTPUT_FILE,".png"), p2)

# plot spline trajectory 
p3 = plot_bsn_spline(fit)
p3
ggsave(paste0("./plot_spline_",OUTPUT_FILE,".png"), p)





