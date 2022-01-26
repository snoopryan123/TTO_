library(tidyverse)
output_folder = './job_output/'

# load data
input_file = "../../data/TTO_dataset_510.csv"  
D <- read_csv(input_file) #; D <- D %>% drop_na() 
D <- D %>% filter(YEAR == 2019) 
X <- as.matrix(D %>% select(std_BQ, std_PQ, #FIXME 
                            HAND_MATCH, BAT_HOME_IND)) 
OUTPUT_FILE = "rstan5_sim1-33.R" 
source("rstan5_sim1_bsn_main.R")

# generate new epsilon and y vector
set.seed(3) #FIXME 
epsilon = rnorm(N, mean=0, sd=sigma)
y = S%*%alpha + X%*%eta + epsilon 
saveRDS(epsilon, file = paste0(output_folder, "epsilon_", OUTPUT_FILE, ".rds"))
saveRDS(y, file = paste0(output_folder, "y_", OUTPUT_FILE, ".rds"))
#epsilon <- readRDS("job_output/epsilon_rstan5_sim1-1.R.rds") 
#y <- readRDS("job_output/y_rstan5_sim1-1.R.rds") 

# fit the model
fit = fit_model_bsn(fold_num=1)
saveRDS(fit, file = paste0(output_folder, "fit_", OUTPUT_FILE, ".rds"))
#fit <- readRDS("job_output/fit_rstan5_sim1-1.R.rds") 




##########################
#### PLOTS (to check) ####
##########################

draws <- as_tibble(as.matrix(fit))
alpha_post <- as.matrix(draws[,2:28])
eta_post <- as.matrix(draws[,(ncol(draws)-4):(ncol(draws)-1)])
alpha_plot = plot_alpha_post(alpha_post)
alpha_plot
eta_plot = plot_eta_post(eta_post)
eta_plot
#ggsave(paste0(output_folder, "plot_", OUTPUT_FILE, "_alphaPlot.png"), alpha_plot)
#ggsave(paste0(output_folder, "plot_", OUTPUT_FILE, "_etaPlot.png"), eta_plot)

