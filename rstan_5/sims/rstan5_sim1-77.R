library(tidyverse)
output_folder = './job_output/'

# load data
input_file = "../../data/TTO_dataset_510.csv"  
D <- read_csv(input_file) #; D <- D %>% drop_na() 
D <- D %>% filter(YEAR == 2019) 
X <- as.matrix(D %>% select(std_WOBA_FINAL_BAT_19, std_WOBA_FINAL_PIT_19, #FIXME 
                            HAND_MATCH, BAT_HOME_IND)) 
s = 17 #FIXME
OUTPUT_FILE = paste0("rstan5_sim1-",60+s,".R")
source("rstan5_sim1_bsn_main.R")

# generate new epsilon and y vector
set.seed(s) #FIXME 
epsilon = rnorm(N, mean=0, sd=sigma)
y = U%*%beta + O%*%gamma + X%*%eta + epsilon #FIXME
saveRDS(epsilon, file = paste0(output_folder, "epsilon_", OUTPUT_FILE, ".rds"))
saveRDS(y, file = paste0(output_folder, "y_", OUTPUT_FILE, ".rds"))
#epsilon <- readRDS("job_output/epsilon_rstan5_sim1-1.R.rds") 
#y <- readRDS("job_output/y_rstan5_sim1-1.R.rds") 

# fit the model
fit = fit_model_ubi(fold_num=1) #FIXME
saveRDS(fit, file = paste0(output_folder, "fit_", OUTPUT_FILE, ".rds"))
#fit <- readRDS("job_output/fit_rstan5_sim1-1.R.rds") 




##########################
#### PLOTS (to check) ####
##########################

# PLOTS
draws <- as_tibble(as.matrix(fit))
beta_post <- as.matrix(draws[,2:15])
gamma_post <- as.matrix(draws[,(ncol(draws)-8):(ncol(draws)-5)])
delta_post <- as.matrix(draws[,(ncol(draws)-4):(ncol(draws)-1)])
beta_plus_gamma_plot = plot_beta_plus_gamma_post(beta_post, gamma_post)
beta_plus_gamma_plot
delta_plot = plot_delta_post(delta_post)
delta_plot
#ggsave(paste0(output_folder, "plot_", OUTPUT_FILE, "_betaGammePlot.png"), beta_plus_gamma_plot)
#ggsave(paste0(output_folder, "plot_", OUTPUT_FILE, "_etaPlot.png"), delta_plot)

