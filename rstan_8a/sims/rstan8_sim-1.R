library(tidyverse)
output_folder = './job_output/'

### load data
s = 1 #FIXME
OUTPUT_FILE = paste0("rstan8_sim-",s,".R")
input_file = "../../data/TTO_dataset_510.csv"  
D <- read_csv(input_file) #%>% drop_na() 
D <- D %>% filter(YEAR == 2019) %>% filter(BQ>0 & BQ<1 & PQ>0 & PQ<1 )
logit <- function(p) { log(p/(1-p)) }
X <- as.matrix(D %>% mutate(lBQ=logit(BQ), lPQ=logit(PQ)) %>% select(lBQ, lPQ, HAND_MATCH, BAT_HOME_IND)) 
source("rstan8_sim_main.R")

# generate categorical outcome vector y
linpred = U %*% t(beta) + O %*% t(gamma) + X %*% t(eta)
P = exp(linpred) / rowSums( exp(linpred) )
get_outcome <- function(i) { # get the categorical outcome in {1,2,...,7} of row i
  which(rmultinom(1, 1, P[i,]) == 1)
}
set.seed(s) 
y = matrix( sapply(1:nrow(linpred), get_outcome), ncol=1)
saveRDS(y, file = paste0(output_folder, "y_", OUTPUT_FILE, ".rds"))
#y <- readRDS("./job_output/y_rstan8_sim-1.R.rds") 

# fit the model
fit = fit_model_ubi(fold_num=1) #FIXME
saveRDS(fit, file = paste0(output_folder, "fit_", OUTPUT_FILE, ".rds"))
#fit <- readRDS("./job_output/fit_rstan8_sim-1.R.rds") 




