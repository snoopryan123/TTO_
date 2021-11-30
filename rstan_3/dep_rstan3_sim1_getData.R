#################################################################
### simulation 1: all pitchers have the same constant effects ###
#################################################################

output_folder = "./job_output/"
OUTPUT_FILE = "rstan3_sim_1a.R" #FIXME
NUM_ITERS_IN_CHAIN = 1500 #FIXME #10 

library(tidyverse)
library(rstan)
library(ggthemes)
library(latex2exp)
theme_set(theme_bw())
theme_update(plot.title = element_text(hjust = 0.5))
if(!interactive()) pdf(NULL)
cores = strtoi(Sys.getenv('OMP_NUM_THREADS')) ### for HPCC
options(mc.cores = cores) ### for HPCC
# options(mc.cores = parallel::detectCores()) # use this on my computer
rstan_options(auto_write = TRUE)

#####################################
########### OBSERVED DATA ###########
#####################################

# USE THE ACTUAL X DATA MATRIX FROM 2019 
# read data
input_file = "./../data/design_matrix2_3.csv" #FIXME
D <- read_csv(input_file)
D <- D %>% drop_na() %>% filter(YEAR == 2019)
# NO INTERCEPT and INCLUDE FIRST COLUMN
change_factor_names <- function(s) {
  s <- str_remove(s, "factor")
  s <- str_remove_all(s, "\\(")
  s <- str_remove_all(s, "\\)")
  s
}
# categorical dummies for BATTER_SEQ_NUM
BATTER_SEQ_dummies <- D %>% modelr::model_matrix(~ factor(BATTER_SEQ_NUM) + 0)
names(BATTER_SEQ_dummies) <- change_factor_names(names(BATTER_SEQ_dummies))
# categorical dummies for BATTER_IDX
BATTER_IDX_dummies <- D %>% modelr::model_matrix(~ factor(BATTER_IDX) + 0) 
names(BATTER_IDX_dummies) <- change_factor_names(names(BATTER_IDX_dummies))
# categorical dummies for ORDER_CT
ORDER_CT_dummies <- D %>% modelr::model_matrix(~ factor(ORDER_CT) + 0) 
names(ORDER_CT_dummies) <- change_factor_names(names(ORDER_CT_dummies))
# Observed data matrices 
S <- as.matrix(BATTER_SEQ_dummies)
U <- as.matrix(BATTER_IDX_dummies)
O <- as.matrix(ORDER_CT_dummies)
X <- as.matrix(D %>% select(std_WOBA_FINAL_BAT_19, std_WOBA_FINAL_PIT_19, HAND_MATCH, BAT_HOME_IND))

#####################################
########### GENERATE DATA ###########
#####################################

# helpful constant
# mu_y = mean(D$EVENT_WOBA_19)
# sd_y = sd(D$EVENT_WOBA_19)
n = dim(X)[1]
p_s = dim(S)[2] 
p_u = dim(U)[2] 

### GENERATE PARAMETERS
# all pitchers have the same constant effects
b = -.007
m = .001
delta_2 = .01 #.0172
delta_3 = .02 #.0153
eta = c(.09, .07, -.02, .01)
sigma = .125
# alpha, beta, gamma
k = 1:p_s
alpha = b + m*k + delta_2*(k>=10) + delta_3*(k>=19) # plot(1:27, alpha[1:27])
beta = b + m*(1:p_u)
gamma = c(0, delta_2+9*m, delta_2+delta_3+18*m, delta_2+delta_3+27*m)
# check
m1_tto = (rep(beta[1:9],4) + c(rep(gamma[1],9), rep(gamma[2],9), rep(gamma[3],9), rep(gamma[4],9)))[1:p_s]
alpha
m1_tto
sum(m1_tto-alpha) < .00000001
#plot(alpha)
# generate y vector
epsilon = rnorm(n, mean=0, sd=sigma)
y0 = S%*%alpha + X%*%eta + epsilon 
y1 = U%*%beta + O%*%gamma + X%*%eta + epsilon
# keep only the rows where the 2 models generate the same y
sum(y0==y1)/length(y0)
same_y_idxs = which(y0==y1) 
y = as.numeric(y0[same_y_idxs])
X = X[same_y_idxs,]
S = S[same_y_idxs,]
U = U[same_y_idxs,]
O = O[same_y_idxs,]
n = dim(X)[1]
p_x = dim(X)[2]
p_s = dim(S)[2] 
p_u = dim(U)[2] 
p_o = dim(O)[2] 




