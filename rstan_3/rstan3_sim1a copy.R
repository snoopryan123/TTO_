###############
#### SETUP ####
###############

# simulation of BATTER_SEQ_NUM model with TTO effects

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
#p_x = dim(X)[2]
p_s = dim(S)[2] 
# p_u = dim(U)[2] 
# p_o = dim(O)[2] 
# BB = 27

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

#############################
########### RSTAN ###########
#############################

# compile rstan model
seed = 12345
set.seed(seed)
file = 'tto3_bsn.stan' #FIXME
model <- stan_model(file = file, model_name = file)
# training data
data_train <- list(y=y,S=S,X=X,n=nrow(X),p_s=ncol(S),p_x=ncol(X))
# Train the models
fit <- sampling(model,
                data = data_train,
                iter = NUM_ITERS_IN_CHAIN,
                chains = cores, #1 #cores, 
                cores = cores, # HPCC
                seed = seed)
# save the stan objects
saveRDS(fit, file = paste0(output_folder, "fit_", OUTPUT_FILE, ".rds"))
#fit <- readRDS("job_output/fit_rstan3_sim_1.R.rds") 

##############################################################
########### PLOT POSTERIOR PARAMETER DISTRIBUTIONS ###########
##############################################################

# draws and fit summary
draws <- as_tibble(as.matrix(fit))
alpha_post <- as.matrix(draws[,2:28])
eta_post <- as.matrix(draws[,(ncol(draws)-4):(ncol(draws)-1)])

# plot posterior distribution of alpha
{
  lower <- numeric(BB)
  avg <- numeric(BB)
  upper <- numeric(BB)
  for (i in 1:BB) {
    lower[i] = quantile(alpha_post[,i],.025)
    avg[i] = mean(alpha_post[,i])
    upper[i] = quantile(alpha_post[,i],.975)
  }
  AAA = data.frame(lower = lower,avg = avg,upper= upper,bn = 1:BB)
  alpha_plot = AAA %>%
    ggplot(aes(x=bn, y=avg)) +
    geom_errorbar(aes(x=bn, y=avg, ymin = lower, ymax = upper), fill = "black", width = .4) +
    geom_point(color="dodgerblue2", shape=21, size=2, fill="white") +
    geom_point(aes(x=bn,y=df_alpha_true$y), color="firebrick") +
    geom_vline(aes(xintercept = 9.5), size=1.2) +
    geom_vline(aes(xintercept = 18.5), size=1.2) +
    labs(title = TeX("Posterior distribution of $\\alpha$")) +
    scale_x_continuous(name=TeX("Batter sequence number $k$"),limits = c(0,27.5), breaks = c(0,5,10,15,20,25)) +
    scale_y_continuous(
      name=TeX("$\\alpha_k$"),
      #limits=c(-0.02,0.065), 
      breaks=seq(-0.03,0.09,by=.01)
    ) +
    theme(legend.position="none") 
  alpha_plot
}
ggsave(paste0(output_folder, "plot_", OUTPUT_FILE, "_alphaPlot.png"), alpha_plot)

# plot posterior distribution of eta

{
  ETA.NAMES = c(
    TeX("$\\eta_{batWoba}$"), TeX("$\\eta_{pitWoba}$"), 
      TeX("$\\eta_{hand}$"), TeX("$\\eta_{home}$")
  )
  #ETA.NAMES = paste0("eta",1:length(eta))
  eta_df = data.frame(eta_post)
  names(eta_df) = ETA.NAMES
  eta_mean_df = as_tibble(t(as.matrix(eta)))
  names(eta_mean_df) = ETA.NAMES
  eta_plot = ggplot() + 
    geom_histogram(data=gather(eta_df), aes(value), bins = 20, fill = "grey") + 
    geom_vline(data=gather(eta_mean_df), aes(xintercept=value), color="firebrick") + 
    facet_wrap(~key, scales = 'free_x', labeller = label_parsed) +  #label_parsed
    theme(legend.position="none") +
    labs(title=TeX("Posterior distribution of $\\eta$")) +
    theme(panel.spacing.x = unit(6, "mm")) +
    xlab(TeX("$\\eta$")) +
    ylab(TeX("density")) + 
    scale_y_discrete(breaks=NULL)
  eta_plot
}
ggsave(paste0(output_folder, "plot_", OUTPUT_FILE, "_etaPlot.png"), eta_plot)
