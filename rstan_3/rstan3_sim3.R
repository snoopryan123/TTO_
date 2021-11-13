###############
#### SETUP ####
###############

# simulation of BATTER_SEQ_NUM model with TTO effects

output_folder = "./job_output/"
OUTPUT_FILE = "rstan3_sim_3.R" #FIXME
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
# Observed data matrices 
S <- as.matrix(BATTER_SEQ_dummies)
X <- as.matrix(D %>% select(std_WOBA_FINAL_BAT_19, std_WOBA_FINAL_PIT_19, HAND_MATCH, BAT_HOME_IND))

#####################################
########### GENERATE DATA ###########
#####################################

# helpful constants
mu_y = mean(D$EVENT_WOBA_19)
sd_y = sd(D$EVENT_WOBA_19)
N = dim(X)[1]
P = dim(X)[2]
B = dim(BATTER_SEQ_dummies)[2] 
BB = 27

### GENERATE PARAMETERS
# each pitcher p has his own constant effects, sampled using these hyperparameters
b= -.007
m = .001
delta_2 = .01 #.0172
delta_3 = .02 #.0153
tau0 = .01
tau1 = .002
tau2 = .01
tau3 = .01
eta = c(.09, .07, -.02, .01)
tau4 = .025
sigma = .125
# each pitcher p has his own parameters
pit = unique(D$PIT_ID)
num_pit = length(pit)
b_p = rnorm(num_pit, mean=b, sd=tau0)
m_p = rnorm(num_pit, mean=m, sd=tau1)
delta_2_p = rnorm(num_pit, mean=delta_2, sd=tau2)
delta_3_p = rnorm(num_pit, mean=delta_3, sd=tau3)
eta_p = as_tibble(MASS::mvrnorm(n=num_pit, mu=eta, Sigma=tau4*diag(length(eta))))
colnames(eta_p) = paste0("eta_p_",1:length(eta))
eta_p = as_tibble(eta_p)
# tibble formulalation
temp1 = tibble(PIT_ID = pit, b_p, m_p, delta_2_p, delta_3_p, eta_p)
E <- D %>% select(YEAR, GAME_ID, PIT_ID, BATTER_SEQ_NUM) %>% 
  left_join(temp1) %>% 
  rename(k = BATTER_SEQ_NUM) %>% 
  mutate(alpha = b_p + m_p*k + delta_2_p*(k>=10) + delta_3_p*(k>=19))
# generate y vector
epsilon = rnorm(N, mean=0, sd=sigma)
eta_ = E %>% select(eta_p_1,eta_p_2,eta_p_3,eta_p_4)
X_x_eta = rowSums(X*eta_)
y = E$alpha + X_x_eta + epsilon

##############################################################
########### PLOT SIMULATED PARAMETER DISTRIBUTIONS ###########
##############################################################

# PLOT SIMULATED DISTRIBUTION OF ALPHA
plot_alpha <- function(AAA, descriptor) {
  AAA %>% 
    ggplot(aes(x=k, y=avg)) +
    geom_errorbar(aes(ymin = lower, ymax = upper), fill = "black", width = .4) +
    geom_point(color="dodgerblue2", shape=21, size=2, fill="white") + 
    geom_vline(aes(xintercept = 9.5), size=1.2) +
    geom_vline(aes(xintercept = 18.5), size=1.2) +
    labs(title = TeX(sprintf("%s distribution of $\\alpha$ parameters", descriptor))) +
    theme(legend.position="none") +
    scale_x_continuous(name=TeX("Batter sequence number $k$"),limits = c(0,27.5), breaks = c(0,5,10,15,20,25)) +
    scale_y_continuous(
      name=TeX(sprintf("%s distribution of $\\alpha_k$", descriptor)),
      #limits=c(-0.02,0.065), breaks=seq(-0.02,0.065,by=.01)
    ) 
}

true_alpha_df = tibble(k=D$BATTER_SEQ_NUM, alpha=E$alpha) %>% 
  filter(k <= 27) %>% group_by(k) %>%
  summarise(lower = quantile(alpha,.025),
            avg = mean(alpha),
            upper = quantile(alpha, .975)) 
true_alpha_plot = plot_alpha(true_alpha_df,"Simulated")
true_alpha_plot
ggsave(paste0(output_folder, "plot_", OUTPUT_FILE, "_alphaTrue.png"), true_alpha_plot)

# PLOT SIMULATED DISTRIBUTION OF ETA
plot_eta <- function(eta, descriptor) {
  eta_df = data.frame(eta)
  names(eta_df)  = c(
    TeX("$\\eta_{batWoba}$"),TeX("$\\eta_{pitWoba}$"), 
    TeX("$\\eta_{hand}$"),TeX("$\\eta_{home}$")
  )
  true_eta_plot = ggplot(gather(eta_df), aes(value)) + 
    geom_histogram(bins = 30, color = "black", fill = "grey") + 
    facet_wrap(~key, scales = 'free_x', labeller = label_parsed) +  
    labs(title=TeX(sprintf("%s distribution of $\\eta$ parameters", descriptor))) +
    scale_x_continuous(
      #limits = c(0.4,0.9),
      breaks= scales::pretty_breaks(n=4)) +
    theme(panel.spacing.x = unit(6, "mm")) +
    xlab(TeX("$\\eta$ value")) +
    ylab(TeX("density of $\\eta$")) + scale_y_discrete(breaks=NULL)
}
true_eta_plot = plot_eta(eta_, "Simulated")
true_eta_plot
ggsave(paste0(output_folder, "plot_", OUTPUT_FILE, "_etaTrue.png"), true_eta_plot)


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
#fit <- readRDS("job_output/fit_rstan3_sim_2.R.rds") 

##############################################################
########### PLOT POSTERIOR PARAMETER DISTRIBUTIONS ###########
##############################################################

# draws and fit summary
draws <- as_tibble(as.matrix(fit))
alpha_post <- as.matrix(draws[,2:28])
eta_post <- as.matrix(draws[,(ncol(draws)-4):(ncol(draws)-1)])

# plot posterior distribution of alpha
post_alpha_df = tibble(k=1:BB,lower=numeric(BB),avg=numeric(BB),upper=numeric(BB)) %>%
  group_by(k) %>%
  mutate(lower = quantile(alpha_post[,k],.025),
         avg = mean(alpha_post[,k]),
         upper = quantile(alpha_post[,k], .975)) %>%
  ungroup()
post_alpha_plot = plot_alpha(post_alpha_df, "Posterior")
post_alpha_plot
ggsave(paste0(output_folder, "plot_", OUTPUT_FILE, "_alphaPost.png"), post_alpha_plot)

# plot posterior distribution of eta
post_eta_plot = plot_eta(eta_post, "Posterior")
post_eta_plot
ggsave(paste0(output_folder, "plot_", OUTPUT_FILE, "_etaPost.png"), post_eta_plot)




