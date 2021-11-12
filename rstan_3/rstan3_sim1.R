###############
#### SETUP ####
###############

# simulation of BATTER_SEQ_NUM model with TTO effects

output_folder = "./job_output/"
OUTPUT_FILE = "rstan3_sim_1.R" #FIXME
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
########### GENERATE DATA ###########
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

#################################################

# helpful constant
mu_y = mean(D$EVENT_WOBA_19)
sd_y = sd(D$EVENT_WOBA_19)
N = dim(X)[1]
P = dim(X)[2]
B = dim(BATTER_SEQ_dummies)[2] 
BB = 27

### GENERATE PARAMETERS
# all pitchers have the same constant effects
b = -.007
m = .001
delta_2 = .01 #.0172
delta_3 = .02 #.0153
k = 1:B
alpha = b + m*k + delta_2*(k>=10) + delta_3*(k>=19)
eta = c(.09, .07, -.02, .01)
sigma = .125
# generate y vector
epsilon = rnorm(N, mean=0, sd=sigma)
y = S%*%alpha + X%*%eta + epsilon 
y = as.numeric(y)

##############################################################
########### PLOT SIMULATED PARAMETER DISTRIBUTIONS ###########
##############################################################

# PLOT TRUE DISTRIBUTION OF ALPHA
df_alpha_true = data.frame(x=1:27,y=alpha[1:27])
true_alpha_plot = df_alpha_true %>%
  ggplot(aes(x=x, y=y)) +
  geom_point(color="dodgerblue2", shape=21, size=2, fill="white") +
  geom_vline(aes(xintercept = 9.5), size=1.2) +
  geom_vline(aes(xintercept = 18.5), size=1.2) +
  labs(title = paste(OUTPUT_FILE, "all pitchers have the same constant effects")) +
  #labs(title = TeX("Simulated distribution of $\\alpha$ parameters")) +
  theme(legend.position="none") +
  scale_x_continuous(name=TeX("Batter sequence number $k$"),limits = c(0,27.5), breaks = c(0,5,10,15,20,25)) +
  scale_y_continuous(
    name=TeX("Simulated distribution of $\\alpha_k$"),
    limits=c(-0.02,0.065), breaks=seq(-0.02,0.065,by=.01)
  )
true_alpha_plot
ggsave(paste0(output_folder, "plot_", OUTPUT_FILE, "_alphaTrue.png"), true_alpha_plot)

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

plot_alpha <- function(alpha, descriptor) {
  lower <- numeric(BB)
  avg <- numeric(BB)
  upper <- numeric(BB)
  for (i in 1:BB) {
    lower[i] = quantile(alpha[,i],.025)
    avg[i] = mean(alpha[,i])
    upper[i] = quantile(alpha[,i],.975)
  }
  AAA = data.frame(lower = lower,avg = avg,upper= upper,bn = 1:BB)
  AAA %>%
    ggplot(aes(x=bn, y=avg)) +
    geom_errorbar(aes(ymin = lower, ymax = upper), fill = "black", width = .4) +
    geom_point(color="dodgerblue2", shape=21, size=2, fill="white") +
    geom_vline(aes(xintercept = 9.5), size=1.2) +
    geom_vline(aes(xintercept = 18.5), size=1.2) +
    labs(title = TeX(sprintf("%s distribution of $\\alpha$ parameters", descriptor))) +
    theme(legend.position="none") +
    scale_x_continuous(name=TeX("Batter sequence number $k$"),limits = c(0,27.5), breaks = c(0,5,10,15,20,25)) +
    scale_y_continuous(
      name=TeX(sprintf("%s distribution of $\\alpha_k$", descriptor)),
      #limits=c(-0.02,0.065), 
      breaks=seq(-0.03,0.09,by=.01)
    )
}

plot_eta <- function(eta, descriptor) {
  eta_df = data.frame(eta)
  names(eta_df)  = c(
    TeX("$\\eta_{batWoba}$"),TeX("$\\eta_{pitWoba}$"), 
    TeX("$\\eta_{hand}$"),TeX("$\\eta_{home}$")
  )
  true_eta_plot = ggplot(gather(eta_df), aes(value)) + 
    geom_histogram(bins = 20, color = "black", fill = "grey") + 
    facet_wrap(~key, scales = 'free_x', labeller = label_parsed) +  
    labs(title=TeX(sprintf("%s distribution of $\\eta$ parameters", descriptor))) +
    scale_x_continuous(
      #limits = c(0.4,0.9),
      breaks= scales::pretty_breaks(n=4)) +
    theme(panel.spacing.x = unit(6, "mm")) +
    xlab(TeX("$\\eta$ value")) +
    ylab(TeX("density of $\\eta$")) + scale_y_discrete(breaks=NULL)
}

# draws and fit summary
draws <- as_tibble(as.matrix(fit))

alpha_post <- as.matrix(draws[,2:28])
eta_post <- as.matrix(draws[,(ncol(draws)-4):(ncol(draws)-1)])

post_alpha_plot = plot_alpha(alpha_post, "Posterior")
post_alpha_plot
ggsave(paste0(output_folder, "plot_", OUTPUT_FILE, "_alphaPost.png"), post_alpha_plot)

post_eta_plot = plot_eta(eta_post, "Posterior")
post_eta_plot
ggsave(paste0(output_folder, "plot_", OUTPUT_FILE, "_etaPost.png"), post_eta_plot)




