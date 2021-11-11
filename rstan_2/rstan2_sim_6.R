###############
#### SETUP ####
###############

# simulation of BATTER_SEQ_NUM model with TTO effects

output_folder = "./job_output/"
OUTPUT_FILE = "rstan2_sim_6.R" #FIXME
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
# X data matrix 
X <- bind_cols(BATTER_SEQ_dummies, D %>% select(std_WOBA_FINAL_BAT_19, std_WOBA_FINAL_PIT_19, HAND_MATCH, BAT_HOME_IND))

#################################################

### CHOOSE TRUE PARAMETERS
N = dim(X)[1]
B = dim(BATTER_SEQ_dummies)[2] #27
BB = 27
P = dim(X)[2]
x = 1:B

TTO2_effect = .01
TTO3_effect = .02
TTO4_effect = 0
#alpha_mean = -0.007 + 0.001*x # coefficients(m) 
alpha_mean = 
  -0.007 + 0.001*x + 
  (floor((x-1)/9)==1)*TTO2_effect + 
  (floor((x-1)/9)==2)*(TTO2_effect+TTO3_effect) +
  (floor((x-1)/9)==3)*(TTO2_effect+TTO3_effect+TTO4_effect)
alpha_mean = alpha_mean + rnorm(B, sd=.0015)
tau1 = 0.0025 # sd of noise added to alpha_mean
alpha = do.call(rbind, replicate(N, alpha_mean + rnorm(B, mean=0, sd=tau1), simplify=FALSE)) #G

eta_mean = c(.09, .07, -.02, .01) # s[40:43,1]
tau2 = 0.001 # sd of noise added to eta_mean # s[40:43,]
eta = do.call(rbind, replicate(N, eta_mean + rnorm(length(eta_mean), mean=0, sd=tau2), simplify=FALSE))

sig = 0.125 #FIXME ???

# PLOT TRUE DISTRIBUTION OF ALPHA
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
      limits=c(-0.02,0.06)
    ) 
}
true_alpha_plot = plot_alpha(alpha, "True")
true_alpha_plot
ggsave(paste0(output_folder, "plot_", OUTPUT_FILE, "_trueAlpha.png"), true_alpha_plot)

# PLOT TRUE DISTRIBUTION OF ETA
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
true_eta_plot = plot_eta(eta, "True")
true_eta_plot
ggsave(paste0(output_folder, "plot_", OUTPUT_FILE, "_trueEta.png"), true_eta_plot)

# generate y vector
epsilon = rnorm(N, mean=0, sd=sig)
beta = cbind(alpha, eta)
X_x_beta = rowSums(X*beta)
y = X_x_beta + epsilon 

#############################
########### RSTAN ###########
#############################

# compile rstan model
seed = 12345
set.seed(seed)
file = 'tto2_1.stan' #FIXME
model <- stan_model(file = file, model_name = file)

# training data
data_train <- list(y = y, X = X, n = nrow(X), p = ncol(X))

# Train the models
fit <- sampling(model,
                data = data_train,
                iter = NUM_ITERS_IN_CHAIN,
                chains = cores, #1 #cores, 
                cores = cores, # HPCC
                seed = seed)
# save the stan objects
saveRDS(fit, file = paste0(output_folder, "fit_", OUTPUT_FILE, ".rds"))
#fit <- readRDS("job_output/fit_rstan2_sim_5.R.rds") 

#############################
########### PLOTS ###########
#############################

# draws and fit summary
draws <- as_tibble(as.matrix(fit))

alpha_post <- as.matrix(draws[,2:28])
eta_post <- as.matrix(draws[,(ncol(draws)-4):(ncol(draws)-1)])

post_alpha_plot = plot_alpha(alpha_post, "Posterior")
post_alpha_plot
ggsave(paste0(output_folder, "plot_", OUTPUT_FILE, "_postAlpha.png"), post_alpha_plot)

# alpha_plots = cowplot::plot_grid(true_alpha_plot, post_alpha_plot)
# alpha_plots
# cowplot::save_plot(paste0(output_folder, "plot_", OUTPUT_FILE, "_alphaPlot.png"),alpha_plots)

post_eta_plot = plot_eta(eta_post, "Posterior")
post_eta_plot
ggsave(paste0(output_folder, "plot_", OUTPUT_FILE, "_postEta.png"), post_eta_plot)



