###############
#### SETUP ####
###############

# simulation of BATTER_SEQ_NUM model with TTO effects

output_folder = "./job_output/"
OUTPUT_FILE = "rstan3_sim_2.R" #FIXME
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
input_file = "./../data/TTO_dataset_411.csv"  #FIXME 
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

# helpful constant
mu_y = mean(D$EVENT_WOBA_19)
sd_y = sd(D$EVENT_WOBA_19)
N = dim(X)[1]
P = dim(X)[2]
B = dim(BATTER_SEQ_dummies)[2] 
BB = 27

### GENERATE PARAMETERS
# all pitchers have the same constant MEAN effects
b = -.007
m = .001
delta_2 = .01 #.0172
delta_3 = .02 #.0153
k = 1:B
alpha_mean = b + m*k + delta_2*(k>=10) + delta_3*(k>=19)
eta_mean = c(.09, .07, -.02, .01)
sigma = .125
# add iid homoscedastic noise to alpha and eta
nu_1 = .01
nu_2 = .025 # sd(D$_) == 0.5
alpha = do.call(rbind, replicate(N, alpha_mean + rnorm(B, mean=0, sd=nu_1), simplify=FALSE))
eta = do.call(rbind, replicate(N, eta_mean + rnorm(length(eta_mean), mean=0, sd=nu_2), simplify=FALSE))
# generate y vector
epsilon = rnorm(N, mean=0, sd=sigma)
S_x_alpha = rowSums(S*alpha)
X_x_eta = rowSums(X*eta)
y = S_x_alpha + X_x_eta + epsilon 

##############################################################
########### PLOT SIMULATED PARAMETER DISTRIBUTIONS ###########
##############################################################

# save the alpha and eta distributions
saveRDS(alpha, file = paste0(output_folder, "alpha_", OUTPUT_FILE, ".rds"))
saveRDS(eta, file = paste0(output_folder, "eta_", OUTPUT_FILE, ".rds"))
#alpha <- readRDS("job_output/alpha_rstan3_sim_2.R.rds") 
#eta <- readRDS("job_output/eta_rstan3_sim_2.R.rds") 

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

AAA_alpha = tibble(k=D$BATTER_SEQ_NUM, alpha=S_x_alpha) %>% 
  filter(k <= 27) %>% group_by(k) %>%
  summarise(lower = quantile(alpha,.025),
            avg = mean(alpha),
            upper = quantile(alpha, .975)) 
true_alpha_plot = plot_alpha(AAA_alpha,"Simulated")
true_alpha_plot
#ggsave(paste0(output_folder, "plot_", OUTPUT_FILE, "_alphaTrue.png"), true_alpha_plot)

# PLOT SIMULATED DISTRIBUTION OF ETA
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
true_eta_plot = plot_eta(eta, "Simulated")
true_eta_plot
#ggsave(paste0(output_folder, "plot_", OUTPUT_FILE, "_etaTrue.png"), true_eta_plot)


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
#alpha <- readRDS("job_output/alpha_rstan3_sim_2.R.rds") 
#eta <- readRDS("job_output/eta_rstan3_sim_2.R.rds") 

##############################################################
########### PLOT POSTERIOR PARAMETER DISTRIBUTIONS ###########
##############################################################

# draws and fit summary
draws <- as_tibble(as.matrix(fit))
alpha_post <- as.matrix(draws[,2:28])
eta_post <- as.matrix(draws[,(ncol(draws)-4):(ncol(draws)-1)])

# plot posterior distribution of alpha
{
  true_alpha_df = tibble(k=D$BATTER_SEQ_NUM, alpha=rowSums(S*alpha)) %>% 
    filter(k <= 27) %>% group_by(k) %>%
    summarise(lower = quantile(alpha,.025),
              avg = mean(alpha),
              upper = quantile(alpha, .975)) 
  colnames(alpha_post) = 1:ncol(alpha_post)
  post_alpha_df = gather(as_tibble(alpha_post)) %>% 
    mutate(key = as.numeric(key)) %>% rename(k = key) %>% arrange(k) %>%
    filter(k <= 27) %>% group_by(k) %>%
    summarise(lower = quantile(value,.025),
              avg = mean(value),
              upper = quantile(value, .975)) 
  true_alpha_df$post = "simulated"
  post_alpha_df$post = "posterior"
  alpha_df = bind_rows(true_alpha_df, post_alpha_df)
  alpha_plot = alpha_df %>% 
    ggplot(aes(x=k, y=avg)) +
    geom_errorbar(aes(ymin = lower, ymax = upper), fill = "black", width = .4) +
    geom_point(color="dodgerblue2", shape=21, size=2, fill="white") + 
    geom_vline(aes(xintercept = 9.5), size=1.2) +
    geom_vline(aes(xintercept = 18.5), size=1.2) +
    facet_wrap(~factor(post, levels=c("simulated", "posterior"))) +
    labs(title = TeX("Distribution of $\\alpha$")) +
    theme(legend.position="none") +
    scale_x_continuous(
      name=TeX("Batter sequence number $k$"),
      limits = c(0,27.5), breaks = c(0,5,10,15,20,25)) +
    scale_y_continuous(
      name=TeX("$\\alpha_k$"),
      #limits=c(-0.02,0.065), 
      breaks=seq(-0.05,0.09,by=.005)
    ) 
  alpha_plot
}
ggsave(paste0(output_folder, "plot_", OUTPUT_FILE, "_alphaPlot.png"), alpha_plot)


# plot posterior distribution of ETA
{
  #ETA.NAMES = paste0("eta_",1:(dim(eta)[2]))
  ETA.NAMES = c(
      TeX("$\\eta_{batWoba}$"),TeX("$\\eta_{pitWoba}$"),
      TeX("$\\eta_{hand}$"),TeX("$\\eta_{home}$")
  )
  eta.1 = as_tibble(eta)
  colnames(eta.1) = ETA.NAMES
  eta.2 = gather(eta.1)
  eta.2$post = "simulated"
  eta_post.1 = as_tibble(eta_post)
  colnames(eta_post.1) = ETA.NAMES
  eta_post.2 = gather(eta_post.1)
  eta_post.2$post = "posterior"
  eta_df = bind_rows(eta.2, eta_post.2)
  eta_mean_df = as_tibble(t(as.matrix(eta_mean)))
  names(eta_mean_df) = ETA.NAMES
  ### plot eta dists.
  eta_plot = ggplot(eta_df) +
    geom_density(aes(x=value, y=..scaled.., fill=post), alpha=0.4) +
    geom_vline(data=gather(eta_mean_df), aes(xintercept=value), color="firebrick") + 
    facet_wrap(~key, nrow=2, scales = 'free_x',labeller = label_parsed) +
    labs(title=TeX("Distributions of $\\eta$")) +
    xlab(TeX("$\\eta$")) #+ ylab(TeX("density of $\\eta$"))
  eta_plot
}
ggsave(paste0(output_folder, "plot_", OUTPUT_FILE, "_etaPlot.png"), eta_plot)


