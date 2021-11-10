###############
#### SETUP ####
###############

# simulation of BATTER_SEQ_NUM model

output_folder = "./job_output/"
OUTPUT_FILE = "rstan2_sim_2.R" #FIXME
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

##### explore the real data before creating simulated data.

# # read data
# input_file = "./../data/design_matrix2_3.csv" #FIXME
# D <- read_csv(input_file)
# D <- D %>% drop_na()
# # create dummy variables for the categorical variables
# # NO INTERCEPT and INCLUDE FIRST COLUMN
# change_factor_names <- function(s) {
#   s <- str_remove(s, "factor")
#   s <- str_remove_all(s, "\\(")
#   s <- str_remove_all(s, "\\)")
#   s
# }
# # categorical dummies for BATTER_SEQ_NUM
# BATTER_SEQ_dummies <- D %>% modelr::model_matrix(~ factor(BATTER_SEQ_NUM) + 0)
# names(BATTER_SEQ_dummies) <- change_factor_names(names(BATTER_SEQ_dummies))
# # data
# y <- D %>% select(std_EVENT_WOBA_19)
# X <- bind_cols(BATTER_SEQ_dummies, D %>% select(std_WOBA_FINAL_BAT_19, std_WOBA_FINAL_PIT_19, HAND_MATCH, BAT_HOME_IND))
# fit <- readRDS("job_output/fit_rstan2_2_removePit.R.rds")
# # draws and fit summary
# NAMES <- c("sigma", names(X), "lp__")
# s <- summary(fit)$summary
# rownames(s) <- NAMES
# #plot(s[2:28,1])
# d = data.frame(x=1:27,y=s[2:28,1])
# m = lm(y~x, data=d)
# ## Coefficients: (Intercept) -0.0065590, x 0.0009337, sigma 0.005033198
# #d %>% ggplot() + geom_point(aes(x=x,y=y)) + geom_abline(intercept=m$coefficients[1],slope=m$coefficients[2])
# 

#################################################

### CHOOSE TRUE PARAMETERS
G = 2430
B = 27
N = G*B
x = 1:27
alpha_mean = -0.007 + 0.001*x # coefficients(m) 
alpha_mean = alpha_mean + rnorm(B, sd=.0015)
tau1 = 0.0025 # sd of noise added to alpha_mean
alpha = do.call(rbind, replicate(G, alpha_mean + rnorm(B, mean=0, sd=tau1), simplify=FALSE))
eta_mean = c(.09, .07, -.02, .01) # s[40:43,1]
tau2 = 0.001 # sd of noise added to eta_mean # s[40:43,]
eta = do.call(rbind, replicate(N, eta_mean + rnorm(length(eta_mean), mean=0, sd=tau2), simplify=FALSE))
sig = 0.2 #FIXME - this is what's different in this file # sigma(m) 

# PLOT TRUE DISTRIBUTION OF ALPHA
plot_alpha <- function(alpha, descriptor) {
  lower <- numeric(B)
  avg <- numeric(B)
  upper <- numeric(B)
  for (i in 1:B) {
    lower[i] = quantile(alpha[,i],.025)
    avg[i] = mean(alpha[,i])
    upper[i] = quantile(alpha[,i],.975)
  }
  AAA = data.frame(lower = lower,avg = avg,upper= upper,bn = 1:B)
  AAA %>% 
    ggplot(aes(x=bn, y=avg)) +
    geom_errorbar(aes(ymin = lower, ymax = upper), fill = "black", width = .4) +
    geom_point(color="dodgerblue2", shape=21, size=2, fill="white") + 
    geom_vline(aes(xintercept = 9.5), size=1.2) +
    geom_vline(aes(xintercept = 18.5), size=1.2) +
    labs(title = TeX(sprintf("%s distribution of $\\alpha$ parameters", descriptor))) +
    theme(legend.position="none") +
    scale_x_continuous(name=TeX("Batter sequence number $k$"),limits = c(0,28), breaks = c(0,5,10,15,20,25)) +
    scale_y_continuous(name=TeX(sprintf("%s distribution of $\\alpha_k$", descriptor))) 
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

# generate S matrix
S_x_alpha = as.vector(t(alpha))
S0 = diag(B)
S = do.call(rbind, replicate(G, S0, simplify=FALSE))
# generate X matrix
x_b = rnorm(N) # x_b ~ normal(0,1)
x_p = rnorm(N) # x_p ~ normal(0,1)
hand = as.numeric(rbernoulli(N, p=0.55)) # HAND_MATCH ~ bernoulli(0.55) # sum(X$HAND_MATCH)/length(X$HAND_MATCH)
home = as.numeric(rbernoulli(N, p=0.5)) # BAT_HOME_IND ~ bernoulli(0.5) # sum(X$BAT_HOME_IND)/length(X$BAT_HOME_IND)
X = cbind(x_b, x_p, hand, home)
X_x_eta = rowSums(X*eta)
# generate y vector
epsilon = rnorm(N, mean=0, sd=sig)
y = S_x_alpha + X_x_eta + epsilon 
### check
f <- function(x) mean(y[seq(x,length(y),by=27)])
plot(1:27, sapply(1:27, f))

# package the generated data
# package and save the generated data
data_gen <- list(S=S,X=X,y=y,alpha=alpha,eta=eta)
saveRDS(data_gen, file = paste0(output_folder, "genData_", OUTPUT_FILE, ".rds"))
#data_gen <- readRDS("job_output/genData_rstan2_sim_1.R.rds") 

#############################
########### RSTAN ###########
#############################

# compile rstan model
seed = 12345
set.seed(seed)
file = 'tto2_1.stan' #FIXME
model <- stan_model(file = file, model_name = file)

# training data
XX = cbind(S,X)
data_train <- list(y = y, X = XX, n = nrow(XX), p = ncol(XX))

# Train the models
fit <- sampling(model,
                data = data_train,
                iter = NUM_ITERS_IN_CHAIN,
                chains = cores, #1 #cores, 
                cores = cores, # HPCC
                seed = seed)
# save the stan objects
saveRDS(fit, file = paste0(output_folder, "fit_", OUTPUT_FILE, ".rds"))
#fit <- readRDS("job_output/fit_rstan2_sim_1.R.rds") 

#############################
########### PLOTS ###########
#############################

# draws and fit summary
draws <- as_tibble(as.matrix(fit))

alpha_post <- as.matrix(draws[,2:28])
eta_post <- as.matrix(draws[,29:32])

post_alpha_plot = plot_alpha(alpha_post, "Posterior")
post_alpha_plot
ggsave(paste0(output_folder, "plot_", OUTPUT_FILE, "_postAlpha.png"), post_alpha_plot)

post_eta_plot = plot_eta(eta_post, "Posterior")
post_eta_plot
ggsave(paste0(output_folder, "plot_", OUTPUT_FILE, "_postEta.png"), post_eta_plot)

