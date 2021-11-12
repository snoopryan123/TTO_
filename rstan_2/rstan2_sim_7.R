###############
#### SETUP ####
###############

# simulation of BATTER_SEQ_NUM model with TTO effects

output_folder = "./job_output/"
OUTPUT_FILE = "rstan2_sim_7.R" #FIXME
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
pit <- D$PIT_ID
# X data matrix 
X <- bind_cols(BATTER_SEQ_dummies, D %>% select(std_WOBA_FINAL_BAT_19, std_WOBA_FINAL_PIT_19, HAND_MATCH, BAT_HOME_IND))

#################################################

mu_y = mean(D$EVENT_WOBA_19)
sd_y = sd(D$EVENT_WOBA_19)

### CHOOSE TRUE PARAMETERS
N = dim(X)[1]
B = dim(BATTER_SEQ_dummies)[2] #27
BB = 27
P = dim(X)[2]



### GENERATE ALPHA_K DISTRIBUTIONS
# each pitcher has his own mean TTO effects
delta_2 = .01#.0172 # mean TTO2 effect #.01
delta_3 = .02#.0153 # mean TTO3 effect #.02
tau_2 = delta_2/1.645 /20
tau_3 = delta_3/1.645 /20
num_pit = length(unique(pit))
TTO2_mean_fx = rnorm(num_pit, mean=delta_2, sd=tau_2)
TTO3_mean_fx = rnorm(num_pit, mean=delta_3, sd=tau_3)
# each pitcher has his own mean intercept and slope of fatigue
mu_0 = -.015
mu_1 = .001
nu_0 = .0025
nu_1 = .0001 
incpt_mean_fx = rnorm(num_pit, mean= mu_0, sd=nu_0) #-.007
slope_mean_fx = rnorm(num_pit, mean= mu_1, sd=nu_1)
#########
temp1 = tibble(PIT_ID = unique(pit), 
                 TTO2_mean_fx,
                 TTO3_mean_fx,
                 incpt_mean_fx,
                 slope_mean_fx)
E <- D %>% select(YEAR, GAME_ID, PIT_ID, BATTER_SEQ_NUM) %>% left_join(temp1)
E <- E %>% rename(k = BATTER_SEQ_NUM) %>% mutate(l = floor((k-1)/9)+1)
#E 
# in each game, add noise to the intercept, slope, and TTO effects for the pitcher

# E1 <- E %>% group_by(GAME_ID, PIT_ID) %>% 
#   filter(row_number() == 1) %>% select(-c(k,l)) %>%
#   mutate(TTO2_fx = TTO2_mean_fx,
#          TTO3_fx = TTO3_mean_fx,
#          incpt = incpt_mean_fx,
#          slope = slope_mean_fx,
#   )%>%
#   ungroup()

E1 <- E %>% group_by(GAME_ID, PIT_ID) %>%
  filter(row_number() == 1) %>% select(-c(k,l)) %>%
  mutate(TTO2_fx = rnorm(1, mean=TTO2_mean_fx, sd=.0015/2),
         TTO3_fx = rnorm(1, mean=TTO3_mean_fx, sd=.0045/2),
         incpt = rnorm(1, mean=incpt_mean_fx, sd=.0015/2),
         slope = rnorm(1, mean=slope_mean_fx, sd=.00065/2)
  )%>%
  ungroup()

#E1
E2 <- E1 %>% right_join(E) 
#E2
E3 <- E2 %>% select(-c(TTO2_mean_fx,TTO3_mean_fx,incpt_mean_fx,slope_mean_fx))
#E3
E4 <- E3 %>% mutate(
  alpha = incpt + slope*k +
            (l==2)*TTO2_fx +
            (l==3)*(TTO2_fx+TTO3_fx) +
            (l==4)*(TTO2_fx+TTO3_fx+0)
)
#E4
#E4 = E4 %>% group_by(row_number()) %>% mutate(alpha = rnorm(1,alpha,sd=.001))
E5 <- E4 %>% group_by(k) %>% summarise(lower = quantile(alpha,.025),
                                       avg = mean(alpha),
                                       upper = quantile(alpha,.975),)
#E6

true_alpha_plot = plot_alpha(E5, "Simulated")
true_alpha_plot

# TTO2_effect = .01
# TTO3_effect = .02
# TTO4_effect = 0
#alpha_mean = -0.007 + 0.001*x # coefficients(m) 
# x = 1:B
# alpha_mean = 
#   -0.007 + 0.001*x + 
#   (floor((x-1)/9)==1)*TTO2_effect + 
#   (floor((x-1)/9)==2)*(TTO2_effect+TTO3_effect) +
#   (floor((x-1)/9)==3)*(TTO2_effect+TTO3_effect+TTO4_effect)
# alpha_mean = alpha_mean + rnorm(B, sd=.0015)
# nu1 = 0.0025 # sd of noise added to alpha_mean
# alpha = do.call(rbind, replicate(N, alpha_mean + rnorm(B, mean=0, sd=nu1), simplify=FALSE)) #G

eta_mean = c(.09, .07, -.02, .01) # s[40:43,1]
nu2 = 0.001 # sd of noise added to eta_mean # s[40:43,]
eta = do.call(rbind, replicate(N, eta_mean + rnorm(length(eta_mean), mean=0, sd=nu2), simplify=FALSE))

sig = 0.125 #FIXME ???

# PLOT TRUE DISTRIBUTION OF ALPHA
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
true_alpha_plot = plot_alpha(E5, "True")
true_alpha_plot
#ggsave(paste0(output_folder, "plot_", OUTPUT_FILE, "_trueAlpha.png"), true_alpha_plot)


# # PLOT TRUE DISTRIBUTION OF ALPHA
# plot_alpha <- function(alpha, descriptor) {
#   lower <- numeric(BB)
#   avg <- numeric(BB)
#   upper <- numeric(BB)
#   for (i in 1:BB) {
#     lower[i] = quantile(alpha[,i],.025)
#     avg[i] = mean(alpha[,i])
#     upper[i] = quantile(alpha[,i],.975)
#   }
#   AAA = data.frame(lower = lower,avg = avg,upper= upper,bn = 1:BB)
#   AAA %>% 
#     ggplot(aes(x=bn, y=avg)) +
#     geom_errorbar(aes(ymin = lower, ymax = upper), fill = "black", width = .4) +
#     geom_point(color="dodgerblue2", shape=21, size=2, fill="white") + 
#     geom_vline(aes(xintercept = 9.5), size=1.2) +
#     geom_vline(aes(xintercept = 18.5), size=1.2) +
#     labs(title = TeX(sprintf("%s distribution of $\\alpha$ parameters", descriptor))) +
#     theme(legend.position="none") +
#     scale_x_continuous(name=TeX("Batter sequence number $k$"),limits = c(0,27.5), breaks = c(0,5,10,15,20,25)) +
#     scale_y_continuous(
#       name=TeX(sprintf("%s distribution of $\\alpha_k$", descriptor)),
#       limits=c(-0.02,0.065), breaks=seq(-0.02,0.065,by=.01)
#     ) 
# }
# true_alpha_plot = plot_alpha(alpha, "True")
# true_alpha_plot
# ggsave(paste0(output_folder, "plot_", OUTPUT_FILE, "_trueAlpha.png"), true_alpha_plot)

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
#fit <- readRDS("job_output/fit_rstan2_sim_6.R.rds") 

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



