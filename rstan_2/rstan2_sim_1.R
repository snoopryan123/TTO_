###############
#### SETUP ####
###############

# simulation of BATTER_SEQ_NUM model

OUTPUT_FILE = "rstan2_sim_1.R" #FIXME
NUM_ITERS_IN_CHAIN = 1000 #FIXME #10 

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

######################################
########### SIMULATED DATA ###########
######################################

##### explore the real data before creating simulated data.

# # read data
# input_file = "./../data/design_matrix2_3.csv" #FIXME
# output_folder = "./job_output/"
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
sig = 0.005 # sigma(m)

# PLOT TRUE DISTRIBUTION OF ALPHA
lower <- numeric(B)
avg <- numeric(B)
upper <- numeric(B)
for (i in 1:B) {
  lower[i] = quantile(alpha[,i],.025)
  avg[i] = mean(alpha[,i])
  upper[i] = quantile(alpha[,i],.975)
}
AAA = data.frame(lower = lower,avg = avg,upper= upper,bn = 1:B)
true_alpha_plot = AAA %>% 
  ggplot(aes(x=bn, y=avg)) +
  geom_errorbar(aes(ymin = lower, ymax = upper), fill = "black", width = .4) +
  geom_point(color="dodgerblue2", shape=21, size=2, fill="white") + 
  geom_vline(aes(xintercept = 9.5), size=1.2) +
  geom_vline(aes(xintercept = 18.5), size=1.2) +
  labs(title = TeX("True distribution of $\\alpha$ parameters")) +
  theme(legend.position="none") +
  scale_x_continuous(name=TeX("Batter Sequence Number $k$"),limits = c(0,28), breaks = c(0,5,10,15,20,25)) +
  scale_y_continuous(name=TeX("Distribution of $\\alpha_k$"))
true_alpha_plot

# PLOT TRUE DISTRIBUTION OF ETA
eta_df = data.frame(eta)
names(eta_df)  = c(TeX("$\\eta_{batWoba}$"),TeX("$\\eta_{pitWoba}$"), TeX("$\\eta_{hand}$"),TeX("$\\eta_{home}$"))
true_eta_plot = ggplot(gather(eta_df), aes(value)) + 
  geom_histogram(bins = 20, color = "black", fill = "grey") + 
  facet_wrap(~key, scales = 'free_x', labeller = label_parsed) +  
  labs(title=TeX("True distribution of $\\eta$ parameters")) +
  theme(panel.spacing.x = unit(6, "mm")) +
  ylab("") + scale_y_discrete(breaks=NULL)
true_eta_plot

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





#############################
########### RSTAN ###########
#############################

# compile rstan models
seed = 12345
set.seed(seed)
file = 'tto2_1.stan' #FIXME
model <- stan_model(file = file, model_name = file)

# training data
data_train <- list(y = y[[1]], X = X, n = nrow(X), p = ncol(X))

# Train the models
fit <- sampling(model,
                data = data_train,
                iter = NUM_ITERS_IN_CHAIN,
                chains = cores, #1 #cores, 
                cores = cores, # HPCC
                seed = seed)
# save the stan objects
saveRDS(fit, file = paste0(output_folder, "fit_", OUTPUT_FILE, ".rds"))

#fit <- readRDS("job_output/fit_rstan2_2_removePit.R.rds") 

# posterior histogram
# stan_hist(fit)
# stan_hist(fit, include=FALSE, pars=NA)
# # convergence plot
# stan_trace(fit)
# stan_trace(fit, include=FALSE, pars=NA)
# # autocorrelation plot
# stan_ac(fit)
# stan_ac(fit, include=FALSE, pars=NA)

#############################
########### PLOTS ###########
#############################

# draws and fit summary
NAMES <- c("sigma", names(X), "lp__")
s <- summary(fit)$summary
rownames(s) <- NAMES
draws <- as_tibble(as.matrix(fit))
names(draws) <- NAMES
# write.csv(data.frame(ss), file = paste0(output_folder, "fit_ss", OUTPUT_FILE, ".csv"), row.names=TRUE)

# due to autocorrelation, keep every other posterior sample
#draws <- draws[seq(1,nrow(draws),2),]

# RESCALE the coefficients back to un-standardized form
#mu_y = mean(D$EVENT_WOBA_19)
sd_y = sd(D$EVENT_WOBA_19)

transform_back <- function(x) {
  2*sd_y*x # +mu_y
}

# compute mean and 2.5%, 97.5% quantiles of posterior samples
p = 27 #dim(BATTER_SEQ_dummies)[2]
bsn <- paste0("BATTER_SEQ_NUM", 1:p)
lower <- numeric(p)
avg <- numeric(p)
upper <- numeric(p)
for (i in 1:length(bsn)) {
  b = bsn[i]
  x = transform_back(draws[[bsn[i]]])
  lower[i] = quantile(x,.025)
  avg[i] = mean(x)
  upper[i] = quantile(x,.975)
}

# plot
A4 = data.frame(
  lower = lower,
  avg = avg,
  upper= upper,
  bn = 1:p
)


plot1 = A4 %>% 
  ggplot(aes(x=bn, y=avg)) +
  geom_errorbar(aes(ymin = lower, ymax = upper), fill = "black", width = .4) +
  geom_point(color="dodgerblue2", shape=21, size=2, fill="white") + 
  #geom_smooth( color="firebrick", se = FALSE) +
  #geom_line(aes(y = avg), color="firebrick", size=1) +
  labs(title = paste0(OUTPUT_FILE, "")) +
  theme(legend.position="none") +
  scale_x_continuous(name="batter sequence number", 
                     limits = c(0,28),
                     breaks = c(0,5,10,15,20,25)) +
  scale_y_continuous(name="posterior change in wOBA", 
                     limits = c(-.03,.03),
                     breaks = seq(-.03,.03,.005)) 
plot1


ggsave(paste0(output_folder, "plot_", OUTPUT_FILE, ".png"), plot1)

# PRODUCTION PLOT
theme_update(plot.title = element_text(hjust = 0.5))
production_plot = A4 %>% 
  ggplot(aes(x=bn, y=avg)) +
  geom_errorbar(aes(ymin = lower, ymax = upper), fill = "black", width = .4) +
  geom_point(color="dodgerblue2", shape=21, size=2, fill="white") + 
  # geom_line(aes(y = c(avg[1:9], rep(NA,18))), color="firebrick", size=1) +
  # geom_line(aes(y = c(rep(NA,9), avg[10:18], rep(NA,9))), color="firebrick", size=1) +
  # geom_line(aes(y = c(rep(NA,18), avg[19:27])), color="firebrick", size=1) +
  geom_vline(aes(xintercept = 9.5), size=1.2) +
  geom_vline(aes(xintercept = 18.5), size=1.2) +
  labs(title = "Pitcher Effectiveness") +
  theme(legend.position="none") +
  scale_x_continuous(name="Batter Sequence Number", 
                     limits = c(0,28),
                     breaks = c(0,5,10,15,20,25)) +
  scale_y_continuous(name="Posterior Change in wOBA", 
                     limits = c(-.02, .03),
                     breaks = seq(-.03, .03, .005)
  ) 
production_plot
#ggsave("plot_model_2.png", production_plot)


