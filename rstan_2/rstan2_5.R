###############
#### SETUP ####
###############

### removed pitchers, batter seq num, 1 spline

OUTPUT_FILE = "rstan2_5.R" #FIXME
NUM_ITERS_IN_CHAIN = 1500 #FIXME #10 

library(tidyverse)
library(rstan)
library(ggthemes)
library(splines)
theme_set(theme_bw())
cores = strtoi(Sys.getenv('OMP_NUM_THREADS')) ### for HPCC
options(mc.cores = cores) ### for HPCC
# options(mc.cores = parallel::detectCores()) # use this on my computer
rstan_options(auto_write = TRUE)

############################
########### DATA ###########
############################

# read data
input_file = "./../data/design_matrix2_3.csv" #FIXME
output_folder = "./job_output/"
D <- read_csv(input_file) 
D <- D %>% drop_na()
# create dummy variables for the categorical variables
# NO INTERCEPT and INCLUDE FIRST COLUMN 
change_factor_names <- function(s) {
  s <- str_remove(s, "factor")
  s <- str_remove_all(s, "\\(")
  s <- str_remove_all(s, "\\)")
  s
}
# categorical dummies for BATTER_SEQ_NUM
# BATTER_SEQ_dummies <- D %>% modelr::model_matrix(~ factor(BATTER_SEQ_NUM) + 0) 
# names(BATTER_SEQ_dummies) <- change_factor_names(names(BATTER_SEQ_dummies))

# cubic spline over BATTER_SEQ_NUM
# https://mc-stan.org/users/documentation/case-studies/splines_in_stan.html
D <- D %>% rename(b = BATTER_SEQ_NUM)
a = D$b #c(D$b[1:10], 11,12,13,15,14,20,12)
B_ <- bs(a, knots=c(9,18,27,36), degree=3, intercept = TRUE) # creating the B-splines
colnames(B_) = paste0("B",1:ncol(B_))
B = as_tibble(B_)

# data 
y <- D %>% select(std_EVENT_WOBA_19)
#X <- D %>% select(std_WOBA_FINAL_BAT_19, std_WOBA_FINAL_PIT_19, HAND_MATCH, BAT_HOME_IND)
X <- bind_cols(B, D %>% select(std_WOBA_FINAL_BAT_19, std_WOBA_FINAL_PIT_19, 
                               HAND_MATCH, BAT_HOME_IND))

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

#fit <- readRDS("job_output/fit_rstan2_5.R.rds") 

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

# quantiles and mean of posterior samples
p = 8 #dim(BATTER_SEQ_dummies)[2]
bsn <- paste0("B", 1:p)
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

# spline basis matrix 
aa = unique(D$b) #c(D$b[1:10], 11,12,13,15,14,20,12)
BB_ <- bs(aa, knots=c(9,18,27,36), degree=3, intercept = TRUE) # creating the B-splines
colnames(BB_) = paste0("B",1:ncol(BB_))
BB = as_tibble(BB_)
bbb = as.matrix(BB)

# quantiles of each batter sequence number
lower_ = bbb %*% lower
avg_ = bbb %*% avg
upper_ = bbb %*% upper

# plot
A4 = data.frame(
  lower = lower_[1:27],
  avg = avg_[1:27],
  upper= upper_[1:27],
  bn = 1:27
)
theme_set(theme_bw())
plot1 = A4 %>% 
  ggplot(aes(x=bn, y=avg)) +
  geom_errorbar(aes(ymin = lower, ymax = upper), fill = "black", width = .4) +
  geom_line(aes(y = avg), color="firebrick", size=1) +
  labs(title = paste0(OUTPUT_FILE, ": 1 cubic spline")) +
  theme(legend.position="none") +
  scale_x_continuous(name="batter sequence number", 
                   limits = c(0,28),
                   breaks = c(0,5,10,15,20,25)) +
  scale_y_continuous(name="posterior change in wOBA", 
                     limits = c(-.026,.026),
                     breaks = seq(-.026,.026,.004)) 
plot1

#ggsave(paste0(output_folder, "plot_", OUTPUT_FILE, ".png"), plot1)


