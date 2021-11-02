###############
#### SETUP ####
###############

OUTPUT_FILE = "rstan2_11.R" #FIXME
NUM_ITERS_IN_CHAIN = 1000 #FIXME #10 

library(tidyverse)
library(rstan)
library(ggthemes)
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

# categorical dummies for BATTER_IDX
BATTER_IDX_dummies <- D %>% modelr::model_matrix(~ factor(BATTER_IDX) + 0) 
names(BATTER_IDX_dummies) <- change_factor_names(names(BATTER_IDX_dummies))
# categorical dummies for ORDER_CT
ORDER_CT_dummies <- D %>% modelr::model_matrix(~ factor(ORDER_CT) + 0) 
names(ORDER_CT_dummies) <- change_factor_names(names(ORDER_CT_dummies))
# 8,9,other x HAND_MATCH interaction terms
intr1 = D$HAND_MATCH * BATTER_IDX_dummies$BATTER_IDX8
intr2 = D$HAND_MATCH * BATTER_IDX_dummies$BATTER_IDX9
#names(intr1) = paste0("batWoba_x_", names(intr1))
# data 
y <- D %>% select(std_EVENT_WOBA_19)
X <- bind_cols(BATTER_IDX_dummies, 
               ORDER_CT_dummies,
               D %>% select(std_WOBA_FINAL_BAT_19, std_WOBA_FINAL_PIT_19, HAND_MATCH, BAT_HOME_IND),
               intr1, intr2)
names(X)[26] = "HAND_x_B8"
names(X)[27] = "HAND_x_B9"

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

#fit <- readRDS("job_output/fit_rstan2_10.R.rds") 

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
bidx <- paste0("BATTER_IDX", 1:9)
oc <- paste0("ORDER_CT", 1:3)
lower <- numeric(p)
avg <- numeric(p)
upper <- numeric(p)
for (i in 1:length(oc)) {
  o = oc[i]
  x0 = transform_back(draws[[o]])
  for (j in 1:length(bidx)) {
    b = bidx[j]
    xb = transform_back(draws[[b]])
    x = x0 + xb
    lower[(i-1)*length(bidx) + j] = quantile(x,.025)
    avg[(i-1)*length(bidx) + j] = mean(x)
    upper[(i-1)*length(bidx) + j] = quantile(x,.975)
  }
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
  labs(title = paste0(OUTPUT_FILE, " interaction: HAND_MATCH x vs B8,B9 ")) +
  theme(legend.position="none") +
  scale_x_continuous(name="batter sequence number", 
                     limits = c(0,28),
                     breaks = c(0,5,10,15,20,25)) +
  scale_y_continuous(name="posterior change in wOBA", 
                     #limits = c(-.03,.03),
                     breaks = seq(-.03,.03,.005)) 
plot1


ggsave(paste0(output_folder, "plot_", OUTPUT_FILE, ".png"), plot1)


