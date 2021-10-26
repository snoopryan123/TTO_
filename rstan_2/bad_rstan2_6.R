###############
#### SETUP ####
###############

### removed pitchers, batter seq num, 3 cubics (allow for TTO discontinuities)

OUTPUT_FILE = "rstan2_6.R" #FIXME
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

# cubic spline over BATTER_SEQ_NUM
# https://mc-stan.org/users/documentation/case-studies/splines_in_stan.html
# D <- D %>% rename(b = BATTER_SEQ_NUM)
# a = D$b #c(D$b[1:10], 11,12,13,15,14,20,12)
# B_ <- bs(a, knots=c(9,18,27,36), degree=3, intercept = TRUE) # creating the B-splines
# colnames(B_) = paste0("B",1:ncol(B_))
# B = as_tibble(B_)

# 1 Cubic for each TTO, allow for discontinuities at TTO
D <- D %>% rename(b = BATTER_SEQ_NUM)
a = D$b #c(D$b[1:10], 11,12,13,15,14,20,12)

model.matrix( ~ poly(a, degree=3, raw=TRUE) )


cubic.mat <- D %>% select(ORDER_CT, b) %>%
                    mutate(b10 = ifelse(ORDER_CT == 1,  1,   0),
                           b11 = ifelse(ORDER_CT == 1,  b,   0),
                           b12 = ifelse(ORDER_CT == 1,  b^2, 0),
                           b13 = ifelse(ORDER_CT == 1,  b^3, 0),
                           b20 = ifelse(ORDER_CT == 2, 1,   0),
                           b21 = ifelse(ORDER_CT == 2, b,   0),
                           b22 = ifelse(ORDER_CT == 2, b^2, 0),
                           b23 = ifelse(ORDER_CT == 2, b^3, 0),
                           b30 = ifelse(ORDER_CT == 3, 1,   0),
                           b31 = ifelse(ORDER_CT == 3, b,   0),
                           b32 = ifelse(ORDER_CT == 3, b^2, 0),
                           b33 = ifelse(ORDER_CT == 3, b^3, 0),
                           b40 = ifelse(ORDER_CT >= 4, 1,   0),
                           b41 = ifelse(ORDER_CT >= 4, b,   0),
                           b42 = ifelse(ORDER_CT >= 4, b^2, 0),
                           b43 = ifelse(ORDER_CT >= 4, b^3, 0),
                    ) %>% select(-c(ORDER_CT,b))

# data 
y <- D %>% select(std_EVENT_WOBA_19)
#X <- D %>% select(std_WOBA_FINAL_BAT_19, std_WOBA_FINAL_PIT_19, HAND_MATCH, BAT_HOME_IND)
X <- bind_cols(cubic.mat, D %>% select(std_WOBA_FINAL_BAT_19, std_WOBA_FINAL_PIT_19, 
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

#fit <- readRDS("job_output/fit_rstan2_6.R.rds") 

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

cubic <- function(a0,a1,a2,a3,b) {
  a0 + a1*b + a2*b^2 + a3*b^3
}

#######

ff<-extract(fit)
beta = t(ff$beta)
rownames(beta) <- names(X)
beta = beta[1:16,]
beta = transform_back(beta)

xx = rowMeans(beta)

c1 = cubic(xx[["b10"]], xx[["b11"]], xx[["b12"]], xx[["b13"]], 1:9)
c2 = cubic(xx[["b20"]], xx[["b21"]], xx[["b22"]], xx[["b23"]], 10:18)
c3 = cubic(xx[["b30"]], xx[["b31"]], xx[["b32"]], xx[["b33"]], 19:27)
yy = c(c1, c2, c3)
xxx=1:27

p0 = data.frame(yy) %>% ggplot(aes(x=xxx, y=yy)) +
    geom_point() + 
    labs(y="change in wOBA",
         x = "batter sequence number",
         title = OUTPUT_FILE) 
p0

ggsave(paste0(output_folder, "plot_", OUTPUT_FILE, ".png"), p0)


#FIXME
# ADD 95% POSTERIOR INTERVAL ON THE SPLINE



