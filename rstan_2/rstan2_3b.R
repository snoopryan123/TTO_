# CUBICS WITH DISCONTINUITIES 

###############
#### SETUP ####
###############

OUTPUT_FILE = "rstan2_3.R" #FIXME
NUM_ITERS_IN_CHAIN = 2000 #FIXME #10 

library(tidyverse)
library(rstan)
library(splines)
library(ggthemes)
theme_set(theme_classic())
cores = strtoi(Sys.getenv('OMP_NUM_THREADS')) ### for HPCC
options(mc.cores = cores) ### for HPCC
# options(mc.cores = parallel::detectCores()) # use this on my computer
rstan_options(auto_write = TRUE)

############################
########### DATA ###########
############################

# read data
input_file = "./../data/design_matrix2_0.csv" #FIXME
output_folder = "./job_output/"
D <- read_csv(input_file) 
D <- D %>% drop_na()
# data 
D <- D %>% rename(b = BATTER_SEQ_NUM)

# CUBICS WITH DISCONTINUITIES 
# https://mc-stan.org/users/documentation/case-studies/splines_in_stan.html

a = D$b #c(D$b[1:10], 11,12,13,15,14,20,12)
B_ <- bs(a, knots=c(9,18,27,36), degree=3, intercept = TRUE) # creating the B-splines
colnames(B_) = paste0("B",1:ncol(B_))
B = as_tibble(B_)

y <- D %>% select(std_EVENT_WOBA_19)
#X <- D %>% select(std_WOBA_FINAL_BAT_19, std_WOBA_FINAL_PIT_19, HAND_MATCH, BAT_HOME_IND)
X <- bind_cols(B, D %>% select(std_WOBA_FINAL_BAT_19, std_WOBA_FINAL_PIT_19, HAND_MATCH, BAT_HOME_IND))

#############################
########### RSTAN ###########
#############################

# compile rstan models
seed = 12345
set.seed(seed)
file = 'tto2_1.stan' #FIXME
model <- stan_model(file = file, model_name = file)

# training data
# data_train <- list(y = y[[1]], X = X, B = B,
#                    n = nrow(X), p_c = ncol(X), p_b = ncol(B))
data_train <- list(y = y[[1]], X = X, n = nrow(X), p = ncol(X))


# Train the models
fit <- sampling(model,
                data = data_train,
                iter = NUM_ITERS_IN_CHAIN,
                chains = cores, #1 #cores, 
                cores = cores, # HPCC
                #include = FALSE, pars = c("linpred_c"),
                seed = seed)
# save the stan objects
saveRDS(fit, file = paste0(output_folder, "fit_", OUTPUT_FILE, ".rds"))

#fit <- readRDS("job_output/fit_rstan2_3.R.rds") 

#############################
########### PLOTS ###########
#############################

# draws and fit summary
NAMES <- c("sigma", names(X), "lp__")
s <- summary(fit)$summary
rownames(s) <- NAMES
#draws <- as_tibble(as.matrix(fit))
#names(draws) <- NAMES
# write.csv(data.frame(ss), file = paste0(output_folder, "fit_ss", OUTPUT_FILE, ".csv"), row.names=TRUE)

# RESCALE the coefficients back to un-standardized form
mu_y = mean(D$EVENT_WOBA_19) #FIXME
sd_y = sd(D$EVENT_WOBA_19) #FIXME

transform_back <- function(x) {
  mu_y + 2*sd_y*x
}

###
ff<-extract(fit)
beta = t(ff$beta)
rownames(beta) <- names(X)
beta = beta[1:8,]

aa = unique(D$b) #c(D$b[1:10], 11,12,13,15,14,20,12)
BB_ <- bs(aa, knots=c(9,18,27,36), degree=3, intercept = TRUE) # creating the B-splines
colnames(BB_) = paste0("B",1:ncol(BB_))
BB = as_tibble(BB_)

beta_xxx = s[1:ncol(B)+1,1]
bbb = as.matrix(BB)
aaa=bbb%*%beta_xxx
p0 = data.frame(aaa) %>% ggplot(aes(x=1:length(aaa), y=aaa)) + geom_point()
p0

AAA = t(bbb %*% beta)
colnames(AAA) = paste0("b",1:nrow(BB))
#AAA = as_tibble(AAA)
AAA = as_tibble(transform_back(AAA))
A1 = reshape2::melt(AAA)
#transform_back

plot1 = A1 %>% ggplot(aes(x=variable, y=value)) +
  geom_boxplot() +
  labs(y="wOBA", 
       x = "batter sequence number",
       title = OUTPUT_FILE)
plot1

#
ggsave(paste0(output_folder, "plot_", OUTPUT_FILE, ".png"), plot1)


