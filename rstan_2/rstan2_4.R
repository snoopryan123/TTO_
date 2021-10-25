###############
#### SETUP ####
###############

OUTPUT_FILE = "rstan2_4.R" #FIXME
NUM_ITERS_IN_CHAIN = 1500 #FIXME #10 

library(tidyverse)
library(rstan)
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
input_file = "./../data/design_matrix2_2.csv" #FIXME
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
BATTER_SEQ_dummies <- D %>% modelr::model_matrix(~ factor(BATTER_SEQ_NUM) + 0) 
names(BATTER_SEQ_dummies) <- change_factor_names(names(BATTER_SEQ_dummies))
# categorical dummies for OUTS_CT
OUTS_CT_dummies <- D %>% modelr::model_matrix(~ factor(OUTS_CT) + 0) 
names(OUTS_CT_dummies) <- change_factor_names(names(OUTS_CT_dummies))
# data 
y <- D %>% select(std_EVENT_WOBA_19)
X <- bind_cols(BATTER_SEQ_dummies, 
               OUTS_CT_dummies,
               D %>% select(std_WOBA_FINAL_BAT_19,std_WOBA_FINAL_PIT_19, 
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

#fit <- readRDS("job_output/fit_rstan2_2.R.rds") 

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
mu_y = mean(D$EVENT_WOBA_19) #FIXME
sd_y = sd(D$EVENT_WOBA_19) #FIXME

transform_back <- function(x) {
  #mu_y + 2*sd_y*x
  2*sd_y*x
}

# 
A0 = draws %>%
     mutate(b1 = transform_back(BATTER_SEQ_NUM1),
            b2 = transform_back(BATTER_SEQ_NUM2),
            b3 = transform_back(BATTER_SEQ_NUM3),
            b4 = transform_back(BATTER_SEQ_NUM4),
            b5 = transform_back(BATTER_SEQ_NUM5),
            b6 = transform_back(BATTER_SEQ_NUM6),
            b7 = transform_back(BATTER_SEQ_NUM7),
            b8 = transform_back(BATTER_SEQ_NUM8),
            #b9 = transform_back(BATTER_SEQ_NUM9),
            b10 = transform_back(BATTER_SEQ_NUM10),
            b11 = transform_back(BATTER_SEQ_NUM11),
            b12 = transform_back(BATTER_SEQ_NUM12),
            b13 = transform_back(BATTER_SEQ_NUM13),
            b14 = transform_back(BATTER_SEQ_NUM14),
            b15 = transform_back(BATTER_SEQ_NUM15),
            b16 = transform_back(BATTER_SEQ_NUM16),
            b17 = transform_back(BATTER_SEQ_NUM17),
            #b18 = transform_back(BATTER_SEQ_NUM18),
            b19 = transform_back(BATTER_SEQ_NUM19),
            b20 = transform_back(BATTER_SEQ_NUM20),
            b21 = transform_back(BATTER_SEQ_NUM21),
            b22 = transform_back(BATTER_SEQ_NUM22),
            b23 = transform_back(BATTER_SEQ_NUM23),
            b24 = transform_back(BATTER_SEQ_NUM24),
            b25 = transform_back(BATTER_SEQ_NUM25),
            b26 = transform_back(BATTER_SEQ_NUM26),
            #b27 = transform_back(BATTER_SEQ_NUM27),
            ) %>%
  select(b1,b2,b3,b4,b5,b6,b7,b8,
         #b9,
         b10,b11,b12,b13,b14,b15,b16,b17,
         #b18,
         b19,b20,b21,b22,b23,b24,b25,b26,
         #b27
         )

A1 = reshape2::melt(A0)
plot1 = A1 %>% ggplot(aes(x=variable, y=value)) +
               geom_boxplot() +
               labs(y="residual wOBA", 
                    x = "batter sequence number",
                    title = OUTPUT_FILE)
plot1

#
ggsave(paste0(output_folder, "plot_", OUTPUT_FILE, ".png"), plot1)


