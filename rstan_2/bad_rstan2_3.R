###############
#### SETUP ####
###############

OUTPUT_FILE = "rstan2_3.R" #FIXME
NUM_ITERS_IN_CHAIN = 2000 #FIXME #10 

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
input_file = "./../data/design_matrix2_1.csv" #FIXME
output_folder = "./job_output/"
D <- read_csv(input_file) 
D <- D %>% drop_na()
# data 
D <- D %>% rename(b = BATTER_SEQ_NUM)
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

# a = D$b[1:50]
# splines::ns(a, knots = c(9,18,27,36), Boundary.knots = c(1,38))

y <- D %>% select(std_EVENT_WOBA_19)
X <- bind_cols(cubic.mat, D %>% select(std_WOBA_FINAL_BAT_19, std_WOBA_FINAL_PIT_19, HAND_MATCH, BAT_HOME_IND))

#############################
########### RSTAN ###########
#############################

# compile rstan models
seed = 12345
set.seed(seed)
file = 'tto2_1.stan' #FIXME
model <- stan_model(file = file, model_name = file)

# training data
y_train <- y
X_train <- X
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

#fit <- readRDS("job_output/fit_rstan2_3.R.rds") 

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

# RESCALE the coefficients back to un-standardized form
mu_y = mean(D$EVENT_WOBA_19) #FIXME
sd_y = sd(D$EVENT_WOBA_19) #FIXME

transform_back <- function(x) {
  mu_y + 2*sd_y*x
}

cubic <- function(a0,a1,a2,a3,b) {
  a0 + a1*b + a2*b^2 + a3*b^3
}

# 
A0 = draws %>%
     mutate(b1 = transform_back(cubic(b10,b11,b12,b13,1)),
            b2 = transform_back(cubic(b10,b11,b12,b13,2)),
            b3 = transform_back(cubic(b10,b11,b12,b13,3)),
            b4 = transform_back(cubic(b10,b11,b12,b13,4)),
            b5 = transform_back(cubic(b10,b11,b12,b13,5)),
            b6 = transform_back(cubic(b10,b11,b12,b13,6)),
            b7 = transform_back(cubic(b10,b11,b12,b13,7)),
            b8 = transform_back(cubic(b10,b11,b12,b13,8)),
            #b9 = transform_back(cubic(b10,b11,b12,b13,9)),
            b10 = transform_back(cubic(b20,b21,b22,b23,1)),
            b11 = transform_back(cubic(b20,b21,b22,b23,2)),
            b12 = transform_back(cubic(b20,b21,b22,b23,3)),
            b13 = transform_back(cubic(b20,b21,b22,b23,4)),
            b14 = transform_back(cubic(b20,b21,b22,b23,5)),
            b15 = transform_back(cubic(b20,b21,b22,b23,6)),
            b16 = transform_back(cubic(b20,b21,b22,b23,7)),
            b17 = transform_back(cubic(b20,b21,b22,b23,8)),
            #b18 = transform_back(cubic(b20,b21,b22,b23,9)),
            b19 = transform_back(cubic(b30,b31,b32,b33,1)),
            b20 = transform_back(cubic(b30,b31,b32,b33,2)),
            b21 = transform_back(cubic(b30,b31,b32,b33,3)),
            b22 = transform_back(cubic(b30,b31,b32,b33,4)),
            b23 = transform_back(cubic(b30,b31,b32,b33,5)),
            b24 = transform_back(cubic(b30,b31,b32,b33,6)),
            b25 = transform_back(cubic(b30,b31,b32,b33,7)),
            b26 = transform_back(cubic(b30,b31,b32,b33,8)),
            #b27 = transform_back(cubic(b30,b31,b32,b33,9)),
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
               labs(y="wOBA", 
                    x = "batter sequence number",
                    title = OUTPUT_FILE)
plot1

#
ggsave(paste0(output_folder, "plot_", OUTPUT_FILE, ".png"), plot1)


