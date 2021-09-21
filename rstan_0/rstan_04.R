library(tidyverse)
library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

################################
#### COMPILER OPTIMIZATIONS ####
################################

dotR <- file.path(Sys.getenv("HOME"), ".R")
if (!file.exists(dotR)) dir.create(dotR)
M <- file.path(dotR, "Makevars")
if (!file.exists(M)) file.create(M)
cat("\nCXX14FLAGS += -O3 -mtune=native -arch x86_64 -ftemplate-depth-256",
    file = M, sep = "\n", append = FALSE)

############################
########### DATA ###########
############################

# read data
#D <- read_csv("design_matrix_0.csv")
input_file = "design_matrix_2015-2020.csv" #"design_matrix_2020.csv"
D <- read_csv(input_file, col_types = "ddddddddddcccc")
names(D)

D <- D %>% drop_na() #FIXME

# create dummy variables for the categorical variables
BATTER_IDX_dummies <- D %>% modelr::model_matrix(~ BATTER_IDX)
BATTER_IDX_dummies <- BATTER_IDX_dummies %>% 
                      relocate(BATTER_IDX2, .after = `(Intercept)`) %>%
                      relocate(BATTER_IDX3, .after = BATTER_IDX2) %>%
                      relocate(BATTER_IDX4, .after = BATTER_IDX3) %>%
                      relocate(BATTER_IDX5, .after = BATTER_IDX4) %>%
                      relocate(BATTER_IDX6, .after = BATTER_IDX5) %>%
                      relocate(BATTER_IDX7, .after = BATTER_IDX6) %>%
                      relocate(BATTER_IDX8, .after = BATTER_IDX7) %>%
                      relocate(BATTER_IDX9, .after = BATTER_IDX8) %>%
                      relocate(BATTER_IDX10, .after = BATTER_IDX9) %>%
                      relocate(BATTER_IDX11, .after = BATTER_IDX10) %>%
                      relocate(BATTER_IDX12, .after = BATTER_IDX11) %>%
                      relocate(BATTER_IDX13, .after = BATTER_IDX12) 
ORDER_CT_dummies <- D %>% modelr::model_matrix(~ ORDER_CT) %>% select(-`(Intercept)`)

# design matrix
X0 = bind_cols(BATTER_IDX_dummies, ORDER_CT_dummies)
X1 = bind_cols(D %>% select(WOBA_CUMU_BAT, WOBA_CUMU_PIT, HAND_MATCH, BAT_HOME_IND, 
                            PIT_REST, DAYS_SINCE_SZN_START, IN_DIV, IN_LEAGUE, 
                            PITCH_COUNT_CUMU), X0)
# FIELD_POS <chr>, OUTS_CT <chr> --> categorical too

# design matrix & response variable
X = as.matrix(X1)
y = D$EVENT_WOBA

#############################
########### RSTAN ###########
#############################

tto_dat <- list(n = nrow(X),
                p = ncol(X),
                X = X,
                y = y) 

# compile .stan file
file = 'tto4.stan'
model <- stan_model(file = file, model_name = "linear regression, ind. normal priors")
# obtain posterior samples of the parameters
fit <- sampling(model, data = tto_dat, pars = c("beta"), 
                iter = 2000, chains = 1, seed = 12345)

fit
stan_hist(fit, pars=c("beta"))
traceplot(fit,pars=c("beta"))

# save the stan object
saveRDS(fit, file = "fit_04_2015-2020_.rds")
##########################################
#fit <- readRDS("fit_04_2015-2020_.rds") # be careful! dont override the code



draws <- as_tibble(as.matrix(fit))
names(draws) <- c(colnames(X), "lp__")
fit_summary <- summary(fit)
f <- fit_summary$summary
rownames(f) <- names(draws)
f[,c(1,9,10)]

#############################
########### PLOTS ###########
#############################

A0 = tibble(draws) %>% mutate(b11 = `(Intercept)`,
                              b12 = `(Intercept)` + BATTER_IDX2,
                              b13 = `(Intercept)` + BATTER_IDX3,
                              b14 = `(Intercept)` + BATTER_IDX4,
                              b15 = `(Intercept)` + BATTER_IDX5,
                              b16 = `(Intercept)` + BATTER_IDX6,
                              b17 = `(Intercept)` + BATTER_IDX7,
                              b18 = `(Intercept)` + BATTER_IDX8,
                              b19 = `(Intercept)` + BATTER_IDX9,
                              b21 = `(Intercept)` + ORDER_CT2,
                              b22 = `(Intercept)` + BATTER_IDX2 + ORDER_CT2,
                              b23 = `(Intercept)` + BATTER_IDX3 + ORDER_CT2,
                              b24 = `(Intercept)` + BATTER_IDX4 + ORDER_CT2,
                              b25 = `(Intercept)` + BATTER_IDX5 + ORDER_CT2,
                              b26 = `(Intercept)` + BATTER_IDX6 + ORDER_CT2,
                              b27 = `(Intercept)` + BATTER_IDX7 + ORDER_CT2,
                              b28 = `(Intercept)` + BATTER_IDX8 + ORDER_CT2,
                              b29 = `(Intercept)` + BATTER_IDX9 + ORDER_CT2,
                              b31 = `(Intercept)` + ORDER_CT3,
                              b32 = `(Intercept)` + BATTER_IDX2 + ORDER_CT3,
                              b33 = `(Intercept)` + BATTER_IDX3 + ORDER_CT3,
                              b34 = `(Intercept)` + BATTER_IDX4 + ORDER_CT3,
                              b35 = `(Intercept)` + BATTER_IDX5 + ORDER_CT3,
                              b36 = `(Intercept)` + BATTER_IDX6 + ORDER_CT3,
                              b37 = `(Intercept)` + BATTER_IDX7 + ORDER_CT3,
                              b38 = `(Intercept)` + BATTER_IDX8 + ORDER_CT3,
                              b39 = `(Intercept)` + BATTER_IDX9 + ORDER_CT3) %>%
  select(b11,b12,b13,b14,b15,b16,b17,b18,b19,b21,b22,b23,b24,b25,b26,b27,b28,b29,b31,b32,b33,b34,b35,b36,b37,b38,b39)


A1 = A0 + mean(draws$WOBA_CUMU_BAT) * (.3) +
          mean(draws$WOBA_CUMU_PIT) * (.3) +
          mean(draws$HAND_MATCH) * 0 +
          mean(draws$BAT_HOME_IND) * 1 +
          mean(draws$PIT_REST) * 5 +
          mean(draws$DAYS_SINCE_SZN_START) * 30 +
          mean(draws$IN_DIV) * 1 +
          mean(draws$IN_LEAGUE) * 1 +
          mean(draws$PITCH_COUNT_CUMU) * 0
  
A2 = reshape2::melt(A1)

library(ggthemes)
theme_set(theme_classic())
plot1 = A2 %>% ggplot(aes(x=variable, y=value)) + 
               geom_boxplot() +
               labs(y="wOBA", x = "time thru order number + batter index number",
                    title = "Pitchers Exhibit Continuous Fatigue")
plot1



