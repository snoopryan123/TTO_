library(tidyverse)
library(rstanarm)

################################
########### THE CODE ###########
################################

#D <- read_csv("design_matrix_0.csv")
D <- read_csv("design_matrix_0.csv", col_types = "ddddddddddcccc")
names(D)

prior_i = normal(location = c(.3), scale = c(.03))
prior1 = normal(location = c(.3), scale = c(.03))
post1 <- stan_glm(EVENT_WOBA ~ BATTER_IDX + ORDER_CT  ,# + WOBA_CUMU_BAT + WOBA_CUMU_PIT + DAYS_SINCE_SZN_START, 
                  data = D,
                  family = gaussian(link = "identity"),
                  prior_intercept = prior_i,
                  prior = prior1,
                  seed = 12345)
post1
draws <- as.data.frame(post1)
post1$coefficients

# D1 <- D %>% select(-c(FIELD_POS, OUTS_CT))
# post2 <- stan_glm(EVENT_WOBA ~ ., 
#                   data = D1,
#                   family = gaussian(link = "identity"),
#                   seed = 12345)
