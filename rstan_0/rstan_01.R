library(tidyverse)
library(rstanarm)
library(ggplot2)
library(ggthemes)
theme_set(theme_classic())
#theme_set(theme_wsj())


################################
########### THE CODE ###########
################################

#X <- read_csv("design_matrix_0.csv")
X <- read_csv("design_matrix_0.csv", col_types = "ddddddddddcccc")
names(X)

prior_i = normal(location = c(.3), scale = c(.03))
prior1 = normal(location = c(.3), scale = c(.03))
post1 <- stan_glm(EVENT_WOBA ~ BATTER_IDX + ORDER_CT + WOBA_CUMU_BAT + WOBA_CUMU_PIT + DAYS_SINCE_SZN_START, 
                  data = X,
                  family = gaussian(link = "identity"),
                  #prior_intercept = prior_i,
                  #prior = prior1,
                  seed = 12345)
post1
draws <- as.data.frame(post1)
post1$coefficients

X1 <- X %>% select(-c(FIELD_POS, OUTS_CT))
post2 <- stan_glm(EVENT_WOBA ~ ., 
                  data = X1,
                  family = gaussian(link = "identity"),
                  seed = 12345)


######################################################################

# plot posterior distributions of BATTER_IDX
b1 <- ggplot(data=draws, aes(`(Intercept)`)) + 
  geom_histogram(binwidth = .002) +
  geom_vline(aes(xintercept = mean(`(Intercept)`)), colour="red")
b2 <- ggplot(data=draws, aes(BATTER_IDX2 + `(Intercept)`)) + 
  geom_histogram(binwidth = .002) +
  geom_vline(aes(xintercept = mean(BATTER_IDX2 + `(Intercept)`)), colour="red")
b9 <- ggplot(data=draws, aes(BATTER_IDX9 + `(Intercept)`)) + 
  geom_histogram(binwidth=.002) +
  geom_vline(aes(xintercept = mean(BATTER_IDX9 + `(Intercept)`)), colour="red")

cowplot::plot_grid(b1, b2, b9, labels = "AUTO", ncol=2)

# plot posterior distributions of ORDER_CT
o1 <- ggplot(data=draws, aes(`(Intercept)`)) + 
  geom_histogram(bins = 40) +
  geom_vline(aes(xintercept = mean(`(Intercept)`)), colour="red")
o2 <- ggplot(data=draws, aes(ORDER_CT2 + `(Intercept)`)) + 
  geom_histogram(bins = 40) +
  geom_vline(aes(xintercept = mean(ORDER_CT2 + `(Intercept)`)), colour="red")
o3 <- ggplot(data=draws, aes(ORDER_CT3 + `(Intercept)`)) + 
  geom_histogram(bins = 40) +
  geom_vline(aes(xintercept = mean(ORDER_CT3 + `(Intercept)`)), colour="red")
o4 <- ggplot(data=draws, aes(ORDER_CT4 + `(Intercept)`)) + 
  geom_histogram(bins = 40) +
  geom_vline(aes(xintercept = mean(ORDER_CT4 + `(Intercept)`)), colour="red")

cowplot::plot_grid(o1, o2, o3, o4, labels = "AUTO", ncol=2)

# overlapping histograms of ORDER_CT_2 and ORDER_CT_3
o <- ggplot(data=draws) + 
    #geom_histogram(aes(`(Intercept)`), fill = "grey85", , alpha=1, bins = 40) +
    geom_histogram(aes(ORDER_CT2 + `(Intercept)`), fill = "grey45", alpha=.5, bins = 40) +
    geom_histogram(aes(ORDER_CT3 + `(Intercept)`), fill = "grey5", alpha=.5, bins = 40) +
    #geom_vline(aes(xintercept = mean(`(Intercept)`)), colour="green") +
    geom_vline(aes(xintercept = mean(ORDER_CT2 + `(Intercept)`)), colour="blue") + 
    geom_vline(aes(xintercept = mean(ORDER_CT3 + `(Intercept)`)), colour="red")
o

# overlapping histograms of ORDER_CT_1 and ORDER_CT_2
o <- ggplot(data=draws) + 
  geom_histogram(aes(`(Intercept)`), fill = "grey45", alpha=.5, bins = 40) +
  geom_histogram(aes(ORDER_CT2 + `(Intercept)`), fill = "grey5", alpha=.5, bins = 40) +
  geom_vline(aes(xintercept = mean(`(Intercept)`)), colour="green") + 
  geom_vline(aes(xintercept = mean(ORDER_CT2 + `(Intercept)`)), colour="blue")
o

# overlapping histograms of ORDER_CT_1 and ORDER_CT_2 and ORDER_CT_3
o <- ggplot(data=draws) + 
  geom_histogram(aes(`(Intercept)`), fill = "grey45", alpha=1, bins = 40) +
  geom_histogram(aes(ORDER_CT2 + `(Intercept)`), fill = "grey5", alpha=.6, bins = 40) +
  geom_histogram(aes(ORDER_CT3 + `(Intercept)`), fill = "grey85", alpha=.3, bins = 40) +
  geom_vline(aes(xintercept = mean(`(Intercept)`)), colour="green") + 
  geom_vline(aes(xintercept = mean(ORDER_CT2 + `(Intercept)`)), colour="blue") +
  geom_vline(aes(xintercept = mean(ORDER_CT3 + `(Intercept)`)), colour="red")
o

######################################################################

# plot BATTER_IDX with ORDER_CT !!!

# plot posterior distributions of BATTER_IDX
c29 <- ggplot(data=draws, aes(`(Intercept)` + ORDER_CT2 + BATTER_IDX9)) + 
  geom_histogram(binwidth = .002) +
  geom_vline(aes(xintercept = mean(`(Intercept)` + ORDER_CT2 + BATTER_IDX9)), colour="red")
c31 <- ggplot(data=draws, aes(`(Intercept)` + ORDER_CT3)) + 
  geom_histogram(binwidth = .002) +
  geom_vline(aes(xintercept = mean(`(Intercept)` + ORDER_CT3)), colour="red")
c35 <- ggplot(data=draws, aes(`(Intercept)` + ORDER_CT3 + BATTER_IDX5)) + 
  geom_histogram(binwidth = .002) +
  geom_vline(aes(xintercept = mean(`(Intercept)` + ORDER_CT3 + BATTER_IDX9)), colour="red")
c39 <- ggplot(data=draws, aes(`(Intercept)` + ORDER_CT3 + BATTER_IDX9)) + 
  geom_histogram(binwidth = .002) +
  geom_vline(aes(xintercept = mean(`(Intercept)` + ORDER_CT3 + BATTER_IDX9)), colour="red")

cowplot::plot_grid(c29, c31, c35, c39, labels = "AUTO", ncol=2)

######################################################################

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
A1 = reshape2::melt(A0)
ggplot(A1, aes(x=variable, y=value)) + 
  geom_boxplot()

