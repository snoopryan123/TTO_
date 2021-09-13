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

post1 <- stan_glm(EVENT_WOBA ~ BATTER_IDX + ORDER_CT, 
                  data = X,
                  family = gaussian(link = "identity"),
                  seed = 12345)

draws <- as.data.frame(post1)

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


