library(tidyverse)
library(rstanarm)
library(ggplot2)
library(bayesplot)
theme_set(bayesplot::theme_default())

# input_filename = "retro_final_PA_2020.csv"
# D <- read_csv(input_filename)


################################
########### THE CODE ###########
################################

X <- read_csv("design_matrix_0.csv")

names(X)

post1 <- stan_glm(EVENT_WOBA ~ WOBA_CUMU_BAT + WOBA_CUMU_PIT, 
                  data = X,
                  family = gaussian(link = "identity"),
                  seed = 12345)




