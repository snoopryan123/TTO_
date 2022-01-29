library(tidyverse)
library(cowplot)
library(grid)
 
OUTPUT_FILE = "rstan5_plots_intrp.R"
### load data
input_file = "../../data/TTO_dataset_510.csv"  
D <- read_csv(input_file)
#D <- D %>% drop_na() 
D <- D %>% filter(YEAR == 2019) 
#### X <- as.matrix(D %>% select(std_BQ, std_PQ, HAND_MATCH, BAT_HOME_IND)) 
####source("rstan5_plots_main.R")

#############################
########### PLOTS ###########
#############################

D %>% group_by(ORDER_CT) %>%
  summarise(mean_woba = mean(EVENT_WOBA_19),
            sd_woba = sd(EVENT_WOBA_19))




D %>% select(EVENT_WOBA_19, ORDER_CT) %>%
  ggplot() +
  facet_wrap(~ ORDER_CT, ncol=2) +
  geom_histogram(aes(x=EVENT_WOBA_19))






