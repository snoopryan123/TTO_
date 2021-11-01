###############
#### SETUP ####
###############

library(tidyverse)
library(rstan)
library(ggthemes)
theme_set(theme_bw())
# cores = strtoi(Sys.getenv('OMP_NUM_THREADS')) ### for HPCC
# options(mc.cores = cores) ### for HPCC
# # options(mc.cores = parallel::detectCores()) # use this on my computer
# rstan_options(auto_write = TRUE)

############################
########### DATA ###########
############################

# read data
input_file = "./../data/design_matrix2_3.csv" #FIXME
output_folder = "./job_output/"
D <- read_csv(input_file) 
D <- D %>% drop_na()

#############################
########### EXPLORE #########
#############################

library(cowplot)

po = D %>% filter(BATTER_SEQ_NUM <= 7) %>% ggplot() +
  geom_histogram(aes(std_WOBA_FINAL_BAT_19)) +
  facet_grid(col = vars(HAND_MATCH)) +
  labs(title="BATTER 1-7") + 
  scale_x_continuous(lim = c(-5,2))
p8 = D %>% filter(BATTER_SEQ_NUM==8) %>% ggplot() +
  geom_histogram(aes(std_WOBA_FINAL_BAT_19)) +
  facet_grid(col = vars(HAND_MATCH)) +
  labs(title="BATTER 8") + 
  scale_x_continuous(lim = c(-5,2))
p9 = D %>% filter(BATTER_SEQ_NUM==9) %>% ggplot() +
  geom_histogram(aes(std_WOBA_FINAL_BAT_19)) +
  facet_grid(col = vars(HAND_MATCH)) +
  labs(title="BATTER 9") + 
  scale_x_continuous(lim = c(-5,2))

plot_grid(po, p8, p9 ,nrow=3) 

#############################

po = D %>% filter(BATTER_SEQ_NUM <= 7) %>% ggplot() +
  geom_histogram(aes(std_WOBA_FINAL_BAT_19)) +
  labs(title="BATTER 1-7") + 
  scale_x_continuous(lim = c(-2,2))
p8 = D %>% filter(BATTER_SEQ_NUM==8) %>% ggplot() +
  geom_histogram(aes(std_WOBA_FINAL_BAT_19)) +
  labs(title="BATTER 8") + 
  scale_x_continuous(lim = c(-2,2))
p9 = D %>% filter(BATTER_SEQ_NUM==9) %>% ggplot() +
  geom_histogram(aes(std_WOBA_FINAL_BAT_19)) +
  labs(title="BATTER 9") + 
  scale_x_continuous(lim = c(-2,2))

plot_grid(po, p8, p9 ,nrow=3) 

