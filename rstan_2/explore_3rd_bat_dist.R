###############
#### SETUP ####
###############

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

#############################
########### EXPLORE #########
#############################

library(cowplot)

d3 = D %>% filter(BATTER_IDX==3)
d3 %>% ggplot(aes(EVENT_WOBA_19)) +
  geom_histogram()

D %>% filter(BATTER_IDX <= 8) %>% ggplot() +
  geom_histogram(aes(EVENT_WOBA_19)) +
  facet_grid(col = vars(BATTER_IDX))

# number of outs histogram vs. batter index number 1-9

D %>% filter(BATTER_IDX <= 8) %>% ggplot() +
  geom_histogram(bins=3,aes(OUTS_CT),col="white") +
  facet_grid(col = vars(BATTER_IDX))



D %>% filter(BATTER_IDX <= 3) %>% ggplot() +
  geom_point(aes(x=OUTS_CT, y=EVENT_WOBA_19)) +
  facet_grid(col = vars(BATTER_IDX))

p1 = D %>% filter(BATTER_SEQ_NUM ==1) %>% ggplot() +
  geom_histogram(bins=7,aes(EVENT_WOBA_19),col="white") +
  facet_grid(col = vars(OUTS_CT)) +
  labs(title="BATTER ==1")
p2 = D %>% filter(BATTER_SEQ_NUM ==2) %>% ggplot() +
  geom_histogram(bins=7,aes(EVENT_WOBA_19),col="white") +
  facet_grid(col = vars(OUTS_CT)) +
  labs(title="BATTER ==2")
p3 = D %>% filter(BATTER_SEQ_NUM ==3) %>% ggplot() +
  geom_histogram(bins=7,aes(EVENT_WOBA_19),col="white") +
  facet_grid(col = vars(OUTS_CT)) +
  labs(title="BATTER ==3")
p4 = D %>% filter(BATTER_SEQ_NUM ==4) %>% ggplot() +
  geom_histogram(bins=7,aes(EVENT_WOBA_19),col="white") +
  facet_grid(col = vars(OUTS_CT)) +
  labs(title="BATTER ==4")
p5 = D %>% filter(BATTER_SEQ_NUM ==5) %>% ggplot() +
  geom_histogram(bins=7,aes(EVENT_WOBA_19),col="white") +
  facet_grid(col = vars(OUTS_CT)) +
  labs(title="BATTER ==5")
p6 = D %>% filter(BATTER_SEQ_NUM ==6) %>% ggplot() +
  geom_histogram(bins=7,aes(EVENT_WOBA_19),col="white") +
  facet_grid(col = vars(OUTS_CT)) +
  labs(title="BATTER ==6")
p7 = D %>% filter(BATTER_SEQ_NUM ==7) %>% ggplot() +
  geom_histogram(bins=7,aes(EVENT_WOBA_19),col="white") +
  facet_grid(col = vars(OUTS_CT)) +
  labs(title="BATTER ==7")
p8 = D %>% filter(BATTER_SEQ_NUM ==8) %>% ggplot() +
  geom_histogram(bins=7,aes(EVENT_WOBA_19),col="white") +
  facet_grid(col = vars(OUTS_CT)) +
  labs(title="BATTER ==8")
plot_grid(p1,p2,p3,p4,p5,p6,p7,p8,nrow=2) 

D %>% filter(BATTER_IDX == 3) %>% 
  ggplot(aes(x=OUTS_CT, y=EVENT_WOBA_19, size=y)) +
  geom_point() +
  scale_size_discrete(range = c(1, 10))

