
library(tidyverse)
library(latex2exp)

# load data
input_file = "../data/TTO_dataset_510.csv"  
D <- read_csv(input_file) #; D <- D %>% drop_na() 
D <- D %>% filter(YEAR == 2019) 
X <- as.matrix(D %>% select(std_WOBA_FINAL_BAT_19, std_WOBA_FINAL_PIT_19,  
                            HAND_MATCH, BAT_HOME_IND)) 
X2 <- as.matrix(D %>% select(std_BQ, std_PQ,  
                            HAND_MATCH, BAT_HOME_IND))
IS_SIM=FALSE
source("rstan5_main.R")

########################################
########### SIGMA COMPARISON ###########
########################################

fitA <- readRDS(paste0("./job_output/fit_rstan5-1.R.rds"))
fitB <- readRDS(paste0("./job_output/fit_rstan5-4.R.rds"))
fitC <- readRDS(paste0("./job_output/fit_rstan5-2.R.rds"))
fitD <- readRDS(paste0("./job_output/fit_rstan5-5.R.rds"))
#plot_bsn0(fitA)
### draws
sigmaA <- as_tibble(as.matrix(fitA))$sigma
sigmaB <- as_tibble(as.matrix(fitB))$sigma
sigmaC <- as_tibble(as.matrix(fitC))$sigma
sigmaD <- as_tibble(as.matrix(fitD))$sigma
### sigma
mean(sigmaA)
mean(sigmaB)
mean(sigmaC)
mean(sigmaD)
sd(sigmaA)
sd(sigmaB)
sd(sigmaC)
sd(sigmaD)



# length_ratios2 = lengths_ubi2/lengths_bsn2
# mean(length_ratios)
# sd(length_ratios)
# mean(length_ratios2)
# sd(length_ratios2)
# lp = as_tibble(length_ratios) %>% ggplot(aes(x=value)) + 
#   geom_histogram(binwidth=.001) + 
#   geom_vline(xintercept = mean(length_ratios), color="firebrick") +
#   geom_vline(xintercept = 1, color="dodgerblue2") +
#   xlim(c(0.9,1.2)) + 
#   xlab("Ratio of UBI length to BSN length") +
#   annotate(x=1,y=+Inf,label="1",vjust=2,geom="label",color="dodgerblue2") +
#   annotate(x=mean(length_ratios),y=2900,label="Mean ratio of lengths",vjust=2,geom="label",color="firebrick")
# lp
# #ggsave("job_output/plot_ppLengthRatio.png", lp)

