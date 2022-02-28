library(tidyverse)
output_folder = './job_output/'

### load data
input_file = "./../data/TTO_dataset_510.csv"  
D <- read_csv(input_file)
##D <- D %>% drop_na() 
#FIXME 
D <- D %>% filter(YEAR == 2019) %>% filter(BQ>0 & BQ<1 & PQ>0 & PQ<1 )
logit <- function(p) { log(p/(1-p)) }
X <- as.matrix(D %>% mutate(lBQ=logit(BQ), lPQ=logit(PQ)) %>% select(lBQ, lPQ, HAND_MATCH, BAT_HOME_IND)) 
OUTPUT_FILE = "rstan8-1.R"

### rstan
source("rstan8_main.R")
fit = fit_model_bsn(NA) 
saveRDS(fit, file = paste0(output_folder, "fit_", OUTPUT_FILE, ".rds"))

### plot
# fit <- readRDS("./job_output/fit_rstan8-1.R.rds") 
p = plot_bsn0(fit)
p
ggsave(paste0("./plot_",OUTPUT_FILE,".png"), p)





# ### cross entropy loss
# draws=as.matrix(fit)
# alpha_draws = draws[,str_detect(colnames(draws), "^alpha")]
# eta_draws = draws[,str_detect(colnames(draws), "^eta")]
# alpha_draws_1 = alpha_draws[,endsWith(colnames(alpha_draws), "1]")]
# alpha_draws_2 = alpha_draws[,endsWith(colnames(alpha_draws), "2]")]
# alpha_draws_3 = alpha_draws[,endsWith(colnames(alpha_draws), "3]")]
# alpha_draws_4 = alpha_draws[,endsWith(colnames(alpha_draws), "4]")]
# alpha_draws_5 = alpha_draws[,endsWith(colnames(alpha_draws), "5]")]
# alpha_draws_6 = alpha_draws[,endsWith(colnames(alpha_draws), "6]")]
# alpha_draws_7 = alpha_draws[,endsWith(colnames(alpha_draws), "7]")]
# eta_draws_1 = eta_draws[,endsWith(colnames(eta_draws), "1]")]
# eta_draws_2 = eta_draws[,endsWith(colnames(eta_draws), "2]")]
# eta_draws_3 = eta_draws[,endsWith(colnames(eta_draws), "3]")]
# eta_draws_4 = eta_draws[,endsWith(colnames(eta_draws), "4]")]
# eta_draws_5 = eta_draws[,endsWith(colnames(eta_draws), "5]")]
# eta_draws_6 = eta_draws[,endsWith(colnames(eta_draws), "6]")]
# eta_draws_7 = eta_draws[,endsWith(colnames(eta_draws), "7]")]
# raw_p1 = exp( S %*% t(alpha_draws_1) + X %*% t(eta_draws_1))
# raw_p2 = exp( S %*% t(alpha_draws_2) + X %*% t(eta_draws_2))
# raw_p3 = exp( S %*% t(alpha_draws_3) + X %*% t(eta_draws_3))
# raw_p4 = exp( S %*% t(alpha_draws_4) + X %*% t(eta_draws_4))
# raw_p5 = exp( S %*% t(alpha_draws_5) + X %*% t(eta_draws_5))
# raw_p6 = exp( S %*% t(alpha_draws_6) + X %*% t(eta_draws_6))
# raw_p7 = exp( S %*% t(alpha_draws_7) + X %*% t(eta_draws_7))
# raw_summed_p = raw_p1+raw_p2+raw_p3+raw_p4+raw_p5+raw_p6+raw_p7
# p1 = 1 / raw_summed_p
# p2 = raw_p2 * p1
# p3 = raw_p3 * p1
# p4 = raw_p4 * p1
# p5 = raw_p5 * p1
# p6 = raw_p6 * p1
# p7 = raw_p7 * p1
# # aaa=p1+p2+p3+p4+p5+p6+p7; aaa[1:10,1:10]
# p1[1:10,1:10]
# 
# # posterior predictive intervals on the probabilities
# pplower1 = apply(p1, 1, function(x) quantile(x,.025))
# pplower2 = apply(p2, 1, function(x) quantile(x,.025))
# pplower3 = apply(p3, 1, function(x) quantile(x,.025))
# pplower4 = apply(p4, 1, function(x) quantile(x,.025))
# pplower5 = apply(p5, 1, function(x) quantile(x,.025))
# pplower6 = apply(p6, 1, function(x) quantile(x,.025))
# pplower7 = apply(p7, 1, function(x) quantile(x,.025))
# ppmean1 = apply(p1, 1, function(x) mean(x))
# ppmean2 = apply(p2, 1, function(x) mean(x))
# ppmean3 = apply(p3, 1, function(x) mean(x))
# ppmean4 = apply(p4, 1, function(x) mean(x))
# ppmean5 = apply(p5, 1, function(x) mean(x))
# ppmean6 = apply(p6, 1, function(x) mean(x))
# ppmean7 = apply(p7, 1, function(x) mean(x))
# ppmeans = cbind(ppmean1,ppmean2,ppmean3,ppmean4,ppmean5,ppmean6,ppmean7)
# ppupper1 = apply(p1, 1, function(x) quantile(x,.975))
# ppupper2 = apply(p2, 1, function(x) quantile(x,.975))
# ppupper3 = apply(p3, 1, function(x) quantile(x,.975))
# ppupper4 = apply(p4, 1, function(x) quantile(x,.975))
# ppupper5 = apply(p5, 1, function(x) quantile(x,.975))
# ppupper6 = apply(p6, 1, function(x) quantile(x,.975))
# ppupper7 = apply(p7, 1, function(x) quantile(x,.975))
# 
# y_test
# y_test_class_matrix = modelr::model_matrix(~ factor(y_test) + 0,data=as.data.frame(y_test)) 
# cross_entropy_loss_bsn[i] = sum( -1 * y_test_class_matrix * log(ppmeans) ) / length(y_test)

