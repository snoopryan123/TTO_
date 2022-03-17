library(tidyverse)
output_folder = './job_output/'

### load data
input_file = "./../data/TTO_dataset_510.csv"  
D <- read_csv(input_file) %>% filter(!PIT_IS_BAT) # %>% drop_na() 
D <- D %>% filter(YEAR == 2019) %>% filter(BQ>0 & BQ<1 & PQ>0 & PQ<1)
D <- D %>% filter(ORDER_CT <= 3) # keep only 1TTO, 2TTO, 3TTO
logit <- function(p) { log(p/(1-p)) }
X <- as.matrix(D %>% mutate(lBQ=logit(BQ), lPQ=logit(PQ)) %>% select(lBQ, lPQ, HAND_MATCH, BAT_HOME_IND)) 
OUTPUT_FILE = "rstan8-4.R"

### rstan
source("rstan8_main.R")
fit = fit_model_cubic_w_shifts(NA) 
saveRDS(fit, file = paste0(output_folder, "fit_", OUTPUT_FILE, ".rds"))

### plot
# # fit <- readRDS("./job_output/fit_rstan8-3.R.rds") 
# p = plot_bsn0(fit)
# p
# ggsave(paste0("./plot_",OUTPUT_FILE,".png"), p)





###########################
### CROSS ENTROPY LOSS ####
###########################

# bsn_fit_to_posterior_probs <- function(S_test,X_test,fit) {
#   draws=as.matrix(fit)
#   alpha_draws = draws[,str_detect(colnames(draws), "^alpha")]
#   eta_draws = draws[,str_detect(colnames(draws), "^eta")]
#   
#   linpreds = list()
#   for (k in 1:7) {
#     print(k)
#     alpha_draws_k = alpha_draws[,endsWith(colnames(alpha_draws), paste0(k,"]"))]
#     eta_draws_k = eta_draws[,endsWith(colnames(eta_draws), paste0(k,"]"))]
#     linpred_k = S_test%*%t(alpha_draws_k) + X_test%*%t(eta_draws_k)
#     linpreds[[length(linpreds)+1]] = linpred_k
#   }
#   linpreds = lapply(linpreds, exp)
#   ## linpreds[[1]][1:10,1:10]
#   sum_linpreds = Reduce("+", linpreds)
#   normalize <- function(A) { A / sum_linpreds} 
#   probs = lapply(linpreds, normalize)
#   ## probs[[1]][1,1]+probs[[2]][1,1]+probs[[3]][1,1]+probs[[4]][1,1]+probs[[5]][1,1]+probs[[6]][1,1]+probs[[7]][1,1]
#   ## probs[[7]][1:1000]
#   probs
# }
# 
# cross_entropy_loss_posterior <- function(probs,y_test) {
#   cross_entropy_losses = list()
#   for (i in 1:length(y_test)) {
#     entropy_i = as.matrix( probs[[y_test[i]]][i,] )
#     cross_entropy_losses[[length(cross_entropy_losses) + 1]] = entropy_i
#   }
#   cross_entropy_loss_M = t(do.call(cbind, cross_entropy_losses))
#   ## cross_entropy_loss_M[1:10,1:10]
#   cross_entropy_loss_M = -log(cross_entropy_loss_M)
#   cross_entropy_losses = rowMeans(cross_entropy_loss_M)
#   mean(cross_entropy_losses)
# }
# 
# ### test data matrices
# X_test = X[1:5000,]
# S_test = S[1:5000,]
# y_test = y[1:5000,]
# ### posterior probabilities for each outcome
# probs = bsn_fit_to_posterior_probs(S_test,X_test,fit)
# # probs[[1]][1:1000]
# ### cross entropy loss
# cel = cross_entropy_loss_posterior(probs,y_test)
# cel
# ### empirical proportions of each outcome
# # as_tibble(y_test) %>% group_by(value) %>% summarise(count=n()) %>% ungroup() %>% mutate(prop = count/sum(count))
# # c(mean(probs[[1]]),mean(probs[[2]]), mean(probs[[3]]), mean(probs[[4]]), mean(probs[[5]]), mean(probs[[6]]), mean(probs[[7]]))

