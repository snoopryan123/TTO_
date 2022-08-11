library(tidyverse)
output_folder = './job_output/'

### load data
OUTPUT_FILE = paste0("rstan8_sim-",s,".R")
input_file = "../../data/TTO_dataset_510.csv"  
D <- read_csv(input_file) #%>% drop_na() 
D <- D %>% filter(YEAR == 2019) %>% filter(BQ>0 & BQ<1 & PQ>0 & PQ<1 )  #%>% filter(ORDER_CT <= 3)
logit <- function(p) { log(p/(1-p)) }
X <- as.matrix(D %>% mutate(lBQ=logit(BQ), lPQ=logit(PQ)) %>% select(lBQ, lPQ, HAND_MATCH, BAT_HOME_IND)) 
source("rstan8_sim_main.R")

# generate categorical outcome vector y
linpred = S %*% t(alpha) + X %*% t(eta)
P = exp(linpred) / rowSums( exp(linpred) )
get_outcome <- function(i) { # get the categorical outcome in {1,2,...,7} of row i
  which(rmultinom(1, 1, P[i,]) == 1)
}
set.seed(s) 
y = matrix( sapply(1:nrow(linpred), get_outcome), ncol=1)
# as_tibble(as.matrix(y)) %>% group_by(V1) %>% summarise(count=n()) %>% ungroup() %>% mutate(prop=count/sum(count))
saveRDS(y, file = paste0(output_folder, "y_", OUTPUT_FILE, ".rds"))
#y <- readRDS("./job_output/y_rstan8_sim-51.R.rds") 

# fit the model
fit = fit_model_spline(fold_num=1) #FIXME
saveRDS(fit, file = paste0(output_folder, "fit_", OUTPUT_FILE, ".rds"))
#fit <- readRDS("./job_output/fit_rstan8_sim-101.R.rds") 

#####################################
### base rate cross entropy loss ####
#####################################

cross_entropy_losses = numeric()
for (ss in 101:125) {
  print(ss)
  set.seed(ss) 
  y_s = matrix( sapply(1:nrow(linpred), get_outcome), ncol=1)
  
  
  y_train = y_s #y_s[train_rows,]
  y_train_df = as_tibble(y_train) %>% rename(y=V1)
  base_rate_df = y_train_df %>% group_by(y) %>% summarise(count=n()) %>% mutate(p = count/sum(count))
  # y_test_df = as_tibble(y_test) %>% rename(y=value)
  cross_entropy_losses = c(cross_entropy_losses,
                           mean(-log(as.numeric(as.matrix(modelr::model_matrix(~ factor(y) + 0, data=y_train_df)) %*% matrix(base_rate_df$p, ncol=1))))
  )
}
mean(cross_entropy_losses)

-log(1/7)

###########################
### CROSS ENTROPY LOSS ####
###########################

get_bat_seq_draws <- function(draws) {
  alpha_draws = draws[,str_detect(colnames(draws), "^alpha")]
  bat_seq_draws = list()
  for (k in 1:7) {
    alpha_draws_k = alpha_draws[,endsWith(colnames(alpha_draws), paste0(k,"]"))]
    bat_seq_draws_k = alpha_draws_k %*% t(bbb)
    bat_seq_draws[[length(bat_seq_draws) + 1]] = bat_seq_draws_k
  }
  bat_seq_draws
}

spline_fit_to_posterior_probs <- function(SPL_test,X_test,fit) {
  draws=as.matrix(fit)
  alpha_draws = draws[,str_detect(colnames(draws), "^alpha")]
  eta_draws = draws[,str_detect(colnames(draws), "^eta")]
  
  linpreds = list()
  for (k in 1:7) {
    print(k)
    alpha_draws_k = alpha_draws[,endsWith(colnames(alpha_draws), paste0(k,"]"))]
    eta_draws_k = eta_draws[,endsWith(colnames(eta_draws), paste0(k,"]"))]
    linpred_k = SPL_test%*%t(alpha_draws_k) + X_test%*%t(eta_draws_k)
    linpreds[[length(linpreds)+1]] = linpred_k
  }
  linpreds = lapply(linpreds, exp)
  ## linpreds[[1]][1:10,1:10]
  sum_linpreds = Reduce("+", linpreds)
  normalize <- function(A) { A / sum_linpreds}
  probs = lapply(linpreds, normalize)
  ## probs[[1]][1,1]+probs[[2]][1,1]+probs[[3]][1,1]+probs[[4]][1,1]+probs[[5]][1,1]+probs[[6]][1,1]+probs[[7]][1,1]
  ## probs[[1]][1:1000]
  ## dim(probs[[7]])
  probs
}

cross_entropy_loss_posterior <- function(probs,y_test) {
  cross_entropy_losses = list()
  for (i in 1:length(y_test)) {
    entropy_i = as.matrix( probs[[y_test[i]]][i,] )
    cross_entropy_losses[[length(cross_entropy_losses) + 1]] = entropy_i
  }
  cross_entropy_loss_M = t(do.call(cbind, cross_entropy_losses))
  ## cross_entropy_loss_M[1:10,1:10]
  cross_entropy_loss_M = -log(cross_entropy_loss_M)
  cross_entropy_losses = rowMeans(cross_entropy_loss_M)
  mean(cross_entropy_losses)
}

### test data matrices
test_rows = which(folds == 1)
X_test = X[test_rows,]
SPL_test = SPL[test_rows,]
y_test = y[test_rows,]

### batter sequence draws 1,...,36 for each category
draws <- as.matrix(fit)
bat_seq_draws = get_bat_seq_draws(draws) 
### posterior probabilities for each outcome
probs = spline_fit_to_posterior_probs(SPL_test,X_test,fit)
# probs[[1]][1:1000]
### cross entropy loss
cel = cross_entropy_loss_posterior(probs,y_test)
print(cel)
### empirical proportions of each outcome
# as_tibble(y_test) %>% group_by(value) %>% summarise(count=n()) %>% ungroup() %>% mutate(prop = count/sum(count))
# c(mean(probs[[1]]),mean(probs[[2]]), mean(probs[[3]]), mean(probs[[4]]), mean(probs[[5]]), mean(probs[[6]]), mean(probs[[7]]))

#####################################
### base rate cross entropy loss ####


