
#########################
library(gt)
source("sim_10fcv_config.R")
source("../model9_getData.R") ### get observed data 

SIM_NUM = 1 #1 #2
#########################

fit_to_posterior_probs <- function(fit,model_num,SPL_test,O_test,X_test) {
  draws=as.matrix(fit)
  alpha_draws = draws[,str_detect(colnames(draws), "^alpha")]
  eta_draws = draws[,str_detect(colnames(draws), "^eta")]
  if (model_num != 2) { 
    beta_draws = draws[,str_detect(colnames(draws), "^beta")] 
  }
  
  linpreds = list()
  for (k in 1:7) {
    print(k)
    alpha_draws_k = alpha_draws[,endsWith(colnames(alpha_draws), paste0(k,"]"))]
    eta_draws_k = eta_draws[,endsWith(colnames(eta_draws), paste0(k,"]"))]
    if (model_num != 2) { 
      beta_draws_k = beta_draws[,endsWith(colnames(beta_draws), paste0(k,"]"))] 
    }
    
    if (model_num == 1) {
      linpred_k = SPL_test%*%t(alpha_draws_k) + O_test%*%t(beta_draws_k) + X_test%*%t(eta_draws_k)
    } else if (model_num == 2) {
      linpred_k = SPL_test%*%t(alpha_draws_k) + X_test%*%t(eta_draws_k)
    } else if (model_num == 3) {
      ### here, SPL <- Incpt
      linpred_k = SPL_test%*%t(alpha_draws_k) + O_test%*%t(beta_draws_k) + X_test%*%t(eta_draws_k)
    }
    
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

cross_entropy_loss_vec <- function(probs,y_test) {
  cross_entropy_losses = list()
  for (i in 1:length(y_test)) {
    entropy_i = as.matrix( probs[[y_test[i]]][i,] )
    cross_entropy_losses[[length(cross_entropy_losses) + 1]] = entropy_i
  }
  cross_entropy_loss_M = t(do.call(cbind, cross_entropy_losses))
  ## cross_entropy_loss_M[1:10,1:10]
  cross_entropy_loss_M = -log(cross_entropy_loss_M)
  cross_entropy_losses = rowMeans(cross_entropy_loss_M)
  cross_entropy_losses
}

#########################
s = 1
fold_num = 1

source("sim_simulateData.R") ### get simulated outcomes and "true" params
print("***** s="); print(s); print("*****");

### import fit from rstan
fit1 <- readRDS(paste0(output_folder,"fit_sim", SIM_NUM, "_model", 1, "_", s, ".rds"))
fit2 <- readRDS(paste0(output_folder,"fit_sim", SIM_NUM, "_model", 2, "_", s, ".rds"))
fit3 <- readRDS(paste0(output_folder,"fit_sim", SIM_NUM, "_model", 3, "_", s, ".rds"))

### out of sample test data
test_rows = if (is.na(fold_num)) TRUE else which(folds == fold_num)
y_test = y[test_rows,]
SPL_test = SPL[test_rows,] 
O_test = O[test_rows,]
X_test = X[test_rows,]
Incpt_test = matrix(Incpt[test_rows,]) 

### get posterior probs
probs1 <- fit_to_posterior_probs(fit1,1,SPL_test,O_test,X_test)
probs2 <- fit_to_posterior_probs(fit2,2,SPL_test,NA,X_test)
probs3 <- fit_to_posterior_probs(fit3,3,Incpt_test,O_test,X_test)

celv1 <- cross_entropy_loss_vec(probs1,y_test)
celv2 <- cross_entropy_loss_vec(probs2,y_test)
celv3 <- cross_entropy_loss_vec(probs3,y_test)

### out of sample celv
plot_cel = tibble(cel_1=mean(celv1), cel_2=mean(celv2), cel_3=mean(celv3)) %>% gt()
# plot_cel
gtsave(plot_cel, paste0("plot_cel_sim", SIM_NUM, "_10fcv_s", s, ".png"))


### out of sample ecological celv
bsn_test = D[test_rows,]$BATTER_SEQ_NUM
eco_df = tibble(t=bsn_test, cel1=celv1, cel2=celv2, cel3=celv3)
options(pillar.sigfig=5)

plot_eco_cel = eco_df %>% 
  group_by(t) %>%
  summarise(
    cel1 = mean(cel1),
    cel2 = mean(cel2),
    cel3 = mean(cel3),
  ) %>%
  summarise(
    eco_cel_1 = mean(cel1),
    eco_cel_2 = mean(cel2),
    eco_cel_3 = mean(cel3),
  ) %>%
  gt()
# plot_eco_cel
gtsave(plot_eco_cel, paste0("plot_cel_eco_sim", SIM_NUM, "_10fcv_s", s, ".png"))





