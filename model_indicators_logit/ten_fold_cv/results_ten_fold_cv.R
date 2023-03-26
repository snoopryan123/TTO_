
########################
YRS = 2018
IS_SIM = TRUE ### even though this is 10 fold cv and not a sim, we need this to be TRUE to get the code to work!
NUM_FOLDS = 10 #FIXME
source("../A_getData.R") ### get observed data 
########################

### observed base rates
base_rates = tibble(k=y[,1]) %>% group_by(k) %>% summarise(count=n()) %>% mutate(p=count/sum(count)) %>% select(-count)
base_rates
write_csv(base_rates, "results/base_rates.csv")

fit_to_posterior_probs <- function(fit,S,X,probs_as_list=FALSE) {
  draws=as.matrix(fit)
  alpha_draws <- draws[,startsWith(colnames(draws), "alpha")]
  eta_draws = draws[,str_detect(colnames(draws), "^eta")]
  linpreds = list()
  for (k in 1:7) {
    print(k)
    alpha_draws_k = alpha_draws[,endsWith(colnames(alpha_draws), paste0(k,"]"))]
    eta_draws_k = eta_draws[,endsWith(colnames(eta_draws), paste0(k,"]"))]
    linpred_k = S%*%t(alpha_draws_k) + X%*%t(eta_draws_k)
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
  if(probs_as_list) {
    return(probs)
  }
  
  ### turn to tibble
  probs_df = tibble()
  for (k in 1:7) {
    print(k)
    probs_df_k0 = probs[[k]]
    probs_df_k = reshape2::melt(probs_df_k0) %>%
      as_tibble() %>%
      rename(t = Var1, iter=Var2, p=value) %>%
      arrange(t, iter) %>%
      mutate(k = k) 
    probs_df = bind_rows(probs_df, probs_df_k)
  }
  # probs_tilde_df %>% group_by(k) %>% summarise(count=n(), count2=n()/27) ## check
  rm(probs)
  return(probs_df)
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

################################################

cel_model = numeric(NUM_FOLDS)
cel_base_rates = numeric(NUM_FOLDS)
for (fold_num in 1:NUM_FOLDS) {
  print(paste0("**** fold ", fold_num," ****"))
  
  ### cross entropy loss using base rates
  train_rows = which(folds != fold_num)
  test_rows = which(folds == fold_num)
  y_train = y[train_rows,]
  y_test = y[test_rows,]
  base_rates = tibble(k=y_train) %>% group_by(k) %>% summarise(count=n()) %>% mutate(p=count/sum(count)) %>% select(-count)
  base_rate_CEL_test = tibble(k=y_test) %>% left_join(base_rates) %>% mutate(cel = -log(p)) %>% summarise(cel = mean(cel))
  cel_base_rates[fold_num] = base_rate_CEL_test$cel
  
  ### import fit from rstan
  OUTPUT_FILE = paste0("job_output/", "fit_ten_fold_cv_", fold_num, "_model_indicators_logit.rds")
  fit <- readRDS(OUTPUT_FILE)
  draws <- as.matrix(fit)

  alpha_draws <- draws[,startsWith(colnames(draws), "alpha")]
  eta_draws <- draws[,startsWith(colnames(draws), "eta")]
  
  ### cross entropy loss of our model
  S_test = S[test_rows,]
  X_test = X[test_rows,]
  y_test = y[test_rows,]
  p_test = fit_to_posterior_probs(fit,S_test,X_test,probs_as_list=TRUE)
  cel_test = cross_entropy_loss_posterior(p_test, y_test)
  cel_model[fold_num] = cel_test
}

write_csv(tibble(cel_model_test=mean(cel_model)), paste0("results/cel_model_ten_fold_cv.csv"))
write_csv(tibble(cel_base_rates_test=mean(cel_base_rates)), paste0("results/cel_base_rates_ten_fold_cv.csv"))

