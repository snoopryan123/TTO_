library(tidyverse)
output_folder = './job_output/'

### load data
input_file = "../../data/TTO_dataset_510.csv"  
D <- read_csv(input_file) #%>% drop_na() 
D <- D %>% filter(YEAR == 2019) %>% filter(BQ>0 & BQ<1 & PQ>0 & PQ<1 ) %>% filter(ORDER_CT <= 3)
logit <- function(p) { log(p/(1-p)) }
X <- as.matrix(D %>% mutate(lBQ=logit(BQ), lPQ=logit(PQ)) %>% select(lBQ, lPQ, HAND_MATCH, BAT_HOME_IND)) 
source("rstan8_sim_main.R")

#####################################
### base rate cross entropy loss ####
#####################################

cross_entropy_losses = numeric()
for (s in 101:125) {
  print(fold_num)
  train_rows = which(folds != fold_num)
  test_rows = which(folds == fold_num)
  y_train = y[train_rows,]
  y_train_df = as_tibble(y_train) %>% rename(y=value)
  base_rate_df = y_train_df %>% group_by(y) %>% summarise(count=n()) %>% mutate(p = count/sum(count))
  y_test_df = as_tibble(y_test) %>% rename(y=value)
  cross_entropy_losses = c(cross_entropy_losses,
                           -log(as.numeric(as.matrix(modelr::model_matrix(~ factor(y) + 0, data=y_test_df)) %*% matrix(base_rate_df$p, ncol=1)))
  )
}
mean(cross_entropy_losses)

-log(1/7)

#########################
### HELPER FUNCTIONS ####
#########################

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

spline_get_all_params <- function(bat_seq_draws,eta_draws) {
  all_params = list()
  for (k in 1:7) {
    # print(k)
    bat_seq_draws_k = bat_seq_draws[[k]]
    eta_draws_k = eta_draws[,endsWith(colnames(eta_draws), paste0(k,"]"))]
    all_params_k = cbind(bat_seq_draws_k, eta_draws_k)
    all_params[[length(all_params)+1]] = all_params_k
  }
  all_params
}

bsn_post_means_and_ci <- function(all_params) {
  params_true = cbind(alpha,eta)
  pp_df = tibble()
  for (k in 1:7) {
    all_params_k = all_params[[k]]
    pplower_k = apply(all_params_k, 2, function(x) quantile(x,.025))
    ppmeans_k = colMeans(all_params_k)
    ppupper_k = apply(all_params_k, 2, function(x) quantile(x,.975))
    p_true_k = params_true[k,]
    p_names = c(paste0("alpha",1:p_s),paste0("eta",1:p_x))
    pp_df_k = tibble(k=k,pplower=pplower_k,ppmean=ppmeans_k,ppupper=ppupper_k,
                     var=p_names,param_true = p_true_k)
    pp_df = bind_rows(pp_df, pp_df_k)
  }
  pp_df %>% arrange(-k) %>% mutate(covered = pplower <= param_true & param_true <= ppupper,
                                   pplength = ppupper - pplower)
}

spl_detect_avg_tto_effect <- function(bat_seq_draws) {
  tto12_avg_effect_detected = numeric(6)
  tto23_avg_effect_detected = numeric(6)
  for (k in 2:7) {
    # print(k)
    alpha_draws_k = bat_seq_draws[[k]][,1:27]
    a_tto1 = rowMeans(alpha_draws_k[,1:9])
    a_tto2 = rowMeans(alpha_draws_k[,10:18])
    a_tto3 = rowMeans(alpha_draws_k[,19:27])
    a21lower = quantile(a_tto2 - a_tto1,.025)
    a32lower = quantile(a_tto3 - a_tto2,.025)
    tto12_avg_effect_detected[k-1] = a21lower > 0
    tto23_avg_effect_detected[k-1] = a32lower > 0
  }
  list(tto12_avg_effect_detected=tto12_avg_effect_detected,
       tto23_avg_effect_detected=tto23_avg_effect_detected)
}

spl_detect_BL_tto_effect <- function(bat_seq_draws) {
  tto12_BL_effect_detected = numeric(6)
  tto23_BL_effect_detected = numeric(6)
  for (k in 2:7) {
    # print(k)
    alpha_draws_k = bat_seq_draws[[k]][,1:27]
    a_tto12 = alpha_draws_k[,10] - alpha_draws_k[,9]
    a_tto23 = alpha_draws_k[,19] - alpha_draws_k[,18]
    a21lower = quantile(a_tto12,.025)
    a32lower = quantile(a_tto23,.025)
    tto12_BL_effect_detected[k-1] = a21lower > 0
    tto23_BL_effect_detected[k-1] = a32lower > 0
  }
  list(tto12_BL_effect_detected=tto12_BL_effect_detected,
       tto23_BL_effect_detected=tto23_BL_effect_detected)
}

###############
### metrics ###
###############

NSIM = 25 #FIXME #25

## cross entropy loss 
cel_vec = numeric(NSIM)
## coverage ??
### proportion of TTO parameters that are covered
prop_tto_params_covered = numeric(NSIM)
### which params are covered
np = p_s+p_x
covered_params = matrix(nrow=NSIM,ncol=6*np)
colnames(covered_params) = paste0(
  rep(c(paste0("alpha",1:p_s),paste0("eta",1:p_x)),6),
  paste0("_k",c(rep(2,np),rep(3,np),rep(4,np),rep(5,np),rep(6,np),rep(7,np)))
)
## average length of credible intervals for TTO params
avg_length_ci_tto_params = numeric(NSIM)
## for each category, proportion of simulations in which a 2TTO avg. effect is detected
tto2_avg_detected = matrix(nrow=NSIM,ncol=6)
## for each category, proportion of simulations in which a 3TTO avg. effect is detected
tto3_avg_detected = matrix(nrow=NSIM,ncol=6)
## for each category, proportion of simulations in which a 2TTO batter-learning effect is detected
tto2_BL_detected = matrix(nrow=NSIM,ncol=6)
## for each category, proportion of simulations in which a 3TTO batter-learning effect is detected
tto3_BL_detected = matrix(nrow=NSIM,ncol=6)

##########################
### loop over all sims ###
##########################

test_rows = which(folds == 1)
for (i in 1:NSIM) {
  ii = i + 100
  print(paste0("i = ",ii))

  # posterior samples & y vector
  fit <- readRDS(paste0("job_output/fit_rstan8_sim-",ii,".R.rds"))
  draws <- as.matrix(fit)
  y <- readRDS(paste0("job_output/y_rstan8_sim-",ii,".R.rds"))
  
  ### test data matrices
  X_test = X[test_rows,]
  S_test = S[test_rows,]
  SPL_test = SPL[test_rows,]
  y_test = y[test_rows,]
  n_test = nrow(X_test)
  
  ### posterior probabilities for each outcome
  probs = spline_fit_to_posterior_probs(SPL_test,X_test,fit)
  # probs[[1]][1:1000]
  
  ### cross entropy loss
  cel = cross_entropy_loss_posterior(probs,y_test)
  cel_vec[i] = cel
  
  ### empirical proportions of each outcome
  # as_tibble(y_test) %>% group_by(value) %>% summarise(count=n()) %>% ungroup() %>% mutate(prop = count/sum(count))
  # c(mean(probs[[1]]),mean(probs[[2]]), mean(probs[[3]]), mean(probs[[4]]), mean(probs[[5]]), mean(probs[[6]]), mean(probs[[7]]))
  
  ### bat_seq_draws and eta_draws
  bat_seq_draws = get_bat_seq_draws(draws)
  eta_draws = draws[,str_detect(colnames(draws), "^eta")]
    
  ### posterior means & CI's for all parameters
  all_params = spline_get_all_params(bat_seq_draws,eta_draws) 
  pp_df = bsn_post_means_and_ci(all_params)
  
  ### which params are covered
  covered_params[i,] =  unname(
    (pp_df %>% filter(k!=1) %>% select(k,var,covered) %>% arrange(k))$covered)
  
  ### proportion of TTO parameters that are covered
  tto_params = paste0("alpha",1:27)
  prop_tto_params_covered[i] = mean((pp_df %>% filter(k!=1) %>% filter(var %in% tto_params))$covered)
  
  ## average length of credible intervals for TTO params
  avg_length_ci_tto_params[i] = mean((pp_df %>% filter(k!=1) %>% filter(var %in% tto_params))$pplength)

  ## for each category, was a 2TTO & 3TTO avg. effect detected
  detect_tto_avg_effect = spl_detect_avg_tto_effect(bat_seq_draws)
  tto2_avg_detected[i,] = detect_tto_avg_effect[[1]]
  tto3_avg_detected[i,] = detect_tto_avg_effect[[2]]
  
  ## for each category, was a 2TTO & 3TTO batter-learning effect detected
  detect_tto_BL_effect = spl_detect_BL_tto_effect(bat_seq_draws)
  tto2_BL_detected[i,] = detect_tto_BL_effect[[1]]
  tto3_BL_detected[i,] = detect_tto_BL_effect[[2]]
}

######################
### FINAL METRICS ####
######################

print("average cross entropy loss")
print(mean(cel_vec))
print("average proportion of TTO parameters (alpha_k) that are covered")
print(mean(prop_tto_params_covered))
print("avg. average length of credible intervals for TTO params (alpha_k)")
print(mean(avg_length_ci_tto_params))
print("for each category, avg. proportion of simulations in which a 2TTO avg. effect is detected")
print(colMeans(tto2_avg_detected))
print("for each category, avg. proportion of simulations in which a 3TTO avg. effect is detected")
print(colMeans(tto3_avg_detected))
print("for each category, avg.proportion of simulations in which a 2TTO batter-learning effect is detected")
print(colMeans(tto2_BL_detected))
print("for each category, avg. proportion of simulations in which a 3TTO batter-learning effect is detected")
print(colMeans(tto3_BL_detected))
## coverage ??
print("average proportion of all parameters that are covered")
mean(covered_params)
print("average proportion that each parameter is covered")
print(colMeans(covered_params))





##################################
### plot sim for presentation ####
##################################

get_eta_draws <- function(fit) {
  draws=as.matrix(fit)
  eta_draws = draws[,str_detect(colnames(draws), "^eta")]
  all_params = list()
  for (k in 1:7) {
    # print(k)
    eta_draws_k = eta_draws[,endsWith(colnames(eta_draws), paste0(k,"]"))]
    all_params[[length(all_params)+1]] = eta_draws_k
  }
  all_params
}

# posterior samples & y vector
fit <- readRDS(paste0("job_output/fit_rstan8_sim-",101,".R.rds"))
draws <- as.matrix(fit)

############################
bat_seq_draws = get_bat_seq_draws(draws)
eta_draws = get_eta_draws(fit)

sim_plot_tib = tibble()
for (k in 2:7) {
  print(k)
  alpha_k = bat_seq_draws[[k]]
  post_alpha_k_L = apply(alpha_k, MARGIN=2, FUN=function(x) quantile(x, 0.05))
  post_alpha_k_M = apply(alpha_k, MARGIN=2, FUN=function(x) quantile(x, 0.5))
  post_alpha_k_U = apply(alpha_k, MARGIN=2, FUN=function(x) quantile(x, 0.95))
  true_alpha_k = alpha[k,]
  sim_plot_tib_k = tibble(
    k = k,
    bsn = 1:27,
    post_alpha_k_L = post_alpha_k_L,
    post_alpha_k_M = post_alpha_k_M,
    post_alpha_k_U = post_alpha_k_U,
    true_alpha_k = true_alpha_k
  )
  sim_plot_tib = bind_rows(sim_plot_tib, sim_plot_tib_k)
}

simPlot_paramScale = sim_plot_tib %>%
  mutate(label = case_when(
    k == 2 ~ "uBB",
    k == 3 ~ "HBP",
    k == 4 ~ "1B",
    k == 5 ~ "2B",
    k == 6 ~ "3B",
    k == 7 ~ "HR",
  )) %>%
  mutate(label = fct_reorder(label,k)) %>%
  ggplot(aes(x=bsn)) +
  facet_wrap(~label, nrow=3) +
  geom_point(aes(y=post_alpha_k_M), color="dodgerblue2", fill="white") +
  geom_errorbar(aes(ymin=post_alpha_k_L, ymax=post_alpha_k_U)) +
  geom_point(aes(y=true_alpha_k), color="firebrick") +
  scale_x_continuous(name=TeX("Batter Sequence Number $t$"), 
                     limits = c(0,26.5), 
                     breaks = seq(3,24,by=3)) +
  ylab("TTO parameters")
simPlot_paramScale
ggsave("simPlot_paramScale.png", simPlot_paramScale, width=7, height=5)

############################3
prob_ktx <- function(k,t,x,bat_seq_draws,eta_draws) {
  # batter sequence number t \in {1,...,27}
  # confounder vector x^T = (logit(BQ), logit(PQ), hand, home)
  # return posterior distribution of P(y = k| t, x)
  # bat_seq_draws === alpha_{tk}
  log_numerator = bat_seq_draws[[k]][,t] + eta_draws[[k]] %*% matrix(x)
  denominator = 1
  for (j in 2:7) {
    denominator = denominator + exp(bat_seq_draws[[j]][,t] + eta_draws[[j]] %*% matrix(x))
  }
  log_p = log_numerator - log(denominator)
  p = exp(log_p)
  p
}

get_prob_tibble <- function(x, bat_seq_draws, eta_draws) {
  all_prob_ktx = tibble()
  for (t in 1:27) {
    print(t)
    for (k in 2:7) {
      pktx = as.numeric(prob_ktx(k,t,x, bat_seq_draws,eta_draws))
      tib_pktx = tibble(p = pktx, k=k, t=t)
      all_prob_ktx = bind_rows(all_prob_ktx, tib_pktx)
    }
  }
  all_prob_ktx
}

x1 = c(logit(.315), logit(.315), 1, 0)
probs1 = get_prob_tibble(x1, bat_seq_draws, eta_draws)
probs1

prob_df_forPlot = probs1 %>%
  group_by(k,t, .groups="drop") %>%
  summarise(
    p_L = quantile(p, 0.05),
    p_M = quantile(p, 0.5),
    p_U = quantile(p, 0.95),
  ) %>% mutate(label = case_when(
    k == 2 ~ "uBB",
    k == 3 ~ "HBP",
    k == 4 ~ "1B",
    k == 5 ~ "2B",
    k == 6 ~ "3B",
    k == 7 ~ "HR",
  )) %>%
  mutate(
    alpha = alpha[k,t],
    linpred = alpha + (x1%*%eta[k-1,])[[1]],
    p_raw_true = exp(linpred)
    # eta = eta[k,t]
  ) %>%
  group_by(t) %>%
  mutate(p_true = p_raw_true / (sum(p_raw_true) + 1)) %>%
  ungroup() 
  
simPlot_probScale = prob_df_forPlot %>%
  # mutate(label = fct_reorder(label,
  #                            c("1B", "2B", "HR", "uBB", "HBP", "3B")
  #                            )) %>%
  ggplot(aes(x=t)) +
  facet_wrap(~label, nrow=3, scales = "free") +
  geom_errorbar(aes(ymin=p_L, ymax=p_U)) +
  geom_point(aes(y=p_M), color="dodgerblue2", fill="white", shape=4) +
  geom_point(aes(y=p_true), color="firebrick", shape=17) +
  scale_x_continuous(name=TeX("Batter Sequence Number $t$"), 
                     limits = c(0,26.5), 
                     breaks = seq(3,24,by=3)) +
  ylab("Probability")
simPlot_probScale
ggsave("simPlot_probScale.png", simPlot_probScale, width=9, height=6)

#######
xw_df_forPlot = prob_df_forPlot %>%
  select(k,t,p_L,p_M,p_U,p_true) %>%
  mutate(w = categories[k]*1000) %>%
  mutate(
    xw_L = p_L*w,
    xw_M = p_M*w,
    xw_U = p_U*w,
    xw_true = p_true*w
  ) %>%
  group_by(t) %>%
  summarise(
    xw_L = sum(xw_L),
    xw_M = sum(xw_M),
    xw_U = sum(xw_U),
    xw_true = sum(xw_true)
  )

simPlot_xwScale = xw_df_forPlot %>%
  ggplot(aes(x=t)) +
  geom_errorbar(aes(ymin=xw_L, ymax=xw_U)) +
  geom_point(aes(y=xw_M), color="dodgerblue2", fill="white", shape=4) +
  geom_point(aes(y=xw_true), color="firebrick", shape=17) +
  scale_x_continuous(name=TeX("Batter Sequence Number $t$"), 
                     limits = c(0,26.5), 
                     breaks = seq(3,24,by=3)) +
  ylab("Expected wOBA")
simPlot_xwScale
ggsave("simPlot_xwScale.png", simPlot_xwScale, width=9, height=6)


simPlot_xwScale_2 = xw_df_forPlot %>%
  ### comment out this next mutate
  mutate(
    xw_true = ifelse(10 <= t & t <= 18, xw_true - 160, xw_true),
    xw_L = ifelse(10 <= t & t <= 18, xw_L - 160, xw_L),
    xw_M = ifelse(10 <= t & t <= 18, xw_M - 160, xw_M),
    xw_U = ifelse(10 <= t & t <= 18, xw_U - 160, xw_U),
    
    xw_true = ifelse(19 <= t & t <= 27, xw_true - 490, xw_true),
    xw_L = ifelse(19 <= t & t <= 27, xw_L - 490, xw_L),
    xw_M = ifelse(19 <= t & t <= 27, xw_M - 490, xw_M),
    xw_U = ifelse(19 <= t & t <= 27, xw_U - 490, xw_U)
  ) %>%
  ggplot(aes(x=t)) +
  geom_errorbar(aes(ymin=xw_L, ymax=xw_U)) +
  geom_point(aes(y=xw_M), color="dodgerblue2", fill="white", shape=4) +
  geom_point(aes(y=xw_true), color="firebrick", shape=17) +
  scale_x_continuous(name=TeX("Batter Sequence Number $t$"), 
                     limits = c(0,26.5), 
                     breaks = seq(3,24,by=3)) +
  scale_y_continuous(name = "Expected wOBA",
                     limits = c(150,450),
                     breaks = seq(200, 400, by=100)
  )
simPlot_xwScale_2
# ggsave("simPlot_xwScale_2.png", simPlot_xwScale_2, width=9, height=6)



