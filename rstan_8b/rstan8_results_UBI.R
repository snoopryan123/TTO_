library(tidyverse)
output_folder = './job_output/'

### load data
input_file = "../../data/TTO_dataset_510.csv"  
D <- read_csv(input_file) #%>% drop_na() 
D <- D %>% filter(YEAR == 2019) %>% filter(BQ>0 & BQ<1 & PQ>0 & PQ<1 )
logit <- function(p) { log(p/(1-p)) }
X <- as.matrix(D %>% mutate(lBQ=logit(BQ), lPQ=logit(PQ)) %>% select(lBQ, lPQ, HAND_MATCH, BAT_HOME_IND)) 
source("rstan8_main.R")

#########################
### HELPER FUNCTIONS ####
#########################

ubi_fit_to_posterior_probs <- function(U_test,O_test,X_test,fit) {
  draws=as.matrix(fit)
  beta_draws = draws[,str_detect(colnames(draws), "^beta")]
  gamma_draws = draws[,str_detect(colnames(draws), "^gamma")]
  eta_draws = draws[,str_detect(colnames(draws), "^eta")]
  
  linpreds = list()
  for (k in 1:7) {
    print(k)
    beta_draws_k = beta_draws[,endsWith(colnames(beta_draws), paste0(k,"]"))]
    gamma_draws_k = gamma_draws[,endsWith(colnames(gamma_draws), paste0(k,"]"))]
    eta_draws_k = eta_draws[,endsWith(colnames(eta_draws), paste0(k,"]"))]
    linpred_k = U_test%*%t(beta_draws_k) + O_test%*%t(gamma_draws_k) + X_test%*%t(eta_draws_k)
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

ubi_get_all_params <- function(fit) {
  draws=as.matrix(fit)
  beta_draws = draws[,str_detect(colnames(draws), "^beta")]
  gamma_draws = draws[,str_detect(colnames(draws), "^gamma")]
  eta_draws = draws[,str_detect(colnames(draws), "^eta")]
  all_params = list()
  for (k in 1:7) {
    # print(k)
    beta_draws_k = beta_draws[,endsWith(colnames(beta_draws), paste0(k,"]"))]
    gamma_draws_k = gamma_draws[,endsWith(colnames(gamma_draws), paste0(k,"]"))]
    eta_draws_k = eta_draws[,endsWith(colnames(eta_draws), paste0(k,"]"))]
    all_params_k = cbind(beta_draws_k, gamma_draws_k, eta_draws_k)
    all_params[[length(all_params)+1]] = all_params_k
  }
  all_params
}

ubi_post_means_and_ci <- function(all_params) {
  params_true = cbind(beta,gamma,eta)
  pp_df = tibble()
  for (k in 1:7) {
    all_params_k = all_params[[k]]
    pplower_k = apply(all_params_k, 2, function(x) quantile(x,.025))
    ppmeans_k = colMeans(all_params_k)
    ppupper_k = apply(all_params_k, 2, function(x) quantile(x,.975))
    p_true_k = params_true[k,]
    p_names = c(paste0("beta",1:p_u),paste0("gamma",1:p_o),paste0("eta",1:p_x))
    pp_df_k = tibble(k=k,pplower=pplower_k,ppmean=ppmeans_k,ppupper=ppupper_k,
                     var=p_names,param_true = p_true_k)
    pp_df = bind_rows(pp_df, pp_df_k)
  }
  pp_df %>% arrange(-k) %>% mutate(covered = pplower <= param_true & param_true <= ppupper,
                                   pplength = ppupper - pplower)
}

ubi_beta_plus_gamma_draws <- function(fit) {
  draws=as.matrix(fit)
  beta_draws = draws[,str_detect(colnames(draws), "^beta")]
  gamma_draws = draws[,str_detect(colnames(draws), "^gamma")]
  beta_plus_gamma_draws = list()
  for (k in 2:7) {
    # print(k)
    beta_draws_k = beta_draws[,endsWith(colnames(beta_draws), paste0(k,"]"))][,1:9]
    gamma_draws_k = gamma_draws[,endsWith(colnames(gamma_draws), paste0(k,"]"))][,1:3]
    bk = cbind(beta_draws_k,beta_draws_k,beta_draws_k)
    gk = cbind(matrix(gamma_draws_k[,1], nrow(gamma_draws_k), 9),
               matrix(gamma_draws_k[,2], nrow(gamma_draws_k), 9),
               matrix(gamma_draws_k[,3], nrow(gamma_draws_k), 9))
    bgk = bk + gk
    beta_plus_gamma_draws[[length(beta_plus_gamma_draws)+1]] = bgk
  }
  beta_plus_gamma_draws
}



ubi_get_avg_tto_effect_dfs <- function(tto_draws) {
  a12_df = tibble()
  a23_df = tibble()
  for (k in 2:7) {
    #print(k)
    tto_draws_k = tto_draws[[k-1]]
    a_tto1 = rowMeans(tto_draws_k[,1:9])
    a_tto2 = rowMeans(tto_draws_k[,10:18])
    a_tto3 = rowMeans(tto_draws_k[,19:27])
    a12 = a_tto2 - a_tto1
    a23 = a_tto3 - a_tto2
    q12 = quantile(a12, c(.025,.975))
    q23 = quantile(a23, c(.025,.975))
    a12_df_k = tibble(v=a12, k=k) %>% filter(q12[1] <= v & v <= q12[2])
    a23_df_k = tibble(v=a23, k=k) %>% filter(q23[1] <= v & v <= q23[2])
    a12_df = bind_rows(a12_df, a12_df_k)
    a23_df = bind_rows(a23_df, a23_df_k)
  }
  list(a12_df, a23_df)
}

ubi_get_BL_tto_effect_dfs <- function(tto_draws) {
  a12_df = tibble()
  a23_df = tibble()
  for (k in 2:7) {
    #print(k)
    tto_draws_k = tto_draws[[k-1]]
    a_tto12 = tto_draws_k[,10] - tto_draws_k[,9]
    a_tto23 = tto_draws_k[,19] - tto_draws_k[,18]
    q12 = quantile(a_tto12,c(.025,.975))
    q23 = quantile(a_tto23,c(.025,.975))
    a12_df_k = tibble(v=a_tto12, k=k) %>% filter(q12[1] <= v & v <= q12[2])
    a23_df_k = tibble(v=a_tto23, k=k) %>% filter(q23[1] <= v & v <= q23[2])
    a12_df = bind_rows(a12_df, a12_df_k)
    a23_df = bind_rows(a23_df, a23_df_k)
  }
  list(a12_df, a23_df)
}


ubi_xWoba_post <- function() {
  U1_ = diag(9)
  U1 = cbind(rbind(U1_,U1_,U1_,U1_), matrix(0, nrow=36, ncol = p_u-9))
  O1 = matrix(0, nrow=36, ncol=p_o)
  for (i in 1:9) {O1[i,1] = 1}
  for (i in 10:18) {O1[i,2] = 1}
  for (i in 19:27) {O1[i,3] = 1}
  for (i in 28:36) {O1[i,4] = 1}
  X1 = matrix( c(logit(mean(D$BQ)),logit(mean(D$PQ)),1,1), nrow=36, ncol=4, byrow=TRUE)
  probs1 = ubi_fit_to_posterior_probs(U1,O1,X1,fit)
  # probs1[[1]][1:10,1:10]
  xw = matrix(0, ncol = dim(probs1[[1]])[1], nrow = dim(probs1[[1]])[2])
  for (k in 1:7) {
    xw = xw + t(probs1[[k]]) * categories[k]
  }
  xw
}

get_tto_means_and_ci <- function(xw) {
  # compute mean and 2.5%, 97.5% quantiles of posterior samples
  p = dim(xw)[2] #27 
  lower <- numeric(p)
  avg <- numeric(p)
  upper <- numeric(p)
  for (i in 1:p) {
    x = xw[,i]
    lower[i] = quantile(x,.025)
    avg[i] = mean(x)
    upper[i] = quantile(x,.975)
  }
  A = (data.frame(lower = lower,avg = avg,upper= upper,bn = 1:p))[1:27,]
  A
}

plot_xWOBA_over_time <- function(df) {
  pxw = df %>% 
    ggplot(aes(x=bn, y=avg)) +
    geom_errorbar(aes(ymin = lower, ymax = upper), fill = "black", width = .4) +
    geom_point(color="dodgerblue2", shape=21, size=2, fill="white") + 
    # geom_line(aes(y = c(avg[1:9], rep(NA,18))), color="firebrick", size=1) +
    # geom_line(aes(y = c(rep(NA,9), avg[10:18], rep(NA,9))), color="firebrick", size=1) +
    # geom_line(aes(y = c(rep(NA,18), avg[19:27])), color="firebrick", size=1) +
    geom_vline(aes(xintercept = 9.5), size=1.2) +
    geom_vline(aes(xintercept = 18.5), size=1.2) +
    labs(title = "Trend in Expected wOBA over the Course of a Game") + 
    theme(legend.position="none") +
    scale_x_continuous(name=TeX("Batter Sequence Number $m$"), 
                       limits = c(0,28),
                       breaks = c(0,5,10,15,20,25)) +
    scale_y_continuous(name="Expected wOBA", 
                       # limits = c(.2, .4),
                       breaks = seq(-1, 1, .05)
    ) 
  pxw
}

plot_xWOBA_over_time_spline <- function(df) {
  pxw = df %>% 
    ggplot(aes(x=bn, y=avg)) +
    geom_errorbar(aes(ymin = lower, ymax = upper), fill = "black", width = .4) +
    geom_point(color="dodgerblue2", shape=21, size=2, fill="white") + 
    # geom_line(aes(y = c(avg[1:9], rep(NA,18))), color="firebrick", size=1) +
    # geom_line(aes(y = c(rep(NA,9), avg[10:18], rep(NA,9))), color="firebrick", size=1) +
    # geom_line(aes(y = c(rep(NA,18), avg[19:27])), color="firebrick", size=1) +
    geom_vline(aes(xintercept = 9.5), size=1.2) +
    geom_vline(aes(xintercept = 18.5), size=1.2) +
    labs(title = "Trend in Expected wOBA over the Course of a Game: Spline") + 
    theme(legend.position="none") +
    scale_x_continuous(name=TeX("Batter Sequence Number $m$"), 
                       limits = c(0,28),
                       breaks = c(0,5,10,15,20,25)) +
    scale_y_continuous(name="Expected wOBA", 
                       # limits = c(.2, .4),
                       breaks = seq(-1, 1, .05)
    ) 
  pxw
}



plot_prob_trend_by_k <- function(dfk) {
  pxwk = dfk %>%
    ggplot(aes(x=bn, y=pmean)) +
    facet_wrap(~k) +
    geom_errorbar(aes(ymin = plower, ymax = pupper), fill = "black", width = .4) +
    geom_point(color="dodgerblue2", shape=21, size=2, fill="white") + 
    # geom_line(aes(y = c(avg[1:9], rep(NA,18))), color="firebrick", size=1) +
    # geom_line(aes(y = c(rep(NA,9), avg[10:18], rep(NA,9))), color="firebrick", size=1) +
    # geom_line(aes(y = c(rep(NA,18), avg[19:27])), color="firebrick", size=1) +
    geom_vline(aes(xintercept = 9.5), size=1.2) +
    geom_vline(aes(xintercept = 18.5), size=1.2) +
    labs(title = "Trend in the Probability of Each Outcome over the Course of a Game") + 
    theme(legend.position="none") +
    scale_x_continuous(name=TeX("Batter Sequence Number $m$"), 
                       limits = c(0,28),
                       breaks = c(0,5,10,15,20,25)) +
    scale_y_continuous(name="Probability", 
                       # limits = c(.2, .4),
                       breaks = seq(-1, 1, .05)
    ) 
  pxwk
}

plot_hists_by_category <- function(df, xTitle) {
  df %>% ggplot() +
    facet_wrap(~k) +
    geom_histogram(aes(x=v, y=..density..), color="white",fill="dodgerblue2",bins=50) +
    geom_vline(xintercept = 0) +
    theme(panel.spacing = unit(2, "lines")) +
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank()) +
    xlab(xTitle)
}

get_prob_trend_df <- function() {
  U1_ = diag(9)
  U1 = cbind(rbind(U1_,U1_,U1_,U1_), matrix(0, nrow=36, ncol = p_u-9))
  O1 = matrix(0, nrow=36, ncol=p_o)
  for (i in 1:9) {O1[i,1] = 1}
  for (i in 10:18) {O1[i,2] = 1}
  for (i in 19:27) {O1[i,3] = 1}
  for (i in 28:36) {O1[i,4] = 1}
  X1 = matrix( c(logit(mean(D$BQ)),logit(mean(D$PQ)),1,1), nrow=36, ncol=4, byrow=TRUE)
  probs1 = ubi_fit_to_posterior_probs(U1,O1,X1,fit)
  # probs1[[1]][1:10,1:10]
  pp1_df = tibble()
  for (k in 1:7) {
    probs1_k = t(probs1[[k]])
    plower_k = apply(probs1_k, 2, function(x) quantile(x,.025))
    pmeans_k = colMeans(probs1_k)
    pupper_k = apply(probs1_k, 2, function(x) quantile(x,.975))
    # p_names = c(paste0("alpha",1:(dim(S)[2])),paste0("eta",1:(dim(X)[2])))
    pp1_df_k = tibble(k=k,plower=plower_k,pmean=pmeans_k,pupper=pupper_k,bn=1:36)
    pp1_df = bind_rows(pp1_df, pp1_df_k)
  }
  pp1_df %>% arrange(-k)
}




ubi_tto_post_means_and_ci <- function(beta_plus_gamma_draws) {
  b_true = cbind(beta[2:7,1:9],beta[2:7,1:9],beta[2:7,1:9])
  g_true = cbind(matrix(gamma[2:7,1], nrow(gamma)-1, 9),
                 matrix(gamma[2:7,2], nrow(gamma)-1, 9),
                 matrix(gamma[2:7,3], nrow(gamma)-1, 9))
  params_true = b_true + g_true
  
  bg_df = tibble()
  for (k in 2:7) {
    bgk = beta_plus_gamma_draws[[k-1]]
    pplower_k = apply(bgk, 2, function(x) quantile(x,.025))
    ppmeans_k = colMeans(bgk)
    ppupper_k = apply(bgk, 2, function(x) quantile(x,.975))
    p_true_k = params_true[k-1,]
    p_names = paste0("bg",1:27)
    bg_df_k = tibble(k=k,pplower=pplower_k,ppmean=ppmeans_k,ppupper=ppupper_k,
                     var=p_names,param_true = p_true_k)
    bg_df = bind_rows(bg_df, bg_df_k)
  }
  bg_df %>% arrange(-k) %>% mutate(covered = pplower <= param_true & param_true <= ppupper,
                                   pplength = ppupper - pplower)
}

ubi_detect_avg_tto_effect <- function(fit) {
  draws=as.matrix(fit)
  gamma_draws = draws[,str_detect(colnames(draws), "^gamma")]
  tto12_avg_effect_detected = numeric(6)
  tto23_avg_effect_detected = numeric(6)
  for (k in 2:7) {
    # print(k)
    gamma_draws_k = gamma_draws[,endsWith(colnames(gamma_draws), paste0(k,"]"))][,1:3]
    g21 = gamma_draws_k[,2] - gamma_draws_k[,1]
    g32 = gamma_draws_k[,3] - gamma_draws_k[,2]
    g21lower = quantile(g21,.025)
    g32lower = quantile(g32,.025)
    tto12_avg_effect_detected[k-1] = g21lower > 0
    tto23_avg_effect_detected[k-1] = g32lower > 0
  }
  list(tto12_avg_effect_detected=tto12_avg_effect_detected,
       tto23_avg_effect_detected=tto23_avg_effect_detected)
}

ubi_detect_BL_tto_effect <- function(beta_plus_gamma_draws) {
  tto12_BL_effect_detected = numeric(6)
  tto23_BL_effect_detected = numeric(6)
  for (k in 2:7) {
    # print(k)
    bgk = beta_plus_gamma_draws[[k-1]]
    bgk21lower = quantile(bgk[,10] - bgk[,9],.025)
    bgk32lower = quantile(bgk[,19] - bgk[,18],.025)
    tto12_BL_effect_detected[k-1] = bgk21lower > 0
    tto23_BL_effect_detected[k-1] = bgk32lower > 0
  }
  list(tto12_BL_effect_detected=tto12_BL_effect_detected,
       tto23_BL_effect_detected=tto23_BL_effect_detected)
}

# p = plot_ubi0(fit)
# p
# ggsave(paste0("./plot_",OUTPUT_FILE,".png"), p)




###############
### RESULTS ###
###############

# posterior samples & y vector
fit <- readRDS("job_output/fit_rstan8-2.R.rds")
draws <- as.matrix(fit)
beta_draws = draws[,str_detect(colnames(draws), "^beta")]
gamma_draws = draws[,str_detect(colnames(draws), "^gamma")]
p_u = dim(beta_draws)[2]/7
p_o = dim(gamma_draws)[2]/7


### posterior probabilities for each outcome
# probs = ubi_fit_to_posterior_probs(U,O,X,fit)
# probs[[1]][1:1000]

### posterior means & CI's for all parameters
# all_params = ubi_get_all_params(fit)
# pp_df = ubi_post_means_and_ci(all_params)
# probs_df = bsn_get_probs_means_and_ci(probs)

###############
### PLOTS ###
###############

### 1-27 TTO draws
tto_draws = ubi_beta_plus_gamma_draws(fit)

## for each category, was a 2TTO & 3TTO avg. effect detected
ugated = ubi_get_avg_tto_effect_dfs(tto_draws)
t12_df = ugated[[1]]
t23_df = ugated[[2]]
t12_df$k = factor(t12_df$k, labels = category_strings[2:7])
t23_df$k = factor(t23_df$k, labels = category_strings[2:7])

t12t = "magnitude of mean 2TTO effect"
#p12t = TeX("$\\frac{1}{9} \\sum_{m=10}^{18} \\alpha_m - \\frac{1}{9} \\sum_{m=1}^{9} \\alpha_m$")
t12 = plot_hists_by_category(t12_df, t12t)
t12
# ggsave("plots_ubi/plot_mean2TTOeffect.png", t12)

t23t = "magnitude of mean 3TTO effect"
t23 = plot_hists_by_category(t23_df, t23t)
t23
# ggsave("plots_ubi/plot_mean3TTOeffect.png", t23)

## for each category, was a 2TTO & 3TTO BL effect detected
ugbted = ubi_get_BL_tto_effect_dfs(tto_draws)
b12_df = ugbted[[1]]
b23_df = ugbted[[2]]
b12_df$k = factor(b12_df$k, labels = category_strings[2:7])
b23_df$k = factor(b23_df$k, labels = category_strings[2:7])

tpb12t = "magnitude of batter learning 2TTO effect"
tpb12 = plot_hists_by_category(b12_df, tpb12t)
tpb12
# ggsave("plots_ubi/plot_BL_2TTOeffect.png", tpb12)

tpb23t = "magnitude of batter learning 3TTO effect"
tpb23 = plot_hists_by_category(b23_df, tpb23t)
tpb23
# ggsave("plots_ubi/plot_BL_3TTOeffect.png", tpb23)

### plot trend in expected wOBA over the course of a game
xw = ubi_xWoba_post()
A = get_tto_means_and_ci(xw)
pxw = plot_xWOBA_over_time(A)
pxw
# ggsave("plots_ubi/plot_xwoba19.png", pxw)

### plot trend in expected wOBA **SPLINE** over the course of a game
# repeating a knot 4 times means the spline itself is discontinuous at that knot
knots = c(9.5,9.5,9.5,9.5,  18.5,18.5,18.5,18.5)
spline_lower <- lm(lower ~ bs(1:27, knots = knots), data = A )
spline_avg <- lm(avg ~ bs(1:27, knots = knots), data = A )
spline_upper <- lm(upper ~ bs(1:27, knots = knots), data = A )
spline_A = as_tibble(data.frame(lower=fitted(spline_lower),avg=fitted(spline_avg),
                                upper=fitted(spline_upper),bn=1:27))
pxws = plot_xWOBA_over_time_spline(spline_A)
pxws
# ggsave("plots_ubi/plot_xwoba19_spline.png", pxws)



### plot trend over the course of a game for each outcome, on probability scale (spline ???)
prob_trend_df = get_prob_trend_df()
prob_trend_df1 = prob_trend_df %>% filter(k!=1 & bn<=27)
prob_trend_df1$k = factor(prob_trend_df1$k, labels = category_strings[2:7])

plot_prob_trend_by_k(prob_trend_df1)

plot_prob_trend_by_k(prob_trend_df1 %>% filter(k %in% c("BB","2B","HR")))

plot_prob_trend_by_k(prob_trend_df1 %>% filter(k %in% c("1B")))

plot_prob_trend_by_k(prob_trend_df1 %>% filter(k %in% c("HBP","3B")))




