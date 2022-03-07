library(tidyverse)
library(grid)
library(splines)
theme_set(theme_bw())
theme_update(text = element_text(size=18))
theme_update(plot.title = element_text(hjust = 0.5))
output_folder = './job_output/'

### load data
input_file = "../data/TTO_dataset_510.csv"  
D <- read_csv(input_file) #%>% drop_na() 
D <- D %>% filter(YEAR == 2019) %>% filter(BQ>0 & BQ<1 & PQ>0 & PQ<1 )
logit <- function(p) { log(p/(1-p)) }
X <- as.matrix(D %>% mutate(lBQ=logit(BQ), lPQ=logit(PQ)) %>% select(lBQ, lPQ, HAND_MATCH, BAT_HOME_IND)) 
source("rstan8_main.R")

#########################
### HELPER FUNCTIONS ####
#########################

bsn_fit_to_posterior_probs <- function(S_test,X_test,fit) {
  draws=as.matrix(fit)
  alpha_draws = draws[,str_detect(colnames(draws), "^alpha")]
  eta_draws = draws[,str_detect(colnames(draws), "^eta")]
  
  linpreds = list()
  for (k in 1:7) {
    print(k)
    alpha_draws_k = alpha_draws[,endsWith(colnames(alpha_draws), paste0(k,"]"))]
    eta_draws_k = eta_draws[,endsWith(colnames(eta_draws), paste0(k,"]"))]
    linpred_k = S_test%*%t(alpha_draws_k) + X_test%*%t(eta_draws_k)
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

bsn_get_all_params <- function(fit) {
  draws=as.matrix(fit)
  alpha_draws = draws[,str_detect(colnames(draws), "^alpha")]
  eta_draws = draws[,str_detect(colnames(draws), "^eta")]
  all_params = list()
  for (k in 1:7) {
    # print(k)
    alpha_draws_k = alpha_draws[,endsWith(colnames(alpha_draws), paste0(k,"]"))]
    eta_draws_k = eta_draws[,endsWith(colnames(eta_draws), paste0(k,"]"))]
    all_params_k = cbind(alpha_draws_k, eta_draws_k)
    all_params[[length(all_params)+1]] = all_params_k
  }
  all_params
}

bsn_post_means_and_ci <- function(all_params) {
  pp_df = tibble()
  for (k in 1:7) {
    all_params_k = all_params[[k]]
    pplower_k = apply(all_params_k, 2, function(x) quantile(x,.025))
    ppmeans_k = colMeans(all_params_k)
    ppupper_k = apply(all_params_k, 2, function(x) quantile(x,.975))
    p_names = c(paste0("alpha",1:(dim(S)[2])),paste0("eta",1:(dim(X)[2])))
    pp_df_k = tibble(k=k,pplower=pplower_k,ppmean=ppmeans_k,ppupper=ppupper_k,
                     var=p_names)
    pp_df = bind_rows(pp_df, pp_df_k)
  }
  pp_df %>% arrange(-k)
}


bsn_get_avg_tto_effect_dfs <- function(fit) {
  draws=as.matrix(fit)
  alpha_draws = draws[,str_detect(colnames(draws), "^alpha")]
  a12_df = tibble()
  a23_df = tibble()
  for (k in 2:7) {
    #print(k)
    alpha_draws_k = alpha_draws[,endsWith(colnames(alpha_draws), paste0(k,"]"))][,1:27]
    a_tto1 = rowMeans(alpha_draws_k[,1:9])
    a_tto2 = rowMeans(alpha_draws_k[,10:18])
    a_tto3 = rowMeans(alpha_draws_k[,19:27])
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

bsn_get_BL_tto_effect_dfs <- function(fit) {
  draws=as.matrix(fit)
  alpha_draws = draws[,str_detect(colnames(draws), "^alpha")]
  a12_df = tibble()
  a23_df = tibble()
  for (k in 2:7) {
    #print(k)
    alpha_draws_k = alpha_draws[,endsWith(colnames(alpha_draws), paste0(k,"]"))][,1:27]
    a_tto12 = alpha_draws_k[,10] - alpha_draws_k[,9]
    a_tto23 = alpha_draws_k[,19] - alpha_draws_k[,18]
    q12 = quantile(a_tto12,c(.025,.975))
    q23 = quantile(a_tto23,c(.025,.975))
    a12_df_k = tibble(v=a_tto12, k=k) %>% filter(q12[1] <= v & v <= q12[2])
    a23_df_k = tibble(v=a_tto23, k=k) %>% filter(q23[1] <= v & v <= q23[2])
    a12_df = bind_rows(a12_df, a12_df_k)
    a23_df = bind_rows(a23_df, a23_df_k)
  }
  list(a12_df, a23_df)
}


bsn_xWoba_post <- function() {
  S1 = diag(36)
  X1 = matrix( c(logit(mean(D$BQ)),logit(mean(D$PQ)),1,1), nrow=36, ncol=4, byrow=TRUE)
  probs1 = bsn_fit_to_posterior_probs(S1,X1,fit)
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

get_prob_trend_df <- function(fit) {
  S1 = diag(36)
  X1 = matrix( c(logit(mean(D$BQ)),logit(mean(D$PQ)),1,1), nrow=36, ncol=4, byrow=TRUE)
  probs1 = bsn_fit_to_posterior_probs(S1,X1,fit)
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

###############
### RESULTS ###
###############

# posterior samples & y vector
fit <- readRDS("job_output/fit_rstan8-1.R.rds")
draws <- as.matrix(fit)

### posterior probabilities for each outcome
probs = bsn_fit_to_posterior_probs(S,X,fit)
# probs[[1]][1:1000]

### posterior means & CI's for all parameters
all_params = bsn_get_all_params(fit)
pp_df = bsn_post_means_and_ci(all_params)
probs_df = bsn_get_probs_means_and_ci(probs)

###############
### PLOTS ###
###############

## for each category, was a 2TTO & 3TTO avg. effect detected
bgated = bsn_get_avg_tto_effect_dfs(fit)
a12_df = bgated[[1]]
a23_df = bgated[[2]]
a12_df$k = factor(a12_df$k, labels = category_strings[2:7])
a23_df$k = factor(a23_df$k, labels = category_strings[2:7])

p12t = "magnitude of mean 2TTO effect"
#p12t = TeX("$\\frac{1}{9} \\sum_{m=10}^{18} \\alpha_m - \\frac{1}{9} \\sum_{m=1}^{9} \\alpha_m$")
p12 = plot_hists_by_category(a12_df, p12t)
p12
# ggsave("plots_bsn/plot_mean2TTOeffect.png", p12)

p23t = "magnitude of mean 3TTO effect"
p23 = plot_hists_by_category(a23_df, p23t)
p23
# ggsave("plots_bsn/plot_mean3TTOeffect.png", p23)

## for each category, was a 2TTO & 3TTO BL effect detected
bgbted = bsn_get_BL_tto_effect_dfs(fit)
b12_df = bgbted[[1]]
b23_df = bgbted[[2]]
b12_df$k = factor(b12_df$k, labels = category_strings[2:7])
b23_df$k = factor(b23_df$k, labels = category_strings[2:7])

pb12t = "magnitude of batter learning 2TTO effect"
pb12 = plot_hists_by_category(b12_df, pb12t)
pb12
# ggsave("plots_bsn/plot_BL_2TTOeffect.png", pb12)

pb23t = "magnitude of batter learning 3TTO effect"
pb23 = plot_hists_by_category(b23_df, pb23t)
pb23
# ggsave("plots_bsn/plot_BL_3TTOeffect.png", pb23)

### plot trend in expected wOBA over the course of a game
xw = bsn_xWoba_post()
A = get_tto_means_and_ci(xw)
pxw = plot_xWOBA_over_time(A)
pxw
# ggsave("plots_bsn/plot_xwoba19.png", pxw)

### plot trend in expected wOBA **SPLINE** over the course of a game
# repeating a knot 4 times means the spline itself is discontinuous at that knot
knots = c(9,9,9,9,18,18,18,18)
spline_lower <- lm(lower ~ bs(1:27, knots = knots), data = A )
spline_avg <- lm(avg ~ bs(1:27, knots = knots), data = A )
spline_upper <- lm(upper ~ bs(1:27, knots = knots), data = A )
spline_A = as_tibble(data.frame(lower=fitted(spline_lower),avg=fitted(spline_avg),
                                upper=fitted(spline_upper),bn=1:27))
pxws = plot_xWOBA_over_time_spline(spline_A)
pxws
# ggsave("plots_bsn/plot_xwoba19_spline.png", pxws)



### plot trend over the course of a game for each outcome, on probability scale (spline ???)
prob_trend_df = get_prob_trend_df(fit)
prob_trend_df1 = prob_trend_df %>% filter(k!=1 & bn<=27)
prob_trend_df1$k = factor(prob_trend_df1$k, labels = category_strings[2:7])

plot_prob_trend_by_k(prob_trend_df1)

plot_prob_trend_by_k(prob_trend_df1 %>% filter(k %in% c("BB","2B","HR")))

plot_prob_trend_by_k(prob_trend_df1 %>% filter(k %in% c("1B")))

plot_prob_trend_by_k(prob_trend_df1 %>% filter(k %in% c("HBP","3B")))



#########################
### PITCH COUNT ####
#########################

