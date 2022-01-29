###############
#### SETUP ####
###############

IS_SIM = TRUE
source("../rstan5_main.R")

#####################################
########### GENERATE DATA ###########
#####################################

# helpful constants
N = dim(X)[1]
P = dim(X)[2]
p_s = dim(S)[2] 
p_u = dim(U)[2] 
B = dim(BATTER_SEQ_dummies)[2] 
BB = 27

### GENERATE PARAMETERS
# all pitchers have the same constant effects
b = -.0345 #-.007
m = .001 #0.0005 #.001
t_2 = 0.035 #.0191 #.01
t_3 = 0.035 #.0172 #.02
t_4 = 0 #.03
k = 1:B
alpha = b + m*k + t_2*(k>=10) + t_3*(k>=19) + t_4*(k>=28) # plot(1:27, alpha[1:27])
eta = c(.09, .07, -.02, .01)
sigma = 0.5 #.125
# UBI simulated params
beta = b + m*(1:p_u)
gamma = c(0, t_2 + 9*m, t_2 + t_3 + 18*m, t_2 + t_3 + t_4 + 27*m)
# check
m1_tto = (rep(beta[1:9],4) + c(rep(gamma[1],9), rep(gamma[2],9), rep(gamma[3],9), rep(gamma[4],9)))[1:p_s]
alpha
m1_tto
sum(m1_tto-alpha)


###############################
########### EXPLORE ###########
###############################

# generated y example
set.seed(1) #FIXME 
epsilon = rnorm(N, mean=0, sd=sigma)
y_generated = S%*%alpha + X%*%eta + epsilon 
### KNOW mean(y_obs) = 0 and sd(y_obs) = 0.5
### WANT mean(y) =~ 0    and sd(y) =~ 0.5
mean(y_generated)
sd(y_generated)




# # observed y
# y_obs = D$std_EVENT_WOBA_19 ##D$EVENT_WOBA_19
# mean(y_obs)
# sd(y_obs)


# # find TTO sizes
# mu_y = mean(D$EVENT_WOBA_19)
# sd_y = sd(D$EVENT_WOBA_19)
# D %>% filter(ORDER_CT <= 3) %>%
#   group_by(ORDER_CT) %>% 
#   summarise(avg_event_std_wOBA_19 = mean(std_EVENT_WOBA_19),
#             avg_event_wOBA_19 =  mean(EVENT_WOBA_19))
# 
# (c(.340, .350, .359) - mu_y)/(2*sd_y)
# diff( (c(.340, .350, .359) - mu_y)/(2*sd_y) )


#################################################
########### plotting helper functions ###########
#################################################

# plot posterior distribution of alpha
plot_alpha_post <- function(alpha_post) {
  #alpha_post = transform_back(alpha_post)
  lower <- numeric(BB)
  avg <- numeric(BB)
  upper <- numeric(BB)
  for (j in 1:BB) {
    lower[j] = quantile(alpha_post[,j],.025)
    avg[j] = mean(alpha_post[,j])
    upper[j] = quantile(alpha_post[,j],.975)
  }
  AAA = data.frame(lower = lower,avg = avg,upper= upper,bn = 1:BB)
  df_alpha_true = tibble(bn=1:BB,y=alpha[1:BB])
  alpha_plot = AAA %>%
    ggplot(aes(x=bn, y=avg)) +
    geom_errorbar(aes(x=bn, y=avg, ymin = lower, ymax = upper), fill = "black", width = .4) +
    geom_point(color="dodgerblue2", shape=21, size=2, fill="white") +
    geom_point(aes(x=bn,y=df_alpha_true$y), color="firebrick") +
    geom_vline(aes(xintercept = 9.5), size=1.2) +
    geom_vline(aes(xintercept = 18.5), size=1.2) +
    labs(title = TeX("Posterior distribution of $\\alpha$")) +
    scale_x_continuous(name=TeX("Batter sequence number $k$"),limits = c(0,27.5), breaks = c(0,5,10,15,20,25)) +
    scale_y_continuous(
      name=TeX("$\\alpha_k$"),
      #limits=c(-0.02,0.065), 
      breaks=seq(-0.03,0.09,by=.01)
    ) +
    theme(legend.position="none") 
  alpha_plot
}

# plot posterior distribution of beta + gamma
plot_beta_plus_gamma_post <- function(beta_post, gamma_post) {
  # compute mean and 2.5%, 97.5% quantiles of posterior samples
  p = 27 #dim(BATTER_SEQ_dummies)[2]
  bidx <- paste0("beta[",1:9,"]")
  oc <- paste0("gamma[",1:3,"]")
  lower <- numeric(p)
  avg <- numeric(p)
  upper <- numeric(p)
  for (i in 1:length(oc)) {
    o = oc[i]
    x0 = gamma_post[,o] #transform_back(gamma_post[,o])
    for (j in 1:length(bidx)) {
      b = bidx[j]
      xb = beta_post[,b] #transform_back(beta_post[,b])
      x = x0 + xb
      lower[(i-1)*length(bidx) + j] = quantile(x,.025)
      avg[(i-1)*length(bidx) + j] = mean(x)
      upper[(i-1)*length(bidx) + j] = quantile(x,.975)
    }
  }
  
  # plot
  A4 = data.frame(
    lower = lower,
    avg = avg,
    upper= upper,
    bn = 1:p
  )
  
  XLABS = c("", paste0("(",1,",",1:9,")"), paste0("(",2,",",1:9,")"), paste0("(",3,",",1:9,")"))
  BREAKS = seq(1,28,by=2)#c(1,6,11,16,21,26)#c(0,5,10,15,20,25)
  
  # PRODUCTION PLOT
  theme_update(plot.title = element_text(hjust = 0.5))
  production_plot = A4 %>% 
    ggplot(aes(x=bn, y=avg)) +
    geom_errorbar(aes(ymin = lower, ymax = upper), fill = "black", width = .4) +
    geom_point(color="dodgerblue2", shape=21, size=2, fill="white") + 
    geom_vline(aes(xintercept = 9.5), size=1.2) +
    geom_vline(aes(xintercept = 18.5), size=1.2) +
    labs(title = TeX("Posterior distribution of $\\beta +\\gamma$")) + 
    theme(legend.position="none") +
    scale_x_continuous(name=TeX("(order Count $l$, unique batter index $k$)"), 
                       limits = c(0,28),
                       breaks = BREAKS,
                       labels =  XLABS[BREAKS+1]) +
    scale_y_continuous(name=TeX("$\\beta_{k} + \\gamma_{l}$"), 
                       #limits = c(-.015, .03),
                       breaks = seq(-.03, .03, .005)
    ) 
  production_plot
}

# plot posterior distribution of eta
plot_eta_post <- function(eta_post) {
  ETA.NAMES = c(
    TeX("$\\eta_{batWoba}$"), TeX("$\\eta_{pitWoba}$"), 
      TeX("$\\eta_{hand}$"), TeX("$\\eta_{home}$")
  )
  #ETA.NAMES = paste0("eta",1:length(eta))
  eta_df = data.frame(eta_post)
  names(eta_df) = ETA.NAMES
  eta_mean_df = as_tibble(t(as.matrix(eta)))
  names(eta_mean_df) = ETA.NAMES
  eta_plot = ggplot() + 
    geom_histogram(data=gather(eta_df), aes(value), bins = 20, fill = "grey") + 
    geom_vline(data=gather(eta_mean_df), aes(xintercept=value), color="firebrick") + 
    facet_wrap(~key, scales = 'free_x', labeller = label_parsed) +  #label_parsed
    theme(legend.position="none") +
    labs(title=TeX("Posterior distribution of $\\eta$")) +
    theme(panel.spacing.x = unit(6, "mm")) +
    xlab(TeX("$\\eta$")) +
    ylab(TeX("density")) + 
    scale_y_discrete(breaks=NULL)
  eta_plot
}




