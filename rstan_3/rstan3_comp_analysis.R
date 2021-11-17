
source("rstan3_comp_getData.R")
library(latex2exp)

test_tib_bsn = tibble(y=numeric(0),pplower=numeric(0),ppmean=numeric(0),ppupper=numeric(0))
test_tib_ubi = tibble(y=numeric(0),pplower=numeric(0),ppmean=numeric(0),ppupper=numeric(0))

for (fold_num in 1:10) {
  # testing data matrices
  test_rows = which(folds == fold_num)
  X_test = X[test_rows,]
  S_test = S[test_rows,]
  U_test = U[test_rows,]
  O_test = O[test_rows,]
  y_test = tibble(y=y[test_rows,])
  
  # BSN model fit
  fitB <- readRDS(paste0("./job_output/fit_rstan3_comp_bsn-",fold_num,".R.rds")) 
  alpha_fit = summary(fitB)$summary[2:(dim(S)[2]+1),c(4,1,8)]
  #plot_bsn(alpha_fit) # check
  eta_fit = summary(fitB)$summary[(dim(fitB)[3]-4):(dim(fitB)[3]-1),c(4,1,8)]
  colnames(alpha_fit) = c("pplower", "ppmean", "ppupper")
  colnames(eta_fit) = c("pplower", "ppmean", "ppupper")
  post_predB = S_test%*%alpha_fit + X_test%*%eta_fit
  df_bsn = bind_cols(y_test, as_tibble(post_predB))
  test_tib_bsn = bind_rows(test_tib_bsn, df_bsn)
  
  # UBI model fit
  fitU <- readRDS(paste0("./job_output/fit_rstan3_comp_ubi-",fold_num,".R.rds"))
  beta_fit = summary(fitU)$summary[2:(dim(U)[2]+1),c(4,1,8)]
  gamma_fit = summary(fitU)$summary[(dim(U)[2]+2):(dim(U)[2]+dim(O)[2]+1),c(4,1,8)]
  #plot_ubi(beta_fit, gamma_fit) # check
  delta_fit = summary(fitU)$summary[(dim(fitU)[3]-4):(dim(fitU)[3]-1),c(4,1,8)]
  colnames(beta_fit) = c("pplower", "ppmean", "ppupper")
  colnames(gamma_fit) = c("pplower", "ppmean", "ppupper")
  colnames(delta_fit) = c("pplower", "ppmean", "ppupper")
  post_predU = U_test%*%beta_fit + O_test%*%gamma_fit + X_test%*%delta_fit
  df_ubi = bind_cols(y_test, as_tibble(post_predU))
  test_tib_ubi = bind_rows(test_tib_ubi, df_ubi)
}

########################################
########### MODEL COMPARISON ###########
########################################

# rmse of posterior predictive mean of y
rmse <- function(d) {
  sqrt(sum((d$y - d$ppmean)^2)/nrow(d))
}
rmse(test_tib_bsn)
rmse(test_tib_ubi)

# coverage of posterior predictive intervals of y
covg <- function(d) {
  sum(d$pplower <= d$y & d$y <= d$ppupper)/nrow(d)
}
covg(test_tib_bsn)
covg(test_tib_ubi)

# ratio of lengths of posterior predictive intervals of y
lengths_bsn = test_tib_bsn$ppupper - test_tib_bsn$pplower
lengths_ubi = test_tib_ubi$ppupper - test_tib_ubi$pplower
length_ratios = lengths_ubi/lengths_bsn
mean(length_ratios)
sd(length_ratios)
as_tibble(length_ratios) %>% ggplot(aes(x=value)) + 
  geom_histogram(bins=80) + 
  #geom_vline(xintercept = mean(length_ratios), color="firebrick") +
  xlim(c(0,150)) + 
  xlab("ratio of UBI length to BSN length")

#############################
########### PLOTS ########### for sanity checks
#############################

plot_bsn <- function(A) {
  colnames(A) = c("lower","avg","upper")
  A = as_tibble(A[1:27,])
  A$bn = 1:27
  # PRODUCTION PLOT
  theme_update(plot.title = element_text(hjust = 0.5))
  production_plot = A %>% 
    ggplot(aes(x=bn, y=avg)) +
    geom_errorbar(aes(ymin = lower, ymax = upper), fill = "black", width = .4) +
    geom_point(color="dodgerblue2", shape=21, size=2, fill="white") + 
    geom_vline(aes(xintercept = 9.5), size=1.2) +
    geom_vline(aes(xintercept = 18.5), size=1.2) +
    #labs(title = "Pitcher Effectiveness") +
    labs(title = TeX("Posterior distribution of $\\alpha$")) + 
    theme(legend.position="none") +
    scale_x_continuous(name=TeX("Batter sequence number $k$"),
                       limits = c(0,28),
                       breaks = c(0,5,10,15,20,25)) +
    scale_y_continuous(name=TeX("$\\alpha_k$"),
                       #limits = c(-.02, .03),
                       breaks = seq(-.09, .09, .005)
    ) 
  production_plot
}
#plot_bsn(alpha_fit)


plot_ubi <- function(beta_fit,gamma_fit) {
  B = do.call(rbind, replicate(3, beta_fit[1:9,], simplify=FALSE))
  G = rbind(
    do.call(rbind, replicate(9, gamma_fit[1,], simplify=FALSE)),
    do.call(rbind, replicate(9, gamma_fit[2,], simplify=FALSE)),
    do.call(rbind, replicate(9, gamma_fit[3,], simplify=FALSE))
  )
  A = B+G
  rownames(A) = NULL
  colnames(A) = c("lower","avg","upper")
  A = as_tibble(A)
  A$bn = 1:27
  # PRODUCTION PLOT
  theme_update(plot.title = element_text(hjust = 0.5))
  XLABS = c("", paste0("(",1,",",1:9,")"), paste0("(",2,",",1:9,")"), paste0("(",3,",",1:9,")"))
  BREAKS = seq(1,28,by=2)#c(1,6,11,16,21,26)#c(0,5,10,15,20,25)
  production_plot = A %>% 
    ggplot(aes(x=bn, y=avg)) +
    geom_errorbar(aes(ymin = lower, ymax = upper), fill = "black", width = .4) +
    geom_point(color="dodgerblue2", shape=21, size=2, fill="white") + 
    geom_vline(aes(xintercept = 9.5), size=1.2) +
    geom_vline(aes(xintercept = 18.5), size=1.2) +
    #labs(title = "Pitcher Effectiveness") +
    labs(title = TeX("Posterior distribution of $\\beta +\\gamma$")) + 
    theme(legend.position="none") +
    scale_x_continuous(name=TeX("(order Count $l$, unique batter index $k$)"),
                       limits = c(0,28),
                       breaks = BREAKS,
                       labels =  XLABS[BREAKS+1]) +
    scale_y_continuous(name=TeX("$\\beta_{k} + \\gamma_{l}$"),
                       #limits = c(-.02, .03),
                       #breaks = seq(-.09, .09, .005)
    ) 
  production_plot
}
#plot_ubi(beta_fit,gamma_fit)

