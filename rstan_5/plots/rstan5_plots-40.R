library(tidyverse)

#FIXME
OUTPUT_FILE = "rstan5_plots-40.R"
nums = c(40:48,5)
yrs = 2010:2019

### plot 9 year stack


{
  R = tibble()
  for (iii in 1:length(yrs)) {
    print(yrs[iii])
    fit <- readRDS(paste0("../job_output/fit_rstan5-",nums[iii],".R.rds"))
    draws <- as_tibble(as.matrix(fit))
    
    # compute mean and 2.5%, 97.5% quantiles of posterior samples
    p = 27 #dim(BATTER_SEQ_dummies)[2]
    bidx <- paste0("beta[",1:9,"]")
    oc <- paste0("gamma[",1:3,"]")
    lower <- numeric(p)
    avg <- numeric(p)
    upper <- numeric(p)
    for (i in 1:length(oc)) {
      o = oc[i]
      x0 = transform_back(draws[[o]])
      for (j in 1:length(bidx)) {
        b = bidx[j]
        xb = transform_back(draws[[b]])
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
      bn = 1:p,
      yr = yrs[iii]
    )
    
    R = bind_rows(R,as_tibble(A4))
  }
  
  
  XLABS = c("", paste0("(",1,",",1:9,")"), paste0("(",2,",",1:9,")"), paste0("(",3,",",1:9,")"))
  BREAKS = seq(1,28,by=4)#c(1,6,11,16,21,26)#c(0,5,10,15,20,25)
  
  # PRODUCTION PLOT
  theme_update(plot.title = element_text(hjust = 0.5))
  p = R %>% filter(yr>=2011) %>%
    ggplot(aes(x=bn, y=avg)) +
    facet_wrap(~ yr, ncol=3) +
    geom_errorbar(aes(ymin = lower, ymax = upper), fill = "black", width = .4) +
    geom_point(color="dodgerblue2", shape=21, size=2, fill="white") + 
    # geom_line(aes(y = c(avg[1:9], rep(NA,18))), color="firebrick", size=1) +
    # geom_line(aes(y = c(rep(NA,9), avg[10:18], rep(NA,9))), color="firebrick", size=1) +
    # geom_line(aes(y = c(rep(NA,18), avg[19:27])), color="firebrick", size=1) +
    geom_vline(aes(xintercept = 9.5), size=1.2) +
    geom_vline(aes(xintercept = 18.5), size=1.2) +
    labs(title = TeX("Posterior distribution of $\\beta +\\gamma$")) + 
    theme(legend.position="none") +
    scale_x_continuous(name=TeX("(order Count $l$, unique batter index $k$)"), 
                       limits = c(0,28),
                       breaks = BREAKS,
                       labels =  XLABS[BREAKS+1]) +
    scale_y_continuous(name=TeX("$\\beta_{k} + \\gamma_{l}$"), 
                       limits = c(-.05, .07),
                       breaks = seq(-.1, .1, .02)
    ) 
  p
  ggsave(paste0("./plot_9_yr_stack_",OUTPUT_FILE,".png"), p)
}

{
  R1 = tibble()
  for (iii in 1:length(yrs)) {
    print(yrs[iii])
    fit <- readRDS(paste0("../job_output/fit_rstan5-",nums[iii],".R.rds"))
    draws <- as_tibble(as.matrix(fit))
    draws <- transform_back(draws)
    d1 = draws[["beta[1]"]] + draws[["gamma[1]"]]
    d2 = draws[["beta[2]"]] + draws[["gamma[1]"]]
    d9 = draws[["beta[9]"]] + draws[["gamma[1]"]]
    d10 = draws[["beta[1]"]] + draws[["gamma[2]"]]
    d18 = draws[["beta[9]"]] + draws[["gamma[2]"]]
    d19 = draws[["beta[1]"]] + draws[["gamma[3]"]]
    diff1 = as_tibble(d2 - d1)
    diff2 = as_tibble(d10 - d9)
    diff3 = as_tibble(d19 - d18)
    diff1$name = "diff1"
    diff2$name = "diff2"
    diff3$name = "diff3"
    diffs = bind_rows(diff1,diff2,diff3)
    diffs$yr = yrs[iii]
    R1 = bind_rows(R1,as_tibble(diffs))
  }

  labs <- c(bquote(paste("posterior dist. of ", (beta[2] + gamma[1]) - (beta[1] + gamma[1]))), 
            bquote(paste("posterior dist. of ", (beta[1] + gamma[2]) - (beta[9] + gamma[1]))),
            bquote(paste("posterior dist. of ", (beta[1] + gamma[3]) - (beta[9] + gamma[2]))))
  p1 = R1 %>% filter(yr >= 2011) %>%
    ggplot(aes(value, fill = name)) + 
    facet_wrap(~ yr, ncol=3) +
    geom_density(alpha = 0.2) +
    scale_fill_discrete(labels=labs,
                        name = "") +
    theme(#axis.title.y=element_blank(),
      axis.text.y=element_blank(),
      axis.ticks.y=element_blank()) +
    xlab("change in wOBA") +
    labs(title="Comparing the TTO penalties to the \n difference between the second and first batters")
  p1
  ggsave(paste0("./plot_9_yr_stack_hist_",OUTPUT_FILE,".png"), p1)
}







