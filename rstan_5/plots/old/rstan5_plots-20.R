library(tidyverse)

#FIXME
OUTPUT_FILE = "rstan5_plots-20.R"
nums = c(20:28,2)
yrs = 2010:2019

### plot 8 year stack

{
  R = tibble()
  for (iii in 1:length(yrs)) {
    print(yrs[iii])
    fit <- readRDS(paste0("../job_output/fit_rstan5-",nums[iii],".R.rds"))
    draws <- as_tibble(as.matrix(fit))
    
    # compute mean and 2.5%, 97.5% quantiles of posterior samples
    p = 27 #dim(BATTER_SEQ_dummies)[2]
    bsn <- paste0("alpha[",1:p,"]")
    lower <- numeric(p)
    avg <- numeric(p)
    upper <- numeric(p)
    for (i in 1:length(bsn)) {
      b = bsn[i]
      x = transform_back(draws[[bsn[i]]])
      lower[i] = quantile(x,.025)
      avg[i] = mean(x)
      upper[i] = quantile(x,.975)
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
  
  # PRODUCTION PLOT
  p = R %>% filter(yr>=2012) %>%
    ggplot(aes(x=bn, y=avg)) +
    facet_wrap(~ yr, ncol=2) +
    geom_errorbar(aes(ymin = lower, ymax = upper), fill = "black", width = .4) +
    geom_point(color="dodgerblue2", shape=21, size=2, fill="white") + 
    # geom_line(aes(y = c(avg[1:9], rep(NA,18))), color="firebrick", size=1) +
    # geom_line(aes(y = c(rep(NA,9), avg[10:18], rep(NA,9))), color="firebrick", size=1) +
    # geom_line(aes(y = c(rep(NA,18), avg[19:27])), color="firebrick", size=1) +
    geom_vline(aes(xintercept = 9.5), size=1.2) +
    geom_vline(aes(xintercept = 18.5), size=1.2) +
    theme(legend.position="none") +
    scale_x_continuous(name=TeX("Batter sequence number $k$"), 
                       limits = c(0,28),
                       breaks = c(1,5,9,10,14,18,19,23,27)) +
    # labs(title = TeX("Posterior distribution of $\\alpha$")) + 
    # scale_y_continuous(name=TeX("$\\alpha_k$"), 
    #                    #limits = c(-.055, .07),
    #                    breaks = seq(-.1, .1, .02)
    labs(title = TeX("Trend in wOBA over the course of a game")) + 
    scale_y_continuous(name=TeX("Posterior distribution of $\\alpha_k$"), 
                       #limits = c(-.055, .07),
                       breaks = seq(-.1, .1, .02)
    ) 
  p
  ggsave(paste0("./plot_8_yr_stack_",OUTPUT_FILE,".png"), p)
}

{
  R1 = tibble()
  for (iii in 1:length(yrs)) {
    print(yrs[iii])
    fit <- readRDS(paste0("../job_output/fit_rstan5-",nums[iii],".R.rds"))
    draws <- as_tibble(as.matrix(fit))
    draws <- transform_back(draws)
    d1 = draws[["alpha[1]"]]
    d2 = draws[["alpha[2]"]]
    d9 = draws[["alpha[9]"]]
    d10 = draws[["alpha[10]"]]
    d18 = draws[["alpha[18]"]]
    d19 = draws[["alpha[19]"]]
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
  
  # labs <- c(bquote(paste("posterior dist. of ", alpha[2] - alpha[1])), 
  #           bquote(paste("posterior dist. of ", alpha[10] - alpha[9])),
  #           bquote(paste("posterior dist. of ", alpha[19] - alpha[18])))
  p1 = R1 %>% filter(yr >= 2012) %>%
    ggplot(aes(value, fill = name)) + 
    facet_wrap(~ yr, ncol=2) +
    geom_density(alpha = 0.2) +
    #scale_fill_discrete(labels=labs, name = "") +
    theme(legend.position = "none") +
    theme(#axis.title.y=element_blank(),
      axis.text.y=element_blank(),
      axis.ticks.y=element_blank()) +
    xlab("change in wOBA") +
    labs(title="Comparing the TTO penalties to the \n difference between the second and first batters")
  p1
  ggsave(paste0("./plot_8_yr_stack_hist_",OUTPUT_FILE,".png"), p1)
}







