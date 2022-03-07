library(tidyverse)
library(cowplot)
library(grid)
 
OUTPUT_FILE = "rstan8_plots-3hists.R"
### load data
input_file = "../../data/TTO_dataset_510.csv"  
D <- read_csv(input_file) #D <- D %>% drop_na() 
D <- D %>% filter(YEAR == 2019) %>% filter(BQ>0 & BQ<1 & PQ>0 & PQ<1 )
logit <- function(p) { log(p/(1-p)) }
X <- as.matrix(D %>% mutate(lBQ=logit(BQ), lPQ=logit(PQ)) %>% select(lBQ, lPQ, HAND_MATCH, BAT_HOME_IND)) 

fit1 <- readRDS("../job_output/fit_rstan8-1.R.rds") #BSN
fit2 <- readRDS("../job_output/fit_rstan8-2.R.rds") #UBI

source("rstan8_plots_main.R")
source("../rstan8_main.R")

#############################
########### PLOTS ###########
#############################



### UBI: end of each TTO ###
{
  draws = as_tibble(as.matrix(fit2))
  diffs = tibble()
  for (k in 2:7) {
    d9 = draws[[paste0("beta[9,",k,"]")]] + draws[[paste0("gamma[1,",k,"]")]]
    d10 = draws[[paste0("beta[1,",k,"]")]] + draws[[paste0("gamma[2,",k,"]")]]
    d18 = draws[[paste0("beta[9,",k,"]")]] + draws[[paste0("gamma[2,",k,"]")]]
    d19 = draws[[paste0("beta[1,",k,"]")]] + draws[[paste0("gamma[3,",k,"]")]]
    diff2 = as_tibble(d10 - d9)
    diff3 = as_tibble(d19 - d18)
    diff2$name = "diff2"
    diff3$name = "diff3"
    diffs_k = bind_rows(diff2,diff3)
    diffs_k$k = category_strings[k]
    diffs = bind_rows(diffs, diffs_k)
  }
  labs <- c(bquote(paste("posterior dist. of ", (beta[1] + gamma[2]) - (beta[9] + gamma[1]))),
            bquote(paste("posterior dist. of ", (beta[1] + gamma[3]) - (beta[9] + gamma[2]))))
  p1 = diffs %>% 
    ggplot(aes(value, fill = name)) + 
    facet_wrap(~k) + 
    geom_density(alpha = 0.2) + 
    # geom_histogram(data=subset(diffs,name == 'diff2'),fill = "blue", alpha = 0.3) +
    # geom_histogram(data=subset(diffs,name == 'diff3'),fill = "red", alpha = 0.3) +
    geom_vline(xintercept = 0) +
    scale_fill_discrete(labels=labs,name = "") +
    theme(#axis.title.y=element_blank(),
      # legend.position = "none",
      axis.text.y=element_blank(),
      axis.ticks.y=element_blank()) +
    xlab("change in log odds") +
    labs(title="End of each TTO")
  p1
  # ggsave("plot_end_of_TTO_3hists.png", p1)
}

### UBI: average over each TTO ###
{
  draws = as_tibble(as.matrix(fit2))
  diffs = tibble()
  for (k in 2:7) {
    g1 = draws[[paste0("gamma[1,",k,"]")]]
    g2 = draws[[paste0("gamma[2,",k,"]")]]
    g3 = draws[[paste0("gamma[3,",k,"]")]]
    diff2 = as_tibble(g2 - g1)
    diff3 = as_tibble(g3 - g2)
    diff2$name = "diff2"
    diff3$name = "diff3"
    diffs_k = bind_rows(diff2,diff3)
    diffs_k$k = category_strings[k]
    diffs = bind_rows(diffs, diffs_k)
  }
  labs <- c(bquote(paste("posterior dist. of ", gamma[2] - gamma[1])),
            bquote(paste("posterior dist. of ", gamma[3] - gamma[2])))
  p2 = diffs %>% 
    ggplot(aes(value, fill = name)) + 
    facet_wrap(~k) + 
    geom_density(alpha = 0.2) + 
    # geom_histogram(data=subset(diffs,name == 'diff2'),fill = "blue", alpha = 0.3) +
    # geom_histogram(data=subset(diffs,name == 'diff3'),fill = "red", alpha = 0.3) +
    geom_vline(xintercept = 0) +
    scale_fill_discrete(labels=labs,name = "") +
    theme(#axis.title.y=element_blank(),
      # legend.position = "none",
      axis.text.y=element_blank(),
      axis.ticks.y=element_blank()) +
    xlab("change in log odds") +
    labs(title="Averaged over each TTO")
  p2
  # ggsave("plot_avg_over_TTO_3hists.png", p2)
}


### UBI: 1-27 trend ###


### UBI: 1-27 trend + spline ###






# # plot spline trajectory 
# p3 = plot_bsn_spline(fit)
# p3
# ggsave(paste0("./plot_spline_",OUTPUT_FILE,".png"), p)

