library(tidyverse)
library(ggthemes)
theme_set(theme_bw())
if(!interactive()) pdf(NULL)


YEAR_GROUPS = list(2010:2013, 2014:2016, 2017:2019)
indxs = 11:13

results = list()

# loop over years
for (iii in 1:length(YEAR_GROUPS)) {
  print(iii)
  
  # read data
  input_file = "./../data/design_matrix2_3.csv" #FIXME
  output_folder = "./job_output/"
  D <- read_csv(input_file) 
  D <- D %>% drop_na() %>% filter(YEAR %in% YEAR_GROUPS[[iii]])
  # create dummy variables for the categorical variables
  # NO INTERCEPT and INCLUDE FIRST COLUMN 
  change_factor_names <- function(s) {
    s <- str_remove(s, "factor")
    s <- str_remove_all(s, "\\(")
    s <- str_remove_all(s, "\\)")
    s
  }
  # categorical dummies for BATTER_IDX
  BATTER_IDX_dummies <- D %>% modelr::model_matrix(~ factor(BATTER_IDX) + 0) 
  names(BATTER_IDX_dummies) <- change_factor_names(names(BATTER_IDX_dummies))
  # categorical dummies for ORDER_CT
  ORDER_CT_dummies <- D %>% modelr::model_matrix(~ factor(ORDER_CT) + 0) 
  names(ORDER_CT_dummies) <- change_factor_names(names(ORDER_CT_dummies))
  # data 
  y <- D %>% select(std_EVENT_WOBA_19)
  X <- bind_cols(BATTER_IDX_dummies, 
                 ORDER_CT_dummies,
                 D %>% select(std_WOBA_FINAL_BAT_19, std_WOBA_FINAL_PIT_19, HAND_MATCH, BAT_HOME_IND))
  # NAMES
  NAMES <- c("sigma", names(X), "lp__")
  
  # fit
  fit <- readRDS(paste0("job_output/fit_rstan2_yrs-",indxs[iii],".R.rds"))
  s <- summary(fit)$summary
  rownames(s) <- NAMES
  draws <- as_tibble(as.matrix(fit))
  names(draws) <- NAMES
  
  # RESCALE the coefficients back to un-standardized form
  sd_y = sd(D$EVENT_WOBA_19)
  transform_back <- function(x) {
    2*sd_y*x 
  }
  
  # compute mean and 2.5%, 97.5% quantiles of posterior samples
  p = 27 #dim(BATTER_SEQ_dummies)[2]
  bidx <- paste0("BATTER_IDX", 1:9)
  oc <- paste0("ORDER_CT", 1:3)
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
  A = data.frame(
    lower = lower,
    avg = avg,
    upper= upper,
    bn = 1:p
  )
  
  results[[iii]] = A
}

############################
########### PLOT ###########
############################

yr_grp_desc = c("2010-2013", "2014-2016", "2017-2019")
A = tibble()

for (iii in 1:length(results)) {
  curr = results[[iii]]
  curr$yr = yr_grp_desc[iii]
  A = rbind(A, curr)
  
}

############################
########### PLOT ###########
############################

XLABS = c("", paste0("(",1,",",1:9,")"), paste0("(",2,",",1:9,")"), paste0("(",3,",",1:9,")"))
BREAKS = seq(1,28,by=4)#c(1,6,11,16,21,26)#c(0,5,10,15,20,25)

# PRODUCTION PLOT
theme_update(plot.title = element_text(hjust = 0.5))
production_plot = A %>% 
  ggplot(aes(x=bn, y=avg)) +
  facet_wrap(~ yr, ncol=1) +
  geom_errorbar(aes(ymin = lower, ymax = upper), fill = "black", width = .4) +
  geom_point(color="dodgerblue2", shape=21, size=2, fill="white") + 
  # geom_line(aes(y = c(avg[1:9], rep(NA,18))), color="firebrick", size=1) +
  # geom_line(aes(y = c(rep(NA,9), avg[10:18], rep(NA,9))), color="firebrick", size=1) +
  # geom_line(aes(y = c(rep(NA,18), avg[19:27])), color="firebrick", size=1) +
  geom_vline(aes(xintercept = 9.5), size=1.2) +
  geom_vline(aes(xintercept = 18.5), size=1.2) +
  labs(title = "Pitcher Effectiveness") +
  theme(legend.position="none") +
  scale_x_continuous(name="(order count, unique batter number)", 
                     limits = c(0,28),
                     breaks = BREAKS,
                     labels =  XLABS[BREAKS+1]) +
  scale_y_continuous(name="Posterior Change in wOBA", 
                     #limits = c(-.03, .03),
                     #breaks = seq(-.03, .03, .005)
  ) 
production_plot
ggsave("plot_stack_3_grps.png", production_plot)








