library(tidyverse)
library(ggthemes)
theme_set(theme_bw())
theme_update(plot.title = element_text(hjust = 0.5))
if(!interactive()) pdf(NULL)

YEAR_GROUPS = list(2010:2013, 2014:2016, 2017:2019)
indxs = 11:13

results = list()

# loop over years
for (iii in 1:length(YEARS)) {
  print(iii)
  
  # read data
  input_file = "./../data/design_matrix2_3.csv" #FIXME
  output_folder = "./job_output/"
  D <- read_csv(input_file) 
  D <- D %>% drop_na() %>% filter(YEAR == YEARS[iii])
  # create dummy variables for the categorical variables
  # NO INTERCEPT and INCLUDE FIRST COLUMN 
  change_factor_names <- function(s) {
    s <- str_remove(s, "factor")
    s <- str_remove_all(s, "\\(")
    s <- str_remove_all(s, "\\)")
    s
  }
  # categorical dummies for BATTER_SEQ_NUM
  BATTER_SEQ_dummies <- D %>% modelr::model_matrix(~ factor(BATTER_SEQ_NUM) + 0) 
  names(BATTER_SEQ_dummies) <- change_factor_names(names(BATTER_SEQ_dummies))
  # data 
  y <- D %>% select(std_EVENT_WOBA_19)
  X <- bind_cols(BATTER_SEQ_dummies, D %>% select(std_WOBA_FINAL_BAT_19, std_WOBA_FINAL_PIT_19, HAND_MATCH, BAT_HOME_IND))
  
  # NAMES
  NAMES <- c("sigma", names(X), "lp__")
  
  # fit
  fit <- readRDS(paste0("job_output/fit_rstan2_yrs2-",indxs[iii],".R.rds"))
  s <- summary(fit)$summary
  rownames(s) <- NAMES
  draws <- as_tibble(as.matrix(fit))
  names(draws) <- NAMES
  
  # RESCALE the coefficients back to un-standardized form
  sd_y = sd(D$EVENT_WOBA_19)
  transform_back <- function(x) {
    2*sd_y*x 
  }
  
  #FIXME
  # compute mean and 2.5%, 97.5% quantiles of posterior samples
  p = 27 #dim(BATTER_SEQ_dummies)[2]
  bsn <- paste0("BATTER_SEQ_NUM", 1:p)
  lower <- numeric(p)
  avg <- numeric(p)
  upper <- numeric(p)
  for (j in 1:length(bsn)) {
    b = bsn[j]
    x = transform_back(draws[[b]])
    lower[j] = quantile(x,.025)
    avg[j] = mean(x)
    upper[j] = quantile(x,.975)
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

#XLABS = c("", paste0("(",1,",",1:9,")"), paste0("(",2,",",1:9,")"), paste0("(",3,",",1:9,")"))
#BREAKS = seq(1,28,by=4)#c(1,6,11,16,21,26)#c(0,5,10,15,20,25)

# # PRODUCTION PLOT
plot_me <- function(A) {
  A %>%
  ggplot(aes(x=bn, y=avg)) +
  facet_wrap(~ yr, ncol=2) +
  geom_errorbar(aes(ymin = lower, ymax = upper), fill = "black", width = .4) +
  geom_point(color="dodgerblue2", shape=21, size=2, fill="white") +
  geom_vline(aes(xintercept = 9.5), size=1.2) +
  geom_vline(aes(xintercept = 18.5), size=1.2) +
  labs(title = "Pitcher Effectiveness") +
  theme(legend.position="none") +
  scale_x_continuous(name="Batter sequence number",
                     limits = c(0,28),
                     #breaks = BREAKS, #labels =  XLABS[BREAKS+1]
                     ) +
  scale_y_continuous(name="Posterior Change in wOBA",
                     #limits = c(-.03, .08),
                     breaks = seq(-.05, .09, .01)
  )
}
production_plot = plot_me(A)
production_plot
ggsave("plot_bsn_stack_3_grps.png", production_plot)


