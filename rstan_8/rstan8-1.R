library(tidyverse)
output_folder = './job_output/'

### load data
input_file = "./../data/TTO_dataset_510.csv"  
D <- read_csv(input_file)
##D <- D %>% drop_na() 
#FIXME 
D <- D %>% filter(YEAR == 2019) 
X <- as.matrix(D %>% select(logit(BQ), logit(PQ), HAND_MATCH, BAT_HOME_IND)) 
OUTPUT_FILE = "rstan8-1.R"

### rstan
source("rstan8_main.R")
fit = fit_model_bsn(NA) 
saveRDS(fit, file = paste0(output_folder, "fit_", OUTPUT_FILE, ".rds"))

### plot
# fit <- readRDS("./job_output/fit_rstan8-1.R.rds") 
p = plot_bsn0(fit)
p
ggsave(paste0("./plot_",OUTPUT_FILE,".png"), p)



# 
# 
# draws <- as_tibble(as.matrix(fit))
# A = tibble()
# for (k in 1:num_categories) {
#   # compute mean and 2.5%, 97.5% quantiles of posterior samples
#   p = 27 #dim(BATTER_SEQ_dummies)[2]
#   bsn <- paste0("alpha[",1:p,",",k,"]")
#   lower <- numeric(p)
#   avg <- numeric(p)
#   upper <- numeric(p)
#   for (i in 1:length(bsn)) {
#     b = bsn[i]
#     x = draws[[bsn[i]]]
#     lower[i] = quantile(x,.025)
#     avg[i] = mean(x)
#     upper[i] = quantile(x,.975)
#   }
#   A4 = tibble(
#     lower = lower,
#     avg = avg,
#     upper= upper,
#     bn = 1:p,
#     k=paste0("wOBA_19 = ", categories[k])
#   )
#   A = bind_rows(A,A4)
# }
# # PLOT
# production_plot = A %>% 
#   filter(k==paste0("wOBA_19 = ", categories[7])) %>%
#   ggplot(aes(x=bn, y=avg)) +
#   #facet_wrap(~k) +
#   geom_errorbar(aes(ymin = lower, ymax = upper), fill = "black", width = .4) +
#   geom_point(color="dodgerblue2", shape=21, size=2, fill="white") + 
#   # geom_line(aes(y = c(avg[1:9], rep(NA,18))), color="firebrick", size=1) +
#   # geom_line(aes(y = c(rep(NA,9), avg[10:18], rep(NA,9))), color="firebrick", size=1) +
#   # geom_line(aes(y = c(rep(NA,18), avg[19:27])), color="firebrick", size=1) +
#   geom_vline(aes(xintercept = 9.5), size=1.2) +
#   geom_vline(aes(xintercept = 18.5), size=1.2) +
#   labs(title = TeX("Posterior distribution of $\\alpha$")) + 
#   theme(legend.position="none") +
#   scale_x_continuous(name=TeX("Batter sequence number $k$"), 
#                      limits = c(0,28),
#                      breaks = c(0,5,10,15,20,25)) +
#   scale_y_continuous(name=TeX("$\\alpha_k$"), 
#                      #limits = c(-.02, .03),
#                      #breaks = seq(-2, 2, .05)
#   ) 
# production_plot

