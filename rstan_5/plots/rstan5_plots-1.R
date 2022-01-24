library(tidyverse)
output_folder = './' #'./job_output/'

### load data
input_file = "../../data/TTO_dataset_411.csv"  
D <- read_csv(input_file)
D <- D %>% drop_na() 
#FIXME 
D <- D %>% filter(YEAR == 2019) 
X <- as.matrix(D %>% select(std_WOBA_FINAL_BAT_19, std_WOBA_FINAL_PIT_19, HAND_MATCH, BAT_HOME_IND)) 
OUTPUT_FILE = "rstan5_plots-1.R"
fit <- readRDS("../job_output/fit_rstan5_overall-1.R.rds") 

source("rstan5_plots_main.R")

#############################
########### PLOTS ###########
#############################

# plot full trajectory 
p = plot_bsn0(fit)
p
ggsave(paste0("./plot_",OUTPUT_FILE,".png"), p)

# plot 3 panel histogram B1->B2, B9->B10, B18->B19
p1 = plot_3hist_bsn1(fit)
p1
ggsave(paste0("./plot_3hist1_",OUTPUT_FILE,".png"), p1)

# plot 3 panel histogram avg TTO1, TTO2, TTO3
p2 = plot_3hist_bsn2(fit)
p2
ggsave(paste0("./plot_3hist2_",OUTPUT_FILE,".png"), p2)



### plot trajectory of Juan Soto running avg. estimator AND current avg. woba
# plot((D %>% filter(BAT_ID=="sotoj001") %>% 
#         filter(row_number() >= 200) %>% 
#         select(WOBA_AVG_BAT_19))$WOBA_AVG_BAT_19)
# plot((D %>% filter(BAT_ID=="sotoj001") %>% filter(row_number() >= 200) %>% select(BQ))$BQ)
# ###plot((D %>% filter(BAT_ID=="sotoj001") %>% select(BQ))$BQ)

### plot correlation b/t PQ,BQ and batter_idx
# D %>% group_by(BATTER_IDX) %>% 
#   summarise(wf =mean(WOBA_FINAL_PIT_19), wr = mean(PQ)) %>% 
#   ggplot(aes(x=BATTER_IDX, y=wf)) + geom_point()
# D %>% filter(DATE == "2019-05-01") %>% group_by(BATTER_IDX) %>% 
#   summarise(wf = mean(WOBA_FINAL_BAT_19), wr = mean(BQ)) %>% 
#   ggplot(aes(x=BATTER_IDX, y=wr)) + geom_point()
# D %>% group_by(BATTER_IDX) %>% 
#   summarise(wf = mean(WOBA_FINAL_BAT_19), wr = mean(BQ)) %>% 
#   ggplot(aes(x=BATTER_IDX, y=wf)) + geom_point()




# ###
# w_bat = (D %>% group_by(BAT_ID) %>% summarise(w = unique(WOBA_FINAL_BAT_19)))$w
# w_bat1 = w_bat[w_bat !=0]
# hist(w_bat1, breaks=40)
# mean(w_bat1)
# sd(w_bat1)
# 
# w_pit = (D %>% group_by(PIT_ID) %>% summarise(w = unique(WOBA_FINAL_PIT_19)))$w
# w_pit1 = w_pit[w_pit !=0]
# hist(w_pit1, breaks=40)
# mean(w_pit1)
# sd(w_pit1)



