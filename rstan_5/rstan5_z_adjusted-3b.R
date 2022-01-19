library(tidyverse)
output_folder = './job_output/'

### load data
input_file = "./../data/TTO_dataset_411.csv"  
D <- read_csv(input_file)
D <- D %>% drop_na() 
#FIXME 
D <- D %>% filter(YEAR == 2019) 
D <- D %>% mutate(
  lineup_pos = BATTER_SEQ_NUM %% 9,
  lineup_pos = ifelse(lineup_pos == 0, 9, lineup_pos)
)
source("rstan5_main.R")
# categorical dummies for lineup_pos
lineup_pos_dummies <- D %>% modelr::model_matrix(~ factor(lineup_pos) + 0) 
names(lineup_pos_dummies) <- change_factor_names(names(lineup_pos_dummies))
X <- cbind(as.matrix(D %>% select(std_BQ, std_PQ, HAND_MATCH, BAT_HOME_IND)), lineup_pos_dummies) 
OUTPUT_FILE = "rstan5_z_adjusted-3b.R"

### rstan
fit = fit_model_bsn(NA) 
saveRDS(fit, file = paste0(output_folder, "fit_", OUTPUT_FILE, ".rds"))

### plot
# fit <- readRDS("./job_output/fit_rstan5_z_adjusted-3.rds") 
p = plot_bsn0(fit)
p
ggsave(paste0("./plot_",OUTPUT_FILE,".png"), p)

