
### load the observed data 

library(tidyverse)
# library(ggthemes)
library(latex2exp)
library(splines)
library(xgboost)
theme_set(theme_bw())
theme_update(text = element_text(size=18))
theme_update(plot.title = element_text(hjust = 0.5))
if(!interactive()) pdf(NULL)
output_folder = './job_output/'

#####################################
########### OBSERVED DATA ###########
#####################################

### make sure to get the value of YRS from some file, such as `config.R`
### here, set a default value of YRS = 2018
if (!exists("YRS")) { YRS = 2018 } 
  
### load data D
CHANGE_DIR = if (exists("IS_SIM")) { IS_SIM } else if (exists("IS_COMP")) { IS_COMP } else { FALSE }
og_dir = getwd()
if (CHANGE_DIR) { setwd("..") }
input_file = "../data/TTO_dataset_510.csv"  
D0 <- read_csv(input_file) 
if (CHANGE_DIR) { setwd(og_dir) }
D <- D0 %>% filter(YEAR %in% YRS) ### get YRS from config fig
D <- D %>% filter(ORDER_CT <= 3) 
D <- D %>% filter(BQ>0 & BQ<1 & PQ>0 & PQ<1) 
# only filter PIT_IS_BAT if not a SIM
if (exists("IS_SIM")) { if (!IS_SIM) { D <- D %>% filter(!PIT_IS_BAT) } } else { D <- D %>% filter(!PIT_IS_BAT) }

### Remove Games in which the pitcher is pulled in 2TTO
games_in_which_pit_makes_it_to_3tto = D %>%
  group_by(GAME_ID, PIT_ID) %>%
  # slice_tail() %>%
  select(GAME_ID, PIT_ID, BATTER_SEQ_NUM) %>%
  slice_tail() %>%
  rename(last_bsn = BATTER_SEQ_NUM) %>%
  ungroup() %>%
  filter(last_bsn >= 18)
nrow(D %>% distinct(GAME_ID, PIT_ID))
nrow(games_in_which_pit_makes_it_to_3tto)
D = games_in_which_pit_makes_it_to_3tto %>% left_join(D)

# confounders matrix X 
##### logit <- function(p) { log(p/(1-p)) }
##### X <- as.matrix(D %>% mutate(lBQ=logit(BQ), lPQ=logit(PQ)) %>% select(lBQ, lPQ, HAND_MATCH, BAT_HOME_IND))
X <- as.matrix(D %>% select(BQ, PQ, HAND_MATCH, BAT_HOME_IND))
# NO INTERCEPT and INCLUDE FIRST COLUMN
change_factor_names <- function(s) { str_remove_all(str_remove_all(str_remove(s, "factor"), "\\("), "\\)") }
# categorical dummies for BATTER_SEQ_NUM
BATTER_SEQ_dummies <- D %>% modelr::model_matrix(~ factor(BATTER_SEQ_NUM) + 0)
names(BATTER_SEQ_dummies) <- change_factor_names(names(BATTER_SEQ_dummies))
# categorical dummies for ORDER_CT;; (plus INTERCEPT potentially)
ORDER_CT_dummies <- D %>% modelr::model_matrix(~ factor(ORDER_CT))  # + 0)
ORDER_CT_dummies <- ORDER_CT_dummies[,2:ncol(ORDER_CT_dummies)] ## remove intercept
names(ORDER_CT_dummies) <- change_factor_names(names(ORDER_CT_dummies))
# BSN data matrices 
S <- as.matrix(BATTER_SEQ_dummies)

### LINE, not SPLINE data matrix
# SPL = matrix(D$BATTER_SEQ_NUM, ncol=1)
BSN = matrix(D$BATTER_SEQ_NUM, ncol=1)
INCPT = matrix(1, ncol=1, nrow=length(D$BATTER_SEQ_NUM))

# Batter Learning data matrix
O <- as.matrix(ORDER_CT_dummies)
O2 = O[,"ORDER_CT2"]
O3 = O[,"ORDER_CT3"]

# observed plate appearance outcomes
y_og <- D$EVENT_WOBA_19
categories = sort(unique(y_og))
num_categories = length(categories)
y_to_category <- function(y_ij) { which(y_ij == categories)[1] }
y <- matrix( sapply(y_og, y_to_category), ncol=1)
category_strings <- c("out","BB","HBP","1B","2B","3B","HR")
# 10 Fold CV folds
set.seed(12345) # make sure to have the same folds each time!
kk = if (exists("IS_SIM")) { if (IS_SIM) 5 else 10 } else 10
NUM_FOLDS = kk
folds <- loo::kfold_split_random(K=NUM_FOLDS,N=nrow(y))

### pit vector p, pit_games vector pg, bat vector b, bat_games vector bg
pit_game_indices <- D %>% select(GAME_ID,PIT_ID) %>% distinct() %>% #arrange(PIT_ID,GAME_ID) %>% 
  group_by(PIT_ID) %>% mutate(pit_game_idx = row_number()) %>% ungroup()
pit_indices <- pit_game_indices %>% group_by(PIT_ID) %>% slice_tail() %>% select(-GAME_ID) %>% ungroup() %>% 
  arrange(PIT_ID) %>% mutate(pit_idx = row_number()) %>% rename(num_games = pit_game_idx)
#####
num_pit = nrow(pit_indices)
pit_idxs = (D %>% select(PIT_ID) %>% left_join(pit_indices))$pit_idx
#####
num_pit_games = max(pit_indices$num_games)
pg = (D %>% select(GAME_ID,PIT_ID) %>% left_join(pit_game_indices))$pit_game_idx
#####
bat_game_indices <- D %>% select(GAME_ID,BAT_ID) %>% distinct() %>% #arrange(BAT_ID,GAME_ID) %>% 
  group_by(BAT_ID) %>% mutate(bat_game_idx = row_number()) %>% ungroup()
bat_indices <- bat_game_indices %>% group_by(BAT_ID) %>% slice_tail() %>% select(-GAME_ID) %>% ungroup() %>% 
  arrange(BAT_ID) %>% mutate(bat_idx = row_number()) %>% rename(num_games = bat_game_idx)
#####
num_bat = nrow(bat_indices)
bat_idxs = (D %>% select(BAT_ID) %>% left_join(bat_indices))$bat_idx
#####
num_bat_games = max(bat_indices$num_games)
bg = (D %>% select(GAME_ID,BAT_ID) %>% left_join(bat_game_indices))$bat_game_idx


# #####################################
# #### LOAD PROPENSITY SCORE MODEL ####
# #####################################
# 
# xgb_propensity_model = xgb.load("xgb_selection_model.xgb")
# source("A_selection_model_functions.R")
# D$pi = predict_probs_xgb(
#   xgb_propensity_model, 
#   D %>% group_by(GAME_ID,PIT_ID) %>% mutate(mean_game_woba = mean(EVENT_WOBA_19)) %>% ungroup()
# )
# 
# if (exists("USE_ROW_WEIGHTS")) { 
#   if (USE_ROW_WEIGHTS) {
#     row_weights = 1/(D$pi)
#   } else {
#     row_weights = rep(1, nrow(D))
#   }
# } else {
#   row_weights = rep(1, nrow(D))
# }



# ####### visualize the row weights ####### 
# tibble(row_weights) %>%
#   ggplot() +
#   geom_histogram(aes(x=row_weights), binwidth=0.01)
# ### visualize the row weights
# tibble(row_weights) %>%
#   filter(row_weights<3) %>%
#   ggplot() +
#   geom_histogram(aes(x=row_weights), binwidth=0.01)
###
# sum(row_weights > 3)
# sum(row_weights > 5)
# sum(row_weights > 20)
# dim(D)[1]
