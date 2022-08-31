
### load the observed data 

library(tidyverse)
# library(ggthemes)
library(latex2exp)
library(splines)
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
D <- read_csv(input_file) 
if (CHANGE_DIR) { setwd(og_dir) }
D <- D %>% filter(YEAR %in% YRS) ### get YRS from config fig
D <- D %>% filter(ORDER_CT <= 3) 
D <- D %>% filter(BQ>0 & BQ<1 & PQ>0 & PQ<1) 
# only filter PIT_IS_BAT if not a SIM
if (exists("IS_SIM")) { if (!IS_SIM) { D <- D %>% filter(!PIT_IS_BAT) } } else { D <- D %>% filter(!PIT_IS_BAT) }

# confounders matrix X 
logit <- function(p) { log(p/(1-p)) }
X <- as.matrix(D %>% mutate(lBQ=logit(BQ), lPQ=logit(PQ)) %>% select(lBQ, lPQ, HAND_MATCH, BAT_HOME_IND))
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
SPL = cbind(1, D$BATTER_SEQ_NUM)

# # SPLINE data matrices
# aa = unique(D$BATTER_SEQ_NUM) 
# # ### spline model with discontinuities between each TTO
# # knots = c(rep(9.5,4), rep(18.5,4), rep(27.5,4))  
# # BB_ <- bs(aa, knots=knots, degree=3, intercept = TRUE) # creating the B-splines
# ### one cubic spline over batters 1...27 with 4 df
# # BB_ <- bs(aa, df=4)
# BB_ <- bs(aa, df=4, intercept=TRUE)
# colnames(BB_) = paste0("B",1:ncol(BB_))
# BB = as_tibble(BB_)
# bbb = as.matrix(BB)
# SPL = S %*% bbb ### spline model: spline basis with 4 degrees of freedom
# 
# # bbb = as.matrix(as_tibble(bs(aa, df=4)))
# # colnames(bbb) = paste0("B",1:ncol(bbb))
# # SPL_1b = S %*% bbb

# SPL = S ### indicator model: 27 b.s.n. indicators

# Batter Learning data matrix
O <- as.matrix(ORDER_CT_dummies)

# intercept matrix (for BL_only model)
Incpt <- matrix(1, nrow=nrow(D), ncol=1)

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
folds <- loo::kfold_split_random(K=kk,N=nrow(y))





