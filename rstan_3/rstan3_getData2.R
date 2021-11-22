########################################
### 10 FOLD CV: COMPARE THE 2 MODELS ###
########################################

output_folder = "./job_output/"
#OUTPUT_FILE = "rstan3_comp.R" #FIXME

library(tidyverse)
library(rstan)
library(ggthemes)
library(latex2exp)
theme_set(theme_bw())
theme_update(plot.title = element_text(hjust = 0.5))
if(!interactive()) pdf(NULL)
NUM_ITERS_IN_CHAIN = 1500 #FIXME #10 
cores = strtoi(Sys.getenv('OMP_NUM_THREADS')) ### for HPCC
options(mc.cores = cores) ### for HPCC
# options(mc.cores = parallel::detectCores()) # use this on my computer
rstan_options(auto_write = TRUE)

#####################################
########### OBSERVED DATA ###########
#####################################

# USE THE ACTUAL X DATA MATRIX
# read data
input_file = "./../data/design_matrix2_3.csv" #FIXME
D <- read_csv(input_file)
D <- D %>% drop_na() #%>% filter(YEAR == 2019) #FIXME
  #filter(YEAR >= 2015 & YEAR <= 2019) #FIXME
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
# categorical dummies for BATTER_IDX
BATTER_IDX_dummies <- D %>% modelr::model_matrix(~ factor(BATTER_IDX) + 0) 
names(BATTER_IDX_dummies) <- change_factor_names(names(BATTER_IDX_dummies))
# categorical dummies for ORDER_CT
ORDER_CT_dummies <- D %>% modelr::model_matrix(~ factor(ORDER_CT) + 0) 
names(ORDER_CT_dummies) <- change_factor_names(names(ORDER_CT_dummies))
# Observed data matrices 
S <- as.matrix(BATTER_SEQ_dummies)
U <- as.matrix(BATTER_IDX_dummies)
O <- as.matrix(ORDER_CT_dummies)
X <- as.matrix(D %>% select(std_WOBA_FINAL_BAT_19, std_WOBA_FINAL_PIT_19, HAND_MATCH, BAT_HOME_IND))
y <- matrix(D$std_EVENT_WOBA_19, ncol=1)
D_bsn = cbind(S,X)
D_ubi = cbind(U,O,X)
# 10 Fold CV folds
set.seed(12345) # make sure to have the same folds each time!
folds <- loo::kfold_split_random(K=10,N=nrow(y))

##################################
########### FIT MODELS ###########
##################################

file = 'tto3_both.stan'
model <- stan_model(file = file, model_name = file)

fit_model <- function(fold_num, key) {
  # training data - exclude FOLD_NUM, unless FOLD_NUM is NA 
  train_rows = ifelse(is.na(fold_num), TRUE, which(folds != fold_num))
  #train_rows = which(folds != fold_num)
  y_train = y[train_rows,]
  D_bsn_train = D_bsn[train_rows,]
  D_ubi_train = D_ubi[train_rows,]
  D_train <- if (key=="ubi") D_ubi_train else D_bsn_train
  data_train <- list(y=y_train, D = D_train,  n=nrow(D_train), p = ncol(D_train))
  # Train the models
  seed = 12345
  set.seed(seed)
  fit <- sampling(model,
                  data = data_train,
                  iter = NUM_ITERS_IN_CHAIN,
                  chains = cores, #1 #cores, 
                  cores = cores, # HPCC
                  seed = seed)
  fit
}

######################################
########### PLOT FUNCTIONS ###########
######################################

plot_bsn_ <- function(fitB) {
  alpha_fit = summary(fitB)$summary[2:(dim(S)[2]+1),c(4,1,8)]
  A = alpha_fit
  colnames(A) = c("lower","avg","upper")
  A = as_tibble(A[1:27,])
  A$bn = 1:27
  # PRODUCTION PLOT
  theme_update(plot.title = element_text(hjust = 0.5))
  production_plot = A %>% 
    ggplot(aes(x=bn, y=avg)) +
    geom_errorbar(aes(ymin = lower, ymax = upper), fill = "black", width = .4) +
    geom_point(color="dodgerblue2", shape=21, size=2, fill="white") + 
    geom_vline(aes(xintercept = 9.5), size=1.2) +
    geom_vline(aes(xintercept = 18.5), size=1.2) +
    #labs(title = "Pitcher Effectiveness") +
    labs(title = TeX("Posterior distribution of $\\alpha$")) + 
    theme(legend.position="none") +
    scale_x_continuous(name=TeX("Batter sequence number $k$"),
                       limits = c(0,28),
                       breaks = c(0,5,10,15,20,25)) +
    scale_y_continuous(name=TeX("$\\alpha_k$"),
                       #limits = c(-.02, .03),
                       breaks = seq(-.09, .09, .005)
    ) 
  production_plot
}

plot_ubi_ <- function(fitU) {
  beta_fit = summary(fitU)$summary[2:(dim(U)[2]+1),c(4,1,8)]
  gamma_fit = summary(fitU)$summary[(dim(U)[2]+2):(dim(U)[2]+dim(O)[2]+1),c(4,1,8)]
  B = do.call(rbind, replicate(3, beta_fit[1:9,], simplify=FALSE))
  G = rbind(
    do.call(rbind, replicate(9, gamma_fit[1,], simplify=FALSE)),
    do.call(rbind, replicate(9, gamma_fit[2,], simplify=FALSE)),
    do.call(rbind, replicate(9, gamma_fit[3,], simplify=FALSE))
  )
  A = B+G
  rownames(A) = NULL
  colnames(A) = c("lower","avg","upper")
  A = as_tibble(A)
  A$bn = 1:27
  # PRODUCTION PLOT
  theme_update(plot.title = element_text(hjust = 0.5))
  XLABS = c("", paste0("(",1,",",1:9,")"), paste0("(",2,",",1:9,")"), paste0("(",3,",",1:9,")"))
  BREAKS = seq(1,28,by=2)#c(1,6,11,16,21,26)#c(0,5,10,15,20,25)
  production_plot = A %>% 
    ggplot(aes(x=bn, y=avg)) +
    geom_errorbar(aes(ymin = lower, ymax = upper), fill = "black", width = .4) +
    geom_point(color="dodgerblue2", shape=21, size=2, fill="white") + 
    geom_vline(aes(xintercept = 9.5), size=1.2) +
    geom_vline(aes(xintercept = 18.5), size=1.2) +
    #labs(title = "Pitcher Effectiveness") +
    labs(title = TeX("Posterior distribution of $\\beta +\\gamma$")) + 
    theme(legend.position="none") +
    scale_x_continuous(name=TeX("(order Count $l$, unique batter index $k$)"),
                       limits = c(0,28),
                       breaks = BREAKS,
                       labels =  XLABS[BREAKS+1]) +
    scale_y_continuous(name=TeX("$\\beta_{k} + \\gamma_{l}$"),
                       #limits = c(-.02, .03),
                       #breaks = seq(-.09, .09, .005)
    ) 
  production_plot
}
