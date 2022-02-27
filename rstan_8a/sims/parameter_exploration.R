library(tidyverse)
output_folder = './job_output/'

### load data
input_file = "../../data/TTO_dataset_510.csv"  
D <- read_csv(input_file)
##D <- D %>% drop_na() 
#FIXME 
D <- D %>% filter(YEAR == 2019) %>% filter(BQ>0 & BQ<1 & PQ>0 & PQ<1 )
logit <- function(p) { log(p/(1-p)) }
X <- as.matrix(D %>% mutate(lBQ=logit(BQ), lPQ=logit(PQ)) %>% select(lBQ, lPQ, HAND_MATCH, BAT_HOME_IND)) 

### rstan
source("../rstan8_main.R")

### plot
fit1 <- readRDS("../job_output/fit_rstan8-1.R.rds") 
fit2 <- readRDS("../job_output/fit_rstan8-2.R.rds") 
p1 = plot_bsn0(fit1)
p1
p2 = plot_ubi0(fit2)
p2

#############################
### PARAMETER EXPLORATION ###
#############################

draws1 <- as_tibble(as.matrix(fit1))
cm1 = colMeans(draws1)

M=9#27
alphas = tibble()
for (k in 2:7) {
  ak = tibble(alpha = cm1[paste0("alpha[",1:M,",",k,"]")], bn=1:M, k=k)
  alphas = bind_rows(alphas, ak)
}
pa = alphas %>% ggplot(aes(x=bn,y=alpha)) + 
  facet_wrap(~k) + 
  geom_line() +
  #geom_smooth(se=FALSE) +
  stat_smooth(method = "lm", formula = y ~ x, size = 1, se=FALSE) + #+ I(x^2) + I(x^3)
  geom_point() 
pa

alpha_consts = tibble()
for (kk in 2:7) {
  lma = lm(alpha~bn, data=alphas%>%filter(k==kk))
  temp = tibble(incpt = lma[[1]][[1]], slope=lma[[1]][[2]], k=kk)
  alpha_consts = bind_rows(alpha_consts, temp)
}
alpha_consts

### ETA
etas = tibble()
for (k in 2:7) {
  etak = tibble(eta = cm1[paste0("eta[",1:4,",",k,"]")], k=k)
  etas = bind_rows(etas, etak)
}

# #####################################
# draws <- as_tibble(as.matrix(fit2))
# cm = colMeans(draws)
# 
# betas = tibble()
# for (k in 2:7) {
#   bk = tibble(beta = cm[paste0("beta[",1:9,",",k,"]")], bn=1:9, k=k)
#   betas = bind_rows(betas, bk)
# }
# pb = betas %>% ggplot(aes(x=bn,y=beta)) + 
#   facet_wrap(~k) + 
#   geom_line() +
#   #geom_smooth(se=FALSE) +
#   stat_smooth(method = "lm", formula = y ~ x, size = 1, se=FALSE) + #+ I(x^2) + I(x^3)
#   geom_point() 
# pb




