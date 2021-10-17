# https://avehtari.github.io/modelselection/CV-FAQ.html#1_Where_to_start
# https://mc-stan.org/loo/reference/loo-glossary.html


#source("rstan1_10FoldCVa-a.R")
OUTPUT_FILE = "rstan_1_loo_compare_a1-4_7"

library(tidyverse)
library(rstan)
library(loo)

e1 <- readRDS("job_output/rstan1_10FoldCVa-1.R.rds") 
e2 <- readRDS("job_output/rstan1_10FoldCVa-2.R.rds") 
e3 <- readRDS("job_output/rstan1_10FoldCVa-3.R.rds") 
e4 <- readRDS("job_output/rstan1_10FoldCVa-4.R.rds") 
e7 <- readRDS("job_output/rstan1_10FoldCVa-7.R.rds") 


L = loo_compare(e1, e2, e3, e4, e7)
L
saveRDS(L, file = paste0("./job_output/", OUTPUT_FILE, ".rds"))


# If elpd difference (elpd_diff in loo package) is less than 4, the difference is small. 
# If elpd difference (elpd_diff in loo package) is larger than 4, then 
# compare that difference to standard error of elpd_diff (provided e.g. by loo package). 

# https://avehtari.github.io/modelselection/CV-FAQ.html#11_What_is_the_interpretation_of_ELPD__elpd_loo__elpd_diff
# https://avehtari.github.io/modelselection/CV-FAQ.html#15_How_to_interpret_in_Standard_error_(SE)_of_elpd_difference_(elpd_diff)

#### Interpret
# L <- readRDS("job_output/rstan_1_loo_compare_a1-4.rds")
# A <- rbind(unname(L[,1] + 2*L[,2]), unname(L[,1]), unname(L[,1] - 2*L[,2]))
# colnames(A) <- paste0("model ", 4:1)
# rownames(A) <- c("elpd_diff + 2*se_diff", "elpd_diff", "elpd_diff - 2*se_diff")
# A <- round(A,1)
# A


