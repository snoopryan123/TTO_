
#source("rstan1_10FoldCVa-a.R")
OUTPUT_FILE = "rstan_1_loo_compare_a1-5"

library(tidyverse)
library(rstan)
library(loo)

e1 <- readRDS("job_output/rstan1_10FoldCVa-1.R.rds") 
e2 <- readRDS("job_output/rstan1_10FoldCVa-2.R.rds") 
e3 <- readRDS("job_output/rstan1_10FoldCVa-3.R.rds") 
e4 <- readRDS("job_output/rstan1_10FoldCVa-4.R.rds") 
e5 <- readRDS("job_output/rstan1_10FoldCVa-5.R.rds") 

L = loo_compare(e1, e2, e3, e4, e5)
L
saveRDS(L, file = paste0("./job_output/", OUTPUT_FILE, ".rds"))
#e <- readRDS("job_output/rstan_1_loo_compare_a1-5.rds") 
