
#source("rstan1_10FoldCVa-a.R")

library(tidyverse)
library(rstan)
library(loo)
print()

e1 <- readRDS("job_output/rstan1_10FoldCVa-1.R.rds") 
e2 <- readRDS("job_output/rstan1_10FoldCVa-2.R.rds") 
e3 <- readRDS("job_output/rstan1_10FoldCVa-3.R.rds") 
e4 <- readRDS("job_output/rstan1_10FoldCVa-4.R.rds") 
e5 <- readRDS("job_output/rstan1_10FoldCVa-5.R.rds") 

L = loo_compare(e1, e2, e3, e4, e5)
L