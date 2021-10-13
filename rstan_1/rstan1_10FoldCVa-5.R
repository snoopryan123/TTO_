
source("rstan1_10FoldCVa-a.R")

OUTPUT_FILE = "rstan1_10FoldCVa-5.R" #FIXME
X = X5 #FIXME

elpd_kfold <- compute_elpd_kfold(X)
saveRDS(elpd_kfold, file = paste0("./job_output/", OUTPUT_FILE, ".rds"))
#e <- readRDS("job_output/rstan1_10FoldCVa-5.R.rds") 