

library("rstan")

# Prepare data 
url <- "http://stat.columbia.edu/~gelman/arm/examples/arsenic/wells.dat"
wells <- read.table(url)
wells$dist100 <- with(wells, dist / 100)
X <- model.matrix(~ dist100 + arsenic, wells)
y = wells$switch
standata <- list(y = y, X = X, N = nrow(X), P = ncol(X))

# seed
seed = 12345
set.seed(seed)
# compile .stan file
file1 = 'loo_ex.stan' #FIXME
model1 <- stan_model(file = file1, model_name = file1)

# K-fold cross validation
K = 2
fold <- kfold_split_random(K = K, N = length(y))
# Prepare a matrix with the number of post-warmup iterations by number of observations
log_pd_kfold <- matrix(nrow = 1000, ncol = length(y))
# Loop over the folds
for(k in 1:K){
  data_train <- list(N = length(y[fold != k]),
                     P = ncol(X),
                     X = X[fold != k,],
                     y = y[fold != k]) 
  data_test <- list(N = length(y[fold == k]),
                    P = ncol(X),
                    X = X[fold == k,],
                    y = y[fold == k]) 
  fit1 <- sampling(model1, 
                  data = data_train, 
                  iter = 2000, 
                  chains = 1,
                  seed = seed)
  gen_test1 <- gqs(model1, draws = as.matrix(fit1), data= data_test, seed=seed)
  log_pd_kfold[, fold == k] <- extract_log_lik(gen_test1)
}

elpd_kfold <- elpd(log_pd_kfold)