
######################################
### XGBoost Propensity Score Model ###
######################################

### propensity score model: P(observed|t,pq,mean_game_woba)
library(xgboost)
library(rlist)

XGB_FEATURES = c("BATTER_SEQ_NUM", "PQ", "mean_game_woba")
XGB_MONOTONE_CONSTRAINTS = "(0, -1,-1)"

get_xgb_train_DMatrix <- function(dataset) {
  train_features_xgb = dataset %>% select(all_of(XGB_FEATURES))
  train_labels_xgb = dataset$observed
  train_set_xgbDM = xgboost::xgb.DMatrix(
    model.matrix(~ . + 0, data = train_features_xgb), 
    label = train_labels_xgb
  )
  return(train_set_xgbDM)
}

train_xgb <- function(train_set, params, nrounds, watchSet=FALSE, print_every_n=10, param_tuning=F) {
  train_set_xgbDM =  get_xgb_train_DMatrix(train_set) 
  
  if (is.data.frame(watchSet)) { ### validation set evaluation
    val_set_xgbDM = get_xgb_train_DMatrix(watchSet)
    watchlist <- list(train=train_set_xgbDM, validation=val_set_xgbDM)
  } else { ### just train set for watchlist
    watchlist <- list(train=train_set_xgbDM)
  }
  
  set.seed(45554451) #########################
  if (param_tuning) { ### if tuning parameters
    xgb <- xgb.train( 
      data = train_set_xgbDM, 
      watchlist = watchlist,
      params = params, 
      nrounds = 15000,
      early_stopping_rounds = 50,
      print_every_n = print_every_n,
      verbose = 2
    )
  } else {
    xgb <- xgb.train( 
      data = train_set_xgbDM, 
      watchlist = watchlist,
      params = params, 
      nrounds = nrounds, 
      print_every_n = print_every_n,
      verbose = 2
    )
  }
  return(xgb)
}

predict_probs_xgb <-  function(xgb, test_set) {
  test_features_xgb = test_set %>% select(all_of(XGB_FEATURES))
  test_set_xgbDM = xgboost::xgb.DMatrix(
    model.matrix(~ . + 0, data = test_features_xgb)
  )
  xgb_pred = predict(xgb, test_set_xgbDM)
  return(xgb_pred)
}
  


