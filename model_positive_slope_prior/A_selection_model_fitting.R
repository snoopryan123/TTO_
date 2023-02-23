
source("A_getData.R") ### get observed data 

### "full" dataframe, including observed and unobserved at-bats of batters 1-27 for each game
df.pit_games = D0 %>% distinct(GAME_ID, PIT_ID)
df.full = df.pit_games[rep(seq_len(nrow(df.pit_games)), each = 27), ]
df.full$BATTER_SEQ_NUM = rep(1:27, nrow(df.pit_games))
df.full = df.full %>% 
  left_join(
    D0 %>% 
      distinct(GAME_ID, PIT_ID, BATTER_SEQ_NUM, PQ, EVENT_WOBA_19) %>%
      mutate(observed = 1)
  ) %>%
  mutate(observed = ifelse(is.na(observed), 0, observed)) %>%
  group_by(GAME_ID, PIT_ID) %>%
  mutate(mean_game_woba = mean(EVENT_WOBA_19, na.rm=T)) %>%
  ungroup() %>%
  mutate(
    mean_game_woba = zoo::na.locf(mean_game_woba),
    PQ = zoo::na.locf(PQ),
  )

######################################
### XGBoost Propensity Score Model ###
######################################

source("A_selection_model_functions.R")

################################
### XGBoost Parameter Tuning ###
################################

library(dials)
### Ben Baldwin's param tuning from https://www.opensourcefootball.com/posts/2021-04-13-creating-a-model-from-scratch-using-xgboost-in-r/
{
  ###############################################################
  grid_size = 20 #1 #20 #40
  go_xgb_param_grid = grid_latin_hypercube(
    dials::loss_reduction(),
    #################
    dials::min_n(),
    dials::finalize(dials::mtry(), df.full), # this finalize thing is because mtry depends on # of columns in data
    dials::tree_depth(),
    dials::learn_rate(range = c(-1.5, -0.5), trans = scales::log10_trans()),
    sample_size = dials::sample_prop(),
    #################
    # dials::min_n(range=c(20,30)),
    # dials::mtry(range = c(round(length(df.full) * 0.8), length(df.full))),
    # dials::tree_depth(range=c(3,4)),
    # dials::learn_rate(range = c(-1.5, -1), trans = scales::log10_trans()),
    # sample_size = dials::sample_prop(range = c(0.8, 1)),
    #################
    size = grid_size
  ) %>% mutate(
    mtry = mtry / length(df.full),
    monotone_constraints = XGB_MONOTONE_CONSTRAINTS
  ) %>% rename(
    eta = learn_rate,
    gamma = loss_reduction,
    subsample = sample_size,
    colsample_bytree = mtry,
    max_depth = tree_depth,
    min_child_weight = min_n
  )
  go_xgb_param_grid
  # function to perform xgb.cv for a given row in a hyperparameter grid
  get_row <- function(row) {
    params <-
      list(
        booster = "gbtree",
        objective = "binary:logistic",
        eval_metric = c("logloss"),
        eta = row$eta,
        gamma = row$gamma,
        subsample = row$subsample,
        colsample_bytree = row$colsample_bytree,
        max_depth = row$max_depth,
        min_child_weight = row$min_child_weight,
        monotone_constraints = row$monotone_constraints
      )
    
    ### randomly split half-and-half into training and validation set, by games
    set.seed(32380) 
    all_game_idxs = unique(df.full$GAME_ID)
    fold1_game_idxs = sample(all_game_idxs, size=round(length(all_game_idxs)/2), replace=FALSE)
    df.folds = df.full %>% 
      mutate(
        fold1 = GAME_ID %in% fold1_game_idxs
      ) 
    folds = list(Fold1 = which(df.folds$fold1), Fold2 = which(!df.folds$fold1))
    
    # fold1 = sort(sample(1:nrow(df.train), replace=FALSE, size=0.5*nrow(df.train)))
    # fold2 = setdiff(1:nrow(df.train), fold1)
    # folds = list(Fold1 = fold1, Fold2 = fold2)
    xgb.cv.df.train = get_xgb_train_DMatrix(df.full)
    
    # do the cross validation
    xgb_cv_model <- xgb.cv(
      data = xgb.cv.df.train,
      params = params,
      folds = folds,
      metrics = list("logloss"),
      nrounds = 15000,
      early_stopping_rounds = 50,
      print_every_n = 10
    )

    # bundle up the results together for returning
    output <- params
    output$iter <- xgb_cv_model$best_iteration
    output$logloss <- xgb_cv_model$evaluation_log[output$iter]$test_logloss_mean

    row_result <- bind_rows(output)

    return(row_result)
  }
  # get results
  results = map_df(1:nrow(go_xgb_param_grid), function(x) {
    print(paste0("row ", x)); return(get_row(go_xgb_param_grid %>% dplyr::slice(x)))
  })
  # visualize param tuning
  results %>%
    dplyr::select(logloss, eta, gamma, subsample, colsample_bytree, max_depth, min_child_weight) %>%
    tidyr::pivot_longer(
      eta:min_child_weight,
      values_to = "value",
      names_to = "parameter"
    ) %>%
    ggplot(aes(value, logloss, color = parameter)) +
    geom_point(alpha = 0.8, show.legend = FALSE, size = 3) +
    facet_wrap(~parameter, scales = "free_x") +
    labs(x = NULL, y = "logloss") +
    theme_minimal()
  # re-tune, with better param range based on these plots...
  # Collect best parameters
  results %>% arrange(logloss) %>% select(eta, subsample, colsample_bytree, max_depth, logloss, min_child_weight, iter)
  best_model <- results %>% arrange(logloss) %>% slice_head()
  best_model
  params <- list(
    booster = "gbtree",
    objective = "binary:logistic",
    eval_metric = c("logloss"),
    eta = best_model$eta,
    gamma = best_model$gamma,
    subsample = best_model$subsample,
    colsample_bytree = best_model$colsample_bytree,
    max_depth = best_model$max_depth,
    min_child_weight = best_model$min_child_weight,
    monotone_constraints = best_model$monotone_constraints,
    nrounds = best_model$iter
  )
  params
  list.save(params, "xgb_selection_params.yaml")
  ###############################################################
}

################################
### XGBoost Propensity Model ###
################################

### final propensity model
xgb_params = list.load("xgb_selection_params.yaml")
m = train_xgb(df.full, xgb_params, xgb_nrounds)
xgb.save(m, "xgb_selection_model.xgb")

##################################
### XGBoost Model Sanity Plots ###
##################################

# pqs = seq(min(df.full$PQ[df.full$PQ>0]), max(df.full$PQ), length.out=10)
# mwgs = seq(min(df.full$mean_game_woba[df.full$mean_game_woba>0]), 
#            max(df.full$mean_game_woba), length.out=10)
# version_ = "a"
pqs = quantile(df.full$PQ, 1:8/8)
mwgs = quantile(df.full$mean_game_woba, 1:8/8)
version_ = "b"

### plot selection model: pitcher quality vs. batter sequence number
df.plot.1 = tibble()
for (pq in pqs) {
  df.plot.1 = bind_rows(
    df.plot.1, 
    tibble(
      BATTER_SEQ_NUM = 1:27,
      PQ = pq,
      mean_game_woba = mean(df.full$mean_game_woba)
    )
  )
}
df.plot.1$pi = predict_probs_xgb(m, df.plot.1) 
plot.1 = df.plot.1 %>%
  mutate(
    pq_ = factor(round(PQ,2)),
    pi_ = round(pi,2)
  ) %>%
  ggplot(aes(x=BATTER_SEQ_NUM, y = factor(pq_))) +
  geom_tile(aes(fill=pi)) +
  geom_text(aes(label=pi_), color="white") +
  ylab("pitcher quality") +
  xlab("batter sequence number") +
  scale_x_continuous(breaks=seq(3,27,by=3)) 
# plot.1
ggsave(paste0("selection_bias_model_plot_1",version_,".png"), plot.1, width=15, height=5)

### plot selection model: mean_game_woba vs. batter sequence number
df.plot.2 = tibble()
for (mgw in mwgs) {
  df.plot.2 = bind_rows(
    df.plot.2, 
    tibble(
      BATTER_SEQ_NUM = 1:27,
      PQ = mean(df.full$PQ),
      mean_game_woba = mgw
    )
  )
}
df.plot.2$pi = predict_probs_xgb(m, df.plot.2) 
plot.2 = df.plot.2 %>%
  mutate(
    mgw_ = factor(round(mean_game_woba, 2)),
    pi_ = round(pi,2)
  ) %>%
  ggplot(aes(x=BATTER_SEQ_NUM, y = factor(mgw_))) +
  geom_tile(aes(fill=pi)) +
  geom_text(aes(label=pi_), color="white") +
  ylab("mean game wOBA") +
  xlab("batter sequence number") +
  scale_x_continuous(breaks=seq(3,27,by=3)) 
# plot.2
ggsave(paste0("selection_bias_model_plot_2",version_,".png"), plot.2, width=15, height=5)

### plot selection model: pitcher quality vs. mean_game_woba, averaged over batter sequence number
df.plot.3 = tibble()
for (mgw in mwgs) {
  for (pq in pqs) {
    df.plot.3 = bind_rows(
      df.plot.3, 
      tibble(
        BATTER_SEQ_NUM = 1:27,
        PQ = pq,
        mean_game_woba = mgw
      )
    )
  }
}
df.plot.3$pi = predict_probs_xgb(m, df.plot.3) 
df.plot.3 = df.plot.3 %>%
  group_by(PQ, mean_game_woba) %>%
  summarise(pi = mean(pi))
plot.3 = df.plot.3 %>%
  mutate(
    pq_ = factor(round(PQ, 2)),
    mgw_ = factor(round(mean_game_woba, 2)),
    pi_ = round(pi,2)
  ) %>%
  ggplot(aes(x=factor(pq_), y = factor(mgw_))) +
  geom_tile(aes(fill=pi)) +
  geom_text(aes(label=pi_), color="white") +
  ylab("mean game wOBA") +
  xlab("pitcher quality") 
plot.3
ggsave(paste0("selection_bias_model_plot_3",version_,".png"), plot.3, width=15, height=5)




