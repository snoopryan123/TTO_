
library(tidyverse)
library(ggthemes)
library(latex2exp)
library(splines)
theme_set(theme_bw())
theme_update(text = element_text(size=18))
theme_update(plot.title = element_text(hjust = 0.5))

#####################################
########### OBSERVED DATA ###########
#####################################

### load data
input_file = "../../data/TTO_dataset_510.csv"  
D0 <- read_csv(input_file) %>% filter(!PIT_IS_BAT) 
# D0 <- D0 %>% filter(ORDER_CT <= 3)
D0 <- D0 %>% filter(YEAR >= 2012)
D0 <- D0 %>% filter(BQ>0 & BQ<1 & PQ>0 & PQ<1 )
D0 <- D0 %>% group_by(GAME_ID,PIT_ID) %>% 
  mutate(PITCH_CT = lag(PITCH_COUNT_CUMU, default=0)) %>% ungroup()
D0 <- D0 %>% mutate(EVENT_WOBA_PTS_19 = EVENT_WOBA_19*1000)
# D <- D %>% filter(YEAR == 2019) 
# D <- D %>% filter(2017 <= YEAR & YEAR <= 2019)
# D <- D %>% filter(YEAR == 2018) 
D00 <- D0 %>% filter(2014 <= YEAR & YEAR <= 2018)
D <- D00 %>% filter(ORDER_CT <= 3)
# logit <- function(p) { log(p/(1-p)) }
# X <- as.matrix(D %>% mutate(lBQ=logit(BQ), lPQ=logit(PQ)) %>% select(lBQ, lPQ, HAND_MATCH, BAT_HOME_IND)) 
OUTPUT_FILE = "explore_1" 

# set.seed(12345)
# train_idx = sample(1:nrow(D), size=nrow(D)*0.8)
# D_train = D[train_idx,]
# D_test = D[train_idx,]
D_train = D
D_test = D


m1 = lm(EVENT_WOBA_19 ~ PITCH_CT + 
          std_BQ + std_PQ + HAND_MATCH + BAT_HOME_IND, data=D_train)

m2 = lm(EVENT_WOBA_19 ~ PITCH_CT + (ORDER_CT==2) + (ORDER_CT==3) +
          std_BQ + std_PQ + HAND_MATCH + BAT_HOME_IND, data=D_train)

pitch_ct_spl_df = 4 #4 #7
m3 = lm(EVENT_WOBA_19 ~ bs(PITCH_CT, df=pitch_ct_spl_df) + 
          std_BQ + std_PQ + HAND_MATCH + BAT_HOME_IND, data=D_train)

m4 = lm(EVENT_WOBA_19 ~ bs(PITCH_CT, df=pitch_ct_spl_df) + (ORDER_CT==2) + (ORDER_CT==3) +
          std_BQ + std_PQ + HAND_MATCH + BAT_HOME_IND, data=D_train)

rmse <- function(x,y) { sqrt(mean((x-y)**2)) }
eval_model <- function(m) {
  rmse(D_test$EVENT_WOBA_19, predict(m,D_test))
}

eval_model(m4)
eval_model(m3)

# eval_model(m2)
# eval_model(m1)


# model.matrix(
#    ~ bs(PITCH_CT, df=4) + (ORDER_CT==2) + (ORDER_CT==3) +
#     std_BQ + std_PQ + HAND_MATCH + BAT_HOME_IND, data=D
# )

# model.matrix(
#   ~ PITCH_CT + (ORDER_CT==2) + (ORDER_CT==3) +
#     std_BQ + std_PQ + HAND_MATCH + BAT_HOME_IND, data=D
# )

# cor(D$PITCH_CT, D$ORDER_CT)


# mean((D %>% filter(ORDER_CT==1))$PITCH_CT) # 15
# mean((D %>% filter(ORDER_CT==2))$PITCH_CT) # 51
# mean((D %>% filter(ORDER_CT==3))$PITCH_CT) # 80

plot_df1 = tibble(PITCH_CT = 0:80, ORDER_CT = c(rep(1,16), rep(2,36), rep(3,29)),
                  std_BQ = mean(D$BQ), std_PQ = mean(D$PQ), HAND_MATCH = 1, BAT_HOME_IND = 0)

plot_df2 = tibble(PITCH_CT = 0:80, ORDER_CT = c(rep(1,27), rep(2,27), rep(3,27)),
                  std_BQ = mean(D$BQ), std_PQ = mean(D$PQ), HAND_MATCH = 1, BAT_HOME_IND = 0)

plot_df3 = tibble(PITCH_CT = 0:101, ORDER_CT = c(rep(1,34), rep(2,34), rep(3,34)),
                  std_BQ = mean(D$BQ), std_PQ = mean(D$PQ), HAND_MATCH = 1, BAT_HOME_IND = 0)


plot_lm_overTime1 <- function(m, plot_df=plot_df3) {
  tibble(xwoba = predict(m, plot_df)) %>%
    mutate(x = row_number()) %>%
    ggplot() +
    geom_point(aes(x=x, y=xwoba*1000)) +
    labs(x="pitch count", ) +
    scale_y_continuous(
      name = "expected wOBA",
      breaks=seq(0,1000,by=10)
    )
}

plot_lm_overTime1(m4)

summary(m4)


# plot_lm_overTime1(m3)


# plot_lm_overTime1(m2)
# 
# plot_lm_overTime1(m3)
# 
# plot_lm_overTime1(m1)


#####################################
###### Propensity Score Model #######
#####################################

D_pulled_0 = D00 %>%
  filter(YEAR == 2018) %>% #FIXME # use less rows
  select(GAME_ID, PIT_ID, BATTER_SEQ_NUM, PQ) %>% #PITCH_CT,
  group_by(GAME_ID, PIT_ID) %>%
  slice_tail() %>%
  ungroup() %>%
  mutate(idx = row_number())

D_pulled_1 = tibble(
  BATTER_SEQ_NUM = rep(1:27, nrow(D_pulled_0)),
  idx = c( sapply(1:nrow(D_pulled_0), function(i) rep(i,27)) )
)

D_pulled = D_pulled_1 %>%
  left_join(D_pulled_0 %>% rename(pulled_after_t = BATTER_SEQ_NUM)) %>%
  mutate(STILL_IN_GAME = as.numeric(BATTER_SEQ_NUM <= pulled_after_t))

set.seed(12345)
train_idx = sample(1:nrow(D_pulled), size=nrow(D_pulled)*0.75)
test_idx = setdiff(1:nrow(D_pulled), train_idx)
D_pulled_train = D_pulled[train_idx,]
D_pulled_test = D_pulled[test_idx,]

m1 = glm(STILL_IN_GAME ~ BATTER_SEQ_NUM + PQ, family="binomial", data=D_pulled_train)
m_s3 = glm(STILL_IN_GAME ~ bs(BATTER_SEQ_NUM, df=3), family="binomial", data=D_pulled_train)
m_s4 = glm(STILL_IN_GAME ~ bs(BATTER_SEQ_NUM, df=4), family="binomial", data=D_pulled_train)
m_s5 = glm(STILL_IN_GAME ~ bs(BATTER_SEQ_NUM, df=5), family="binomial", data=D_pulled_train)
m_s6 = glm(STILL_IN_GAME ~ bs(BATTER_SEQ_NUM, df=6), family="binomial", data=D_pulled_train)
m_s7 = glm(STILL_IN_GAME ~ bs(BATTER_SEQ_NUM, df=7), family="binomial", data=D_pulled_train)
m_s8 = glm(STILL_IN_GAME ~ bs(BATTER_SEQ_NUM, df=8), family="binomial", data=D_pulled_train)
m_s9 = glm(STILL_IN_GAME ~ bs(BATTER_SEQ_NUM, df=9) + PQ, family="binomial", data=D_pulled_train)
m_s10 = glm(STILL_IN_GAME ~ bs(BATTER_SEQ_NUM, df=10), family="binomial", data=D_pulled_train)

cel <- function(p,y) {
  mean( -( y*log(p) + (1-y)*log(1-p) ) )
}

test_pullModel <- function(m, dat=D_pulled_test) {
  cel(predict(m, dat, type="response"), dat$STILL_IN_GAME)
}

test_pullModel(m1)
test_pullModel(m_s3)
test_pullModel(m_s4)
test_pullModel(m_s5)
test_pullModel(m_s6)
test_pullModel(m_s7)
test_pullModel(m_s8)
test_pullModel(m_s9)
test_pullModel(m_s10)

### plot best model
m_best = m_s9 #m_s9b 
plot_pulled_df1 = tibble(BATTER_SEQ_NUM = 1:27, PQ=0.315)

tibble(
  p = predict(m_best, plot_pulled_df1, type="response"),
)

plot_StillInGameProb = plot_pulled_df1 %>%
  mutate(p = predict(m_best, plot_pulled_df1, type="response")) %>%
  ggplot() +
  geom_point(aes(x=BATTER_SEQ_NUM, y=p)) +
  scale_x_continuous(name="batter sequence number",
                     breaks=seq(1,27,by=3)) +
  scale_y_continuous(name="probability",
                     breaks=seq(0,1,by=0.1))
plot_StillInGameProb
ggsave("plot_StillInGameProb.png", plot_StillInGameProb, width=8, height=6)



# D_pulled = D00 %>%
#   select(GAME_ID, PIT_ID, PITCH_CT, BATTER_SEQ_NUM) %>%
#   group_by(GAME_ID, PIT_ID) %>%
#   mutate(PULLED_AFTER = ifelse(row_number() == n(), 1, 0)) %>%
#   ungroup()
# 
# set.seed(12345)
# train_idx = sample(1:nrow(D_pulled), size=nrow(D_pulled)*0.75)
# test_idx = setdiff(1:nrow(D_pulled), train_idx)
# D_pulled_train = D_pulled[train_idx,]
# D_pulled_test = D_pulled[test_idx,]
# 
# m_s9 = glm(PULLED_AFTER ~ bs(BATTER_SEQ_NUM, df=9), family="binomial", data=D_pulled_train)
#
# m1 = glm(PULLED_AFTER ~ BATTER_SEQ_NUM, family="binomial", data=D_pulled_train)
# m_s3 = glm(PULLED_AFTER ~ bs(BATTER_SEQ_NUM, df=3), family="binomial", data=D_pulled_train)
# m_s4 = glm(PULLED_AFTER ~ bs(BATTER_SEQ_NUM, df=4), family="binomial", data=D_pulled_train)
# m_s5 = glm(PULLED_AFTER ~ bs(BATTER_SEQ_NUM, df=5), family="binomial", data=D_pulled_train)
# m_s6 = glm(PULLED_AFTER ~ bs(BATTER_SEQ_NUM, df=6), family="binomial", data=D_pulled_train)
# m_s7 = glm(PULLED_AFTER ~ bs(BATTER_SEQ_NUM, df=7), family="binomial", data=D_pulled_train)
# m_s8 = glm(PULLED_AFTER ~ bs(BATTER_SEQ_NUM, df=8), family="binomial", data=D_pulled_train)
# m_s9 = glm(PULLED_AFTER ~ bs(BATTER_SEQ_NUM, df=9), family="binomial", data=D_pulled_train)
# m_s10 = glm(PULLED_AFTER ~ bs(BATTER_SEQ_NUM, df=10), family="binomial", data=D_pulled_train)
# 
# m1a = glm(PULLED_AFTER ~ PITCH_CT, family="binomial", data=D_pulled_train)
# m_s3a = glm(PULLED_AFTER ~ bs(PITCH_CT, df=3), family="binomial", data=D_pulled_train)
# m_s4a = glm(PULLED_AFTER ~ bs(PITCH_CT, df=4), family="binomial", data=D_pulled_train)
# m_s5a = glm(PULLED_AFTER ~ bs(PITCH_CT, df=5), family="binomial", data=D_pulled_train)
# m_s6a = glm(PULLED_AFTER ~ bs(PITCH_CT, df=6), family="binomial", data=D_pulled_train)
# m_s7a = glm(PULLED_AFTER ~ bs(PITCH_CT, df=7), family="binomial", data=D_pulled_train)
# 
# m1b = glm(PULLED_AFTER ~ BATTER_SEQ_NUM + PITCH_CT, family="binomial", data=D_pulled_train)
# m_s3b = glm(PULLED_AFTER ~ bs(BATTER_SEQ_NUM, df=3) + bs(PITCH_CT, df=3), family="binomial", data=D_pulled_train)
# m_s4b = glm(PULLED_AFTER ~ bs(BATTER_SEQ_NUM, df=4) + bs(PITCH_CT, df=4), family="binomial", data=D_pulled_train)
# m_s5b = glm(PULLED_AFTER ~ bs(BATTER_SEQ_NUM, df=5) + bs(PITCH_CT, df=5), family="binomial", data=D_pulled_train)
# m_s6b = glm(PULLED_AFTER ~ bs(BATTER_SEQ_NUM, df=6) + bs(PITCH_CT, df=6), family="binomial", data=D_pulled_train)
# m_s7b = glm(PULLED_AFTER ~ bs(BATTER_SEQ_NUM, df=7) + bs(PITCH_CT, df=7), family="binomial", data=D_pulled_train)
# m_s8b = glm(PULLED_AFTER ~ bs(BATTER_SEQ_NUM, df=8) + bs(PITCH_CT, df=8), family="binomial", data=D_pulled_train)
# m_s9b = glm(PULLED_AFTER ~ bs(BATTER_SEQ_NUM, df=9) + bs(PITCH_CT, df=9), family="binomial", data=D_pulled_train)
# 
# cel <- function(p,y) {
#   mean( -( y*log(p) + (1-y)*log(1-p) ) )
# }
# 
# test_pullModel <- function(m, dat=D_pulled_test) {
#   cel(predict(m, dat, type="response"), dat$PULLED_AFTER)
# }
# 
# test_pullModel(m_s9)
# test_pullModel(m1a)
# test_pullModel(m_s3a)
# test_pullModel(m_s4a)
# test_pullModel(m_s5a)
# test_pullModel(m_s6a)
# test_pullModel(m_s7a)
# 
# test_pullModel(m1b)
# test_pullModel(m_s3b)
# test_pullModel(m_s4b)
# test_pullModel(m_s5b)
# test_pullModel(m_s6b)
# test_pullModel(m_s7b)
# test_pullModel(m_s8b)
# test_pullModel(m_s9b)

# ### plot best model
# m_best = m_s9 #m_s9b 
# 
# plot_pulled_df1 = tibble(PITCH_CT = 1:20, BATTER_SEQ_NUM = 1)
# plot_pulled_df2 = tibble(PITCH_CT = 40, BATTER_SEQ_NUM = 10:18)
# # plot_pulled_df2 = tibble(PITCH_CT = 80, BATTER_SEQ_NUM = 19:27)
# plot_pulled_df3 = tibble(PITCH_CT = 100, BATTER_SEQ_NUM = 19:27)
# plot_pulled_df4 = tibble(PITCH_CT = 120, BATTER_SEQ_NUM = 28:34)
# 
# plot(
#   predict(m_best, plot_pulled_df1, type="response")
# )
# plot(
#   predict(m_best, plot_pulled_df2, type="response")
# )
# plot(
#   predict(m_best, plot_pulled_df3, type="response")
# )
# plot(
#   predict(m_best, plot_pulled_df4, type="response")
# )



###### PI ######

# lambda = predict(m_best, tibble(BATTER_SEQ_NUM=1:max(D00$BATTER_SEQ_NUM)), type="response")
# PI = 1 - c(0, cumsum(lambda)[1:26])
# PI
# plot(PI)

# PI = predict(m_best, tibble(BATTER_SEQ_NUM=1:27), type="response")
# plot(PI)



# ###### XGBoost Propensity Score Model #######
# 
# library(xgboost)
# 
# ### xgboost with just BATTER_SEQ_NUM
# train_set_xgb = D_pulled_train %>% select(BATTER_SEQ_NUM, PULLED_AFTER)
# test_set_xgb1 = D_pulled_test %>% select(BATTER_SEQ_NUM, PULLED_AFTER)
# train_set_xgb_ = xgboost::xgb.DMatrix(
#   model.matrix(~ . + 0, data = train_set_xgb %>% select(-PULLED_AFTER)), 
#   label = train_set_xgb$PULLED_AFTER)
# m_x1 = xgb.train(
#   data = train_set_xgb_,
#   objective = "binary:logistic",
#   max.depth = 2,
#   verbose = 2,
#   nrounds = 100
# )
# 
# ### xgboost with just PITCH_CT
# train_set_xgb = D_pulled_train %>% select(PITCH_CT, PULLED_AFTER)
# test_set_xgb2 = D_pulled_test %>% select(PITCH_CT, PULLED_AFTER)
# train_set_xgb_ = xgboost::xgb.DMatrix(
#   model.matrix(~ . + 0, data = train_set_xgb %>% select(-PULLED_AFTER)), 
#   label = train_set_xgb$PULLED_AFTER)
# m_x2 = xgb.train(
#   data = train_set_xgb_,
#   objective = "binary:logistic",
#   max.depth = 2,
#   verbose = 2,
#   nrounds = 100
# )
# 
# ### xgboost with both BATTER_SEQ_NUM and PITCH_CT
# train_set_xgb = D_pulled_train %>% select(BATTER_SEQ_NUM, PITCH_CT, PULLED_AFTER)
# test_set_xgb3 = D_pulled_test %>% select(BATTER_SEQ_NUM, PITCH_CT, PULLED_AFTER)
# train_set_xgb_ = xgboost::xgb.DMatrix(
#   model.matrix(~ . + 0, data = train_set_xgb %>% select(-PULLED_AFTER)), 
#   label = train_set_xgb$PULLED_AFTER)
# m_x3 = xgb.train(
#   data = train_set_xgb_,
#   objective = "binary:logistic",
#   max.depth = 2,
#   verbose = 2,
#   nrounds = 100
# )
# 
# ### xgboost with both BATTER_SEQ_NUM and PITCH_CT
# train_set_xgb = D_pulled_train %>% select(BATTER_SEQ_NUM, PITCH_CT, PULLED_AFTER)
# test_set_xgb3 = D_pulled_test %>% select(BATTER_SEQ_NUM, PITCH_CT, PULLED_AFTER)
# train_set_xgb_ = xgboost::xgb.DMatrix(
#   model.matrix(~ . + 0, data = train_set_xgb %>% select(-PULLED_AFTER)), 
#   label = train_set_xgb$PULLED_AFTER)
# m_x4 = xgb.train(
#   data = train_set_xgb_,
#   objective = "binary:logistic",
#   max.depth = 5,
#   verbose = 2,
#   nrounds = 100
# )
# 
# test_pullModel_xgb <- function(xgb, test_set_xgb) {
#   test_set_xgb_ = xgboost::xgb.DMatrix(
#     model.matrix(~ . + 0, data = test_set_xgb %>% select(-PULLED_AFTER)))
#   cel(predict(xgb, test_set_xgb_), test_set_xgb$PULLED_AFTER)
# }
# 
# 
# test_pullModel_xgb(m_x1, test_set_xgb1) 
# test_pullModel_xgb(m_x2, test_set_xgb2) 
# test_pullModel_xgb(m_x3, test_set_xgb3) 
# test_pullModel_xgb(m_x4, test_set_xgb3) 
# 
# ### plot best xgb model
# xgb_best = m_x3
# 
# plot_pulled_xgbDf1 = xgboost::xgb.DMatrix(
#   model.matrix(~ . + 0 , data = plot_pulled_df1)
# )
# predict(xgb_best, plot_pulled_xgbDf1) ### not working... why??



#####################################
######  #######
#####################################

D$w = 1/predict(m_best, D, type="response")
pitch_ct_spl_df = 4 #4 #7

m3a = lm(EVENT_WOBA_19 ~ bs(PITCH_CT, df=pitch_ct_spl_df) + 
          std_BQ + std_PQ + HAND_MATCH + BAT_HOME_IND, data=D,
         weights = D$w)

m4a = lm(EVENT_WOBA_19 ~ bs(PITCH_CT, df=pitch_ct_spl_df) + (ORDER_CT==2) + (ORDER_CT==3) +
          std_BQ + std_PQ + HAND_MATCH + BAT_HOME_IND, data=D,
         weights = D$w)

plot_df_4 = tibble(
  # PITCH_CT = 0:80, ORDER_CT = c(rep(1,27), rep(2,27), rep(3,27)),
  PITCH_CT = 0:101, ORDER_CT = c(rep(1,34), rep(2,34), rep(3,34)),
  # PITCH_CT = 0:119, ORDER_CT = c(rep(1,40), rep(2,40), rep(3,40)),
  std_BQ = mean(D$BQ), std_PQ = mean(D$PQ), HAND_MATCH = 1, BAT_HOME_IND = 0
)

plot_lm_overTime1(m3, plot_df=plot_df_4)
plot_lm_overTime1(m3a, plot_df=plot_df_4)

plot_lm_overTime1(m4, plot_df=plot_df_4)
plot_lm_overTime1(m4a, plot_df=plot_df_4)



#####################################
###### Propensity Score Model #######
#####################################

# yrs = 2014:2018
yrs_list = list(
  2014:2019, 2017:2019, 2014:2016, 2014, 2015, 2016, 2017, 2018, 2019
)
for (i in 1:length(yrs_list)) {
  # yrs = 2019
  yrs = yrs_list[[i]]
  D1 <- D0 %>% filter(YEAR %in% yrs) %>% filter(ORDER_CT <= 3)
  D1$w = 1/predict(m_best, D1, type="response")
  pitch_ct_spl_df = 4 #4 #7
  bsn_spl_df = 4
  
  print(yrs)
  
  m_uw = lm(
    EVENT_WOBA_19 ~ 
      # bs(PITCH_CT, df=pitch_ct_spl_df) + (ORDER_CT==2) + (ORDER_CT==3) +
      bs(BATTER_SEQ_NUM, df=bsn_spl_df) +
      std_BQ + std_PQ + HAND_MATCH + BAT_HOME_IND, 
    data=D1
  )
  m_w = lm(
    EVENT_WOBA_19 ~ 
      # bs(PITCH_CT, df=pitch_ct_spl_df) + (ORDER_CT==2) + (ORDER_CT==3) +
      bs(BATTER_SEQ_NUM, df=bsn_spl_df) +
      std_BQ + std_PQ + HAND_MATCH + BAT_HOME_IND, 
    weights = D1$w,
    data=D1
  )
  
  plot_df_1 = tibble(
    BATTER_SEQ_NUM = 1:27, 
    std_BQ = mean(D1$BQ), std_PQ = mean(D1$PQ), HAND_MATCH = 1, BAT_HOME_IND = 0
  )
  
  p_uw_1 = plot_lm_overTime1(m_uw, plot_df=plot_df_1)
  p_w_1 = plot_lm_overTime1(m_w, plot_df=plot_df_1)
  
  p_uw_1
  p_w_1
  
  yrs_str = str_remove_all( paste(yrs, collapse=''), '20')
  ggsave(paste0("p_1_", yrs_str, "_uw_", "_.png"), p_uw_1, width=8, height=6)
  ggsave(paste0("p_1_", yrs_str, "_w_", "_.png"), p_w_1, width=8, height=6)

  # plot_df_1 = tibble(
  #   PITCH_CT = 0:80, ORDER_CT = c(rep(1,27), rep(2,27), rep(3,27)),
  #   std_BQ = mean(D1$BQ), std_PQ = mean(D1$PQ), HAND_MATCH = 1, BAT_HOME_IND = 0
  # )
  # plot_df_2 = tibble(
  #   PITCH_CT = 0:101, ORDER_CT = c(rep(1,34), rep(2,34), rep(3,34)),
  #   std_BQ = mean(D1$BQ), std_PQ = mean(D1$PQ), HAND_MATCH = 1, BAT_HOME_IND = 0
  # )
  # plot_df_3 = tibble(
  #   PITCH_CT = 0:119, ORDER_CT = c(rep(1,40), rep(2,40), rep(3,40)),
  #   std_BQ = mean(D1$BQ), std_PQ = mean(D1$PQ), HAND_MATCH = 1, BAT_HOME_IND = 0
  # )
  # 
  # p_uw_1 = plot_lm_overTime1(m_uw, plot_df=plot_df_1)
  # p_w_1 = plot_lm_overTime1(m_w, plot_df=plot_df_1)
  # 
  # p_uw_2 = plot_lm_overTime1(m_uw, plot_df=plot_df_2)
  # p_w_2 = plot_lm_overTime1(m_w, plot_df=plot_df_2)
  # 
  # p_uw_3 = plot_lm_overTime1(m_uw, plot_df=plot_df_3)
  # p_w_3 = plot_lm_overTime1(m_w, plot_df=plot_df_3)
  # 
  # p_uw_1
  # p_w_1
  # p_uw_2
  # p_w_2
  # p_uw_3
  # p_w_3
  # 
  # yrs_str = str_remove_all( paste(yrs, collapse=''), '20')
  # ggsave(paste0("p_1_", yrs_str, "_uw_", "_.png"), p_uw_1, width=8, height=6)
  # ggsave(paste0("p_1_", yrs_str, "_w_", "_.png"), p_w_1, width=8, height=6)
  # ggsave(paste0("p_2_", yrs_str, "_uw_", "_.png"), p_uw_2, width=8, height=6)
  # ggsave(paste0("p_2_", yrs_str, "_w_", "_.png"), p_w_2, width=8, height=6)
  # ggsave(paste0("p_3_", yrs_str, "_uw_", "_.png"), p_uw_3, width=8, height=6)
  # ggsave(paste0("p_3_", yrs_str, "_w_", "_.png"), p_w_3, width=8, height=6)
}



