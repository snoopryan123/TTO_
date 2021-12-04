library(tidyverse)

### create moving avg. estimator for batter and pitcher quality

input_filename = "design_matrix2_3.csv" #FIXME
output_filename = "design_matrix2_4.csv" #FIXME
D0 <- read_csv(input_filename)

#######################################
########### PITCHER QUALITY ###########
#######################################

########### theta_bar_0 for pitchers ########### 
CUTOFF_WOBA_APP = 1 #FIXME #50

pit_szns = D0 %>% group_by(YEAR, PIT_ID) %>% 
  summarise(num_woba_app = sum(WOBA_APP)) %>% 
  ungroup() %>% 
  arrange(PIT_ID,YEAR) %>%
  group_by(PIT_ID) %>% 
  mutate(num_woba_app_prev = lag(num_woba_app,default=0),
         use_prev_szn_woba = num_woba_app_prev >= CUTOFF_WOBA_APP
    #PIT_PREV_SZN = lag(YEAR,default=0),
  ) %>%
  left_join(D0 %>% select(YEAR,PIT_ID,WOBA_FINAL_PIT_19) %>% distinct()) %>%
  mutate(woba_prev = lag(WOBA_FINAL_PIT_19)) %>%
  ungroup()
pit_szns

year_avg_woba_df = pit_szns %>% 
  #filter(num_woba_app >= CUTOFF_WOBA_APP) %>% 
  group_by(YEAR) %>% 
  summarise(year_avg_woba = mean(WOBA_FINAL_PIT_19)) %>%
  mutate(prev_year_avg_woba = lag(year_avg_woba))
year_avg_woba_df

pit_szns1 = pit_szns %>% left_join(year_avg_woba_df %>% select(YEAR,prev_year_avg_woba)) %>%
  mutate(theta_bar_0 = ifelse(use_prev_szn_woba, woba_prev, prev_year_avg_woba)) 
pit_szns1

theta_bar_0_df = pit_szns1 %>% select(YEAR,PIT_ID,theta_bar_0)
theta_bar_0_df

########### sigma for pitchers ########### 

cumsd <- function(x) {
  TTR::runSD(x, n = 1, cumulative = TRUE)
}

pit_szns2 = pit_szns1 %>% select(-c(use_prev_szn_woba,prev_year_avg_woba,theta_bar_0)) %>% #select(YEAR,PIT_ID,WOBA_FINAL_PIT_19,woba_prev) %>% 
  group_by(PIT_ID) %>%
  mutate(has_2_prev_szns = n() > 2 & row_number() > 2) %>%
  mutate(sigma = ifelse(has_2_prev_szns, c(-1, cumsd(woba_prev[2:n()])), NA)) %>%
  ungroup()
pit_szns2
#View(pit_szns2)  

med_sig = pit_szns2 %>% group_by(YEAR)  %>% drop_na() %>% 
  summarise(med_sig = median(sigma)) 
med_sig[med_sig$YEAR <= 2009,]$med_sig = NA #want 2010-2019
med_sig

pit_szns3 = pit_szns2 %>% left_join(med_sig) %>%
  mutate(sigma = ifelse(is.na(sigma), med_sig, sigma)) %>%
  select(-c(med_sig))
pit_szns3

sigma_df = theta_bar_0_df %>% left_join(pit_szns3 %>% select(YEAR,PIT_ID,sigma))
sigma_df

########### tau for pitchers ########### 

pit_games = D0 %>% group_by(YEAR,GAME_ID,PIT_ID,DATE) %>%
  summarise(game_avg_woba = mean(EVENT_WOBA_19)) %>%
  ungroup() %>%
  arrange(PIT_ID,YEAR)
pit_games

pit_szns10 = pit_games %>% group_by(YEAR,PIT_ID) %>%
  summarise(sd_game2game = sd(game_avg_woba)) %>%
  ungroup() %>%
  arrange(PIT_ID,YEAR) 
pit_szns10


pit_szns11 = pit_szns10 %>%
  group_by(PIT_ID) %>%
  mutate(tau = lag(sd_game2game)) %>%
  ungroup()
pit_szns11

med_tau = pit_szns11 %>% group_by(YEAR) %>% drop_na() %>%
  summarise(med_tau = median(tau)) %>%
  mutate(med_tau = ifelse(YEAR >= 2010, med_tau, NA)) #want 2010-2019
med_tau

pit_szns12 = pit_szns11 %>% left_join(med_tau) %>%
  mutate(tau = ifelse(is.na(tau), med_tau, tau)) %>%
  select(-c(sd_game2game,med_tau))
pit_szns12

tau_df = sigma_df %>% left_join(pit_szns12) 
tau_df = tau_df %>% filter(YEAR >= 2010) # want years 2010-2019
tau_df

########### pitcher quality ########### 

pit_games1 = pit_games %>% filter(YEAR >= 2010) %>%
  rename(theta = game_avg_woba) %>% left_join(tau_df)
pit_games1

pit_games2 = pit_games1 %>% 
  relocate(theta, .after=tau) %>%
  arrange(PIT_ID,DATE) %>%
  group_by(YEAR,PIT_ID) %>%
  mutate(i = row_number(),
         sum_1_im1_theta = cumsum(lag(theta, default=0))) %>%
  ungroup() %>%
  mutate(PQ = ( (1/tau^2)*sum_1_im1_theta + (1/sigma^2)*theta_bar_0 )/( (i-1)/(tau^2) + (1/sigma^2) ))
pit_games2

pit_games3 = pit_games2 %>% select(YEAR,DATE,PIT_ID,PQ) 
pit_games3

pit_games4 = pit_games3 %>% left_join(pit_szns %>% select(YEAR,PIT_ID,num_woba_app, num_woba_app_prev))
pit_games4

D1 = D0 %>% filter(YEAR >= 2010) %>% left_join(pit_games4)
# check
C1 = D1 %>% select(YEAR,DATE,GAME_ID,PIT_ID,WOBA_FINAL_PIT_19,PQ,num_woba_app,num_woba_app_prev) %>% distinct()
View(D1 %>% filter(PIT_ID == "weavj003"))
View(C1)

########### write csv ########### 
X <- tau_df
write_csv(X, output_filename)
                  
######################################
########### BATTER QUALITY ###########
######################################







