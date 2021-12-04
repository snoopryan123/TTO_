library(tidyverse)

### create moving avg. estimator for batter and pitcher quality

input_filename = "design_matrix4_10.csv" #FIXME
output_filename = "design_matrix4_11.csv" #FIXME
E0 <- read_csv(input_filename)

#############################################################################
#### WOBA_AVG_BAT_19, WOBA_FINAL_BAT_19, WOBA_AVG_PIT_19, WOBA_FINAL_PIT_19, 
#### NUM_WOBA_APP_BAT, NUM_WOBA_APP_FINAL_BAT, NUM_WOBA_APP_PIT, NUM_WOBA_APP_FINAL_PIT,
#############################################################################

{
  E00 = E0 %>% 
      group_by(YEAR, PIT_ID) %>%
      arrange(DATE, row_idx) %>%
      mutate(
        NUM_WOBA_APP_PIT = cumsum(WOBA_APP),
        NUM_WOBA_APP_FINAL_PIT = NUM_WOBA_APP_PIT[n()],
        WOBA_AVG_PIT_19 = cumsum(EVENT_WOBA_19)/NUM_WOBA_APP_PIT,
        WOBA_FINAL_PIT_19 = WOBA_AVG_PIT_19[n()]
      ) %>%
      ungroup() %>%
      group_by(YEAR, BAT_ID) %>%
      arrange(DATE, row_idx) %>%
      mutate(
        NUM_WOBA_APP_BAT = cumsum(WOBA_APP),
        NUM_WOBA_APP_FINAL_BAT = NUM_WOBA_APP_BAT[n()],
        WOBA_AVG_BAT_19 = cumsum(EVENT_WOBA_19)/NUM_WOBA_APP_BAT,
        WOBA_FINAL_BAT_19 = WOBA_AVG_BAT_19[n()],
      ) %>%
      ungroup() 
}

################################################
########### PITCHER QUALITY FUNCTION ###########
################################################

get_PQ <- function(E00) {
    ########### theta_bar_0 for pitchers ########### 
    G0 = E00
    ###CUTOFF_WOBA_APP = 1 #FIXME #50
  
    # prev_szn_avg_woba === pitcher's end-of-season avg. wOBA from the previous season
    pit_szns = G0 %>% group_by(YEAR, PIT_ID) %>%
      summarise(final_szn_woba = unique(WOBA_FINAL_PIT_19)) %>% 
      arrange(PIT_ID,YEAR) %>%
      ungroup() %>%
      group_by(PIT_ID) %>%
      mutate(prev_szn_avg_woba = lag(final_szn_woba)) %>%
      ungroup()
    pit_szns
      
    # prev_szn_avg_woba_allPit === for a given season y, the mean wOBA 
    szns = pit_szns %>% select(-c(final_szn_woba)) %>% drop_na() %>%
      group_by(YEAR) %>%
      summarise(prev_szn_avg_woba_allPit = mean(prev_szn_avg_woba)) 
    szns
    
    # insert prev_szn_avg_woba_allPit into pit_szns dataset 
    pit_szns1 = pit_szns %>% left_join(szns)
    pit_szns1
    
    # theta_bar_0 
    pit_szns2 = pit_szns1 %>% 
      mutate(pit_played_in_prev_szn = !is.na(prev_szn_avg_woba),
             theta_bar_0 = ifelse(pit_played_in_prev_szn, prev_szn_avg_woba, prev_szn_avg_woba_allPit)) 
    pit_szns2
    
    # theta_bar_0 dataframe
    theta_bar_0_df = pit_szns2 %>% select(YEAR,PIT_ID,theta_bar_0)
    theta_bar_0_df
    
    ########### sigma for pitchers ########### 
    
    cumsd <- function(x) {
      TTR::runSD(x, n = 1, cumulative = TRUE)
    }
    
    pit_szns2 = pit_szns1 %>% select(-c(use_prev_szn_avg_woba,prev_year_avg_woba,theta_bar_0)) %>% #select(YEAR,PIT_ID,WOBA_FINAL_PIT_19,woba_prev) %>% 
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
    
    pit_games = E00 %>% group_by(YEAR,GAME_ID,PIT_ID,DATE) %>%
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
    
    R = E00 %>% filter(YEAR >= 2010) %>% left_join(pit_games4) 
    # check
    C1 = R %>% select(YEAR,DATE,GAME_ID,PIT_ID,WOBA_FINAL_PIT_19,PQ,num_woba_app,num_woba_app_prev) %>% distinct()
    #View(R %>% filter(PIT_ID == "weavj003"))
    #View(C1)
    
    return(R %>% select(-c(num_woba_app, num_woba_app_prev)))
}

###########################################
########### GET PITCHER QUALITY ###########
###########################################

# get Pitcher Quality
E1a = get_PQ(E00)
# check
# "weavj003" "pinej001" "takah001" "cainm001"
#(E1a %>% filter(YEAR==2010) %>%select(PIT_ID) %>% distinct() )[50:60,]
View(E1a %>% filter(PIT_ID == "cainm001",YEAR==2010) %>% arrange(DATE,row_idx) %>% 
       mutate(cum_avg_woba = cumsum(EVENT_WOBA_19)/cumsum(WOBA_APP)) %>%
       select(row_idx,YEAR,DATE,GAME_ID,PIT_ID,NUM_WOBA_APP_PIT,NUM_WOBA_APP_FINAL_PIT,WOBA_APP,EVENT_WOBA_19,WOBA_FINAL_PIT_19,cum_avg_woba,WOBA_AVG_PIT_19,PQ))


##########################################
########### GET BATTER QUALITY ###########
##########################################

# get Batter Quality by re-using the Pitcher Quality function
E0b = E00 %>% select(row_idx,YEAR,GAME_ID,DATE,BAT_ID,
                     NUM_WOBA_APP_BAT,NUM_WOBA_APP_FINAL_BAT,
                     WOBA_FINAL_BAT_19,WOBA_AVG_BAT_19,
                     WOBA_APP,EVENT_WOBA_19) 
E1b = E0b %>% 
  rename(PIT_ID = BAT_ID, 
         WOBA_FINAL_PIT_19 = WOBA_FINAL_BAT_19) %>% 
  get_PQ() %>% 
  rename(BQ = PQ,
         BAT_ID = PIT_ID,
         WOBA_FINAL_BAT_19 = WOBA_FINAL_PIT_19)
# check
View(E1b %>% filter(BAT_ID == "mauej001",YEAR==2012) %>% arrange(DATE) %>% 
       mutate(cum_avg_woba = cumsum(EVENT_WOBA_19)/cumsum(WOBA_APP)) %>%
       select(row_idx,YEAR,DATE,GAME_ID,BAT_ID,NUM_WOBA_APP_BAT,NUM_WOBA_APP_FINAL_BAT,WOBA_APP,EVENT_WOBA_19,WOBA_FINAL_BAT_19,cum_avg_woba,WOBA_AVG_BAT_19,BQ))

########### write csv ########### 
# new DF with both Pitcher and Batter quality

X = E1a %>% left_join(E1b)

write_csv(X, output_filename)
                  
######################################
########### BATTER QUALITY ###########
######################################







