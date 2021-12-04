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

get_PQ <- function(G0, sigma_scale=1) {
    
    ########### theta_bar_0 for pitchers ########### 
    {
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
    }  
  
    ########### sigma for pitchers ########### 
    {
      # cumulative s.d.
      cumsd <- function(x) {
        TTR::runSD(x, n = 1, cumulative = TRUE)
      }
      
      # sigma === pitcher-specific szn-by-szn s.d. in avg. wOBA over all his previous seasons
      pit_szns21 = pit_szns2 %>% select(-c(final_szn_woba,pit_played_in_prev_szn)) %>%
        group_by(PIT_ID) %>%
        mutate(has_prev_szns =  row_number() > 1,
               sigma = ifelse(!has_prev_szns, NA, c(-1,cumsd(prev_szn_avg_woba[2:n()])) )) %>%
        ungroup()
      pit_szns21[999:1010,]
  
      # sigma === median of the sigma of all pitchers who did pitch in the previous season
      med_sig = pit_szns21 %>% group_by(YEAR)  %>% drop_na() %>% summarise(med_sig = median(sigma)) 
      med_sig[med_sig$YEAR < 2010,]$med_sig = NA #want 2010-2019
      med_sig
      
      # fix the NA's of sigma
      pit_szns22 = pit_szns21 %>% left_join(med_sig) %>%
        mutate(sigma = ifelse(is.na(sigma), med_sig, sigma)) %>%
        select(-c(has_prev_szns))
      pit_szns22[999:1010,]
      
      # sigma dataframe
      sigma_df = pit_szns22 %>% select(YEAR,PIT_ID,theta_bar_0,sigma)
      sigma_df
    }
  
    ########### tau for pitchers ########### 
    {
      # avg. wOBA for each pitcher-game
      pit_games = G0 %>% group_by(YEAR,GAME_ID,PIT_ID,DATE) %>%
        summarise(game_avg_woba = mean(EVENT_WOBA_19)) %>%
        ungroup() %>%
        arrange(PIT_ID,YEAR,DATE)
      pit_games
      
      # tau === pitcher-specific game-to-game s.d. in avg. wOBA over all games from previous season
      pit_szns31 = pit_games %>% group_by(YEAR,PIT_ID) %>%
        summarise(sd_game2game = sd(game_avg_woba)) %>%
        ungroup() %>%
        arrange(PIT_ID,YEAR) %>%
        group_by(PIT_ID) %>%
        mutate(tau = lag(sd_game2game)) %>%
        ungroup()
      pit_szns31[999:1010,]
      
      # tau === median of the tau of all pitchers who did pitch in the previous season
      med_tau = pit_szns31 %>% group_by(YEAR) %>% drop_na() %>% summarise(med_tau = median(tau)) %>%
        mutate(med_tau = ifelse(YEAR >= 2010, med_tau, NA)) #want 2010-2019
      med_tau
      
      # fix the NA's of tau
      pit_szns32 = pit_szns31 %>% left_join(med_tau) %>%
        mutate(tau = ifelse(is.na(tau), med_tau, tau)) %>%
        select(-c(sd_game2game,med_tau))
      pit_szns32[999:1010,]
      
      # tau dataframe
      tau_df = sigma_df %>% left_join(pit_szns32) 
      tau_df # recall we want only years 2010-2019, so dont worry abt the NA's
      
      
    }
  
    ########### pitcher quality ########### 
    {
      # use only years 2010-2019
      PIT_SZNS = tau_df %>% filter(YEAR >= 2010)
      PIT_SZNS
      
      # fix sigma==0 or tau==0, since these guys played 1 game that season...
      #PIT_SZNS %>% filter(sigma <= .001 | tau <= .001)
      PIT_SZNS %>% filter(sigma == 0 | tau == 0)
      (PIT_SZNS %>% filter(is.na(sigma) | is.na(tau)))
      ### "adamc001"
      # View(G0 %>% filter(YEAR==2015,PIT_ID == "adamc001"))
      PIT_SZNS = PIT_SZNS %>% 
        group_by(YEAR) %>%
        mutate(
          # sigma = ifelse(!is.na(sigma) & sigma != 0, sigma, (med_sig %>% filter(YEAR == unique(YEAR)))$med_sig ),
          # tau = ifelse(!is.na(tau) & tau != 0, tau,  (med_tau %>% filter(YEAR == unique(YEAR)))$med_tau )
          sigma = ifelse(!is.na(sigma) & sigma != 0, sigma, med_sig[med_sig$YEAR==2010,]$med_sig),
          tau = ifelse(!is.na(tau) & tau != 0, tau,  med_tau[med_tau$YEAR==2010,]$med_tau)
        ) %>% ungroup()
     
      # theta
      pit_games41 = pit_games %>% 
        filter(YEAR >= 2010) %>%
        rename(theta = game_avg_woba) %>% 
        left_join(PIT_SZNS)
      pit_games41
      
      #FIXME # adjust sigma??? 
      pit_games41 = pit_games41 %>% mutate(sigma = sigma*sigma_scale)
      
      # pitcher quality
      pit_games42 = pit_games41 %>% 
        relocate(theta, .after=tau) %>%
        arrange(PIT_ID,DATE) %>%
        group_by(YEAR,PIT_ID) %>%
        mutate(i = row_number(),
               sum_1_im1_theta = cumsum(lag(theta, default=0))) %>%
        ungroup() %>%
        mutate(PQ = ( (1/tau^2)*sum_1_im1_theta + (1/sigma^2)*theta_bar_0 )/( (i-1)/(tau^2) + (1/sigma^2) ))
      pit_games42
      
      pit_games43 = pit_games42 %>% select(YEAR,DATE,PIT_ID,PQ) 
      pit_games43
    }
    
    R = G0 %>% filter(YEAR >= 2010) %>% left_join(pit_games43) 
    return(R)
}

###########################################
########### GET PITCHER QUALITY ###########
###########################################

# get Pitcher Quality
E1a = get_PQ(E00, sigma_scale=4)
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
E0b = E00 %>% select(row_idx,YEAR,GAME_ID,DATE,
                     BAT_ID, WOBA_FINAL_BAT_19, WOBA_AVG_BAT_19,
                     #NUM_WOBA_APP_BAT,NUM_WOBA_APP_FINAL_BAT,
                     WOBA_APP,EVENT_WOBA_19) 
E00b = E0b %>% rename(
  PIT_ID = BAT_ID, 
  WOBA_FINAL_PIT_19 = WOBA_FINAL_BAT_19,
  WOBA_AVG_PIT_19 = WOBA_AVG_BAT_19
)
E1b = get_PQ(E00b, sigma_scale = 4) %>%
  rename(BQ = PQ,
         BAT_ID = PIT_ID,
         WOBA_FINAL_BAT_19 = WOBA_FINAL_PIT_19,
         WOBA_AVG_BAT_19 = WOBA_AVG_PIT_19)
# check "mauej001" "ellsj001" "vottj001" "ryanb002" // "manzt001" "duncs001"
View(E1b %>% filter(BAT_ID == "ryanb002",YEAR==2010) %>% arrange(DATE) %>% 
       mutate(cum_avg_woba = cumsum(EVENT_WOBA_19)/cumsum(WOBA_APP)) %>%
       select(row_idx,YEAR,DATE,GAME_ID,BAT_ID,
              #NUM_WOBA_APP_BAT,NUM_WOBA_APP_FINAL_BAT,
              WOBA_APP,EVENT_WOBA_19,WOBA_FINAL_BAT_19,cum_avg_woba,WOBA_AVG_BAT_19,BQ))

# new DF with both Pitcher and Batter quality
X1 = E1a %>% left_join(E1b)

# check columns with NA
names(X1)[sapply(1:ncol(X1),fun <- function(i) {sum(is.na(X1[,i]))}) > 0]
# sum(is.na(X1$BQ))
# which(is.na(X1$BQ))
# View(X1[(108696-10):(108696+10),])

#############################################################################
#### get data ready for RSTAN
#### standardize the relevant columns, and remove unnecessary columns
#############################################################################

# keep relevant columns
X2 = X1 %>% select(-c(row_idx, PIT_ID, BAT_ID,
                      NUM_WOBA_APP_PIT, NUM_WOBA_APP_FINAL_PIT, 
                      NUM_WOBA_APP_BAT, NUM_WOBA_APP_FINAL_BAT,
                      WOBA_AVG_PIT_19, WOBA_AVG_BAT_19))

# standardize the vector x to have mean 0 and s.d. 1/2
std <- function(x) {
  (x-mean(x))/(sd(x) * 2)
}

# standardize these columns
X3 = X2 %>% 
  group_by(YEAR) %>%
  mutate(std_EVENT_WOBA_19 = std(EVENT_WOBA_19),
         std_WOBA_FINAL_BAT_19 = std(WOBA_FINAL_BAT_19),
         std_WOBA_FINAL_PIT_19 = std(WOBA_FINAL_PIT_19),
         std_BQ = std(BQ),
         std_PQ = std(PQ)) %>%
  ungroup()

# hist checks
hist(X3$std_PQ)
hist(X3$std_BQ)


########### write csv ########### 
R = X3
write_csv(R, output_filename)
            



