library(tidyverse)

### create moving avg. estimator for batter and pitcher quality

input_filename = "TTO_dataset_410.csv" #FIXME
output_filename = "TTO_dataset_610.csv" #FIXME
E_og <- read_csv(input_filename)
E0 <- E_og %>% 
  mutate(OB = 1 - (EVENT_WOBA_19 == 0)) %>% 
  relocate(OB, .after=EVENT_WOBA_19) %>%
  select(-c(EVENT_WOBA_19))

#############################################################################
#### OBP_BAT, OBP_FINAL_BAT, OBP_PIT, OBP_FINAL_PIT, 
#### NUM_PA_BAT, NUM_PA_FINAL_BAT, NUM_PA_PIT, NUM_PA_FINAL_PIT
#############################################################################

### dataset with updated columns
{
  E00 = E0 %>% 
      group_by(YEAR, PIT_ID) %>%
      arrange(DATE, row_idx) %>%
      mutate(
        NUM_PA_PIT = cumsum(WOBA_APP),
        NUM_PA_FINAL_PIT = NUM_PA_PIT[n()],
        OBP_PIT = cumsum(OB)/NUM_PA_PIT,
        OBP_FINAL_PIT = OBP_PIT[n()]
      ) %>%
      ungroup() %>%
      group_by(YEAR, BAT_ID) %>%
      arrange(DATE, row_idx) %>%
      mutate(
        NUM_PA_BAT = cumsum(WOBA_APP),
        NUM_PA_FINAL_BAT = NUM_PA_BAT[n()],
        OBP_BAT = cumsum(OB)/NUM_PA_BAT,
        OBP_FINAL_BAT = OBP_BAT[n()],
      ) %>%
      ungroup() 
}

##############################################################
########### JUSTIFYING THE CHOICE OF NU AND TAU ###########
##############################################################

### nu === median of season-by-season player-specific s.d. in event woba
{
  nu_by_yr_PIT = E00 %>% 
    group_by(YEAR,PIT_ID) %>%
    summarise(szn_woba_p = OBP_FINAL_PIT[n()]) %>%
    group_by(PIT_ID) %>%
    summarise(nu_p = sd(szn_woba_p)) %>%
    filter(!is.na(nu_p) & nu_p != 0) %>%
    summarise(nu = median(nu_p))
  nu_p = median(nu_by_yr_PIT$nu, na.rm = TRUE)
  nu_p
  
  nu_by_yr_BAT = E00 %>% 
    group_by(YEAR,BAT_ID) %>%
    summarise(szn_woba_b = OBP_FINAL_BAT[n()]) %>%
    group_by(BAT_ID) %>%
    summarise(nu_b = sd(szn_woba_b)) %>%
    filter(!is.na(nu_b) & nu_b != 0) %>%
    summarise(nu = median(nu_b))
  nu_b = median(nu_by_yr_BAT$nu, na.rm = TRUE)
  nu_b
  
  mean(c(nu_b,nu_p))
  print(paste("nu is ", round(mean(c(nu_b,nu_p)),2)) ) # .05
}

### tau === median of event-by-event player-specific s.d. in event woba
{
  tau_by_yr_PIT = E00 %>% 
    group_by(YEAR,PIT_ID) %>%
    summarise(tau_p = sd(OB)) %>%
    filter(!is.na(tau_p) & tau_p != 0) %>%
    ungroup() %>% #group_by(YEAR) %>%
    summarise(tau = median(tau_p))
  #tau_p = median(tau_by_yr_PIT$tau, na.rm = TRUE)
  tau_p = tau_by_yr_PIT$tau
  tau_p
  
  tau_by_yr_BAT = E00 %>% 
    group_by(YEAR,BAT_ID) %>%
    summarise(tau_b = sd(OB)) %>%
    filter(!is.na(tau_b) & tau_b != 0) %>%
    ungroup() %>% #group_by(YEAR) %>%
    summarise(tau = median(tau_b))
  #tau_b = median(tau_by_yr_BAT$tau, na.rm = TRUE)
  tau_b = tau_by_yr_BAT$tau
  tau_b
  
  mean(c(tau_b,tau_p))
  print(paste("tau is ", round(mean(c(tau_b,tau_p)),2)) ) # .4
}

################################################
########### PITCHER QUALITY FUNCTION ###########
################################################

NU = 0.05 # 0.1
TAU = 0.5

get_PQ <- function(E00, nu=NU, tau=TAU) {

  ########### theta_bar_0 for pitchers ########### 
  {
    # prev_szn_avg_woba === pitcher's end-of-season avg. wOBA from the previous season
    pit_szns = E00 %>% group_by(YEAR, PIT_ID) %>%
      summarise(final_szn_woba = unique(OBP_FINAL_PIT)) %>% 
      ungroup() %>%
      group_by(PIT_ID) %>%
      mutate(prev_szn_avg_woba = lag(final_szn_woba)) %>%
      ungroup() %>%
      arrange(PIT_ID,YEAR)
    pit_szns
    
    # prev_szn_avg_woba_allPit === for a given season y, the mean wOBA 
    szns = pit_szns %>% select(-c(final_szn_woba)) %>% drop_na() %>%
      group_by(YEAR) %>%
      summarise(prev_szn_avg_woba_allPit = median(prev_szn_avg_woba)) %>%
      ungroup()
    szns
    
    # insert prev_szn_avg_woba_allPit into pit_szns dataset 
    pit_szns1 = pit_szns %>% left_join(szns)
    pit_szns1
    
    # theta_bar_0 
    pit_szns2 = pit_szns1 %>% 
      mutate(pit_played_in_prev_szn = !is.na(prev_szn_avg_woba),
             theta_bar_0_pit = ifelse(pit_played_in_prev_szn, prev_szn_avg_woba, prev_szn_avg_woba_allPit)) 
    pit_szns2
    
    # theta_bar_0 dataframe
    theta_bar_0_df = pit_szns2 %>% select(YEAR,PIT_ID,theta_bar_0_pit)
    theta_bar_0_df
  }  

  E11 = E00 %>% 
    left_join(theta_bar_0_df) %>%
    filter(YEAR >= 2010) %>%
    group_by(YEAR, PIT_ID) %>% 
    mutate(
      top = (1/tau^2)*OBP_PIT*(NUM_PA_PIT-1) + (1/nu^2)*theta_bar_0_pit,
      bottom = (NUM_PA_PIT-1)/(tau^2) + (1/nu^2),
      PQ = top/bottom # running avg. estimator
    ) %>%
    ungroup()
  
  R = E11 %>% select(-c(top,bottom))
  
  ### find examples of PIT_ID
  ######(R %>% filter(YEAR == 2019) %>% select(PIT_ID))$PIT_ID
    
  ### view korey kluber's running avg. estimator
  # ex1 = R %>% filter(PIT_ID == "klubc001" & YEAR == 2019) %>% 
  #   mutate(idx = row_number()) %>%
  #   select(row_idx,DATE,GAME_ID,INNING,PIT_ID,BAT_ID,OB, #top,bottom,
  #          NUM_PA_PIT,OBP_PIT,theta_bar_0,PQ,OBP_FINAL_PIT)
  # View(ex1)
  
  return(R)
}     ##nu=0.125

####################################################
########### GET PITCHER & BATTER QUALITY ###########
####################################################

get_PQBQ <- function(full=TRUE, nu=NU, tau=TAU) {
  # get Pitcher Quality
  E1a = get_PQ(E00, nu=nu, tau=tau)
  
  # get Batter Quality by re-using the Pitcher Quality function
  E0b = E00 %>% select(row_idx,YEAR,GAME_ID,DATE,
                       BAT_ID, OBP_FINAL_BAT, OBP_BAT,
                       NUM_PA_BAT, #NUM_PA_FINAL_BAT,
                       WOBA_APP,OB) 
  E00b = E0b %>% rename(
    PIT_ID = BAT_ID, 
    OBP_FINAL_PIT = OBP_FINAL_BAT,
    OBP_PIT = OBP_BAT,
    NUM_PA_PIT = NUM_PA_BAT
  )
  E1b = get_PQ(E00b, nu=nu, tau=tau) %>%
    rename(BQ = PQ,
           theta_bar_0_bat = theta_bar_0_pit,
           BAT_ID = PIT_ID,
           OBP_FINAL_BAT = OBP_FINAL_PIT,
           OBP_BAT = OBP_PIT,
           NUM_PA_BAT = NUM_PA_PIT) 
  X1 = E1a %>% left_join(E1b)
  # check columns with NA
  print("!!!!!"); print(names(X1)[sapply(1:ncol(X1),fun <- function(i) {sum(is.na(X1[,i]))}) > 0]); print("!!!!!");
  if (full) return(X1) else return(X1 %>% select(PQ,BQ))
}

X1 = get_PQBQ(full=TRUE, nu=0.1, tau=TAU) 

##pb2 = get_PQBQ(full=FALSE, nu=0.1, tau=TAU) %>% rename(PQ2=PQ, BQ2=BQ)
##X1a = bind_cols(X1, pb2)

# sum(is.na(X1$BQ))
# which(is.na(X1$BQ))
# View(X1[(108696-10):(108696+10),])

# ## Pitcher Examples
# ex1p = X1 %>% filter(PIT_ID == "klubc001" & YEAR == 2019) %>%
#   mutate(idx = row_number()) %>%
#   select(row_idx,DATE,GAME_ID,INNING,PIT_ID,BAT_ID,OB, #top,bottom,
#          NUM_PA_PIT,OBP_PIT,theta_bar_0_pit,PQ,OBP_FINAL_PIT)
# View(ex1p)
# ## Batter Examples: suare001, zobrb001, goldp001
# ex1b = X1 %>% filter(BAT_ID == "goldp001" & YEAR == 2019) %>%
#   mutate(idx = row_number()) %>%
#   select(row_idx,DATE,GAME_ID,INNING,PIT_ID,BAT_ID,OB, #top,bottom,
#          NUM_PA_BAT,OBP_BAT,theta_bar_0_bat,BQ,OBP_FINAL_BAT)
# View(ex1b)

#############################################################################
#### get data ready for RSTAN
#### standardize the relevant columns, and remove unnecessary columns
#############################################################################

# keep relevant columns
X2 = X1 %>% select(-c(row_idx, 
                    theta_bar_0_pit, theta_bar_0_bat,
                    #PIT_ID, BAT_ID, OBP_PIT, OBP_BAT,
                    NUM_PA_PIT, NUM_PA_FINAL_PIT, 
                    NUM_PA_BAT, NUM_PA_FINAL_BAT))

## Batter Examples: suare001, zobrb001, goldp001, braur002, longe001
######(X3 %>% filter(YEAR == 2019) %>% select(BAT_ID))$BAT_ID
X2 %>% filter(BAT_ID == "longe001" & YEAR == 2019) %>% 
  mutate(idx = row_number()) %>% 
  ggplot() +
  geom_line(aes(x=idx, y=OBP_BAT), col="black", size=1) +
  geom_line(aes(x=idx, y=OBP_FINAL_BAT), col="black", size=1)+
  geom_line(aes(x=idx, y=BQ), col="tomato3", size=1) 
  #geom_line(aes(x=idx, y=BQ2), col="dodgerblue", size=1)

########### write csv ########### 
R = X2
write_csv(R, output_filename)
            



