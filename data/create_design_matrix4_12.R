library(tidyverse)

### modify TTO_dataset_411.csv by adding a column FIRST_SZN_PIT, FIRST_SZN_BAT
### to indicate that it's a player's first season 

E0 <- read_csv("TTO_dataset_410.csv")

D0 <- read_csv("TTO_dataset_411.csv")
output_filename = "TTO_dataset_412.csv"

#########################################################################################
#### WOBA_AVG_BAT_19, WOBA_FINAL_BAT_19, WOBA_AVG_PIT_19, WOBA_FINAL_PIT_19, 
#### NUM_WOBA_APP_BAT, NUM_WOBA_APP_FINAL_BAT, NUM_WOBA_APP_PIT, NUM_WOBA_APP_FINAL_PIT,
#########################################################################################

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

#############################################################################
# PIT_SZNS
{
  pit_szns = E00 %>% group_by(YEAR, PIT_ID) %>%
    summarise(final_szn_woba = unique(WOBA_FINAL_PIT_19)) %>% 
    arrange(PIT_ID,YEAR) %>%
    ungroup() %>%
    group_by(PIT_ID) %>%
    mutate(prev_szn_avg_woba = lag(final_szn_woba)) %>%
    ungroup() %>%
    mutate(first_szn_pit = is.na(prev_szn_avg_woba)) 
  pit_szns
  
  first_szn_pit = pit_szns %>% 
    filter(first_szn_pit) %>%
    select(PIT_ID, YEAR) %>%
    arrange(PIT_ID,YEAR) %>%
    rename(FIRST_SZN_PIT = YEAR) 
  first_szn_pit
}

#############################################################################
# BAT_SZNS
{
  bat_szns = E00 %>% group_by(YEAR, BAT_ID) %>%
    summarise(final_szn_woba = unique(WOBA_FINAL_BAT_19)) %>% 
    arrange(BAT_ID,YEAR) %>%
    ungroup() %>%
    group_by(BAT_ID) %>%
    mutate(prev_szn_avg_woba = lag(final_szn_woba)) %>%
    ungroup() %>%
    mutate(first_szn_bat = is.na(prev_szn_avg_woba))
  bat_szns
  
  first_szn_bat = bat_szns %>% 
    filter(first_szn_bat) %>%
    select(BAT_ID, YEAR) %>%
    arrange(BAT_ID,YEAR) %>%
    rename(FIRST_SZN_BAT = YEAR) 
  first_szn_bat
}

#############################################################################


D1 = D0 %>% left_join(first_szn_bat) %>% left_join(first_szn_pit)
# View(tail(D1 %>% select(YEAR,BAT_ID,PIT_ID,FIRST_SZN_PIT,FIRST_SZN_BAT),20))

D2 = D1 %>% 
  mutate(FIRST_SZN_PIT = FIRST_SZN_PIT == YEAR,
         FIRST_SZN_BAT = FIRST_SZN_BAT == YEAR)
#tail(D2 %>% select(YEAR,BAT_ID,PIT_ID,FIRST_SZN_PIT,FIRST_SZN_BAT),20)

########### write csv ########### 
R = D2
write_csv(R, output_filename)




