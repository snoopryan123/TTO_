library(tidyverse)
library(rvest)
library(xml2)

########################################################################
# WOBA_AVG_BAT <- WOBA_CUMU_BAT
# WOBA_AVG_PIT <- WOBA_CUMU_PIT
# EVENT_WOBA_19 === wOBA of this event, using 2019 wOBA weights
# WOBA_AVG_BAT_19 === avg. woba prior to this plate appearance for a given batter during a given season (INDIVIDUAL BATTER'S QUALITY),
#     using 2019 woba weights for every season
# WOBA_AVG_PIT_19 === avg. woba prior to this plate appearance for a given pitcher during a given season (INDIVIDUAL PITCHER'S QUALITY)
#     using 2019 woba weights for every season
# NUM_WOBA_APP_BAT === number of wOBA appearances during the season for a given batter up to this point in the season
# NUM_WOBA_APP_PIT === number of wOBA appearances during the season for a given pitcher up to this point in the season
# WOBA_FINAL_BAT === final avg. wOBA of this batter during this season
# WOBA_FINAL_BAT_19 === final avg. wOBA of this batter during this season, using 2019 woba weights
# NUM_WOBA_APP_FINAL_BAT === final number of wOBA appearances for this batter this season
# WOBA_FINAL_PIT === final avg. wOBA of this pitcher during this season
# WOBA_FINAL_PIT_19 === final avg. wOBA of this pitcher during this season, using 2019 woba weights
# NUM_WOBA_APP_FINAL_PIT === final number of wOBA appearances for this pitcher this season
########################################################################

################################
########### THE CODE ###########
################################

input_filename = "retro07_PA_1990-2020.csv" #FIXME
output_filename = "retro_final_PA_1990-2020b.csv"
E <- read_csv(input_filename)

################################

### scrape WOBA WEIGHTS
content <- read_html("https://www.fangraphs.com/guts.aspx?type=cn")
tables <- content %>% html_table(fill = TRUE)
W <- tables[[9]]
W <- W %>% filter(Season == 2019) #FIXME

################################

{
    # WOBA_AVG_BAT, WOBA_AVG_PIT
    E0 <- E %>% rename(WOBA_AVG_BAT = WOBA_CUMU_BAT,
                       WOBA_AVG_PIT = WOBA_CUMU_PIT)
  
    # EVENT_WOBA_19 === wOBA of this event, using 2019 wOBA weights
    # https://www.fangraphs.com/guts.aspx?type=cn
    # HP, is an AB and PA.
    # SH, SF, IW, W are PA but not AB
    # an event is a WOBA_EVENT iff it is an {AB, W, SH, SF, HP} but not {IW}. Equivalently, {PA}\{IW}
    # ---> include all plate appearances as wOBA except intentional walks !!!
    E1 = E0 %>% mutate(EVENT_WOBA_19 = 
                    ifelse(WOBA_APP & HIT_VAL == 1, W$w1B, # single
                    ifelse(WOBA_APP & HIT_VAL == 2, W$w2B, # double
                    ifelse(WOBA_APP & HIT_VAL == 3, W$w3B, # triple
                    ifelse(WOBA_APP & HIT_VAL == 4, W$wHR, # HR
                    ifelse(WOBA_APP & EVENT_CODE == "W", W$wBB, # uBB / NIBB
                    ifelse(WOBA_APP & EVENT_CODE == "HP", W$wHBP, # HBP / HP
                    #ifelse(EVENT_CD==18, 0.92, # RBOE (reached base on error) --> no longer in the woba formula
                    #ifelse(PA_IND &  (EVENT_CODE != "IW"), 0, NA )))))))) %>% ungroup()
                    0)))))))
    print("E1")
    #View(E1 %>% filter(GAME_ID == "BOS202008180"))
    
    # WOBA_AVG_BAT_19
    # NUM_WOBA_APP_BAT
    E2 <- E1 %>% group_by(YEAR, BAT_ID) %>%
      mutate(cumu.woba.sum.b = cumsum(replace_na(EVENT_WOBA_19, 0)),
             NUM_WOBA_APP_BAT = cumsum(replace_na(WOBA_APP, 0)),
             WOBA_AVG_BAT_19 = cumu.woba.sum.b/NUM_WOBA_APP_BAT) %>% 
      ungroup()
    print("E2")
    
    # WOBA_AVG_PIT_19
    # NUM_WOBA_APP_PIT
    E3 <- E2 %>% group_by(YEAR, PIT_ID) %>%
      mutate(cumu.woba.sum.p = cumsum(replace_na(EVENT_WOBA_19, 0)),
             NUM_WOBA_APP_PIT = cumsum(replace_na(WOBA_APP, 0)),
             WOBA_AVG_PIT_19 = cumu.woba.sum.p/NUM_WOBA_APP_PIT) %>% 
      ungroup()
    print("E3")
    
    # WOBA_FINAL_BAT, WOBA_FINAL_BAT_19, NUM_WOBA_APP_FINAL_BAT
    G4 <- E3 %>% 
          group_by(YEAR, BAT_ID) %>% 
          filter(row_number() == n()) %>%
          select(YEAR, BAT_ID, WOBA_AVG_BAT, WOBA_AVG_BAT_19, NUM_WOBA_APP_BAT) %>%
          rename(WOBA_FINAL_BAT = WOBA_AVG_BAT, 
                 WOBA_FINAL_BAT_19 = WOBA_AVG_BAT_19,
                 NUM_WOBA_APP_FINAL_BAT = NUM_WOBA_APP_BAT) %>%
          ungroup() %>%
          arrange(YEAR, BAT_ID)
    E4 <- E3 %>% left_join(G4)
    View(E4 %>% select(YEAR, BAT_NAME, WOBA_AVG_BAT_19, WOBA_FINAL_BAT_19, NUM_WOBA_APP_BAT, NUM_WOBA_APP_FINAL_BAT))
    
    # WOBA_FINAL_PIT, WOBA_FINAL_PIT_19, NUM_WOBA_APP_FINAL_PIT
    G5 <- E4 %>% 
      group_by(YEAR, PIT_ID) %>% 
      filter(row_number() == n()) %>%
      select(YEAR, PIT_ID, WOBA_AVG_PIT, WOBA_AVG_PIT_19, NUM_WOBA_APP_PIT) %>%
      rename(WOBA_FINAL_PIT = WOBA_AVG_PIT, 
             WOBA_FINAL_PIT_19 = WOBA_AVG_PIT_19,
             NUM_WOBA_APP_FINAL_PIT = NUM_WOBA_APP_PIT) %>%
      ungroup() %>%
      arrange(YEAR, PIT_ID)
    E5 <- E4 %>% left_join(G5)
    View(E5 %>% select(YEAR, PIT_NAME, WOBA_AVG_PIT_19, WOBA_FINAL_PIT_19, NUM_WOBA_APP_PIT, NUM_WOBA_APP_FINAL_PIT))
    
}

##############################

R = E5
R_ = R %>% select(!c(cumu.woba.sum.b, cumu.woba.sum.p))
write_csv(R_, output_filename)

##############################
########### CHECKS ###########
##############################

y=2019
w = W
{
  # CHECK WOBA_AVG_BAT_19
  R1 = R %>% group_by(BAT_ID) %>% filter(row_number() == n()) %>% filter(NUM_WOBA_APP_BAT >= 150) %>%
    ungroup() %>% mutate(wOBA=round(WOBA_AVG_BAT_19,3)) %>% filter(year == 2019) %>%
    arrange(-WOBA_AVG_BAT_19) %>% select(BAT_ID, BAT_NAME, YEAR, wOBA, NUM_WOBA_APP_BAT, WOBA_AVG_BAT_19) 
  View(R1)
}




