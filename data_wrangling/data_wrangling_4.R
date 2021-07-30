library(tidyverse)

# -> add additional FEATURES to Dataset_3 to create Dataset_4
########################################################################
# HAND_MATCH === 1 if pitcher and batter handedness match, else 0
  # ---> some batters have "B" for both hands, what is the handedness match ???
  # unique(D$BAT_HAND)
  # unique(D$PIT_HAND)
# INDIVIDUAL BATTER'S QUALITY
  # WOBA_CUMU_BAT === cumulative woba prior to this plate appearance for a given batter during a given season
# INDIVIDUAL PITCHER'S QUALITY
  # WOBA_CUMU_PIT === cumulative woba prior to this plate appearance for a given pitcher during a given season

########################################################################

################################
########### THE CODE ###########
################################

D <- read_csv("retro3_PA_1990-2020.csv")
#D <- Dog %>% filter(YEAR == 2010) ###FIXME

# FIX ERROR from data_wrangling_2A:  DI is DEFENSIVE INDIFFERENCE not DOUBLE, so should have HIT_VAL = 0, EVENT_WOBA = 0
D <- D %>% mutate(HIT_VAL = ifelse(str_detect(EVENT_TX, "^DI"), 0, HIT_VAL),
                  HIT_BINARY = ifelse(str_detect(EVENT_TX, "^DI"), 0, HIT_BINARY),
                  EVENT_WOBA = ifelse(str_detect(EVENT_TX, "^DI"), 0, EVENT_WOBA))

# WOBA_CUMU_BAT (INDIVIDUAL BATTER'S QUALITY)
D1 <- D %>% group_by(YEAR, BAT_ID) %>%
      mutate(cumu.woba.sum.b = cumsum(replace_na(EVENT_WOBA, 0)),
             cumu.pa.minus.iw.sum.b = cumsum(replace_na(PA_IND, 0)) - cumsum(replace_na(EVENT_CODE == "IW", 0)),
             #cumu.pa.sum.b = cumsum(replace_na(PA_IND, 0)),
             WOBA_CUMU_BAT = cumu.woba.sum.b/cumu.pa.minus.iw.sum.b) %>% # is it plate appearance, or something else?
      #select(!c(cumu.woba.sum, cumu.pa.sum)) %>%
      ungroup()

# WOBA_CUMU_PIT (INDIVIDUAL PITCHER'S QUALITY)
D2 <- D1 %>% group_by(YEAR, PIT_ID) %>%
  mutate(cumu.woba.sum.p = cumsum(replace_na(EVENT_WOBA, 0)),
         cumu.pa.minus.iw.sum.p = cumsum(replace_na(PA_IND, 0)) - cumsum(replace_na(EVENT_CODE == "IW", 0)),
         #cumu.pa.sum.p = cumsum(replace_na(PA_IND, 0)),
         WOBA_CUMU_PIT = cumu.woba.sum.p/cumu.pa.minus.iw.sum.p) %>% # is it plate appearance, or something else?
  #select(!c(cumu.woba.sum, cumu.pa.sum)) %>%
  ungroup()

R = D2
filename = "retro4_PA_1990-2020.csv"
write_csv(result, filename)

########################################################################

##############################
########### RUNNIT ###########
##############################




##############################
########### CHECKS ###########
##############################

{
    # 
    WW = read_csv("woba_weights_Fangraphs.csv")
    y = 2010
    w = WW[WW$Season == y,]
    
    # CHECK WOBA_CUMU_BAT
    # https://www.fangraphs.com/leaders.aspx?pos=all&stats=bat&lg=all&qual=y&type=8&season=2010&month=0&season1=2010&ind=0&team=0&rost=0&age=0&filter=&players=0&startdate=2010-01-01&enddate=2010-12-31&sort=16,d
    R1 = R %>% filter(YEAR == y) %>% group_by(BAT_ID) %>% filter(row_number() == n(), cumu.pa.minus.iw.sum.b >= 500) %>% ungroup() %>% 
               arrange(-WOBA_CUMU_BAT) %>% select(BAT_ID, BAT_NAME, YEAR, cumu.pa.minus.iw.sum.b, WOBA_CUMU_BAT)
    View(R1)
    
    # check individual player data 
    # "Albert Pujols" "Jose Bautista" "Miguel Cabrera" "Erick Aybar" "Robinson Cano"
    # https://www.fangraphs.com/players/robinson-cano/3269/stats?position=2B
    N = "Robinson Cano" #"Albert Pujols"
    R2 = R %>% filter(YEAR== y, BAT_NAME== N) %>% 
      summarise(G=length(unique(GAME_ID)), AB=sum(AB_IND), PA = last(cumu.pa.minus.iw.sum.b), H=sum(HIT_BINARY),
                S=sum(HIT_VAL==1), D=sum(HIT_VAL==2), T= sum(HIT_VAL==3), HR= sum(HIT_VAL==4),
                W= sum(EVENT_CODE=="W"), IW=sum(EVENT_CODE=="IW"), BB=W+IW, HP= sum(EVENT_CODE=="HP"), 
                AVG = H/AB, wOBA = (S*w$w1B + D*w$w2B + T*w$w3B + HR*w$wHR + W*w$wBB + HP*w$wHBP)/(PA)
                )
    R2
    
    # all plate appearances
    R3 = R %>% filter(YEAR== y, BAT_NAME== N) %>% arrange(GAME_ID,INNING,BATTER_SEQ_NUM) %>%
               select(INNING,BATTER_SEQ_NUM,HOME_TEAM_ID,AWAY_TEAM_ID,GAME_ID,EVENT_TX,EVENT_WOBA,PA_IND,PIT_NAME)
    View(R3)
    
    # some plate appearances
    R4 = R %>% filter(YEAR== y, BAT_NAME== N) %>% arrange(GAME_ID,INNING,BATTER_SEQ_NUM) %>%
      filter(str_detect(EVENT_TX, "^DI")) %>%
      select(INNING,BATTER_SEQ_NUM,HOME_TEAM_ID,AWAY_TEAM_ID,GAME_ID,EVENT_TX,EVENT_WOBA,AB_IND,PA_IND,PIT_NAME)
    View(R4)
    
    # need to fix AB and PA !!!
    
    
}






# xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
# PARK_EFFECT ???
# HOME_FIELD_EFFECT ???
# HOW FAR INTO THE SEASON WE ARE EFFECT ???
# NUM_DAYS_REST === number of days of rest the starting pitcher has prior to this game -> data????


