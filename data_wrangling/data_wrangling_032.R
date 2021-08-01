library(tidyverse)

# -> add additional FEATURES to Dataset_3 to create Dataset_4
########################################################################
# SP_IND === True if is a starting pitcher in this game
# PITCH_COUNT_CUMU === pitch count up to this point in the game
# PITCH_COUNT_FINAL === final pitch count of the game for each starter
# consecutive.bat.row
# BATTER_SEQ_NUM
# ORDER_CT === time thru the order number {1,2,3,..}
# HAND_MATCH === 1 if pitcher and batter handedness match, else 0
# PA_IND === TRUE iff it is a plate appearance
# AB_IND === TRUE iff it is an at-bat
# WOBA_APP === TRUE iff it is a wOBA-appearance, i.e. in {AB,W,SF,SH,HP}\{IW}

# INDIVIDUAL BATTER'S QUALITY
  # WOBA_CUMU_BAT === cumulative woba prior to this plate appearance for a given batter during a given season
# INDIVIDUAL PITCHER'S QUALITY
  # WOBA_CUMU_PIT === cumulative woba prior to this plate appearance for a given pitcher during a given season
########################################################################

################################
########### THE CODE ###########
################################

D <- read_csv("retro3_PA_1990-2020.csv")
E <- D %>% select(!c(sp.ind))
#D <- Dog %>% filter(YEAR == 2010) ###FIXME

#########################################################################
########### FIX ERRORS IN THE DATASET!!! and ADD SOME COLUMNS ###########
#########################################################################


#########################################################################

# FIX !!! ERROR from data_wrangling_2A:  DI is DEFENSIVE INDIFFERENCE not DOUBLE, so should have HIT_VAL = 0, EVENT_WOBA = 0
E1 <- E %>% mutate(HIT_VAL = ifelse(str_detect(EVENT_TX, "^DI"), 0, HIT_VAL),
                  HIT_BINARY = ifelse(str_detect(EVENT_TX, "^DI"), 0, HIT_BINARY),
                  EVENT_WOBA = ifelse(str_detect(EVENT_TX, "^DI"), 0, EVENT_WOBA))
print("E1")

# RUNS and RBIs      EVENT_ER_CT, EVENT_RBI_CT, EVENT_RUNS
#FIXME --> do it better with str_detect ????

# SP_IND, PITCH_COUNT_CUMU, PITCH_COUNT_FINAL
E3 <- E2 %>% group_by(GAME_ID, BAT_HOME_IND) %>% mutate(first.p = first(PIT_ID)) %>% ungroup() %>%
             group_by(GAME_ID, PIT_ID) %>%
             mutate(SP_IND = (PIT_ID == first.p),
                    PITCH_COUNT_CUMU = cumsum(replace_na(EVENT_PITCH_COUNT, 0)),
                    PITCH_COUNT_FINAL = sum(EVENT_PITCH_COUNT, na.rm=TRUE)) %>%
             ungroup() %>% select(!c(first.p))
print("E3")
# Check
#View(E3 %>% select(INNING,BAT_HOME_IND,GAME_ID,BAT_NAME,HOME_TEAM_ID,AWAY_TEAM_ID,PIT_NAME,SP_IND,PITCH_SEQ_TX, EVENT_PITCH_COUNT, PITCH_COUNT_CUMU, PITCH_COUNT_FINAL))

# BATTER_SEQ_NUM, ORDER_CT
# For BATTER_SEQ_NUM, do not count the same player twice in a row
E4 <- E3 %>% group_by(GAME_ID, BAT_HOME_IND) %>%
             mutate(consecutive.bat.row = lag(BAT_ID)==BAT_ID,
                    consecutive.bat.row = ifelse(is.na(consecutive.bat.row), FALSE, consecutive.bat.row),
                    BATTER_SEQ_NUM = cumsum(!consecutive.bat.row), #row_number(),
                    ORDER_CT = 1 + (BATTER_SEQ_NUM-1) %/% 9) %>%
             ungroup() 
print("E4")
# Check
#View(E4 %>% select(INNING,BAT_HOME_IND,GAME_ID,HOME_TEAM_ID,AWAY_TEAM_ID,EVENT_TX, BAT_NAME,consecutive.bat.row,BATTER_SEQ_NUM, ORDER_CT))

# HAND_MATCH 
# check unique(D$BAT_HAND) and unique(D$PIT_HAND)
E5 <- E4 %>% mutate(HAND_MATCH = ifelse(is.na(BAT_HAND) | is.na(PIT_HAND), NA,
                                        ifelse(BAT_HAND == "B" | PIT_HAND == "B", TRUE,
                                               BAT_HAND == PIT_HAND)))
print("E5")
# Check
#View(E5 %>% select(INNING,BAT_HOME_IND,GAME_ID,HOME_TEAM_ID,AWAY_TEAM_ID,BAT_NAME,BAT_HAND,PIT_NAME,PIT_HAND,HAND_MATCH))

# fix AB_IND and PA_IND !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

# PA_IND (plate appearance) (so, not a substitution or stolen base) 
# definition:   https://en.wikipedia.org/wiki/Plate_appearance

#FIXME
# A batter is not credited with a plate appearance if, while batting, a preceding runner is put out on the basepaths for the third out in a way other than 
#by the batter putting the ball into play (i.e., picked off, caught stealing). In this case, the same batter continues his turn batting in the next inning with 
#no balls or strikes against him.
# A batter is not credited with a plate appearance if, while batting, the game ends as the winning run scores from third base on a balk, stolen base, wild pitch or passed ball.
# A batter may or may not be credited with a plate appearance (and possibly at bat) in the rare instance when he is replaced by a pinch hitter after having 
#already started his turn at bat. Under Rule 9.15(b), the pinch hitter would receive the plate appearance (and potential of an at-bat) unless the original batter 
#is replaced when having 2 strikes against him and the pinch hitter subsequently completes the strikeout, in which case the plate appearance and at-bat are charged
# to the first batter.
E6 <- E5 %>%  group_by(GAME_ID, BAT_HOME_IND) %>%
              mutate(x = lead(BAT_ID) != BAT_ID,
                     x = replace_na(x, TRUE),
                     PA_IND = x) %>% select(!c(x)) %>%
              ungroup() 
print("E6")
# Check
#RR = E6 %>% filter(YEAR==2010,BAT_NAME=="David Ortiz") %>% select(INNING,BAT_HOME_IND,GAME_ID,HOME_TEAM_ID,AWAY_TEAM_ID,BAT_NAME,PIT_NAME,EVENT_TX,PA_IND)
#View(RR)
#sum(RR$PA_IND)

# E6 <- E5 %>% mutate(PA_IND =  !str_detect(EVENT_TX, "^BK") ### "^CS" "^DI" "^OA" "^PB"  "^WP" "^PO" "^SB" "^NP"

# AB_IND (at bat)
# AT-BAT is a PLATE-APPEARANCE without {SF,SH(sacBunt),W,IW,HP,C(catcher interference)}
# https://www.retrosheet.org/eventfile.htm
E7 <- E6 %>% mutate(AB_IND = PA_IND & !str_detect(EVENT_TX, "SF") & !str_detect(EVENT_TX, "SH") & !str_detect(EVENT_TX, "^W") & 
                        !str_detect(EVENT_TX, "^IW") & !str_detect(EVENT_TX, "^HP") & !str_detect(EVENT_TX, "^C"))
print("E7")
# Check
#RR1 = E7 %>% filter(YEAR==2010,BAT_NAME=="David Ortiz") %>% select(INNING,BAT_HOME_IND,GAME_ID,HOME_TEAM_ID,AWAY_TEAM_ID,BAT_NAME,PIT_NAME,EVENT_TX,AB_IND,PA_IND)
#View(RR1)
#sum(RR1$AB_IND)

# WOBA_APP === TRUE iff it is a wOBA-appearance, i.e. in {AB,W,SF,SH,HP}\{IW}
E8 <- E7 %>% mutate(WOBA_APP = PA_IND & !str_detect(EVENT_TX, "^IW") &
                               !str_detect(EVENT_TX, "^C") &  !str_detect(EVENT_TX, "^INT") )  
# ---> not exactly right, see 20-22 swisher,gonzalez,braun in the rankings. but VERY CLOSE

# View(E7 %>% filter(str_detect(EVENT_TX, "^W") )) # walk
# View(E7 %>% filter(str_detect(EVENT_TX, "SF") )) # sacrifice fly

# E8 <- E7 %>% mutate(WOBA_APP = (AB_IND | str_detect(EVENT_TX, "^W") | str_detect(EVENT_TX, "SF") | str_detect(EVENT_TX, "SH") |
#                                         str_detect(EVENT_TX, "^HP")) & !str_detect(EVENT_TX, "^IW"))
# ---> doesnt work well, even tho its the definition!

# Check
#View(E8 %>% select(EVENT_TX, PA_IND, AB_IND, WOBA_APP))

D0 <- E8
################################
################################

# WOBA_CUMU_BAT (INDIVIDUAL BATTER'S QUALITY)
D1 <- D0 %>% group_by(YEAR, BAT_ID) %>%
      mutate(cumu.woba.sum.b = cumsum(replace_na(EVENT_WOBA, 0)),
             cumu.woba.denom.b = cumsum(replace_na(WOBA_APP, 0)),
             #cumu.pa.sum.b = cumsum(replace_na(PA_IND, 0)),
             WOBA_CUMU_BAT = cumu.woba.sum.b/cumu.woba.denom.b) %>% # is it plate appearance, or something else?
      #select(!c(cumu.woba.sum, cumu.pa.sum)) %>%
      ungroup()

# WOBA_CUMU_PIT (INDIVIDUAL PITCHER'S QUALITY)
D2 <- D1 %>% group_by(YEAR, PIT_ID) %>%
  mutate(cumu.woba.sum.p = cumsum(replace_na(EVENT_WOBA, 0)),
         cumu.woba.denom.p = cumsum(replace_na(WOBA_APP, 0)),
         #cumu.pa.sum.p = cumsum(replace_na(PA_IND, 0)),
         WOBA_CUMU_PIT = cumu.woba.sum.p/cumu.woba.denom.p) %>% # is it plate appearance, or something else?
  #select(!c(cumu.woba.sum, cumu.pa.sum)) %>%
  ungroup()


R = D2
R_ = R %>% select(!c(cumu.woba.sum.b, cumu.woba.denom.b, cumu.woba.sum.p, cumu.woba.denom.p))
filename = "retro4_PA_1990-2020.csv"
#write_csv(R_, filename)

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
    y = 1993
    w = WW[WW$Season == y,]
    
    # CHECK WOBA_CUMU_BAT
    # https://www.fangraphs.com/leaders.aspx?pos=all&stats=bat&lg=all&qual=y&type=8&season=2010&month=0&season1=2010&ind=0&team=0&rost=0&age=0&filter=&players=0&startdate=2010-01-01&enddate=2010-12-31&sort=16,d
    R1 = R %>% filter(YEAR == y) %>% group_by(BAT_ID) %>% filter(row_number() == n(), cumu.woba.denom.b >= 500) %>% ungroup() %>% 
               arrange(-WOBA_CUMU_BAT) %>% select(BAT_ID, BAT_NAME, YEAR, cumu.woba.denom.b, WOBA_CUMU_BAT)
    View(R1)
    
    # check individual player data 
    # https://www.fangraphs.com/players/robinson-cano/3269/stats?position=2B
    N = "Josh Hamilton" #"Shin-Soo Choo" #"Robinson Cano" #"Albert Pujols"
    R2 = R %>% filter(YEAR== y, BAT_NAME== N) %>% 
      summarise(G=length(unique(GAME_ID)), AB=sum(AB_IND), PA = sum(PA_IND), 
                H=sum(HIT_BINARY), S=sum(HIT_VAL==1), D=sum(HIT_VAL==2), T= sum(HIT_VAL==3), HR= sum(HIT_VAL==4),
                W= sum(EVENT_CODE=="W"), IW=sum(EVENT_CODE=="IW"), BB=W+IW, HP= sum(EVENT_CODE=="HP"), 
                AVG = H/AB, wOBA_num = (S*w$w1B + D*w$w2B + T*w$w3B + HR*w$wHR + W*w$wBB + HP*w$wHBP), wOBA_APP = last(cumu.woba.denom.b), 
                wOBA = wOBA_num/(wOBA_APP)
                )
    R2
    
    # all plate appearances
    R3 = R %>% filter(YEAR== y, BAT_NAME== N) %>% arrange(GAME_ID,INNING,BATTER_SEQ_NUM) %>%
               select(INNING,BATTER_SEQ_NUM,HOME_TEAM_ID,AWAY_TEAM_ID,GAME_ID,EVENT_TX,EVENT_WOBA,PA_IND,PIT_NAME)
    View(R3)
    
    # some plate appearances
    R4 = R %>% filter(YEAR== y, BAT_NAME== N) %>% arrange(GAME_ID,INNING,BATTER_SEQ_NUM) %>%
      #filter( str_detect(EVENT_TX, "HR") ) #%>%
      filter( HIT_VAL==1 ) #%>%
      #select(INNING,BATTER_SEQ_NUM,HOME_TEAM_ID,AWAY_TEAM_ID,GAME_ID,EVENT_TX,EVENT_WOBA,HIT_VAL,AB_IND,PA_IND,PIT_NAME)
    View(R4)
    
}






# xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
# PARK_EFFECT ???
# HOME_FIELD_EFFECT ???
# HOW FAR INTO THE SEASON WE ARE EFFECT ???
# NUM_DAYS_REST === number of days of rest the starting pitcher has prior to this game -> data????


