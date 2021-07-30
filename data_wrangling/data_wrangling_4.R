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

#########################################################################
########### FIX ERRORS IN THE DATASET!!! and ADD SOME COLUMNS ###########
#########################################################################


#########################################################################

E <- D

# remove NP rows !!!
E0 <- E %>% filter(EVENT_TX != "NP")
print("E0")

# FIX !!! ERROR from data_wrangling_2A:  DI is DEFENSIVE INDIFFERENCE not DOUBLE, so should have HIT_VAL = 0, EVENT_WOBA = 0
E1 <- E0 %>% mutate(HIT_VAL = ifelse(str_detect(EVENT_TX, "^DI"), 0, HIT_VAL),
                  HIT_BINARY = ifelse(str_detect(EVENT_TX, "^DI"), 0, HIT_BINARY),
                  EVENT_WOBA = ifelse(str_detect(EVENT_TX, "^DI"), 0, EVENT_WOBA))
print("E1")

# problem with DUPLICATE ROWS...
E2 <- E1 %>% distinct(across(c(INNING,BAT_HOME_IND,BAT_ID,COUNT,PITCH_SEQ_TX,EVENT_TX,GAME_ID,PIT_ID)), .keep_all = TRUE)
print("E2")

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
RR = E6 %>% filter(YEAR==2010,BAT_NAME=="David Ortiz") %>% select(INNING,BAT_HOME_IND,GAME_ID,HOME_TEAM_ID,AWAY_TEAM_ID,BAT_NAME,PIT_NAME,EVENT_TX,AB_IND,PA_IND)
View(RR)


E6 <- E5 %>% mutate(PA_IND =  !str_detect(EVENT_TX, "^BK") & !str_detect(EVENT_TX, "^CS") & !str_detect(EVENT_TX, "^DI") & 
                              !str_detect(EVENT_TX, "^OA") & !str_detect(EVENT_TX, "^PB") & !str_detect(EVENT_TX, "^WP") & 
                              !str_detect(EVENT_TX, "^PO") & !str_detect(EVENT_TX, "^SB") & !str_detect(EVENT_TX, "^NP"))

# AB_IND (at bat)
E7 <- E6 %>% mutate(x = !str_detect(EVENT_TX, "^W") & !str_detect(EVENT_TX, "^I") & !str_detect(EVENT_TX, "^C") &
                        !str_detect(EVENT_TX, "SF") & !str_detect(EVENT_TX, "SH"),
                    AB_IND = PA_IND & x) %>% select(!c(x))

# Check

D0 <- E7
################################
################################

# WOBA_CUMU_BAT (INDIVIDUAL BATTER'S QUALITY)
D1 <- D0 %>% group_by(YEAR, BAT_ID) %>%
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
    # https://www.fangraphs.com/players/robinson-cano/3269/stats?position=2B
    N = "David Ortiz" #"Shin-Soo Choo" #"Robinson Cano" #"Albert Pujols"
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
      filter( str_detect(EVENT_TX, "HR") ) #%>%
      #select(INNING,BATTER_SEQ_NUM,HOME_TEAM_ID,AWAY_TEAM_ID,GAME_ID,EVENT_TX,EVENT_WOBA,HIT_VAL,AB_IND,PA_IND,PIT_NAME)
    View(R4)
    
}






# xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
# PARK_EFFECT ???
# HOME_FIELD_EFFECT ???
# HOW FAR INTO THE SEASON WE ARE EFFECT ???
# NUM_DAYS_REST === number of days of rest the starting pitcher has prior to this game -> data????


