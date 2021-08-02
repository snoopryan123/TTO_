library(tidyverse)
library(xml2)
library(rvest)

########################################################################
# EVENT_CODE === {IW, W, HP, NA}  --> [need for wOBA calculation]
# PA_IND === TRUE iff it is a plate appearance
# AB_IND === TRUE iff it is an at-bat
# WOBA_APP === TRUE iff it is a wOBA-appearance, i.e. in {AB,W,SF,SH,HP}\{IW}
# EVENT_WOBA === wOBA of this event
# WOBA_CUMU_BAT === cumulative woba prior to this plate appearance for a given batter during a given season (INDIVIDUAL BATTER'S QUALITY)
# WOBA_CUMU_PIT === cumulative woba prior to this plate appearance for a given pitcher during a given season (INDIVIDUAL PITCHER'S QUALITY)
# EVENT_RUNS === number of runs recorded during this event
# EVENT_ER_CT === number of earned runs recorded during this event (take into accoount UR)
# EVENT_RBI_CT === number of RBIs recorded during this event (take into accoount NR)
# EVENT_PITCH_COUNT== pitch count per event
# PITCH_COUNT_CUMU === pitch count up to this point in the game
# PITCH_COUNT_FINAL === final pitch count of the game for each starter
########################################################################

################################
########### THE CODE ###########
################################

input_filename = "retro04_PA_2020.csv"
output_filename = "retro06_PA_2020.csv"
D <- read_csv(input_filename)
E <- D 

################################

### scrape WOBA WEIGHTS
content <- read_html("https://www.fangraphs.com/guts.aspx?type=cn")
tables <- content %>% html_table(fill = TRUE)
W <- tables[[9]]

################################

# EVENT_CODE === {IW, W, HP, NA}  --> [need for wOBA calculation]
E1 = E0 %>% mutate(EVENT_CODE =  ifelse(str_detect(EVENT_TX, "IW"), "IW",
                                ifelse(str_detect(EVENT_TX, "^W") & !str_detect(EVENT_TX, "^WP"), "W",
                                ifelse(str_detect(EVENT_TX, "HP"), "HP", "other" ))))
print("E1")

# fix AB_IND and PA_IND !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

# PA_IND (plate appearance) (so, not a substitution or stolen base) 
# definition:   https://en.wikipedia.org/wiki/Plate_appearance

#FIXME
#FIXME
#FIXME
# A batter is not credited with a plate appearance if, while batting, a preceding runner is put out on the basepaths for the third out in a way other than 
#by the batter putting the ball into play (i.e., picked off, caught stealing). In this case, the same batter continues his turn batting in the next inning with 
#no balls or strikes against him.
# A batter is not credited with a plate appearance if, while batting, the game ends as the winning run scores from third base on a balk, stolen base, wild pitch or passed ball.
# A batter may or may not be credited with a plate appearance (and possibly at bat) in the rare instance when he is replaced by a pinch hitter after having 
#already started his turn at bat. Under Rule 9.15(b), the pinch hitter would receive the plate appearance (and potential of an at-bat) unless the original batter 
#is replaced when having 2 strikes against him and the pinch hitter subsequently completes the strikeout, in which case the plate appearance and at-bat are charged
# to the first batter.
#FIXME
#FIXME
#FIXME - need the fine-tuning of whats not a PA. We're very close tho!
E2 <- E1 %>%  group_by(GAME_ID, BAT_HOME_IND) %>%
              mutate(x = lead(BAT_ID) != BAT_ID,
                     x = replace_na(x, TRUE),
                     PA_IND = x) %>% select(!c(x)) %>%
              ungroup() 
print("E2")

# AB_IND (at bat)
# AT-BAT is a PLATE-APPEARANCE without {SF,SH(sacBunt),W,IW,HP,C(catcher interference)}
# https://www.retrosheet.org/eventfile.htm
E3 <- E2 %>% mutate(AB_IND = PA_IND & !str_detect(EVENT_TX, "SF") & !str_detect(EVENT_TX, "SH") & !str_detect(EVENT_TX, "^W") & 
                        !str_detect(EVENT_TX, "^IW") & !str_detect(EVENT_TX, "^HP") & !str_detect(EVENT_TX, "^C"))
print("E3")

# WOBA_APP === TRUE iff it is a wOBA-appearance, i.e. in {AB,W,SF,SH,HP}\{IW}
E4 <- E3 %>% mutate(WOBA_APP = (AB_IND | (str_detect(EVENT_TX, "^W") & !str_detect(EVENT_TX, "^WP")) | 
                                          str_detect(EVENT_TX, "SF") | str_detect(EVENT_TX, "SH") |
                                          str_detect(EVENT_TX, "^HP")) & !str_detect(EVENT_TX, "^IW"))
# E4 <- E3 %>% mutate(WOBA_APP = PA_IND & !str_detect(EVENT_TX, "^IW") & !str_detect(EVENT_TX, "^C") &  !str_detect(EVENT_TX, "^INT") )  
print("E4")

# EVENT_WOBA === wOBA of this event
# https://www.fangraphs.com/guts.aspx?type=cn
# HP, is an AB and PA.
# SH, SF, IW, W are PA but not AB
# an event is a WOBA_EVENT iff it is an {AB, W, SH, SF, HP} but not {IW}. Equivalently, {PA}\{IW}
# ---> include all plate appearances as wOBA except intentional walks !!!
E5 = E4 %>% group_by(YEAR) %>% mutate(EVENT_WOBA = 
                ifelse(WOBA_APP & HIT_VAL == 1, W[W$Season == unique(YEAR),]$w1B, # single
                ifelse(WOBA_APP & HIT_VAL == 2, W[W$Season == unique(YEAR),]$w2B, # double
                ifelse(WOBA_APP & HIT_VAL == 3, W[W$Season == unique(YEAR),]$w3B, # triple
                ifelse(WOBA_APP & HIT_VAL == 4, W[W$Season == unique(YEAR),]$wHR, # HR
                ifelse(WOBA_APP & EVENT_CODE == "W", W[W$Season == unique(YEAR),]$wBB, # uBB / NIBB
                ifelse(WOBA_APP & EVENT_CODE == "HP", W[W$Season == unique(YEAR),]$wHBP, # HBP / HP
                #ifelse(EVENT_CD==18, 0.92, # RBOE (reached base on error) --> no longer in the woba formula
                #ifelse(PA_IND &  (EVENT_CODE != "IW"), 0, NA )))))))) %>% ungroup()
                0))))))) %>% ungroup()
print("E5")
#View(E5 %>% filter(GAME_ID == "BOS202008180"))

# WOBA_CUMU_BAT (INDIVIDUAL BATTER'S QUALITY)
E6 <- E5 %>% group_by(YEAR, BAT_ID) %>%
  mutate(cumu.woba.sum.b = cumsum(replace_na(EVENT_WOBA, 0)),
         cumu.woba.denom.b = cumsum(replace_na(WOBA_APP, 0)),
         WOBA_CUMU_BAT = cumu.woba.sum.b/cumu.woba.denom.b) %>% 
  ungroup()
print("E6")

# WOBA_CUMU_PIT (INDIVIDUAL PITCHER'S QUALITY)
E7 <- E6 %>% group_by(YEAR, PIT_ID) %>%
  mutate(cumu.woba.sum.p = cumsum(replace_na(EVENT_WOBA, 0)),
         cumu.woba.denom.p = cumsum(replace_na(WOBA_APP, 0)),
         WOBA_CUMU_PIT = cumu.woba.sum.p/cumu.woba.denom.p) %>% 
  ungroup()
print("E7")

# EVENT_RUNS === number of runs recorded during this event
# EVENT_ER_CT === number of earned runs recorded during this event (take into accoount UR)
# EVENT_RBI_CT === number of RBIs recorded during this event (take into accoount NR)
E8 = E7 %>% mutate(run_tx = str_extract(EVENT_TX, "([^.]+$)"),
                   num.home = str_count(run_tx, "-H"),
                   no.rbi = str_count(run_tx, "NR"),
                   unearned.runs = str_count(run_tx, "UR"), 
                   hr.ind = ifelse(HIT_VAL == 4, 1, 0),
                   EVENT_RUNS = num.home + hr.ind,
                   EVENT_ER_CT = EVENT_RUNS - unearned.runs,
                   EVENT_RBI_CT = EVENT_RUNS - no.rbi) %>%
            select(!c(run_tx, num.home, no.rbi, unearned.runs, hr.ind))
print("E8")
#View(E8 %>% select(EVENT_TX, HIT_VAL, EVENT_RUNS, EVENT_ER_CT, EVENT_RBI_CT))


# EVENT_PITCH_COUNT== pitch count per event
# https://www.retrosheet.org/datause.txt    --> field 5
# pitches: C,S,B,F,X,T,H,L,M,P,K,U,Q,R   --> 14
# not a pitch: N,V,1,2,3,+,>,*,.,        --> 9
E9 = E8 %>% mutate(p = str_remove_all(PITCH_SEQ_TX, "[NV123\\+\\>\\*\\.]"),
                   EVENT_PITCH_COUNT = str_length(p)) %>%
            select(!c(p))
print("E9")

# PITCH_COUNT_CUMU, PITCH_COUNT_FINAL
E10 <- E9 %>% group_by(GAME_ID, PIT_ID) %>%
             mutate(PITCH_COUNT_CUMU = cumsum(replace_na(EVENT_PITCH_COUNT, 0)),
                    PITCH_COUNT_FINAL = sum(EVENT_PITCH_COUNT, na.rm=TRUE)) %>%
             ungroup()
print("E10")
#View(E10 %>% select(INNING,BAT_HOME_IND,GAME_ID,BAT_NAME,HOME_TEAM_ID,AWAY_TEAM_ID,PIT_NAME,SP_IND,PITCH_SEQ_TX, EVENT_PITCH_COUNT, PITCH_COUNT_CUMU, PITCH_COUNT_FINAL))


##############################

R = E10
R_ = R %>% select(!c(cumu.woba.sum.b, cumu.woba.denom.b, cumu.woba.sum.p, cumu.woba.denom.p))
write_csv(R_, output_filename)

##############################
########### CHECKS ###########
##############################

{
    # 
    y = 2020
    w = W[W$Season == y,]
    
    # CHECK WOBA_CUMU_BAT
    # https://www.fangraphs.com/leaders.aspx?pos=all&stats=bat&lg=all&qual=y&type=8&season=2010&month=0&season1=2010&ind=0&team=0&rost=0&age=0&filter=&players=0&startdate=2010-01-01&enddate=2010-12-31&sort=16,d
    # https://www.fangraphs.com/leaders.aspx?pos=all&stats=bat&lg=all&qual=y&type=8&season=2020&month=0&season1=2020&ind=0&team=0&rost=0&age=0&filter=&players=0&startdate=2020-01-01&enddate=2020-12-31&sort=16,d
    R1 = R %>% filter(YEAR == y) %>% group_by(BAT_ID) %>% filter(row_number() == n(), cumu.woba.denom.b >= 150) %>% ungroup() %>% mutate(wOBA=round(WOBA_CUMU_BAT,3)) %>% 
               arrange(-WOBA_CUMU_BAT) %>% select(BAT_ID, BAT_NAME, YEAR, wOBA, cumu.woba.denom.b, WOBA_CUMU_BAT) 
    View(R1)
    
    # check individual player data 
    # https://www.fangraphs.com/players/robinson-cano/3269/stats?position=2B
    N = "Bryce Harper"
    R2 = R %>% filter(YEAR== y, BAT_NAME== N) %>% 
      summarise(G=length(unique(GAME_ID)), AB=sum(AB_IND), PA = sum(PA_IND), 
                H=sum(HIT_BINARY), S=sum(HIT_VAL==1), D=sum(HIT_VAL==2), T= sum(HIT_VAL==3), HR= sum(HIT_VAL==4),
                W= sum(EVENT_CODE=="W"), IW=sum(EVENT_CODE=="IW"), BB=W+IW, HP= sum(EVENT_CODE=="HP"), 
                AVG = H/AB, wOBA_num = (S*w$w1B + D*w$w2B + T*w$w3B + HR*w$wHR + W*w$wBB + HP*w$wHBP), wOBA_APP = last(cumu.woba.denom.b), 
                wOBA = wOBA_num/(wOBA_APP)) %>%
      select(!c(W)) %>% relocate(BB, .before=IW)
    R2
    
    # some plate appearances
    R3 = R %>% filter(YEAR== y, BAT_NAME== N) %>% arrange(GAME_ID,INNING,BATTER_SEQ_NUM) %>%
      #filter( str_detect(EVENT_TX, "HR") ) #%>%
      filter( HIT_VAL==4) %>%
      select(GAME_ID,INNING,BAT_HOME_IND,INNING,BATTER_SEQ_NUM,HOME_TEAM_ID,AWAY_TEAM_ID,GAME_ID,EVENT_TX,EVENT_WOBA,HIT_VAL,AB_IND,PA_IND,PIT_NAME)
    View(R3)
    
    # CHECK pitcher ERA
    # https://www.fangraphs.com/leaders.aspx?pos=all&stats=pit&lg=all&qual=y&type=8&season=2020&month=0&season1=2020&ind=0&team=0&rost=0&age=0&filter=&players=0&startdate=2020-01-01&enddate=2020-12-31&sort=17,a
    
}   







# xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
# PARK_EFFECT ???
# HOME_FIELD_EFFECT ???
# HOW FAR INTO THE SEASON WE ARE EFFECT ???
# NUM_DAYS_REST === number of days of rest the starting pitcher has prior to this game -> data????
