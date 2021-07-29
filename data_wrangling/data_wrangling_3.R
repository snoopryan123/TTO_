#install.packages("retrosheet")
library(retrosheet)
library(tidyverse)
library(pkgcond)
library(stringr)

# -> add new columns to Dataset_2, to create Dataset_3, for a given year 
########################################################################
# EVENT_PITCH_COUNT== pitch count per event
# PITCH_COUNT_CUMU === pitch count up to this point in the game
# PITCH_COUNT_FINAL === final pitch count of the game for each starter
# ORDER_CT === time thru the order number {1,2,3,..}
# HIT_BINARY === 1 if hit else 0
# EVENT_CODE === {IW, W, HP, NA}  --> [need for wOBA calculation]
# EVENT_WOBA === wOBA of this event
# HOME_LEAGUE {AL, NL}
# AWAY_LEAGUE {AL, NL}
# HOME_DIV {NL_E, NL_C, NL_W, AL_E, AL_C, AL_W}
# AWAY_DIV {NL_E, NL_C, NL_W, AL_E, AL_C, AL_W}
# TEAM_MATCH {InterLEAG, IntraDIV, IntraLEAG}
########################################################################

################################
########### THE CODE ###########
################################

W = read_csv("woba_weights_Fangraphs.csv") 
Div = read_csv("mlb_divisions_dataset.csv")

create.dataset.3 <- function(D, year) {
    # # YEAR
    # D1 = D %>% mutate(YEAR = substr(DATE,start=1,stop=4))
    # # BATTER_SEQ_NUM 
    # D2 = D1 %>% group_by(GAME_ID, BAT_HOME_IND) %>%
    #             mutate(BATTER_SEQ_NUM = row_number()) %>%
    #             ungroup()
    # EVENT_PITCH_COUNT== pitch count per event
    # https://www.retrosheet.org/datause.txt    --> field 5
    compute_event_pitch_count <- function(pitch_tx) {
      # pitches: C,S,B,F,X,T,H,L,M,P,K,U,Q,R   --> 14
      # not a pitch: N,V,1,2,3,+,>,*,.,        --> 9
      p = str_remove_all(pitch_tx, "[NV123\\+\\>\\*\\.]")
      nchar(p)
    }
    D3 = D2 %>% mutate(EVENT_PITCH_COUNT = sapply(PITCH_SEQ_TX, compute_event_pitch_count))
    # PITCH_COUNT_CUMU === pitch count up to this point in the game
    # PITCH_COUNT_FINAL === final pitch count of the game for each starter
    D4 = D3 %>% group_by(GAME_ID, BAT_HOME_IND) %>%
                mutate(PITCH_COUNT_CUMU = cumsum(replace_na(EVENT_PITCH_COUNT, 0)),
                       PITCH_COUNT_FINAL = sum(EVENT_PITCH_COUNT, na.rm=TRUE)) %>%
                ungroup()
    # ORDER_CT === time thru the order number {1,2,3,..}
    D4 = D3 %>% group_by(GAME_ID, BAT_HOME_IND) %>%
                mutate(ORDER_CT = 1 + (BATTER_SEQ_NUM-1) %/% 9) %>%
                ungroup()
    # HIT_BINARY === 1 if hit else 0
    D5 = D4 %>% mutate(HIT_BINARY = HIT_VAL > 0)
    # EVENT_CODE === {IW, W, HP, NA}    --> [need for wOBA calculation]
    D6 = D5 %>% mutate(EVENT_CODE = ifelse(grepl("IW", EVENT_TX, fixed=TRUE), "IW",
                                    ifelse(startsWith(EVENT_TX, "W") & !startsWith(EVENT_TX, "WP"), "W",
                                    ifelse(grepl("HP", EVENT_TX, fixed=TRUE), "HP",
                                    "other" ))))
    # EVENT_WOBA === wOBA of this event
    # https://www.fangraphs.com/guts.aspx?type=cn
    # HP, is included in AB_IND and PA_IND
    # SH, SF, IW, W are included in PA_IND but not AB_IND
    # an event is a WOBA_EVENT iff it is an {AB, W, SH, SF, HP} but not {IW}. Equivalently, {PA}\{IW}
    # ---> include all plate appearances as wOBA except intentional walks !!!
    w = W %>% filter(Season == year)
    
    D7 = D6 %>% mutate(EVENT_WOBA = ifelse(HIT_VAL == 1, w$w1B, # single
                                    ifelse(HIT_VAL == 2, w$w2B, # double
                                    ifelse(HIT_VAL == 3, w$w3B, # triple
                                    ifelse(HIT_VAL == 4, w$wHR, # HR
                                    ifelse(EVENT_CODE == "W", w$wBB, # uBB / NIBB
                                    ifelse(EVENT_CODE == "HP", w$wHBP, # HBP / HP
                                    #ifelse(EVENT_CD==18, 0.92, # RBOE (reached base on error) --> no longer in the woba formula
                                    ifelse(PA_IND &  (EVENT_CODE != "IW"), 0, 
                                    NA )))))))) 
    # HOME_DIV {NL_E, NL_C, NL_W, AL_E, AL_C, AL_W}
    # AWAY_DIV {NL_E, NL_C, NL_W, AL_E, AL_C, AL_W}
    # HOME_LEAGUE {AL, NL}
    # AWAY_LEAGUE {AL, NL}
    # TEAM_MATCH {INTRA_DIV, INTRA_LEAGUE}
    d = Div %>% filter(year >= start, year <= end)
    compute_team_div <- function(team_id) {
      d[[team_id]]
    }
    D8 = D7 %>% mutate(HOME_DIV = sapply(HOME_TEAM_ID, compute_team_div),
                       AWAY_DIV = sapply(AWAY_TEAM_ID, compute_team_div),
                       HOME_LEAGUE = substr(HOME_DIV,start=1,stop=2),
                       AWAY_LEAGUE = substr(AWAY_DIV,start=1,stop=2),
                       INTRA_DIV = (HOME_DIV == AWAY_DIV),
                       INTRA_LEAGUE = (HOME_LEAGUE == AWAY_LEAGUE))
    
    result = D8
    filename = paste0("data3_", year, "_sp.csv")
    write_csv(result, filename)
    
    rm(D1)
    rm(D2)
    rm(D3)
    rm(D4)
    rm(D5)
    rm(D6)
    rm(D7)
}

##################################################
################ Years 2010 - 2019 ###############
##################################################

for (yr in 2010:2019) {
  print(yr)
  s = paste0("data2_",yr,"_sp.csv")
  D = read_csv(s)
  create.dataset.3(D, yr)
}

# AGGREGATE the datasets!
A <- tibble()
for (yr in 2010:2019) {
  print(yr)
  s = paste0("data3_",yr,"_sp.csv")
  D = read_csv(s)
  A <- bind_rows(A,D)
}
filename = "data3_2010-19_sp.csv"
write_csv(A, filename)


########################################################################

##########################################
########### EXAMPLE: 2012 DATA ###########
##########################################

D = read_csv("data2_2012_sp.csv")
year = 2012
create.dataset.3(D, year)
E = read_csv("data3_2012_sp.csv")

##############################
########### CHECKS ###########
##############################

{
    # Check BATTER_SEQ_NUM
    D1 %>% filter(GAME_ID == "ANA201204060", BAT_HOME_IND == 1) %>% 
           mutate(BATTER_SEQ_NUM = row_number()) %>% 
           select(GAME_ID, INNING, BAT_HOME_IND,BATTER_SEQ_NUM)
    # Check EVENT_PITCH_COUNT
    P = paste((D2 %>% select(PITCH_SEQ_TX))$PITCH_SEQ_TX, collapse='') # giant string of all PITCH_SEQ_TX concatenated
    s = rawToChar(unique(charToRaw(P))) # all the allowed characters in PITCH_SEQ_TX
    s = paste(sort(unlist(strsplit(s, ""))), collapse = "")
    s
    nchar(s) == 14+9
    # Check EVENT_PITCH_COUNT
    View(D3 %>% select(PITCH_SEQ_TX, EVENT_PITCH_COUNT))
    # Check PITCH_COUNT_CUMU, PITCH_COUNT_FINAL
    View(D4 %>% select(GAME_ID, BAT_HOME_IND, INNING, PITCH_SEQ_TX, EVENT_PITCH_COUNT, PITCH_COUNT_CUMU, PITCH_COUNT_FINAL))
    # Check ORDER_CT
    View(D4 %>% select(GAME_ID, BAT_HOME_IND, INNING, BATTER_SEQ_NUM, ORDER_CT))
    # Check HIT_BINARY
    View(D5 %>% select(HIT_VAL, HIT_BINARY))
    # Check EVENT_CODE
    View(D6 %>% select(EVENT_TX, EVENT_CODE))  
    # Check EVENT_WOBA
    View(D7 %>% select(EVENT_TX, HIT_VAL, EVENT_WOBA, PA_IND, EVENT_CODE))
    # Check Divisions/Leagues
    View(D8 %>% select(HOME_TEAM_ID, AWAY_TEAM_ID, HOME_DIV, AWAY_DIV, HOME_LEAGUE, AWAY_LEAGUE, INTRA_DIV, INTRA_LEAGUE))
}
