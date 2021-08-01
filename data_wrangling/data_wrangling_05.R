#install.packages("retrosheet")
library(tidyverse)
library(stringr)

# -> add new columns to Dataset_2, to create Dataset_3, for a given year 
########################################################################
# HOME_LEAGUE {AL, NL}
# AWAY_LEAGUE {AL, NL}
# HOME_DIV {NL_E, NL_C, NL_W, AL_E, AL_C, AL_W}
# AWAY_DIV {NL_E, NL_C, NL_W, AL_E, AL_C, AL_W}
# TEAM_MATCH {IN_DIV, IN_LEAG}
# EVENT_PITCH_COUNT== pitch count per event
# PITCH_COUNT_CUMU === pitch count up to this point in the game
# PITCH_COUNT_FINAL === final pitch count of the game for each starter
########################################################################

################################
########### THE CODE ###########
################################

W = read_csv("woba_weights_Fangraphs.csv") 
Div = read_csv("mlb_divisions_dataset.csv")

create.dataset.3 <- function(D, filename) {
    # HOME_DIV {NL_E, NL_C, NL_W, AL_E, AL_C, AL_W}
    # AWAY_DIV {NL_E, NL_C, NL_W, AL_E, AL_C, AL_W}
    # HOME_LEAGUE {AL, NL}
    # AWAY_LEAGUE {AL, NL}
    # TEAM_MATCH {IN_DIV, IN_LEAGUE}
    compute_divs <- function(yr, TEAM_IDS) { 
      print(yr)
      f <- function(team_id) {
        Div[yr >= Div$start & yr <= Div$end, ][[team_id]]
      }
      x = sapply(TEAM_IDS, f)
      unname(x)
    }
    #######
    D2 = D %>% group_by(YEAR) %>%
                mutate(HOME_DIV = compute_divs(unique(YEAR), HOME_TEAM_ID),
                       AWAY_DIV = compute_divs(unique(YEAR), AWAY_TEAM_ID),
                       HOME_LEAGUE = substr(HOME_DIV,start=1,stop=2),
                       AWAY_LEAGUE = substr(AWAY_DIV,start=1,stop=2),
                       IN_DIV = (HOME_DIV == AWAY_DIV),
                       IN_LEAGUE = (HOME_LEAGUE == AWAY_LEAGUE)) %>%
                ungroup()
    print("D2")
    
    # EVENT_PITCH_COUNT== pitch count per event
    # https://www.retrosheet.org/datause.txt    --> field 5
    compute_event_pitch_count <- function(pitch_tx) {
      # pitches: C,S,B,F,X,T,H,L,M,P,K,U,Q,R   --> 14
      # not a pitch: N,V,1,2,3,+,>,*,.,        --> 9
      p = str_remove_all(pitch_tx, "[NV123\\+\\>\\*\\.]")
      nchar(p)
    }
    D4 = D2 %>% mutate(EVENT_PITCH_COUNT = sapply(PITCH_SEQ_TX, compute_event_pitch_count))
    print("D4")
    
    
    # PITCH_COUNT_CUMU, PITCH_COUNT_FINAL
    D5 <- D4 %>% group_by(GAME_ID, PIT_ID) %>%
      mutate(PITCH_COUNT_CUMU = cumsum(replace_na(EVENT_PITCH_COUNT, 0)),
             PITCH_COUNT_FINAL = sum(EVENT_PITCH_COUNT, na.rm=TRUE)) %>%
      ungroup() %>% select(!c(first.p))
    print("D5")
    # Check
    #View(D5 %>% select(INNING,BAT_HOME_IND,GAME_ID,BAT_NAME,HOME_TEAM_ID,AWAY_TEAM_ID,PIT_NAME,SP_IND,PITCH_SEQ_TX, EVENT_PITCH_COUNT, PITCH_COUNT_CUMU, PITCH_COUNT_FINAL))
    
    
    
    # RUNS and RBIs      EVENT_ER_CT, EVENT_RBI_CT, EVENT_RUNS
    #FIXME --> do it better with str_detect ????
    
    
    
    ###########
    
    result = D5
    write_csv(result, filename)
}

########################################################################

##########################################
########### EXAMPLE: 2012 DATA ###########
##########################################

D = read_csv("retro2B_PA_1990-2020.csv")
# D = Dog %>% filter(YEAR == 2012)
filename = "retro3_PA_1990-2020.csv"
# takes ~30 min to run on 30 years of data
create.dataset.3(D, filename) 
E = read_csv(filename)

##############################
########### CHECKS ###########
##############################

{
    # # Check EVENT_PITCH_COUNT
    # P = paste((D2 %>% select(PITCH_SEQ_TX))$PITCH_SEQ_TX, collapse='') # giant string of all PITCH_SEQ_TX concatenated
    # s = rawToChar(unique(charToRaw(P))) # all the allowed characters in PITCH_SEQ_TX
    # s = paste(sort(unlist(strsplit(s, ""))), collapse = "")
    # s
    # nchar(s) == 14+9
    # # Check EVENT_PITCH_COUNT
    # View(D3 %>% select(PITCH_SEQ_TX, EVENT_PITCH_COUNT))
    # # Check ORDER_CT
    # View(D4 %>% select(GAME_ID, BAT_HOME_IND, INNING, BATTER_SEQ_NUM, ORDER_CT))
    # # Check EVENT_CODE
    # View(D6 %>% select(EVENT_TX, EVENT_CODE))  
    # # Check Divisions/Leagues
    # View(E %>% select(HOME_TEAM_ID, AWAY_TEAM_ID, HOME_DIV, AWAY_DIV, HOME_LEAGUE, AWAY_LEAGUE, IN_DIV, IN_LEAGUE))
}
