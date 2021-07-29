#install.packages("retrosheet")
library(tidyverse)
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
# TEAM_MATCH {IN_DIV, IN_LEAG}
########################################################################

################################
########### THE CODE ###########
################################

W = read_csv("woba_weights_Fangraphs.csv") 
Div = read_csv("mlb_divisions_dataset.csv")

create.dataset.3 <- function(D, filename) {
    # EVENT_PITCH_COUNT== pitch count per event
    # https://www.retrosheet.org/datause.txt    --> field 5
    compute_event_pitch_count <- function(pitch_tx) {
      # pitches: C,S,B,F,X,T,H,L,M,P,K,U,Q,R   --> 14
      # not a pitch: N,V,1,2,3,+,>,*,.,        --> 9
      p = str_remove_all(pitch_tx, "[NV123\\+\\>\\*\\.]")
      nchar(p)
    }
    D3 = D %>% mutate(EVENT_PITCH_COUNT = sapply(PITCH_SEQ_TX, compute_event_pitch_count))
    print("D3")
    
    # PITCH_COUNT_CUMU === pitch count up to this point in the game
    # PITCH_COUNT_FINAL === final pitch count of the game for each starter
    # ORDER_CT === time thru the order number {1,2,3,..}
    D4 = D3 %>% group_by(GAME_ID, BAT_HOME_IND) %>%
                mutate(PITCH_COUNT_CUMU = cumsum(replace_na(EVENT_PITCH_COUNT, 0)),
                       PITCH_COUNT_FINAL = sum(EVENT_PITCH_COUNT, na.rm=TRUE),
                       ORDER_CT = 1 + (BATTER_SEQ_NUM-1) %/% 9) %>%
                ungroup()
    print("D4")
    
    # HIT_BINARY === 1 if hit else 0
    D5 = D4 %>% mutate(HIT_BINARY = HIT_VAL > 0)
    print("D5")
    
    # EVENT_CODE === {IW, W, HP, NA}    --> [need for wOBA calculation]
    D6 = D5 %>% mutate(EVENT_CODE = ifelse(grepl("IW", EVENT_TX, fixed=TRUE), "IW",
                                    ifelse(startsWith(EVENT_TX, "W") & !startsWith(EVENT_TX, "WP"), "W",
                                    ifelse(grepl("HP", EVENT_TX, fixed=TRUE), "HP",
                                    "other" ))))
    print("D6")
    
    # EVENT_WOBA === wOBA of this event
    # https://www.fangraphs.com/guts.aspx?type=cn
    # HP, is included in AB_IND and PA_IND
    # SH, SF, IW, W are included in PA_IND but not AB_IND
    # an event is a WOBA_EVENT iff it is an {AB, W, SH, SF, HP} but not {IW}. Equivalently, {PA}\{IW}
    # ---> include all plate appearances as wOBA except intentional walks !!!
    D7 = D6 %>% group_by(YEAR) %>%
                mutate(EVENT_WOBA = ifelse(HIT_VAL == 1, W[W$Season == unique(D$YEAR),]$w1B, # single
                                    ifelse(HIT_VAL == 2, W[W$Season == unique(D$YEAR),]$w2B, # double
                                    ifelse(HIT_VAL == 3, W[W$Season == unique(D$YEAR),]$w3B, # triple
                                    ifelse(HIT_VAL == 4, W[W$Season == unique(D$YEAR),]$wHR, # HR
                                    ifelse(EVENT_CODE == "W", W[W$Season == unique(D$YEAR),]$wBB, # uBB / NIBB
                                    ifelse(EVENT_CODE == "HP", W[W$Season == unique(D$YEAR),]$wHBP, # HBP / HP
                                    #ifelse(EVENT_CD==18, 0.92, # RBOE (reached base on error) --> no longer in the woba formula
                                    ifelse(PA_IND &  (EVENT_CODE != "IW"), 0, 
                                    NA )))))))) %>%
                ungroup()
    print("D7")
    
    # HOME_DIV {NL_E, NL_C, NL_W, AL_E, AL_C, AL_W}
    # AWAY_DIV {NL_E, NL_C, NL_W, AL_E, AL_C, AL_W}
    # HOME_LEAGUE {AL, NL}
    # AWAY_LEAGUE {AL, NL}
    # TEAM_MATCH {IN_DIV, IN_LEAGUE}
    browser()
    
    compute_divs <- function(yr, TEAM_IDS) { 
      f <- function(team_id) {
        Div[yr >= Div$start & yr <= Div$end, ][[team_id]]
      }
      x = sapply(TEAM_IDS, f)
      unname(x)
    }
    #######
    D8 = D7 %>% group_by(YEAR) %>%
                mutate(HOME_DIV = compute_divs(unique(D$YEAR), HOME_TEAM_ID),
                       AWAY_DIV = compute_divs(unique(D$YEAR), AWAY_TEAM_ID),
                       HOME_LEAGUE = substr(HOME_DIV,start=1,stop=2),
                       AWAY_LEAGUE = substr(AWAY_DIV,start=1,stop=2),
                       IN_DIV = (HOME_DIV == AWAY_DIV),
                       IN_LEAGUE = (HOME_LEAGUE == AWAY_LEAGUE)) %>%
                ungroup()
    print("D8")
    
    # d = Div %>% filter(year >= start, year <= end)
    # compute_team_div <- function(team_id) { d[[team_id]] }
    # D8 = D7 %>% mutate(HOME_DIV = sapply(HOME_TEAM_ID, compute_team_div),
    #                    AWAY_DIV = sapply(AWAY_TEAM_ID, compute_team_div),
    #                    HOME_LEAGUE = substr(HOME_DIV,start=1,stop=2),
    #                    AWAY_LEAGUE = substr(AWAY_DIV,start=1,stop=2),
    #                    INTRA_DIV = (HOME_DIV == AWAY_DIV),
    #                    INTRA_LEAGUE = (HOME_LEAGUE == AWAY_LEAGUE))
    
    result = D8
    write_csv(result, filename)
}

########################################################################

##########################################
########### EXAMPLE: 2012 DATA ###########
##########################################

Dog = read_csv("retro2B_PA_1990-2020.csv")
D = Dog %>% filter(YEAR == 2012)
filename = "retro3_PA_1990-2020.csv"
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
    # # Check PITCH_COUNT_CUMU, PITCH_COUNT_FINAL
    # View(D4 %>% select(GAME_ID, BAT_HOME_IND, INNING, PITCH_SEQ_TX, EVENT_PITCH_COUNT, PITCH_COUNT_CUMU, PITCH_COUNT_FINAL))
    # # Check ORDER_CT
    # View(D4 %>% select(GAME_ID, BAT_HOME_IND, INNING, BATTER_SEQ_NUM, ORDER_CT))
    # # Check HIT_BINARY
    # View(D5 %>% select(HIT_VAL, HIT_BINARY))
    # # Check EVENT_CODE
    # View(D6 %>% select(EVENT_TX, EVENT_CODE))  
    # # Check EVENT_WOBA
    # View(E %>% select(EVENT_TX, HIT_VAL, EVENT_WOBA, PA_IND, EVENT_CODE))
    # # Check Divisions/Leagues
    View(E %>% select(HOME_TEAM_ID, AWAY_TEAM_ID, HOME_DIV, AWAY_DIV, HOME_LEAGUE, AWAY_LEAGUE, IN_DIV, IN_LEAGUE))
}
