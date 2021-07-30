#install.packages("retrosheet")
library(retrosheet)
library(tidyverse)
library(pkgcond)
library(stringr)

# create new columns
#######################################################
# GAME_ID === unique id of this game [game_id]
# PARK === this park the game is played at [site]
# DATE === date of game [date]
# HOME_TEAM_ID === home team 1st 3 letters [hometeam]
# AWAY_TEAM_ID === away team 1st 3 letters [visteam]
# INNING === inning count (1:9 +) [inning]
# BAT_HOME_IND === 1 if home at bat, 0 if away at bat [team]
# PITCH_SEQ_TX === pitch sequence as text [pitches]
# BAT_ID === batter [retroID]
# BAT_NAME === batter name [name]
# PIT_ID === pitcher [pit.retroID]
# PIT_NAME === pitcher name [pit.name]
# PIT_HAND === L or R [pit.hand]
# COUNT === pitch count [count]
# EVENT_TX === play event text [play]
# PA_IND === 1 if it is an at-bat-event, i.e. a plate appearance (so, not a substitution or stolen base) [*****]
# HIT_VAL === hit or not, and type of hit, c(0,1,2,3,4) [*****]
# AB_IND === TRUE if it is an at-bat else FALSE [*****]
# EVENT_ER_CT === number of earned runs recorded during this event (take into accoount UR) [*****]
# EVENT_RBI_CT === number of RBIs recorded during this event (take into accoount NR) [*****]
# EVENT_RUNS === number of runs recorded during this event [*****]
# EVENT_OUTS_CT === number of outs recorded at that event [*****]
# OUTS_CT === number of outs at that time [*****]
#######################################################

################################
########### THE CODE ###########
################################

create.dataset.2A <- function(D,filename) {
  
    D1 = D %>% rename(GAME_ID = game_id,
                      PARK = site, 
                      DATE = date,
                      HOME_TEAM_ID = hometeam,
                      AWAY_TEAM_ID = visteam,
                      INNING = inning,
                      BAT_HOME_IND = team,
                      PITCH_SEQ_TX = pitches, 
                      BAT_ID = retroID,
                      BAT_NAME = name,
                      PIT_ID = pit.retroID,
                      PIT_NAME = pit.name,
                      PIT_HAND = pit.hand,
                      COUNT = count,
                      EVENT_TX = play) %>%
              select(!c("number"))
    
    ### make sure that we don't have duplicate rows! This error actually occurs a lot in Retrosheet! !!!!!!
    #D1.5 = D1 %>% group_by(GAME_ID, INNING, BAT_ID, PITCH_SEQ_TX, EVENT_TX) %>% filter(row_number() == 1) %>% ungroup()
    
    #######################################################
    
    # https://www.retrosheet.org/eventfile.htm
    #https://www.retrosheet.org/datause.txt
    # PA_IND === 1 if it is an at-bat-event, i.e. a plate appearance (so, not a substitution or stolen base) 
    is_pa_helper <- function(event_tx) {
      !startsWith(event_tx, "BK") & !startsWith(event_tx, "CS") & !startsWith(event_tx, "DI") &
      !startsWith(event_tx, "OA") & !startsWith(event_tx, "PB") & !startsWith(event_tx, "WP") &
      !startsWith(event_tx, "PO") & !startsWith(event_tx, "POCS") &
      !startsWith(event_tx, "SB") & !startsWith(event_tx, "NP")
    }
    D2 = D1 %>% mutate(PA_IND = sapply(EVENT_TX, is_pa_helper))
    print("D2")
    # HIT_VAL === hit or not, and type of hit, c(0,1,2,3,4)
    compute_hit_val <- function(event_tx) {
      if (startsWith(event_tx, "S") & !startsWith(event_tx, "SB") & !startsWith(event_tx, "SF")) {
        1
      } else if (startsWith(event_tx, "D") & !startsWith(event_tx, "DI")) {
        2
      } else if (startsWith(event_tx, "T")) {
        3
      } else if (startsWith(event_tx, "HR") | (startsWith(event_tx, "H") & !startsWith(event_tx, "HP")) ) {
        4
      } else {
        0
      }
    }
    D3 = D2 %>% mutate(HIT_VAL = sapply(EVENT_TX, compute_hit_val))
    print("D3")
    # PH_IND === 1 if pinch hitter else 0
    #D4 = D3 %>% mutate(PH_IND = (BAT_FLD_CD == 11))
    D4 = D3
    print("D4")
    # AB_IND === TRUE if it is an at-bat else FALSE
    is_at_bat_helper <- function(event_tx) {
      if (startsWith(event_tx, "W") | startsWith(event_tx, "I") | startsWith(event_tx, "C") |
          grepl("SF", event_tx, fixed=TRUE) | grepl("SH", event_tx, fixed=TRUE) |
         !is_pa_helper(event_tx) ) {
        0
      } else {
        1
      } 
    }
    D5 = D4 %>% mutate(AB_IND = (PA_IND & sapply(EVENT_TX, is_at_bat_helper)))
    print("D5")
    # EVENT_ER_CT === number of earned runs recorded during this event (take into accoount UR)
    # EVENT_RBI_CT === number of RBIs recorded during this event (take into accoount NR)
    # EVENT_RUNS === number of runs recorded during this event
    compute_rbi <- function(event_tx) {
        hit_val = compute_hit_val(event_tx)
        run_tx = str_extract(event_tx, "([^.]+$)")
        n = length(str_extract_all(run_tx, "-H")[[1]])
        norbi = length(str_extract_all(run_tx, "NR")[[1]])
        hr = if (hit_val == 4) 1 else 0
        n + hr - norbi
    }
    compute_er <- function(event_tx) {
      hit_val = compute_hit_val(event_tx)
      run_tx = str_extract(event_tx, "([^.]+$)")
      n = length(str_extract_all(run_tx, "-H")[[1]])
      unearned_run = length(str_extract_all(run_tx, "UR")[[1]])
      hr = if (hit_val == 4) 1 else 0
      n + hr - unearned_run
    }
    compute_event_runs <- function(event_tx) {
      hit_val = compute_hit_val(event_tx)
      run_tx = str_extract(event_tx, "([^.]+$)")
      n = length(str_extract_all(run_tx, "-H")[[1]])
      hr = if (hit_val == 4) 1 else 0
      n + hr
    }
    D6 = D5 %>% mutate(EVENT_ER_CT = sapply(EVENT_TX, compute_er),
                       EVENT_RBI_CT = sapply(EVENT_TX, compute_rbi),
                       EVENT_RUNS = sapply(EVENT_TX, compute_event_runs))
    print("D6")
    ########################################################
    # EVENT_OUTS_CT
    ########################################################
    
    num.outs.cs <- function(a) {
      # CS2(E1/TH).3-H(NR);1-3 --> 0 outs
      A = length(str_extract_all(a, "CS[23H]")[[1]]) -
          length(str_extract_all(a, "CS[23H]\\([0-9]*E")[[1]])
      return(A)
    }
    
    num.outs.po <- function(a) {
      A.po = length(str_extract_all(a, "PO[H123]")[[1]]) -
             length(str_extract_all(a, "PO[H123]\\([0-9]*E")[[1]])
      A.pocs = length(str_extract_all(a, "POCS[H123]")[[1]]) -
               length(str_extract_all(a, "POCS[H123]\\([0-9]*E")[[1]])
      A = if (startsWith(a, "POCS")) A.pocs else A.po
      return(A)
    }
    
    num.outs.startsWithNum <- function(a) {
      # 63/G --> 1 out
      # 6E3/G --> 0 outs
      A = length(str_extract_all(a, "^[0-9]*")[[1]]) -
          length(str_extract_all(a, "^[0-9]*E")[[1]])
      return(A)
    }
    
    num.baserunner.outs <- function(b) {
      # S9/L.2-3;1X3(936) --> 1 baserunner out
      # FC4.1X2(4E6);B-1 --> 0 baserunner outs
      # FC6/G.2XH(NR)(6E5)(UR);B-2\n --> 0 outs (remove the (NR), (UR))
      b = if (str_detect(b, "\\(NR\\)")) str_remove(b,"\\(NR\\)") else b
      b = if (str_detect(b, "\\(UR\\)")) str_remove(b,"\\(NR\\)") else b
      
      B = length(str_extract_all(b, "[123BH]X[123BH]")[[1]]) -
          length(str_extract_all(b, "[123BH]X[123BH]\\([0-9]*E")[[1]])
      return(B)
    }
    
    starts.with.num <- function(a) {
      A = length(str_extract_all(a, "^[0-9]")[[1]])
      return(A)
    }
    
    # EVENT_OUTS_CT === number of outs recorded at that event
    compute_event_outs_ct <- function(event_tx) {
      s = strsplit(event_tx, ".", fixed=TRUE)[[1]]
      a = s[1] # description of the basic play
      b = if (length(s) > 1) s[2] else NA # the advancement of any runners
      A = 0
      B = 0
      
      if (grepl("DP", event_tx, fixed=TRUE)) {
        return(2)
      } else if (grepl("TP", event_tx, fixed=TRUE)) {
        return(3)
      }
      
      # set the value of A
      {
        if (starts.with.num(a)) {
          # 63/G --> 1 out
          # 6E3/G --> 0 outs
          A = num.outs.startsWithNum(a) 
        } else if (startsWith(a, "C/E")) {
          A = 0
        } else if (startsWith(a, "S")) {
          A = 0
        } else if (startsWith(a, "D")) {
          A = 0
        } else if (startsWith(a, "T")) {
          A = 0
        } else if (startsWith(a, "E")) {
          A = 0
        } else if (startsWith(a, "FC")) {
          # ignore fielder's choice. looks at the baserunner activity
          A = 0
        } else if (startsWith(a, "FLE")) {
          A = 0
        } else if (startsWith(a, "HP") | startsWith(a, "HR") | startsWith(a, "H")) {
          A = 0
        } else if (startsWith(a, "K") & !startsWith(a, "K+")) {
          # sometimes 0, sometimes 1 --> look at baserunner advances
          # K.BX1(2E3) --> 0 outs
          # K.BX1 --> 1 outs
          # K --> 1 out
          A = if (grepl("B", b, fixed=TRUE)) 0 else 1
        } else if (startsWith(a, "K+SB")) {
          A = 1
          return(1)
        } else if (startsWith(a, "K+CS")) {
          # guaranteed double play
          A = 2 
          return(2)
        }  else if (startsWith(a, "K+PO")) {
          # guaranteed double play
          A = 2
          return(2)
        } else if (startsWith(a, "K+E")) {
          A = 0
        } else if (startsWith(a, "K+PB")) {
          # sometimes 0, sometimes 1 --> look at baserunner advances
          #K+PB.B-1 --> 0 outs
          #K+PB.2-3;1-2 --> 1 out
          A = if (grepl("B-", b, fixed=TRUE)) 0 else 1
        } else if (startsWith(a, "K+WP")) {
          #K+WP.1-2 --> 1 out
          #K+WP.B-1 --> 0 outs
          A = if (grepl("B-", b, fixed=TRUE)) 0 else 1
        } else if (startsWith(a, "K+OA")) {
          # sometimes 0, sometimes 1 --> look at baserunner advances
          A = 0
        } else if (startsWith(a, "NP")) {
          A = 0
        } else if ( (startsWith(a, "W") & !startsWith(a, "W+")) |
                    (startsWith(a, "IW") & !startsWith(a, "IW+")) | 
                    (startsWith(a, "I") & !startsWith(a, "I+")) )  {
          A = 0
        } else if (startsWith(a, "W+SB") | startsWith(a, "IW+SB") | startsWith(a, "I+SB")) {
          A = 0
        } else if (startsWith(a, "W+CS") | startsWith(a, "IW+CS") | startsWith(a, "I+CS")) {
          A = 1
        } else if (startsWith(a, "W+PO") | startsWith(a, "IW+PO") | startsWith(a, "I+PO")) {
          A = 1
        } else if (startsWith(a, "W+PB") | startsWith(a, "IW+PB") | startsWith(a, "I+PB")) {
          A = 0
        } else if (startsWith(a, "W+WP") | startsWith(a, "IW+WP") | startsWith(a, "I+WP")) {
          A = 0
        } else if (startsWith(a, "W+E") | startsWith(a, "IW+E") | startsWith(a, "I+E")) {
          A = 0
        } else if (startsWith(a, "W+OA")) {
          A = 0
        } else if (startsWith(a, "BK")) {
          A = 0
        } else if (startsWith(a, "CS")) {
          A = num.outs.cs(a)
        } else if (grepl("DI", a, fixed=TRUE)) {
          A = 0
        } else if (startsWith(a, "OA")) {
          A = 0
          # need to look at b
        } else if (startsWith(a, "PB")) {
          A = 0
        } else if (startsWith(a, "WP")) {
          A = 0
        } else if (startsWith(a, "PO")) {
          # inclueds "POCS"
          A = num.outs.po(a)
        } else if (startsWith(a, "SB")) {
          A = 0
          return(0)
        } else {
          print("*****")
          print(a)
        }
      }
      
      # set the value of B
      B = if (!is.na(b)) num.baserunner.outs(b) else 0
      
      return(A+B)
    }
      
    D7 = D6 %>% mutate(EVENT_OUTS_CT = sapply(EVENT_TX, compute_event_outs_ct))
    
    print("D7")
    
    ########################################################
    
    # OUTS_CT === number of outs PRIOR TO the play
    D8 = D7 %>% group_by(GAME_ID, BAT_HOME_IND, INNING) %>%
                mutate(OUTS_CT_after = cumsum(EVENT_OUTS_CT)) %>%
                ungroup() %>%
                mutate(OUTS_CT = OUTS_CT_after - EVENT_OUTS_CT) %>%
                select(!c(OUTS_CT_after))
    print("D8")
    
    result = D8
    write_csv(result, filename)
}


##############################
########### RUNNIT ###########
##############################
D = read_csv("retro1_PA_1990-2020.csv")
# D = Dog %>% filter(startsWith(as.character(date), "2012"))
# takes about ~45 mins to run on 30 years worth of data
create.dataset.2A(D,"retro2A_PA_1990-2020.csv")
E = read_csv("retro2A_PA_1990-2020.csv")


########################################################################


########################################################
########### SANITY CHECKS FOR EVENT_OUTS_CT ############
########################################################

{
  # # outs check: make sue all OUTS_CT are in c(0,1,2)
  # # CHECK this tibble is empty
  # View(E %>% filter(OUTS_CT >= 3) %>%
  #       select(GAME_ID, BAT_HOME_IND, INNING, EVENT_TX, EVENT_OUTS_CT, OUTS_CT))
  # 
  # # outs check: make sure the inning ends with 3 outs (and drop the last inning)
  # # CHECK this tibble is empty
  # View(E %>% group_by(GAME_ID, BAT_HOME_IND) %>% filter(INNING < max(INNING)) %>% group_by(INNING) %>%
  #        slice_tail() %>% filter(EVENT_OUTS_CT + OUTS_CT != 3) %>% ungroup() %>% ungroup() %>%
  #        select(GAME_ID, BAT_HOME_IND, INNING, EVENT_TX, EVENT_OUTS_CT, OUTS_CT))
  #
  # # specific game and inning check
  # game = "CHA201208260"
  # inning = 7
  # View(E %>% filter(GAME_ID == game, INNING == inning) %>% 
  #        select(GAME_ID, BAT_HOME_IND, INNING, EVENT_TX, EVENT_OUTS_CT, OUTS_CT))
  # 
  # # specific full game check
  # game = "BOS201207310"
  # View(E %>% filter(GAME_ID == game) %>% 
  #        select(GAME_ID, BAT_HOME_IND, INNING, EVENT_TX, EVENT_OUTS_CT, OUTS_CT))
}







