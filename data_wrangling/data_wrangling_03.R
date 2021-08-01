#install.packages("retrosheet")
#library(pkgcond)
#library(stringr)
library(tidyverse)

# create new columns
#######################################################
# GAME_ID === unique id of this game [game_id]
# PARK === this park the game is played at [site]
# DATE === date of game [date]
# YEAR === year of game [year]
# HOME_TEAM_ID === home team 1st 3 letters [hometeam]
# AWAY_TEAM_ID === away team 1st 3 letters [visteam]
# INNING === inning count (1:9 +) [inning]
# BAT_HOME_IND === 1 if home at bat, 0 if away at bat [team]
# BAT_ID === batter [retroID]
# BAT_NAME === batter name [name]
# BAT_HAND === L or R [bat.hand]
# FIELD_POS === field position [fieldPos]
# PIT_ID === pitcher [pit.retroID]
# PIT_NAME === pitcher name [pit.name]
# PIT_HAND === L or R [pit.hand]
# COUNT === pitch count [count]
# PITCH_SEQ_TX === pitch sequence as text [pitches]
# EVENT_TX === play event text [play]
#******************************************************
# HIT_VAL === hit or not, and type of hit, c(0,1,2,3,4) [*****]

#######################################################

################################
########### THE CODE ###########
################################

D = read_csv("retro01_PA_2020.csv") #FIXME

### initial columns: name changes
D1 = D %>% rename(GAME_ID = game_id,
                  PARK = site, 
                  DATE = date,
                  YEAR = year,
                  HOME_TEAM_ID = hometeam,
                  AWAY_TEAM_ID = visteam,
                  INNING = inning,
                  BAT_HOME_IND = team,
                  BAT_ID = retroID,
                  BAT_NAME = name,
                  BAT_HAND = bat.hand,
                  FIELD_POS = fieldPos,
                  PIT_ID = pit.retroID,
                  PIT_NAME = pit.name,
                  PIT_HAND = pit.hand,
                  COUNT = count,
                  PITCH_SEQ_TX = pitches, 
                  EVENT_TX = play) %>% 
          relocate(GAME_ID, PARK, DATE, YEAR, HOME_TEAM_ID, AWAY_TEAM_ID, INNING, BAT_HOME_IND, BAT_ID,
                 BAT_NAME, BAT_HAND, FIELD_POS, PIT_ID, PIT_NAME, PIT_HAND, COUNT, PITCH_SEQ_TX, EVENT_TX)
print("D1")

### remove "NP" (no-play) rows
D2 <- D1 %>% filter(EVENT_TX != "NP")
print("D2")


### remove duplicate rows (sometimes the same play appears twice in a row...)
### ex: Dee Gordon vs. Hoby Milner, inning 6 of 2020 ANA202007290
D3 <- D2 %>% distinct(across(c(GAME_ID,INNING,BAT_HOME_IND,BAT_ID,PIT_ID,COUNT,PITCH_SEQ_TX,EVENT_TX)), .keep_all = TRUE)
print("D3")


### make sure that we don't have duplicate rows! This error actually occurs a lot in Retrosheet! !!!!!!
#D1.5 = D1 %>% group_by(GAME_ID, INNING, BAT_ID, PITCH_SEQ_TX, EVENT_TX) %>% filter(row_number() == 1) %>% ungroup()

#######################################################

# HIT_VAL === hit or not, and type of hit, c(0,1,2,3,4)
D5 = D1 %>% mutate(HIT_VAL =  ifelse(str_detect(EVENT_TX, "^S") & !str_detect(EVENT_TX, "^SB") & 
                                    !str_detect(EVENT_TX, "^SF") & !str_detect(EVENT_TX, "^SH"), 1,
                              ifelse(str_detect(EVENT_TX, "^D") & !str_detect(EVENT_TX, "^DI"), 2,
                              ifelse(str_detect(EVENT_TX, "^T"), 3,
                              ifelse(str_detect(EVENT_TX, "^H") & !str_detect(EVENT_TX, "^HP"), 4, 0)))))
print("D5")
# PH_IND === 1 if pinch hitter else 0
#D4 = D3 %>% mutate(PH_IND = (BAT_FLD_CD == 11))




result = D5
filename = "retro03_PA_2020.csv" #FIXME
write_csv(result, filename)









