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
# HIT_BINARY === 1 if hit else 0
# consecutive.bat.row === 1 if this row is the same batter as the previous row
# BATTER_SEQ_NUM
# ORDER_CT === time thru the order number {1,2,3,..}
# HAND_MATCH === 1 if pitcher and batter handedness match, else 0
# SP_IND === True if is a starting pitcher in this game
#######################################################

################################
########### THE CODE ###########
################################

D = read_csv("retro01_PA_2020.csv") #FIXME
output_filename = "retro03_PA_2020.csv" #FIXME

################################

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
D2 <- D1 %>% filter(EVENT_TX != "NP" | (!is.na(PITCH_SEQ_TX) & str_detect(PITCH_SEQ_TX, "[^\\.]")) )
print("D2")

# HIT_VAL === hit or not, and type of hit, c(0,1,2,3,4)
D3 = D2 %>% mutate(HIT_VAL =  ifelse(str_detect(EVENT_TX, "^S") & !str_detect(EVENT_TX, "^SB") & 
                                    !str_detect(EVENT_TX, "^SF") & !str_detect(EVENT_TX, "^SH"), 1,
                              ifelse(str_detect(EVENT_TX, "^D") & !str_detect(EVENT_TX, "^DI"), 2,
                              ifelse(str_detect(EVENT_TX, "^T"), 3,
                              ifelse(str_detect(EVENT_TX, "^H") & !str_detect(EVENT_TX, "^HP"), 4, 0)))))
print("D3")

# HIT_BINARY === 1 if hit else 0
D4 = D3 %>% mutate(HIT_BINARY = HIT_VAL > 0)
print("D4")

# BATTER_SEQ_NUM, ORDER_CT
# For BATTER_SEQ_NUM, do not count the same player twice in a row; hence use `consecutive.bat.row`
D5 <- D4 %>%  group_by(GAME_ID, BAT_HOME_IND) %>%
              mutate(consecutive.bat.row = lag(BAT_ID)==BAT_ID,
                     consecutive.bat.row = ifelse(is.na(consecutive.bat.row), FALSE, consecutive.bat.row),
                     BATTER_SEQ_NUM = cumsum(!consecutive.bat.row), #row_number(),
                     ORDER_CT = 1 + (BATTER_SEQ_NUM-1) %/% 9) %>%
              ungroup() 
print("D5")
# Check
#View(D5 %>% select(INNING,BAT_HOME_IND,GAME_ID,HOME_TEAM_ID,AWAY_TEAM_ID,EVENT_TX, BAT_NAME,consecutive.bat.row,BATTER_SEQ_NUM, ORDER_CT))

# HAND_MATCH 
# check unique(D$BAT_HAND) and unique(D$PIT_HAND)
D6 <- D5 %>% mutate(HAND_MATCH = ifelse(is.na(BAT_HAND) | is.na(PIT_HAND), NA,
                                        ifelse(BAT_HAND == "B" | PIT_HAND == "B", TRUE,
                                               BAT_HAND == PIT_HAND)))
print("D6")
# Check
#View(D6 %>% select(INNING,BAT_HOME_IND,GAME_ID,HOME_TEAM_ID,AWAY_TEAM_ID,BAT_NAME,BAT_HAND,PIT_NAME,PIT_HAND,HAND_MATCH))

# SP_IND, PITCH_COUNT_CUMU, PITCH_COUNT_FINAL
D7 <- D6 %>%  group_by(GAME_ID, BAT_HOME_IND) %>% mutate(first.p = first(PIT_ID)) %>% ungroup() %>%
              group_by(GAME_ID, PIT_ID) %>%
              mutate(SP_IND = (PIT_ID == first.p)) %>%
              ungroup() %>% select(!c(first.p))
print("D7")
# Check
#View(D7 %>% select(INNING,BAT_HOME_IND,GAME_ID,BAT_NAME,HOME_TEAM_ID,AWAY_TEAM_ID,PIT_NAME,SP_IND))

################################
################################

result = D7
write_csv(result, output_filename)









