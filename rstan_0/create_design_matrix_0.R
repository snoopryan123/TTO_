library(tidyverse)

################################
########### THE CODE ###########
################################

input_filename = "retro_final_PA_1990-2020b.csv" #FIXME
output_filename = "design_matrix_2015-2020b.csv" #FIXME
D0 <- read_csv(input_filename)
D0 <- D0 %>% filter(YEAR >= 2015) #FIXME

# only include starting pitchers and wOBA-appearances
D1 <- D0 %>% filter(SP_IND == 1, WOBA_APP == 1) 

# select relevant columns, and view this
D2 <- D1 %>% select(GAME_ID, INNING, BAT_NAME, PIT_NAME,
                    EVENT_WOBA_19, 
                    WOBA_AVG_BAT_19, WOBA_AVG_PIT_19, 
                    WOBA_FINAL_BAT_19, WOBA_FINAL_PIT_19,
                    NUM_WOBA_APP_BAT, NUM_WOBA_APP_PIT,
                    NUM_WOBA_APP_FINAL_BAT, NUM_WOBA_APP_FINAL_PIT,
                    HAND_MATCH, BAT_HOME_IND, PIT_REST, DAYS_SINCE_SZN_START,
                    IN_DIV, IN_LEAGUE, PITCH_COUNT_CUMU,
                    FIELD_POS, OUTS_CT,
                    #PARK, BATTER_SEQ_NUM)
                    ORDER_CT) %>%
              mutate(across(HAND_MATCH, as.integer)) %>% 
              mutate(across(IN_DIV, as.integer)) %>% 
              mutate(across(IN_LEAGUE, as.integer))

# create BATTER_IDX
# BATTER_IDX === given the game and the team, assign the batters an integer {1,2,3,...} in order of appearance in the game
D3 <- D2 %>% group_by(GAME_ID, BAT_HOME_IND) %>%
             mutate(BATTER_IDX = match(BAT_NAME, unique(BAT_NAME))) %>% 
             ungroup()
#View(D3 %>% select(GAME_ID, INNING, BAT_HOME_IND, PIT_NAME, BAT_NAME, BATTER_IDX))

# `character` columns for the categorical variables
D4 <- D3 %>%  mutate(across(FIELD_POS, as.character)) %>% 
              mutate(across(OUTS_CT, as.character)) %>% 
              mutate(across(ORDER_CT, as.character)) %>% 
              mutate(across(BATTER_IDX, as.character)) 
#View(D4 %>% filter(GAME_ID == "ANA202008180", BAT_HOME_IND == 0) %>% select(GAME_ID, BAT_HOME_IND, INNING, BAT_NAME, PIT_NAME, BATTER_IDX))
#View(D4 %>% filter(GAME_ID == "ANA202008180", BAT_HOME_IND == 1) %>% select(GAME_ID, BAT_HOME_IND, INNING, BAT_NAME, PIT_NAME, BATTER_IDX))

# remove columns only meant for viewing
D5 <- D4 %>% select(-c(GAME_ID, INNING, BAT_NAME, PIT_NAME))

# design matrix!
X <- D5
write_csv(X, output_filename)
                  


