library(tidyverse)

################################
########### THE CODE ###########
################################

input_filename = "retro_final_PA_2020.csv"
D0 <- read_csv(input_filename)

# only include starting pitchers and wOBA-appearances
D1 <- D0 %>% filter(SP_IND == 1, WOBA_APP == 1) 

# select relevant columns, and view this
D2 <- D1 %>% select(GAME_ID, INNING, BAT_NAME, PIT_NAME,
                    EVENT_WOBA, WOBA_CUMU_BAT, WOBA_CUMU_PIT, HAND_MATCH,
                    BAT_HOME_IND, PIT_REST, DAYS_SINCE_SZN_START,
                    IN_DIV, IN_LEAGUE, PITCH_COUNT_CUMU,
                    FIELD_POS, OUTS_CT, # categorical
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

# remove columns only meant for viewing
D4 <- D3 %>% select(-c(GAME_ID, INNING, BAT_NAME, PIT_NAME))




# design matrix!
X <- D4
write_csv(X, "design_matrix_0.csv")
                  


