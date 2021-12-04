library(tidyverse)

################################
########### THE CODE ###########
################################

input_filename = "retro_final_PA_1990-2020d.csv" #FIXME
output_filename = "design_matrix4_10.csv" #FIXME
D0 <- read_csv(input_filename)

# need 2006-2019 to get 2010-2019 for the running avg. wOBA estimator...
# want years 2010-2019
# only include starting pitchers and wOBA-appearances
D1 <- D0 %>% filter(YEAR >= 2006, YEAR <= 2019,SP_IND == 1, WOBA_APP == 1)

# MAKE SURE NO PITCHERS ARE BATTERS
D1a <- D1 %>% group_by(GAME_ID) %>%
              mutate(PIT_IS_BAT = BAT_ID %in% PIT_ID) %>%
              ungroup()
#View(Da %>% select(GAME_ID, BAT_ID, PIT_ID, PIT_IS_BAT))
D1b <- D1a %>% filter(!PIT_IS_BAT)

# select relevant columns, and view this
D2 <- D1b %>% select(row_idx,YEAR,DATE,GAME_ID,INNING, #BAT_NAME,PIT_NAME,
                     PIT_ID, BAT_ID, WOBA_APP, EVENT_WOBA_19, #OUTS_CT, BASE_STATE
                     HAND_MATCH, BAT_HOME_IND, 
                     BATTER_SEQ_NUM,
                     ORDER_CT) %>%
              mutate(across(HAND_MATCH, as.integer)) #%>% 
              #mutate(across(IN_DIV, as.integer)) %>% 
              #mutate(across(IN_LEAGUE, as.integer))

# create BATTER_IDX
# BATTER_IDX === given the game and the team, assign the batters an integer {1,2,3,...} in order of appearance in the game
D3 <- D2 %>% group_by(GAME_ID, BAT_HOME_IND) %>%
             mutate(BATTER_IDX = match(BAT_ID, unique(BAT_ID))) %>% 
             ungroup()
#View(D3 %>% select(GAME_ID, INNING, BAT_HOME_IND, PIT_NAME, BAT_NAME, BATTER_IDX))


# check columns with NA
sapply(1:ncol(D3),fun <- function(i) {sum(is.na(D3[,i]))})
# only HAND_MATCH has NA
# change HAND_MATCH NA to 0.5
D6 <- D3 %>% mutate(HAND_MATCH = ifelse(is.na(HAND_MATCH), 0.5, HAND_MATCH))

###############################################################
  
### design matrix!
X <- D6
write_csv(X, output_filename)
                  


