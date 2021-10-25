library(tidyverse)

################################
########### THE CODE ###########
################################

input_filename = "retro_final_PA_1990-2020b.csv" #FIXME
output_filename = "design_matrix2_2.csv" #FIXME
D0 <- read_csv(input_filename)
D0 <- D0 %>% filter(YEAR >= 2010, YEAR <= 2019) #FIXME

# only include starting pitchers and wOBA-appearances
D1 <- D0 %>% filter(SP_IND == 1, WOBA_APP == 1) 

# select relevant columns, and view this
D2 <- D1 %>% select(GAME_ID, INNING, YEAR, BAT_NAME, PIT_NAME,
                    EVENT_WOBA_19, 
                    WOBA_AVG_BAT_19, WOBA_AVG_PIT_19, 
                    WOBA_FINAL_BAT_19, WOBA_FINAL_PIT_19,
                    NUM_WOBA_APP_BAT, NUM_WOBA_APP_PIT,
                    NUM_WOBA_APP_FINAL_BAT, NUM_WOBA_APP_FINAL_PIT,
                    HAND_MATCH, BAT_HOME_IND, 
                    #PIT_REST, DAYS_SINCE_SZN_START,IN_DIV, IN_LEAGUE, 
                    #PITCH_COUNT_CUMU, FIELD_POS, OUTS_CT,
                    #PARK,
                    OUTS_CT,
                    BATTER_SEQ_NUM,
                    ORDER_CT) %>%
              mutate(across(HAND_MATCH, as.integer)) #%>% 
              #mutate(across(IN_DIV, as.integer)) %>% 
              #mutate(across(IN_LEAGUE, as.integer))

# create BATTER_IDX
# BATTER_IDX === given the game and the team, assign the batters an integer {1,2,3,...} in order of appearance in the game
D3 <- D2 %>% group_by(GAME_ID, BAT_HOME_IND) %>%
             mutate(BATTER_IDX = match(BAT_NAME, unique(BAT_NAME))) %>% 
             ungroup()
#View(D3 %>% select(GAME_ID, INNING, BAT_HOME_IND, PIT_NAME, BAT_NAME, BATTER_IDX))

# remove columns only meant for viewing
D4 <- D3 %>% select(-c(GAME_ID, INNING, BAT_NAME, PIT_NAME))
# D5 <- D4 %>% select(-c(GAME_ID, INNING, BAT_NAME, PIT_NAME))

# remove columns with NA
D6 <- D4 %>% drop_na() #FIXME

###############################################################

# standardize the vector x to have mean 0 and s.d. 1/2
std <- function(x) {
  (x-mean(x))/(sd(x) * 2)
}

# standardize by year
D7 <- D6 %>% group_by(YEAR) %>%
             mutate(std_EVENT_WOBA_19 = std(EVENT_WOBA_19),
                    std_WOBA_FINAL_BAT_19 = std(WOBA_FINAL_BAT_19),
                    std_WOBA_FINAL_PIT_19 = std(WOBA_FINAL_PIT_19)) %>%
             ungroup()
#D7 %>% group_by(YEAR) %>% summarise(m = mean(std_EVENT_WOBA_19), s = sd(std_EVENT_WOBA_19))
#D7 %>% summarise(m = mean(std_EVENT_WOBA_19), s = sd(std_EVENT_WOBA_19))

# select relevant columns
D8 <- D7 %>% select(YEAR, 
                   std_EVENT_WOBA_19, std_WOBA_FINAL_BAT_19, std_WOBA_FINAL_PIT_19,
                   EVENT_WOBA_19, WOBA_FINAL_BAT_19, WOBA_FINAL_PIT_19,
                   # NUM_WOBA_APP_BAT, NUM_WOBA_APP_PIT,
                   # NUM_WOBA_APP_FINAL_BAT, NUM_WOBA_APP_FINAL_PIT,
                   BATTER_IDX, ORDER_CT, BATTER_SEQ_NUM,
                   OUTS_CT,
                   #IN_DIV, PARK
                   HAND_MATCH, BAT_HOME_IND)

# PARK EFFECTS
# unique(D8$PARK)
# sig_parks <- c("BOS07", "DEN02", "PHO01")

###############################################################
  
### design matrix!
X <- D8
write_csv(X, output_filename)
                  


