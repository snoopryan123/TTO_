library(tidyverse)
#library(xml2)
#library(rvest)

########################################################################
# DATE === date (needs to be fixed... from GAME_ID)
# DAYS_SINCE_SZN_START === number of days since the start of the season (how far into the season we are effect)
# PIT_REST === number of days of rest the starting pitcher has prior to this game, with a max. of 6
########################################################################

################################
########### THE CODE ###########
################################

input_filename = "retro06_PA_1990-2020.csv"
output_filename = "retro07_PA_1990-2020.csv"
E <- read_csv(input_filename)
E0 <- E #%>% filter(YEAR %in% c(2019,2020))

# DATE
E1 <- E0 %>% mutate(DATE = paste(str_sub(GAME_ID,4,7), str_sub(GAME_ID,8,9), str_sub(GAME_ID,10,11), sep="-"))

# DAYS_SINCE_SZN_START 
E2 <- E1 %>% group_by(YEAR) %>% arrange(DATE) %>%
             mutate(FIRST_DATE = min(DATE, na.rm=TRUE),
                    DAYS_SINCE_SZN_START = as.vector(difftime(DATE, FIRST_DATE, units="days"))) %>%
             ungroup() %>% select(!c(FIRST_DATE))
# Check
# View(E2 %>% select(GAME_ID, YEAR, DATE, FIRST_DATE, DAYS_SINCE_SZN_START) %>% arrange(DATE))

# PIT_REST
max_date_val = 6
E3 <- E2 %>% group_by(YEAR, PIT_ID) %>% arrange(DATE) %>%
             mutate(PIT_REST = as.vector(difftime(DATE, lag(DATE), units="days"))) %>%
             ungroup() %>% 
             group_by(YEAR, PIT_ID, GAME_ID) %>%
             mutate(PIT_REST = first(PIT_REST),
                    PIT_REST = pmin(PIT_REST, max_date_val)) %>%
             ungroup() %>% group_by(YEAR, PIT_ID) %>%
             mutate(PIT_REST = ifelse(DATE == min(DATE, na.rm=TRUE), max_date_val,  PIT_REST)) %>%
             ungroup() %>% arrange(GAME_ID) 
# Check
###View(E3 %>% select(GAME_ID, PIT_NAME, YEAR, DATE, FIRST_DATE, PIT_REST) %>% arrange(DATE))
#View(E3 %>% filter(PIT_NAME == "Gerrit Cole") %>% select(GAME_ID, PIT_NAME, YEAR, DATE, PIT_REST) %>% arrange(DATE))

##############################

R = E3
write_csv(R, output_filename)


