#install.packages("retrosheet")
library(retrosheet)
library(tidyverse)
library(pkgcond)
library(stringr)

# -> add additional FEATURES to Dataset_3 to create Dataset_4
########################################################################
# HAND_MATCH === 1 if pitcher and batter handedness match, else 0
  # ---> some batters have "B" for both hands, what is the handedness match ???
  # unique(D$BAT_HAND)
  # unique(D$PIT_HAND)

# INDIVIDUAL BATTER'S QUALITY
  # WOBA_CUMU_BAT === cumulative woba prior to this plate appearance for a given batter during a given season
  # shrinkage estimator to predict end-of-season wOBA, to put everything on same scale ???
# INDIVIDUAL PITCHER'S QUALITY
  # WOBA_CUMU_PIT === cumulative woba prior to this plate appearance for a given pitcher during a given season
  # shrinkage estimator to predict end-of-season wOBA, to put everything on same scale ???

# PARK_EFFECT ???
# HOME_FIELD_EFFECT ???
# HOW FAR INTO THE SEASON WE ARE EFFECT ???
# NUM_DAYS_REST === number of days of rest the starting pitcher has prior to this game -> data????
########################################################################

################################
########### THE CODE ###########
################################

D <- read_csv("data3_2010-19_sp.csv")



##### INDIVIDUAL BATTER'S QUALITY

# WOBA_CUMU_BAT
D <- D %>% group_by(YEAR, BAT_ID) %>%
      mutate(cumu.woba.sum = cumsum(replace_na(EVENT_WOBA, 0)),
             cumu.pa.sum = cumsum(replace_na(PA_IND, 0)),
             WOBA_CUMU_BAT = cumu.woba.sum/cumu.pa.sum) %>% # is it plate appearance, or something else?
      select(!c(cumu.woba.sum, cumu.pa.sum)) %>%
      ungroup()



##### INDIVIDUAL PITCHER'S QUALITY

# WOBA_CUMU_PIT
D <- D %>% group_by(YEAR, PIT_ID) %>%
  mutate(cumu.woba.sum = cumsum(replace_na(EVENT_WOBA, 0)),
         cumu.pa.sum = cumsum(replace_na(PA_IND, 0)),
         WOBA_CUMU_PIT = cumu.woba.sum/cumu.pa.sum) %>% # is it plate appearance, or something else?
  select(!c(cumu.woba.sum, cumu.pa.sum)) %>%
  ungroup()




# create.dataset.3 <- function(D, year) {
#     
#   
#     
#     result = D8
#     filename = paste0("data3_", year, "_sp.csv")
#     write_csv(result, filename)
#     
#     rm(D1)
#     rm(D2)
#     rm(D3)
#     rm(D4)
#     rm(D5)
#     rm(D6)
#     rm(D7)
# }

########################################################################

##########################################
########### EXAMPLE: 2012 DATA ###########
##########################################




##############################
########### CHECKS ###########
##############################

{
    # CHECK WOBA_CUMU_BAT
    View(D %>% filter(BAT_ID == "abreb001", YEAR == 2011) %>% 
           select(BAT_ID, YEAR, EVENT_WOBA, PA_IND, WOBA_CUMU_BAT))
  
    # CHECK WOBA_CUMU_PIT
    View(D %>% filter(PIT_ID == "weavj003", YEAR == 2010) %>% 
           select(BAT_ID, YEAR, EVENT_WOBA, PA_IND, WOBA_CUMU_PIT))
    
}
