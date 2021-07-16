#install.packages("retrosheet")
#library(retrosheet)
library(tidyverse)
#library(pkgcond)
#library(stringr)

# -> add additional FEATURES to Dataset_3 to create Dataset_4
########################################################################
# HAND_MATCH === 1 if pitcher and batter handedness match, else 0
  # ---> some batters have "B" for both hands, what is the handedness match ???
  # unique(D$BAT_HAND)
  # unique(D$PIT_HAND)

# INDIVIDUAL BATTER'S QUALITY
  # WOBA_CUMU_BAT === cumulative woba prior to this plate appearance for a given batter during a given season
  # shrinkage estimator to predict end-of-season wOBA, to put everything on same scale ???
  # JS estimator relies on batting avg H/N to be approx. normal, since H is binomial. wOBA doesn't have this. so can we use JS?
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
      #select(!c(cumu.woba.sum, cumu.pa.sum)) %>%
      ungroup()


# SHRINKAGE ESTIMATOR: JAMES STEIN ESTIMATOR
js <- function(X1, sig.sq1) {
    mu1_hat = sum(X1/sig.sq1) / sum(1/sig.sq1)
    P1 = length(X1)
    t.denom = sum( (X1-mu1_hat)^2 / sig.sq1 )
    mu1_hat + max(1 - (P1-3)/t.denom, 0)*(X1 - mu1_hat)
}

D <- D %>% arrange(DATE)
dates_in_order = unique(D %>% arrange(DATE) %>% select(DATE,YEAR))



D <- D %>% mutate(X1 = asin(sqrt( (cumu.woba.sum+0.25)/(cumu.pa.sum+0.5) )),
                 sig.sq1 = 0.25/cumu.pa.sum )
# View(D %>% filter(is.na(X1) | sig.sq1 == Inf))
E <- tibble()

for (k in 1:nrow(dates_in_order)) {
  print(k)

  date = dates_in_order[k,]$DATE
  year = dates_in_order[k,]$YEAR

  # make sure only one appearance per person (their last appearance)! and only use past data!
  # no data bleed!
  DD <- D %>% filter(YEAR == year, DATE < date) %>% 
              filter(!is.na(X1) | !is.infinite(sig.sq1)) %>%
              group_by(BAT_ID) %>%
              slice(n()) %>%   # keep only each batter's last appearance to use as the X1 and sig.sq1 data..
              ungroup()

  DD <- DD %>% mutate(JS_BAT = js(DD$X1, DD$sig.sq1))
  DD1 <- DD %>% select(BAT_ID, JS_BAT)
  
  # join
  L = left_join(D %>% filter(DATE == date), DD1, by="BAT_ID")
  E <- bind_rows(E, L)
  
  if (k==10) browser()
}








##### INDIVIDUAL PITCHER'S QUALITY

# WOBA_CUMU_PIT
D <- D %>% group_by(YEAR, PIT_ID) %>%
  mutate(cumu.woba.sum = cumsum(replace_na(EVENT_WOBA, 0)),
         cumu.pa.sum = cumsum(replace_na(PA_IND, 0)),
         WOBA_CUMU_PIT = cumu.woba.sum/cumu.pa.sum) %>% # is it plate appearance, or something else?
  #select(!c(cumu.woba.sum, cumu.pa.sum)) %>%
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
  
    # https://www.fangraphs.com/leaders.aspx?pos=all&stats=bat&lg=all&qual=y&type=8&season=2011&month=0&season1=2011&ind=0&team=0&rost=0&age=0&filter=&players=0&startdate=2011-01-01&enddate=2011-12-31&sort=16,d
    View(tail(D %>% filter(BAT_NAME == "Jose Bautista", YEAR == 2011) %>% 
           select(BAT_ID, YEAR, EVENT_WOBA, PA_IND, WOBA_CUMU_BAT)))
    
    View(tail(D %>% filter(BAT_NAME == "Miguel Cabrera", YEAR == 2011) %>% 
                select(BAT_ID, YEAR, EVENT_WOBA, PA_IND, WOBA_CUMU_BAT)))
    
    View(tail(D %>% filter(BAT_NAME == "Michael Young", YEAR == 2011) %>% 
                select(BAT_ID, YEAR, EVENT_WOBA, PA_IND, WOBA_CUMU_BAT)))
    
    ### VERY CLOSE, but probably a small error in PA, or the denominator in wOBA (perhaps missing -SF)
  
    # CHECK WOBA_CUMU_PIT
    View(D %>% filter(PIT_ID == "weavj003", YEAR == 2010) %>% 
           select(BAT_ID, YEAR, EVENT_WOBA, PA_IND, WOBA_CUMU_PIT))
    
}
