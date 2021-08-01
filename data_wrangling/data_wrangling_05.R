#install.packages("retrosheet")
library(tidyverse)
library(stringr)

# -> add new columns to Dataset_2, to create Dataset_3, for a given year 
########################################################################
# HOME_LEAGUE {AL, NL}
# AWAY_LEAGUE {AL, NL}
# HOME_DIV {NL_E, NL_C, NL_W, AL_E, AL_C, AL_W}
# AWAY_DIV {NL_E, NL_C, NL_W, AL_E, AL_C, AL_W}
# TEAM_MATCH {IN_DIV, IN_LEAG}
########################################################################

################################
########### THE CODE ###########
################################

W = read_csv("woba_weights_Fangraphs.csv") 
Div = read_csv("mlb_divisions_dataset.csv")

add.divs.to.dataset <- function(D, filename) {
    # HOME_DIV {NL_E, NL_C, NL_W, AL_E, AL_C, AL_W}
    # AWAY_DIV {NL_E, NL_C, NL_W, AL_E, AL_C, AL_W}
    # HOME_LEAGUE {AL, NL}
    # AWAY_LEAGUE {AL, NL}
    # TEAM_MATCH {IN_DIV, IN_LEAGUE}
    compute_divs <- function(yr, TEAM_IDS) { 
      print(yr)
      f <- function(team_id) {
        Div[yr >= Div$start & yr <= Div$end, ][[team_id]]
      }
      x = sapply(TEAM_IDS, f)
      unname(x)
    }
    #######
    D2 = D %>% group_by(YEAR) %>%
                mutate(HOME_DIV = compute_divs(unique(YEAR), HOME_TEAM_ID),
                       AWAY_DIV = compute_divs(unique(YEAR), AWAY_TEAM_ID),
                       HOME_LEAGUE = substr(HOME_DIV,start=1,stop=2),
                       AWAY_LEAGUE = substr(AWAY_DIV,start=1,stop=2),
                       IN_DIV = (HOME_DIV == AWAY_DIV),
                       IN_LEAGUE = (HOME_LEAGUE == AWAY_LEAGUE)) %>%
                ungroup()
    print("D2")
    
    ###########
    
    result = D2
    write_csv(result, filename)
}

########################################################################

##########################################
########### EXAMPLE: 2012 DATA ###########
##########################################

D = read_csv("retro2B_PA_1990-2020.csv")
# D = Dog %>% filter(YEAR == 2012)
filename = "retro3_PA_1990-2020.csv"
# takes ~30 min to run on 30 years of data
add.divs.to.dataset(D, filename) 
E = read_csv(filename)


