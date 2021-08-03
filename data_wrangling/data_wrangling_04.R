#install.packages("retrosheet")
library(tidyverse)
library(stringr)

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

## WARNING: this code is not vectorized, and may take some time to run.

input_filename = "retro03_PA_2020.csv"
output_filename = "retro04_PA_2020.csv"
D <- read_csv(input_filename)

Div = read_csv("mlb_divisions_dataset.csv")
#DivT = as_tibble(cbind(team = names(Div), t(Div)), .name_repair = 'unique')

################################

{
    # HOME_DIV {NL_E, NL_C, NL_W, AL_E, AL_C, AL_W}
    # AWAY_DIV {NL_E, NL_C, NL_W, AL_E, AL_C, AL_W}
    # HOME_LEAGUE {AL, NL}
    # AWAY_LEAGUE {AL, NL}
    # TEAM_MATCH {IN_DIV, IN_LEAGUE}
    compute_divs <- function(yr, TEAM_IDS) { 
      # warning: NOT VECTORIZED...
      print(yr)
      f <- function(team_id) {
        Div[yr >= Div$start & yr <= Div$end, ][[team_id]]
      }
      x = sapply(TEAM_IDS, f)
      unname(x)
    }
    D1 = D %>% group_by(YEAR) %>%
      mutate(HOME_DIV = compute_divs(unique(YEAR), HOME_TEAM_ID),
             AWAY_DIV = compute_divs(unique(YEAR), AWAY_TEAM_ID),
             HOME_LEAGUE = str_sub(HOME_DIV,1,2),
             AWAY_LEAGUE = str_sub(AWAY_DIV,1,2),
             IN_DIV = (HOME_DIV == AWAY_DIV),
             IN_LEAGUE = (HOME_LEAGUE == AWAY_LEAGUE)) %>%
      ungroup()
    print("D1")
    
    ###########
    
    result = D1
    #View(result %>% select(HOME_TEAM_ID,AWAY_TEAM_ID,HOME_DIV,AWAY_DIV,HOME_LEAGUE,AWAY_LEAGUE,IN_DIV,IN_LEAGUE))
    write_csv(result, output_filename)
}




