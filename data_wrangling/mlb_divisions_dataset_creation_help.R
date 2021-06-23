#install.packages("retrosheet")
library(retrosheet)
library(tidyverse)

######################################################################
################### CREATE DIVISIONS DATASET #########################
######################################################################

# get 3 letter codes for each MLB team of all time
all_teams <- character(0)
for (year in 1916:2021) {
  print(year)
  teams <- unname(getTeamIDs(year))
  all_teams <- c(all_teams, teams)
}
All_teams <- unique(all_teams)

# HOME_LEAGUE {AL, NL}
# AWAY_LEAGUE {AL, NL}
# HOME_DIV {NL_East, NL_CNTRL, NL_WEST -  AL_East, AL_CNTRL, AL_WEST }
# AWAY_DIV {NL_East, NL_CNTRL, NL_WEST -  AL_East, AL_CNTRL, AL_WEST }
# TEAM_MATCH {InterLEAG, IntraDIV, IntraLEAG}



