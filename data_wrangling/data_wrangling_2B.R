#install.packages("retrosheet")
library(retrosheet)
library(tidyverse)
library(pkgcond)
library(stringr)

##################################################################################
# make sure the event file folders for the desired years (SAY, 1990-2020) are in the working directory !!!
##################################################################################

# create new columns !
#######################################################
# YEAR
# BATTER_SEQ_NUM 
# BAT_HAND === L or R 
#######################################################

############################################
################ ROSTER CODE ###############
############################################

get_team_names <- function(year) {
  filename = str_glue("{year}eve/TEAM{year}")
  A <- read_csv(filename, col_names=FALSE)
  team_names <- A[,1][[1]]
  return(team_names)
}

get_roster <- function(team, year) {
  print(c(team,year))
  filename = str_glue("{year}eve/{team}{year}.ROS")
  suppressWarnings(
    A <- read_csv(filename, col_names = FALSE)
  )
  colnames(A) <- c("retroID", "Last", "First", "Bat", "Throw", "Team", "Pos")
  A <- A %>% mutate(YEAR = year, team = team, BAT_ID = retroID, BAT_HAND = Bat) %>% select(YEAR, team, BAT_ID, BAT_HAND)
  return(A)
}

get_rosters <- function(year) {
  team_names <- get_team_names(year)
  A = tibble()
  for (t in team_names) {
    r = get_roster(t, year)
    A = bind_rows(A, r)
  }
  return(A)
}

################################
########### THE CODE ###########
################################

create.dataset.2B <- function(D, filename) {
  # YEAR
  D1 = D %>% mutate(YEAR = substr(DATE,start=1,stop=4))
  # BATTER_SEQ_NUM 
  D2 = D1 %>% group_by(GAME_ID, BAT_HOME_IND) %>%
    mutate(BATTER_SEQ_NUM = row_number()) %>%
    ungroup()
  # View(D2 %>% filter(is.na(YEAR))) ---> error in "SEA200709261"
  D2 = D2 %>% mutate(YEAR = ifelse(GAME_ID == "SEA200709261",2007, YEAR))
  
  # get all rosters
  R <- tibble()
  years = unique(D2$YEAR)
  for (year in years) { #2002:2003){#
    print(year)
    rosters = get_rosters(year)
    R <- bind_rows(R, rosters)
  }
  R1 <- R %>% select(!c(team))
  
  # BAT_HAND === L or R 
  R1 = R1 %>% mutate(YEAR = as.integer(YEAR))
  D2 = D2 %>% mutate(YEAR = as.integer(YEAR))
  D3 = left_join(D2, R1)
  
  # write dataset
  result = D3
  write_csv(result, filename)
}


##############################
########### RUNNIT ###########
##############################
D = read_csv("retro2_PA_1990-2020.csv")
# D = Dog %>% filter(startsWith(as.character(date), "2012"))
create.dataset.2B(D,"retro2B_PA_1990-2020.csv")
E = read_csv("retro2B_PA_1990-2020.csv")












# xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
# COLUMNS I left out but perhaps could've put in:
# xxxxx BAT_FLD_CD === batter fielding code 1:10 -> not needed 
# xxxxx BAT_LINEUP_POS -> this is just BATTER_SEQ_NUM 1,2,3,4,5,6,7,8,9
# PH_IND === 1 if pinch hitter else 0 [*****] [depends on BAT_FLD_CD] ----> would be nice, but not too necessary

