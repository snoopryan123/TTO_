library(tidyverse)
library(xml2)
library(rvest)

########################################################################
# GAME_NUM (how far into the season we are effect) === game number within the season, in {1,2,...,162} ???
# NUM_DAYS_REST === number of days of rest the starting pitcher has prior to this game -> data????
########################################################################

################################
########### THE CODE ###########
################################

input_filename = "retro06_PA_1990-2020.csv"
output_filename = "retro07_PA_1990-2020.csv"
E <- read_csv(input_filename)
E0 <- E #%>% filter(YEAR %in% c(2015,2020))




##############################

R = E10
write_csv(R, output_filename)


