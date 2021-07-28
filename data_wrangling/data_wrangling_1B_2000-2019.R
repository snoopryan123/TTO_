#install.packages("retrosheet")
library(retrosheet)
library(tidyverse)
library(pkgcond)

###########################################################################################
################ Aggregate and Sort the games from 2000-2019 (level 1 data) ###############
###########################################################################################

D <- tibble()
for (year in 2000:2019) {
  print(year)
  curr <- read_csv(paste0("retro_PA_1_", year, ".csv")) 
  curr <- if ("umplf" %in% names(curr)) (curr %>% select(!c(umplf))) else curr
  curr <- if ("umprf" %in% names(curr)) (curr %>% select(!c(umprf))) else curr
  curr <- if ("htbf" %in% names(curr)) (curr %>% select(!c(htbf))) else curr
  D <- bind_rows(D, curr)
}
curr <- read_csv("retro_PA_1_stragglers.csv")
D <- bind_rows(D, curr)

#######################

### BAT_SEQ_NUM
E = D %>% group_by(game_id, team) %>% mutate(BAT_SEQ_NUM = row_number()) %>% ungroup()
#View(E)

### SORT the dataset
G = E %>% arrange(game_id, inning, team, BAT_SEQ_NUM)
View(G)

### save data
write_csv(G, "retro_PA_1_2000-2019.csv")

#######################






