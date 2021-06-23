#install.packages("retrosheet")
library(retrosheet)
library(tidyverse)
library(pkgcond)

#########################################
################# THE CODE ##############
#########################################

manipulate.data.1 <- function(E,P,R) {
  G <- E$play
  away_name = E$info$info[which(E$info$category == "visteam")]
  home_name = E$info$info[which(E$info$category == "hometeam")]
  
  # add E$id to G
  if (length(E$id) >= 2) {
    # data is in a non-standard form, so ignore this case!
    print("ignore this game...")
    return(tibble()) 
  }
  id = E$id #E$id[[1]]
  G = G %>% mutate(game_id = id)
  # add E$info to G 
  E2 = tibble()
  E2 = rbind(E2, as.vector(E$info$info))
  names(E2) <- E$info$category
  E2 = E2 %>% select(!c(pitches))
  G = bind_cols(G,E2)
  # add E$start and E$sub to G
  E3 = bind_rows(E$start, E$sub) %>% select(!c(team))
  G = left_join(G,E3,by="retroID")
  # keep certain columns
  G = G %>% select(!c(usedh, umphome, ump1b, ump2b, ump3b, howscored, oscorer, temp, winddir, windspeed, fieldcond, precip,
                      save, daynight, timeofgame, attendance, wp, lp))
  # keep at bats only against the starting pitcher
  # split home & away team PA
  # also, add starting pitcher era from E$data
    # note: a substitution event is indicated by "NP" in the "Play" column
  i = min(which(G$play == "NP" & G$fieldPos == 1 & G$team == 1))
  i = if (i == Inf) nrow(G)+1 else i
  G0 = G[1:(i-1), ] %>% filter(team == 0)
  sp.retroID = (E$start %>% filter(team == 1, fieldPos == 1))$retroID
  sp.name = (E$start %>% filter(team == 1, fieldPos == 1))$name
  sp.era = (E$data %>% filter(retroID == sp.retroID))$ER
  sp.hand = (P[[home_name]] %>% filter(retroID == sp.retroID))$Throw
  G0 = G0 %>% mutate(sp.retroID = sp.retroID, sp.name = sp.name, sp.era = sp.era, sp.hand = sp.hand)
  G0 = left_join(G0, R[[away_name]], by = "retroID")
  ### 
  i = min(which(G$play == "NP" & G$fieldPos == 1 & G$team == 0))
  i = if (i == Inf) nrow(G)+1 else i
  G1 = G[1:(i-1), ] %>% filter(team == 1)
  sp.retroID = (E$start %>% filter(team == 0, fieldPos == 1))$retroID
  sp.name = (E$start %>% filter(team == 0, fieldPos == 1))$name
  sp.era = (E$data %>% filter(retroID == sp.retroID))$ER
  sp.hand = (P[[away_name]] %>% filter(retroID == sp.retroID))$Throw
  G1 = G1 %>% mutate(sp.retroID = sp.retroID, sp.name = sp.name, sp.era = sp.era, sp.hand = sp.hand)
  G1 = left_join(G1, R[[home_name]], by = "retroID")
  # remove superfluous objects from memory
  # keep G0, G1
  rm(E2)
  rm(E3)
  rm(G)
  return(bind_rows(G0,G1))
}

create.dataset.1 <- function(year) {
  print(year)
  result <- tibble()
  team_IDs = getTeamIDs(year)
  R <- get_retrosheet("roster", year) 
  P <- R
  for (i in 1:length(R)) {
    P[[i]] =  R[[i]] %>% select(retroID, Throw)
    R[[i]] = R[[i]] %>% select(retroID, Bat)
  }
  #####################
  for (t in team_IDs) {
    D <- get_retrosheet("play", year, t)
    L = length(D)
    for (i in 1:L) {
      print(paste(t,i,year))
      E <- D[[i]]
      result <- suppress_warnings( bind_rows(result, manipulate.data.1(E,P,R)), "Inf")
      rm(E)
    }
    rm(D)
  }
  
  filename = paste0("data1_", year, "_sp.csv")
  write_csv(result, filename)
}

##################################################
################ Years 2010 - 2019 ###############
##################################################

for (yr in 2010:2019) {
  create.dataset.1(yr)
}

########################################################################

#####################################################################
################ EXAMPLE: 2012 Dodgers, 1st Home Game ###############
#####################################################################

D <- get_retrosheet("play", 2012, "LAN")
R <- get_retrosheet("roster", 2012) 
P <- R
for (i in 1:length(R)) {
  P[[i]] =  R[[i]] %>% select(retroID, Throw)
  R[[i]] = R[[i]] %>% select(retroID, Bat)
}
result <- tibble()
###########
E <- D[[1]]
result <- bind_rows(result, manipulate.data.1(E))
rm(E)

#####################################################################
################ EXAMPLE: All games from 2012 Dodgers ###############
#####################################################################

result <- tibble()

L = length(D)
for (i in 1:L) {
  print(i)
  E <- D[[i]]
  result <- suppress_warnings( bind_rows(result, manipulate.data.1(E)), "Inf")
  rm(E)
}

#################################################################################
################ EXAMPLE: All games from a given year (say, 2012) ###############
#################################################################################

create.dataset.1(2012)
B = read_csv("data1_2012_sp.csv")





