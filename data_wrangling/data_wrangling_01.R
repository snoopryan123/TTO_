library(tidyverse)
library(stringr)
source("data_wrangling_00.R")

#########################################
################ THE CODE ###############
#########################################

write_pbp_year <- function(year) {
  print(year)
  team_names <- getTeamIDs(year)
  rosters <- getRetrosheet("roster", year)
  P = tibble()
  for (team in team_names) {
    pbp_team_year <- get_play_by_play(year, team, rosters)
    P <- bind_rows(P, pbp_team_year)
  }
  write_csv(P, str_glue("retro01_PA_{year}.csv"))
}

get_play_by_play <- function(year, team, rosters) {
  D <- getRetrosheet("play", year, team)
  result <- tibble()
  for (i in 1:length(D)) {
    E <- D[[i]]
    P <- pbpText_to_pbpTbl(E,rosters)
    result <- bind_rows(result, P)
  }
  return(result)
}
  
#### transform retrosheet play-by-play text to play-by-play tibble !
pbpText_to_pbpTbl <- function(E,rosters) {
  # get st_pitchers
  E <- E[!startsWith(E,"data")]
  E <- E[!startsWith(E,"version")]
  E <- c(E[1:7], E[20], E[26:length(E)])
  starters <- E[startsWith(E,"start")]
  st_pitchers <- starters[endsWith(starters,",1")]
  subbers <- E[startsWith(E,"sub")]
  for(i in 1:length(starters)) { starters[i] = substr(starters[i], 7,1000) }
  for(i in 1:length(st_pitchers)) { st_pitchers[i] = substr(st_pitchers[i], 7,1000) }
  for(i in 1:length(subbers)) { subbers[i] = substr(subbers[i], 5,1000) }
  players <- c(starters, subbers)
  E <- E[!startsWith(E,"start")]
  # get id
  id <- strsplit(E[startsWith(E,"id")], ",")[[1]][2]
  print(id)
  E <- E[!startsWith(E,"id")]
  # get info
  E1 <- E[startsWith(E,"info")]
  for(i in 1:length(E1)) { E1[i] = substr(E1[i], 6,1000) }
  E2 <- E1
  for(i in 1:length(E1)) { E1[i] = strsplit(E1[i], ",")[[1]][1] }
  for(i in 1:length(E2)) { E2[i] = strsplit(E2[i], ",")[[1]][2] }
  info.names = E1
  info.values = E2
  E <- E[!startsWith(E,"info")]
  # only keep pitcher `sub` and `play` in E (remove `com`,`badj`, and non-pitcher `sub`)
  E <- E[ startsWith(E,"play") | (startsWith(E,"sub") & endsWith(E,",1")) ]
  ###############################
  # add a column at the end of E: TRUE if pitcher-sub at this `play`, else FALSE
  for (i in 1:length(E)) {
    if (!startsWith(E[i], "sub")) {
      E[i] <- paste0(E[i], ",FALSE")
    }
  }
  x = which(startsWith(E, "sub"))-1
  for (i in x) {
    E[i] = paste0(str_sub(E[i],end=-7), ",TRUE")
  }
  ###############################
  subs.p = E[startsWith(E, "sub")]
  play = E[!startsWith(E, "sub")]
  for(i in 1:length(subs.p)) { subs.p[i] = substr(subs.p[i], 5,1000) }
  for(i in 1:length(subs.p)) { subs.p[i] = str_remove_all(subs.p[i], "\"") }
  for(i in 1:length(play)) { play[i] = substr(play[i], 6,1000) }
  ###############################

  ### create P tibble
  P = strsplit(play[1], ",")[[1]]
  for (i in 2:length(play)) {
    P = rbind(P, strsplit(play[i], ",")[[1]])
  }
  P = as.data.frame(P, row.names = FALSE)
  colnames(P) = c("inning","team","retroID","count","pitches","play","sub.here")
  P$inning = as.numeric(P$inning)
  P$team = as.numeric(P$team) - 1
  P$retroID = as.character(P$retroID)
  P$count = as.numeric(P$count)
  P$pitches = as.character(P$pitches)
  P$play = as.character(P$play)
  P$sub.here = as.logical(P$sub.here)
  P = tibble(P)
  P <- P %>% mutate(game_id = id,
                     visteam = info.values[1],
                     hometeam = info.values[2],
                     site = info.values[3],
                     date = info.values[4],
                     number = info.values[5])#, starttime = info.values[6], sky = info.values[7])
  
  ### create ST tibble (starting pitchers)
  ST = strsplit(st_pitchers[1], ",")[[1]]
  if (length(st_pitchers) >= 2) {
    for (i in 2:length(st_pitchers)) {
      ST = rbind(ST, strsplit(st_pitchers[i], ",")[[1]])
    }
  }
  ST = as.data.frame(ST, row.names = FALSE)
  colnames(ST) = c("pit.retroID", "pit.name", "pit.team", "V4", "V5")
  ST$pit.retroID = as.character(ST$pit.retroID)
  ST$pit.name = as.character(ST$pit.name)
  ST$pit.team = as.numeric(ST$pit.team) - 1
  ST$V4 = NA #as.numeric(ST$V4)
  ST$V5 = NA #as.numeric(ST$V5)
  ST = tibble(ST) %>% select(!c(V4,V5))
  #for(i in 1:nrow(ST)) { ST$pit.name[i] = str_sub(ST$pit.name[i], 2, -2) }
  
  ### create SU tibble (subbed pitchers)
  if (!is.na(subs.p[1])) {
      SU = strsplit(subs.p[1], ",")[[1]]
      if (length(subs.p) >= 2) {
        for (i in 2:length(subs.p)) {
          SU = rbind(SU, strsplit(subs.p[i], ",")[[1]])
        }
        SU = as.data.frame(SU, row.names = FALSE)
      } else {
        SU = as.data.frame(t(SU))
      }
      colnames(SU) = c("pit.retroID", "pit.name", "pit.team", "V4", "V5")
      SU$pit.retroID = as.character(SU$pit.retroID)
      SU$pit.name = as.character(SU$pit.name)
      SU$pit.team = as.numeric(SU$pit.team) - 1
      SU$V4 = NA #as.numeric(SU$V4)
      SU$V5 = NA #as.numeric(SU$V5)
      SU = tibble(SU) %>% select(!c(V4,V5))
  }
  
  ### Add starting pitchers to P
  P = P %>% mutate(pit.retroID = ifelse(team == 0, 
                                        (ST %>% filter(pit.team == 1))$pit.retroID, 
                                        (ST %>% filter(pit.team == 0))$pit.retroID),
                   pit.name = ifelse(team == 0, 
                                       (ST %>% filter(pit.team == 1))$pit.name, 
                                       (ST %>% filter(pit.team == 0))$pit.name))
  
  ### Add relieving pitchers to P
  if (!is.na(subs.p[1])) {
      s = which(P$sub.here)
      for (i in 1:nrow(SU)) {
        subbb = SU[i,]
        P = P %>% mutate(pit.retroID = ifelse(team != subbb$pit.team & row_number() >= s[i], subbb$pit.retroID, pit.retroID),
                         pit.name = ifelse(team != subbb$pit.team & row_number() >= s[i], subbb$pit.name, pit.name))
    
      }
  } 
  P = P %>% select(!c(sub.here))
  
  ##########################################################
  
  # add pit.hand column
  h = unique(P$hometeam)
  v = unique(P$visteam)
  H =  tibble(rosters[[h]]) %>% filter(Pos == "P") %>% select(!c(Bat,First,Last)) %>% 
                          mutate(pit.hand=Throw, pit.retroID=retroID) %>% select(!c(Team,Pos,Throw,retroID)) 
  V =  tibble(rosters[[v]]) %>% filter(Pos == "P") %>% select(!c(Bat,First,Last)) %>% 
                          mutate(pit.hand=Throw, pit.retroID=retroID) %>% select(!c(Team,Pos,Throw,retroID)) 
  HV = bind_rows(H,V)
  P <- left_join(P,HV,by="pit.retroID")
  
  
  ### add fieldPos, name columns
  PL = strsplit(players[1], ",")[[1]]
  if (length(players) >= 2) {
    for (i in 2:length(players)) {
      PL = rbind(PL, strsplit(players[i], ",")[[1]])
    }
  }
  PL = as.data.frame(PL, row.names = FALSE)
  colnames(PL) = c("retroID", "name", "team", "batPos", "fieldPos") 
  PL$retroID = as.character(PL$retroID)
  PL$name = as.character(PL$name)
  PL$team = as.numeric(PL$team)
  PL$batPos = as.numeric(PL$batPos)
  PL$fieldPos = as.numeric(PL$fieldPos)
  PL = tibble(PL) %>% select(!c(batPos,team))
  P <- left_join(P,PL,by="retroID")
  
  # add BAT.HAND column
  H =  tibble(rosters[[h]]) %>% select(retroID, Bat)
  V =  tibble(rosters[[v]])  %>% select(retroID, Bat)
  HV = bind_rows(H,V) %>% rename(bat.hand = Bat)
  P <- left_join(P,HV,by="retroID")
  
  # YEAR
  P = P %>% mutate(year = substr(date,start=1,stop=4))
  
  # re-order the columns!
  P = P %>% relocate(name, .after = retroID) %>% 
            relocate(bat.hand, .after = name) %>%
            relocate(fieldPos, .after = bat.hand) %>%
            relocate(pit.retroID, .after = fieldPos) %>%
            relocate(pit.name, .after = pit.retroID) %>%
            relocate(pit.hand, .after = pit.name) %>%
            relocate(year, .after = date) %>%
  
  return(P)
}

#######################################
################ RUNNIT ###############
#######################################

for (year in 1990:2020) { write_pbp_year(year) }

################################################

write_pbp_year(2020)




