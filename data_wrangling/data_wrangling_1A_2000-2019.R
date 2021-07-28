#install.packages("retrosheet")
library(retrosheet)
library(tidyverse)
library(pkgcond)

#################################################################
################ FIX bad_subs_games for 2000-2019 ###############
#################################################################

bad_subs_games <- read_csv("bad_subs_games.csv")
B <- bad_subs_games$bad_subs_games

# go to https://www.retrosheet.org/game.htm
# manually download the event file folders for 2000-2019, and put them in the working directory 

#########################################
################ THE CODE ###############
#########################################

get_tbl_1 <- function(b) {
  #browser()
  team <- substr(b,1,3)
  year <- substr(b,4,7)
  month <- substr(b,8,9)
  day <- substr(b,10,11)
  game.num <- substr(b,12,12)
  
  fileA <- paste0(year,"eve/",year,team,".EVA")
  fileN <- paste0(year,"eve/",year,team,".EVN")
  
  D <- if (file.exists(fileA)) readLines(fileA) else readLines(fileN)
  i <- which(D == paste0("id,",b))
  
  x = which(startsWith(D[1:length(D)], "id"))
  j <- x[x > i][1] - 1
  j <- if (is.na(j)) length(D) else j
  
  E <- D[i:j]
  E <- E[!startsWith(E,"data")]
  E <- E[!startsWith(E,"version")]
  E <- c(E[1:7], E[20], E[26:length(E)])
  starters <- E[startsWith(E,"start")]
  starters <- starters[endsWith(starters,",1")]
  for(i in 1:length(starters)) { starters[i] = substr(starters[i], 7,1000) }
  E <- E[!startsWith(E,"start")]
  
  id <- strsplit(E[startsWith(E,"id")], ",")[[1]][2]
  E <- E[!startsWith(E,"id")]
  
  E1 <- E[startsWith(E,"info")]
  for(i in 1:length(E1)) { E1[i] = substr(E1[i], 6,1000) }
  E2 <- E1
  for(i in 1:length(E1)) { E1[i] = strsplit(E1[i], ",")[[1]][1] }
  for(i in 1:length(E2)) { E2[i] = strsplit(E2[i], ",")[[1]][2] }
  info.names = E1
  info.values = E2
  E <- E[!startsWith(E,"info")]
  
  E <- E[!(startsWith(E,"sub") & !endsWith(E,",1"))] # only keep pitcher substitutions 
  E <- E[!startsWith(E,"com")]
  E <- E[!startsWith(E,"badj")]
  
  for (i in 1:length(E)) {
    if (!startsWith(E[i], "sub")) {
      E[i] <- paste0(E[i], ",FALSE")
    }
  }
  
  x = which(startsWith(E, "sub"))-1
  for (i in x) {
    E[i] = paste0(str_sub(E[i],end=-7), ",TRUE")
  }
  
  subs.p = E[startsWith(E, "sub")]
  play = E[!startsWith(E, "sub")]
  for(i in 1:length(subs.p)) { subs.p[i] = substr(subs.p[i], 5,1000) }
  for(i in 1:length(play)) { play[i] = substr(play[i], 6,1000) }
  
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
                     number = info.values[5],
                     starttime = info.values[6],
                     sky = info.values[7])
  
  ### create ST tibble (starting pitchers)
  ST = strsplit(starters[1], ",")[[1]]
  if (length(starters) >= 2) {
    for (i in 2:length(starters)) {
      ST = rbind(ST, strsplit(starters[i], ",")[[1]])
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
  for(i in 1:nrow(ST)) { ST$pit.name[i] = str_sub(ST$pit.name[i], 2, -2) }
  
  ### create SU tibble (subbed pitchers)
  if (!is.na(subs.p)) {
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
      for(i in 1:nrow(SU)) { SU$pit.name[i] = str_sub(SU$pit.name[i], 2, -2) }
  }
  
  ### Add starting pitchers to P
  P = P %>% mutate(pit.retroID = ifelse(team == 0, 
                                        (ST %>% filter(pit.team == 1))$pit.retroID, 
                                        (ST %>% filter(pit.team == 0))$pit.retroID),
                   pit.name = ifelse(team == 0, 
                                       (ST %>% filter(pit.team == 1))$pit.name, 
                                       (ST %>% filter(pit.team == 0))$pit.name),
                   sp.ind = 1)
  
  ### Add relieving pitchers to P
  if (!is.na(subs.p)) {
      s = which(P$sub.here)
      P[first(s):nrow(P), ]$sp.ind = 0
      for (i in 1:nrow(SU)) {
        subbb = SU[i,]
        P = P %>% mutate(pit.retroID = ifelse(team != subbb$pit.team & row_number() >= s[i], subbb$pit.retroID, pit.retroID),
                         pit.name = ifelse(team != subbb$pit.team & row_number() >= s[i], subbb$pit.name, pit.name))
    
      }
  } 
  P = P %>% select(!c(sub.here))
  
  
  # add pit.hand column
  R <- get_retrosheet("roster", as.numeric(year))
  h = unique(P$hometeam)
  v = unique(P$visteam)
  H =  tibble(R[[h]]) %>% filter(Pos == "P") %>% select(!c(Bat,First,Last)) %>% 
                          mutate(pit.hand=Throw, pit.retroID=retroID) %>% select(!c(Team,Pos,Throw,retroID)) 
  V =  tibble(R[[v]]) %>% filter(Pos == "P") %>% select(!c(Bat,First,Last)) %>% 
                          mutate(pit.hand=Throw, pit.retroID=retroID) %>% select(!c(Team,Pos,Throw,retroID)) 
  H = bind_rows(H,V)
  P <- left_join(P,H,by="pit.retroID")
  
  # add name column
  H =  tibble(R[[h]]) %>% filter(Pos != "P")
  V =  tibble(R[[v]]) %>% filter(Pos != "P")
  H = bind_rows(H,V)
  H = H %>% mutate(name = paste(First,Last)) %>% select(!c(First,Last,Bat,Throw,Pos,Team))
  P <- left_join(P,H,by="retroID")
  
  # batPos, fieldPos
  P = P %>% mutate(batPos = NA, fieldPos = NA)
  
  # re-order the columns!
  P = P %>% relocate(name, .after = sky) %>% 
            relocate(batPos, .after = name) %>% 
            relocate(  fieldPos, .after = batPos) %>%
            relocate( pit.hand, .after = pit.name)
  
  return(P)
}

#######################################
################ RUNNIT ###############
#######################################

stragglers = tibble()
for (i in 1:length(B)) { #1:length(B)) {
  print(i)
  b <- B[i]
  P <- get_tbl_1(b)
  stragglers = bind_rows(stragglers, P)
}
write_csv(stragglers, "retro_PA_1_stragglers.csv")



#P <- get_tbl_1(B[127])






