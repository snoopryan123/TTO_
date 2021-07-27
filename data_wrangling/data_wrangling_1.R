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
  
  E_was_weird = FALSE
  if (length(E$id) >= 2) {
    # this retrosheet game data contains a `badj` and so is imported weird by the R package...
    print("`BADJ` FIX .....")
    E_was_weird = TRUE
    # fix E !!!
    E$id = E$id[[1]]
    E$version = E$version[[1]]
    E$com = E$com[[1]]
    E$info = E$info %>% filter(E$info$category != "<NA>")
    E$start = E$start %>% filter(E$start$retroID != "<NA>")
    E$play = E$play %>%filter(E$play$inning != "NA")
    E$sub = E$sub %>% filter(E$sub$retroID != "<NA>")
    E$data = E$data %>% filter(E$data$retroID != "<NA>")
    #return(tibble()) 
  }
  # add E$id to G
  id = E$id 
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
  G = G %>% mutate(usedh = NA, umphome = NA, ump1b = NA, ump2b = NA, ump3b = NA, howscored = NA, oscorer = NA, temp = NA, winddir = NA, 
                   windspeed = NA, fieldcond = NA, precip = NA, save = NA, daynight = NA, timeofgame = NA, attendance = NA, wp = NA, lp = NA) %>%
            select(!c(usedh, umphome, ump1b, ump2b, ump3b, howscored, oscorer, temp, winddir, windspeed, fieldcond, precip,
                      save, daynight, timeofgame, attendance, wp, lp))
  
  # add starting pitcher data
  sp0 <- E$start %>% filter(team == 0, fieldPos == 1)
  sp1 <- E$start %>% filter(team == 1, fieldPos == 1)
  sp0.hand = (P[[away_name]] %>% filter(retroID == sp0$retroID))$Throw
  sp1.hand = (P[[home_name]] %>% filter(retroID == sp1$retroID))$Throw
  G <- G %>% mutate(pit.retroID = ifelse(team == 0, sp1$retroID, sp0$retroID), 
                    pit.name = ifelse(team == 0, sp1$name, sp0$name), 
                    pit.hand = ifelse(team == 0, sp1.hand, sp0.hand), 
                    sp.ind = 1)
  
  # remove substitution rows which involve a player on the field changing his position !!! important for cataloging pitcher subs...
  na_rows = which(G$play == "NP")
  subs = E$sub
  sub_rows = if (length(na_rows) >= 1) c(na_rows[1]) else na_rows
  if (length(na_rows) >= 2) {
    for (i in 2:length(na_rows)) {
      curr = G[na_rows[i],]
      prev = G[last(sub_rows),] #G[na_rows[i-1],]
      if (curr$retroID == prev$retroID & curr$inning == prev$inning & curr$fieldPos != prev$fieldPos) {
        # this substitution is merely a player on the field changing his position
      } else {
        sub_rows = c(sub_rows, na_rows[i])
      }
    }
  }
  
  # add relief pitcher data
  a = length(sub_rows)
  b = if (is.null(subs)) 0 else nrow(subs)
  if (a != b) { # check if the number of substitutions makes sense given the plate-appearance data
    print("SUBS ERROR... subs num doesnt line up, so ignore this game...")
    print(length(sub_rows)); print(nrow(subs))
    browser()
    return(tibble())
    # G <- G %>% mutate(pit.retroID = NA, pit.name = NA, pit.hand = NA, sp.ind = NA) # if we wanted to keep this game...
  } else if (a > 0) { # add relief pitcher data
      for (i in 1:length(sub_rows)) {
      r = sub_rows[i] # row in G to document the substitution; need only do this for the Pitcher column
      pit.sub = subs[i,]
      pit.sub.hand = ifelse(pit.sub$team == 0, 
                            (P[[away_name]] %>% filter(retroID == pit.sub$retroID))$Throw,
                            (P[[home_name]] %>% filter(retroID == pit.sub$retroID))$Throw)
      
      if (pit.sub$fieldPos == 1) { # we are recording Pitcher substitutions
        G <- G %>% mutate(pit.retroID = ifelse(row_number() >= r & team != pit.sub$team, pit.sub$retroID, pit.retroID), 
                     pit.name = ifelse(row_number() >= r & team != pit.sub$team, pit.sub$name, pit.name), 
                     pit.hand = ifelse(row_number() >= r & team != pit.sub$team, pit.sub.hand, pit.hand), 
                     sp.ind = ifelse(row_number() >= r & team != pit.sub$team, 0, sp.ind))
      }
    }
  }

  G = if (E_was_weird) G %>% filter(inning != "NA") else G
  return(G)
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
  
  filename = paste0("retro_PA_1_", year, ".csv")
  write_csv(result, filename)
}

##################################################
################ Years 2000 - 2019 ###############
##################################################

for (yr in 2010:2010) { create.dataset.1(yr) }

#####################################################################

#####################################################################
################ EXAMPLE: 2012 Dodgers, 1st Home Game ###############
#####################################################################

# D <- get_retrosheet("play", 2012, "LAN")
# R <- get_retrosheet("roster", 2012)
# P <- R
# for (i in 1:length(R)) {
#   P[[i]] =  R[[i]] %>% select(retroID, Throw)
#   R[[i]] = R[[i]] %>% select(retroID, Bat)
# }
# result <- tibble()
# ###########
# E <- D[[1]]
# result <- bind_rows(result, manipulate.data.1(E,P,R))
# rm(E)

#####################################################################
################ EXAMPLE: All games from 2012 Dodgers ###############
#####################################################################

# result <- tibble()
# 
# L = length(D)
# for (i in 1:L) {
#   print(i)
#   E <- D[[i]]
#   result <- suppress_warnings( bind_rows(result, manipulate.data.1(E)), "Inf")
#   rm(E)
# }

#################################################################################
################ EXAMPLE: All games from a given year (say, 2012) ###############
#################################################################################

# create.dataset.1(2012)
# B = read_csv("data1_2012_sp.csv")





