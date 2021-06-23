# WASABI Baseball Starting Pitcher Time Through the Order (TTO)
# Dec 2020

library(tidyverse)
library(ggplot2)

MLB_2010 <- read_csv(file = "data2010.csv")
MLB_2011 <- read_csv(file = "data2011.csv")
MLB_2012 <- read_csv(file = "data2012.csv")
MLB_2013 <- read_csv(file = "data2013.csv")
MLB_2014 <- read_csv(file = "data2014.csv")
MLB_2015 <- read_csv(file = "data2015.csv")
MLB_2016 <- read_csv(file = "data2016.csv")
MLB_2017 <- read_csv(file = "data2017.csv")
MLB_2018 <- read_csv(file = "data2018.csv")
MLB_2019 <- read_csv(file = "data2019.csv")

table(MLB_2019$GAME_NEW_FL)

MLB_2010_A <- select(MLB_2010, X1, GAME_ID, AWAY_TEAM_ID, INN_CT, BAT_HOME_ID, OUTS_CT, PITCH_SEQ_TX, BAT_ID, BAT_HAND_CD, PIT_ID, PIT_HAND_CD, EVENT_TX, PH_FL, BAT_FLD_CD, BAT_LINEUP_ID, EVENT_CD, BAT_EVENT_FL, AB_FL, H_FL, EVENT_OUTS_CT, RBI_CT, RUNS, Runs.Inning) %>%
  filter(BAT_EVENT_FL==TRUE) %>%
  mutate(pitch_ct_bat = str_length(PITCH_SEQ_TX))  %>%
  mutate(HOME_TEAM_ID=substr(GAME_ID, 1,3), HIT_binary=ifelse(H_FL==0, 0, 1))  %>%
  mutate(HOME_LEAG = ifelse(HOME_TEAM_ID=="ATL", "NL", ifelse(HOME_TEAM_ID=="FLO", "NL", ifelse(HOME_TEAM_ID=="NYN", "NL", ifelse(HOME_TEAM_ID=="PHI", "NL", ifelse(HOME_TEAM_ID=="WAS", "NL", ifelse(HOME_TEAM_ID=="CHN", "NL", ifelse(HOME_TEAM_ID=="CIN", "NL", ifelse(HOME_TEAM_ID=="MIL", "NL", ifelse(HOME_TEAM_ID=="PIT", "NL", ifelse(HOME_TEAM_ID=="SLN", "NL", ifelse(HOME_TEAM_ID=="ARI", "NL", ifelse(HOME_TEAM_ID=="SDN", "NL", ifelse(HOME_TEAM_ID=="COL", "NL", ifelse(HOME_TEAM_ID=="SFN", "NL", ifelse(HOME_TEAM_ID=="LAN", "NL", "AL")))))))))))))))) %>%
  mutate(AWAY_LEAG = ifelse(AWAY_TEAM_ID=="ATL", "NL", ifelse(AWAY_TEAM_ID=="FLO", "NL", ifelse(AWAY_TEAM_ID=="NYN", "NL", ifelse(AWAY_TEAM_ID=="PHI", "NL", ifelse(AWAY_TEAM_ID=="WAS", "NL", ifelse(AWAY_TEAM_ID=="", "NL", ifelse(AWAY_TEAM_ID=="CIN", "NL", ifelse(AWAY_TEAM_ID=="MIL", "NL", ifelse(AWAY_TEAM_ID=="PIT", "NL", ifelse(AWAY_TEAM_ID=="SLN", "NL", ifelse(AWAY_TEAM_ID=="ARI", "NL", ifelse(AWAY_TEAM_ID=="SDN", "NL", ifelse(AWAY_TEAM_ID=="COL", "NL", ifelse(AWAY_TEAM_ID=="SFN", "NL", ifelse(AWAY_TEAM_ID=="LAN", "NL", "AL")))))))))))))))) %>%
  mutate(HOME_DIV = ifelse(HOME_TEAM_ID=="ATL", "NL_East", ifelse(HOME_TEAM_ID=="FLO", "NL_East", ifelse(HOME_TEAM_ID=="NYN", "NL_East", ifelse(HOME_TEAM_ID=="PHI", "NL_East", ifelse(HOME_TEAM_ID=="WAS", "NL_East", ifelse(HOME_TEAM_ID=="CHN", "NL_CNTRL", ifelse(HOME_TEAM_ID=="CIN", "NL_CNTRL", ifelse(HOME_TEAM_ID=="MIL", "NL_CNTRL", ifelse(HOME_TEAM_ID=="PIT", "NL_CNTRL", ifelse(HOME_TEAM_ID=="SLN", "NL_CNTRL", ifelse(HOME_TEAM_ID=="ARI", "NL_WEST", ifelse(HOME_TEAM_ID=="SDN", "NL_WEST", ifelse(HOME_TEAM_ID=="COL", "NL_WEST", ifelse(HOME_TEAM_ID=="SFN", "NL_WEST", ifelse(HOME_TEAM_ID=="LAN", "NL_WEST", ifelse(HOME_TEAM_ID=="BAL", "AL_East", ifelse(HOME_TEAM_ID=="BOS", "AL_East", ifelse(HOME_TEAM_ID=="NYA", "AL_East", ifelse(HOME_TEAM_ID=="TOR", "AL_East", ifelse(HOME_TEAM_ID=="TBA", "AL_East", ifelse(HOME_TEAM_ID=="CHA", "AL_CNTRL", ifelse(HOME_TEAM_ID=="CLE", "AL_CNTRL", ifelse(HOME_TEAM_ID=="DET", "AL_CNTRL", ifelse(HOME_TEAM_ID=="KCA", "AL_CNTRL", ifelse(HOME_TEAM_ID=="MIN", "AL_CNTRL", ifelse(HOME_TEAM_ID=="ANA", "AL_WEST", ifelse(HOME_TEAM_ID=="HOU", "AL_WEST", ifelse(HOME_TEAM_ID=="OAK", "AL_WEST", ifelse(HOME_TEAM_ID=="SEA", "AL_WEST", ifelse(HOME_TEAM_ID=="SFN", "AL_WEST", "AL_WEST"))))))))))))))))))))))))))))))) %>%
  mutate(AWAY_DIV = ifelse(AWAY_TEAM_ID=="ATL", "NL_East", ifelse(AWAY_TEAM_ID=="FLO", "NL_East", ifelse(AWAY_TEAM_ID=="NYN", "NL_East", ifelse(AWAY_TEAM_ID=="PHI", "NL_East", ifelse(AWAY_TEAM_ID=="WAS", "NL_East", ifelse(AWAY_TEAM_ID=="CHN", "NL_CNTRL", ifelse(AWAY_TEAM_ID=="CIN", "NL_CNTRL", ifelse(AWAY_TEAM_ID=="MIL", "NL_CNTRL", ifelse(AWAY_TEAM_ID=="PIT", "NL_CNTRL", ifelse(AWAY_TEAM_ID=="SLN", "NL_CNTRL", ifelse(AWAY_TEAM_ID=="ARI", "NL_WEST", ifelse(AWAY_TEAM_ID=="SDN", "NL_WEST", ifelse(AWAY_TEAM_ID=="COL", "NL_WEST", ifelse(AWAY_TEAM_ID=="SFN", "NL_WEST", ifelse(AWAY_TEAM_ID=="LAN", "NL_WEST", ifelse(AWAY_TEAM_ID=="BAL", "AL_East", ifelse(AWAY_TEAM_ID=="BOS", "AL_East", ifelse(AWAY_TEAM_ID=="NYA", "AL_East", ifelse(AWAY_TEAM_ID=="TOR", "AL_East", ifelse(AWAY_TEAM_ID=="TBA", "AL_East", ifelse(AWAY_TEAM_ID=="CHA", "AL_CNTRL", ifelse(AWAY_TEAM_ID=="CLE", "AL_CNTRL", ifelse(AWAY_TEAM_ID=="DET", "AL_CNTRL", ifelse(AWAY_TEAM_ID=="KCA", "AL_CNTRL", ifelse(AWAY_TEAM_ID=="MIN", "AL_CNTRL", ifelse(AWAY_TEAM_ID=="ANA", "AL_WEST", ifelse(AWAY_TEAM_ID=="HOU", "AL_WEST", ifelse(AWAY_TEAM_ID=="OAK", "AL_WEST", ifelse(AWAY_TEAM_ID=="SEA", "AL_WEST", ifelse(AWAY_TEAM_ID=="SFN", "AL_WEST", "AL_WEST"))))))))))))))))))))))))))))))) %>%
  mutate(TeamMatch = ifelse(AWAY_DIV==HOME_DIV, "IntraDIV", ifelse(AWAY_LEAG==HOME_LEAG, "IntraLEAG", "InterLEAG"))) %>%
  mutate(YEAR=2010) %>%
  group_by(GAME_ID, BAT_HOME_ID) %>%
  mutate(total_batter=1:n(), pitch_count_p=cumsum(pitch_ct_bat), order=total_batter/9, order_ct=ifelse(order<=1, 1, ifelse(order<=2, 2, ifelse(order<=3, 3, ifelse(order<=4, 4, ifelse(order<=5, 5, ifelse(order<=6, 6, ifelse(order<=7,7,8)))))))) %>%
  ungroup() %>% 
  mutate(woba_evnt = ifelse(EVENT_CD==14, 0.72, ifelse(EVENT_CD==16, 0.75, ifelse(H_FL==1, 0.90, ifelse(EVENT_CD==18, 0.92, ifelse(H_FL==2, 1.24,ifelse(H_FL==3, 1.56, ifelse(H_FL==4, 1.95, 0))))))))

# Create data set for starting Pitchers & Relievers
starter_10 <- select(MLB_2010_A, GAME_ID, PIT_ID, order)  %>%
  filter(order <  0.2) %>% subset(select=-order)

#Season2018 Event level data set of only starters
MLB2010_A_s_A <- merge(MLB_2010_A, starter_10, by=c("GAME_ID", "PIT_ID"))


# 2011 YEAR 2011 YEAR
MLB_2011_A <- select(MLB_2011, X1, GAME_ID, AWAY_TEAM_ID, INN_CT, BAT_HOME_ID, OUTS_CT, PITCH_SEQ_TX, BAT_ID, BAT_HAND_CD, PIT_ID, PIT_HAND_CD, EVENT_TX, PH_FL, BAT_FLD_CD, BAT_LINEUP_ID, EVENT_CD, BAT_EVENT_FL, AB_FL, H_FL, EVENT_OUTS_CT, RBI_CT, RUNS, Runs.Inning) %>%
  filter(BAT_EVENT_FL==TRUE) %>%
  mutate(pitch_ct_bat = str_length(PITCH_SEQ_TX))  %>%
  mutate(HOME_TEAM_ID=substr(GAME_ID, 1,3), HIT_binary=ifelse(H_FL==0, 0, 1))  %>%
  mutate(HOME_LEAG = ifelse(HOME_TEAM_ID=="ATL", "NL", ifelse(HOME_TEAM_ID=="FLO", "NL", ifelse(HOME_TEAM_ID=="NYN", "NL", ifelse(HOME_TEAM_ID=="PHI", "NL", ifelse(HOME_TEAM_ID=="WAS", "NL", ifelse(HOME_TEAM_ID=="CHN", "NL", ifelse(HOME_TEAM_ID=="CIN", "NL", ifelse(HOME_TEAM_ID=="MIL", "NL", ifelse(HOME_TEAM_ID=="PIT", "NL", ifelse(HOME_TEAM_ID=="SLN", "NL", ifelse(HOME_TEAM_ID=="ARI", "NL", ifelse(HOME_TEAM_ID=="SDN", "NL", ifelse(HOME_TEAM_ID=="COL", "NL", ifelse(HOME_TEAM_ID=="SFN", "NL", ifelse(HOME_TEAM_ID=="LAN", "NL", "AL")))))))))))))))) %>%
  mutate(AWAY_LEAG = ifelse(AWAY_TEAM_ID=="ATL", "NL", ifelse(AWAY_TEAM_ID=="FLO", "NL", ifelse(AWAY_TEAM_ID=="NYN", "NL", ifelse(AWAY_TEAM_ID=="PHI", "NL", ifelse(AWAY_TEAM_ID=="WAS", "NL", ifelse(AWAY_TEAM_ID=="", "NL", ifelse(AWAY_TEAM_ID=="CIN", "NL", ifelse(AWAY_TEAM_ID=="MIL", "NL", ifelse(AWAY_TEAM_ID=="PIT", "NL", ifelse(AWAY_TEAM_ID=="SLN", "NL", ifelse(AWAY_TEAM_ID=="ARI", "NL", ifelse(AWAY_TEAM_ID=="SDN", "NL", ifelse(AWAY_TEAM_ID=="COL", "NL", ifelse(AWAY_TEAM_ID=="SFN", "NL", ifelse(AWAY_TEAM_ID=="LAN", "NL", "AL")))))))))))))))) %>%
  mutate(HOME_DIV = ifelse(HOME_TEAM_ID=="ATL", "NL_East", ifelse(HOME_TEAM_ID=="FLO", "NL_East", ifelse(HOME_TEAM_ID=="NYN", "NL_East", ifelse(HOME_TEAM_ID=="PHI", "NL_East", ifelse(HOME_TEAM_ID=="WAS", "NL_East", ifelse(HOME_TEAM_ID=="CHN", "NL_CNTRL", ifelse(HOME_TEAM_ID=="CIN", "NL_CNTRL", ifelse(HOME_TEAM_ID=="MIL", "NL_CNTRL", ifelse(HOME_TEAM_ID=="PIT", "NL_CNTRL", ifelse(HOME_TEAM_ID=="SLN", "NL_CNTRL", ifelse(HOME_TEAM_ID=="ARI", "NL_WEST", ifelse(HOME_TEAM_ID=="SDN", "NL_WEST", ifelse(HOME_TEAM_ID=="COL", "NL_WEST", ifelse(HOME_TEAM_ID=="SFN", "NL_WEST", ifelse(HOME_TEAM_ID=="LAN", "NL_WEST", ifelse(HOME_TEAM_ID=="BAL", "AL_East", ifelse(HOME_TEAM_ID=="BOS", "AL_East", ifelse(HOME_TEAM_ID=="NYA", "AL_East", ifelse(HOME_TEAM_ID=="TOR", "AL_East", ifelse(HOME_TEAM_ID=="TBA", "AL_East", ifelse(HOME_TEAM_ID=="CHA", "AL_CNTRL", ifelse(HOME_TEAM_ID=="CLE", "AL_CNTRL", ifelse(HOME_TEAM_ID=="DET", "AL_CNTRL", ifelse(HOME_TEAM_ID=="KCA", "AL_CNTRL", ifelse(HOME_TEAM_ID=="MIN", "AL_CNTRL", ifelse(HOME_TEAM_ID=="ANA", "AL_WEST", ifelse(HOME_TEAM_ID=="HOU", "AL_WEST", ifelse(HOME_TEAM_ID=="OAK", "AL_WEST", ifelse(HOME_TEAM_ID=="SEA", "AL_WEST", ifelse(HOME_TEAM_ID=="SFN", "AL_WEST", "AL_WEST"))))))))))))))))))))))))))))))) %>%
  mutate(AWAY_DIV = ifelse(AWAY_TEAM_ID=="ATL", "NL_East", ifelse(AWAY_TEAM_ID=="FLO", "NL_East", ifelse(AWAY_TEAM_ID=="NYN", "NL_East", ifelse(AWAY_TEAM_ID=="PHI", "NL_East", ifelse(AWAY_TEAM_ID=="WAS", "NL_East", ifelse(AWAY_TEAM_ID=="CHN", "NL_CNTRL", ifelse(AWAY_TEAM_ID=="CIN", "NL_CNTRL", ifelse(AWAY_TEAM_ID=="MIL", "NL_CNTRL", ifelse(AWAY_TEAM_ID=="PIT", "NL_CNTRL", ifelse(AWAY_TEAM_ID=="SLN", "NL_CNTRL", ifelse(AWAY_TEAM_ID=="ARI", "NL_WEST", ifelse(AWAY_TEAM_ID=="SDN", "NL_WEST", ifelse(AWAY_TEAM_ID=="COL", "NL_WEST", ifelse(AWAY_TEAM_ID=="SFN", "NL_WEST", ifelse(AWAY_TEAM_ID=="LAN", "NL_WEST", ifelse(AWAY_TEAM_ID=="BAL", "AL_East", ifelse(AWAY_TEAM_ID=="BOS", "AL_East", ifelse(AWAY_TEAM_ID=="NYA", "AL_East", ifelse(AWAY_TEAM_ID=="TOR", "AL_East", ifelse(AWAY_TEAM_ID=="TBA", "AL_East", ifelse(AWAY_TEAM_ID=="CHA", "AL_CNTRL", ifelse(AWAY_TEAM_ID=="CLE", "AL_CNTRL", ifelse(AWAY_TEAM_ID=="DET", "AL_CNTRL", ifelse(AWAY_TEAM_ID=="KCA", "AL_CNTRL", ifelse(AWAY_TEAM_ID=="MIN", "AL_CNTRL", ifelse(AWAY_TEAM_ID=="ANA", "AL_WEST", ifelse(AWAY_TEAM_ID=="HOU", "AL_WEST", ifelse(AWAY_TEAM_ID=="OAK", "AL_WEST", ifelse(AWAY_TEAM_ID=="SEA", "AL_WEST", ifelse(AWAY_TEAM_ID=="SFN", "AL_WEST", "AL_WEST"))))))))))))))))))))))))))))))) %>%
  mutate(TeamMatch = ifelse(AWAY_DIV==HOME_DIV, "IntraDIV", ifelse(AWAY_LEAG==HOME_LEAG, "IntraLEAG", "InterLEAG"))) %>%
  mutate(YEAR=2011) %>%
  group_by(GAME_ID, BAT_HOME_ID) %>%
  mutate(total_batter=1:n(), pitch_count_p=cumsum(pitch_ct_bat), order=total_batter/9, order_ct=ifelse(order<=1, 1, ifelse(order<=2, 2, ifelse(order<=3, 3, ifelse(order<=4, 4, ifelse(order<=5, 5, ifelse(order<=6, 6, ifelse(order<=7,7,8)))))))) %>%
  ungroup() %>% 
  mutate(woba_evnt = ifelse(EVENT_CD==14, 0.72, ifelse(EVENT_CD==16, 0.75, ifelse(H_FL==1, 0.90, ifelse(EVENT_CD==18, 0.92, ifelse(H_FL==2, 1.24,ifelse(H_FL==3, 1.56, ifelse(H_FL==4, 1.95, 0))))))))

# Create data set for starting Pitchers & Relievers
starter_11 <- select(MLB_2011_A, GAME_ID, PIT_ID, order)  %>%
  filter(order <  0.2) %>% subset(select=-order)

# Event level data set of only starters
MLB2011_A_s_A <- merge(MLB_2011_A, starter_11, by=c("GAME_ID", "PIT_ID"))



# 2012 YEAR 2012 YEAR
MLB_2012_A <- select(MLB_2012, X1, GAME_ID, AWAY_TEAM_ID, INN_CT, BAT_HOME_ID, OUTS_CT, PITCH_SEQ_TX, BAT_ID, BAT_HAND_CD, PIT_ID, PIT_HAND_CD, EVENT_TX, PH_FL, BAT_FLD_CD, BAT_LINEUP_ID, EVENT_CD, BAT_EVENT_FL, AB_FL, H_FL, EVENT_OUTS_CT, RBI_CT, RUNS, Runs.Inning) %>%
  filter(BAT_EVENT_FL==TRUE) %>%
  mutate(pitch_ct_bat = str_length(PITCH_SEQ_TX))  %>%
  mutate(HOME_TEAM_ID=substr(GAME_ID, 1,3), HIT_binary=ifelse(H_FL==0, 0, 1))  %>%
  mutate(HOME_LEAG = ifelse(HOME_TEAM_ID=="ATL", "NL", ifelse(HOME_TEAM_ID=="FLO", "NL", ifelse(HOME_TEAM_ID=="NYN", "NL", ifelse(HOME_TEAM_ID=="PHI", "NL", ifelse(HOME_TEAM_ID=="WAS", "NL", ifelse(HOME_TEAM_ID=="CHN", "NL", ifelse(HOME_TEAM_ID=="CIN", "NL", ifelse(HOME_TEAM_ID=="MIL", "NL", ifelse(HOME_TEAM_ID=="PIT", "NL", ifelse(HOME_TEAM_ID=="SLN", "NL", ifelse(HOME_TEAM_ID=="ARI", "NL", ifelse(HOME_TEAM_ID=="SDN", "NL", ifelse(HOME_TEAM_ID=="COL", "NL", ifelse(HOME_TEAM_ID=="SFN", "NL", ifelse(HOME_TEAM_ID=="LAN", "NL", "AL")))))))))))))))) %>%
  mutate(AWAY_LEAG = ifelse(AWAY_TEAM_ID=="ATL", "NL", ifelse(AWAY_TEAM_ID=="FLO", "NL", ifelse(AWAY_TEAM_ID=="NYN", "NL", ifelse(AWAY_TEAM_ID=="PHI", "NL", ifelse(AWAY_TEAM_ID=="WAS", "NL", ifelse(AWAY_TEAM_ID=="", "NL", ifelse(AWAY_TEAM_ID=="CIN", "NL", ifelse(AWAY_TEAM_ID=="MIL", "NL", ifelse(AWAY_TEAM_ID=="PIT", "NL", ifelse(AWAY_TEAM_ID=="SLN", "NL", ifelse(AWAY_TEAM_ID=="ARI", "NL", ifelse(AWAY_TEAM_ID=="SDN", "NL", ifelse(AWAY_TEAM_ID=="COL", "NL", ifelse(AWAY_TEAM_ID=="SFN", "NL", ifelse(AWAY_TEAM_ID=="LAN", "NL", "AL")))))))))))))))) %>%
  mutate(HOME_DIV = ifelse(HOME_TEAM_ID=="ATL", "NL_East", ifelse(HOME_TEAM_ID=="FLO", "NL_East", ifelse(HOME_TEAM_ID=="NYN", "NL_East", ifelse(HOME_TEAM_ID=="PHI", "NL_East", ifelse(HOME_TEAM_ID=="WAS", "NL_East", ifelse(HOME_TEAM_ID=="CHN", "NL_CNTRL", ifelse(HOME_TEAM_ID=="CIN", "NL_CNTRL", ifelse(HOME_TEAM_ID=="MIL", "NL_CNTRL", ifelse(HOME_TEAM_ID=="PIT", "NL_CNTRL", ifelse(HOME_TEAM_ID=="SLN", "NL_CNTRL", ifelse(HOME_TEAM_ID=="ARI", "NL_WEST", ifelse(HOME_TEAM_ID=="SDN", "NL_WEST", ifelse(HOME_TEAM_ID=="COL", "NL_WEST", ifelse(HOME_TEAM_ID=="SFN", "NL_WEST", ifelse(HOME_TEAM_ID=="LAN", "NL_WEST", ifelse(HOME_TEAM_ID=="BAL", "AL_East", ifelse(HOME_TEAM_ID=="BOS", "AL_East", ifelse(HOME_TEAM_ID=="NYA", "AL_East", ifelse(HOME_TEAM_ID=="TOR", "AL_East", ifelse(HOME_TEAM_ID=="TBA", "AL_East", ifelse(HOME_TEAM_ID=="CHA", "AL_CNTRL", ifelse(HOME_TEAM_ID=="CLE", "AL_CNTRL", ifelse(HOME_TEAM_ID=="DET", "AL_CNTRL", ifelse(HOME_TEAM_ID=="KCA", "AL_CNTRL", ifelse(HOME_TEAM_ID=="MIN", "AL_CNTRL", ifelse(HOME_TEAM_ID=="ANA", "AL_WEST", ifelse(HOME_TEAM_ID=="HOU", "AL_WEST", ifelse(HOME_TEAM_ID=="OAK", "AL_WEST", ifelse(HOME_TEAM_ID=="SEA", "AL_WEST", ifelse(HOME_TEAM_ID=="SFN", "AL_WEST", "AL_WEST"))))))))))))))))))))))))))))))) %>%
  mutate(AWAY_DIV = ifelse(AWAY_TEAM_ID=="ATL", "NL_East", ifelse(AWAY_TEAM_ID=="FLO", "NL_East", ifelse(AWAY_TEAM_ID=="NYN", "NL_East", ifelse(AWAY_TEAM_ID=="PHI", "NL_East", ifelse(AWAY_TEAM_ID=="WAS", "NL_East", ifelse(AWAY_TEAM_ID=="CHN", "NL_CNTRL", ifelse(AWAY_TEAM_ID=="CIN", "NL_CNTRL", ifelse(AWAY_TEAM_ID=="MIL", "NL_CNTRL", ifelse(AWAY_TEAM_ID=="PIT", "NL_CNTRL", ifelse(AWAY_TEAM_ID=="SLN", "NL_CNTRL", ifelse(AWAY_TEAM_ID=="ARI", "NL_WEST", ifelse(AWAY_TEAM_ID=="SDN", "NL_WEST", ifelse(AWAY_TEAM_ID=="COL", "NL_WEST", ifelse(AWAY_TEAM_ID=="SFN", "NL_WEST", ifelse(AWAY_TEAM_ID=="LAN", "NL_WEST", ifelse(AWAY_TEAM_ID=="BAL", "AL_East", ifelse(AWAY_TEAM_ID=="BOS", "AL_East", ifelse(AWAY_TEAM_ID=="NYA", "AL_East", ifelse(AWAY_TEAM_ID=="TOR", "AL_East", ifelse(AWAY_TEAM_ID=="TBA", "AL_East", ifelse(AWAY_TEAM_ID=="CHA", "AL_CNTRL", ifelse(AWAY_TEAM_ID=="CLE", "AL_CNTRL", ifelse(AWAY_TEAM_ID=="DET", "AL_CNTRL", ifelse(AWAY_TEAM_ID=="KCA", "AL_CNTRL", ifelse(AWAY_TEAM_ID=="MIN", "AL_CNTRL", ifelse(AWAY_TEAM_ID=="ANA", "AL_WEST", ifelse(AWAY_TEAM_ID=="HOU", "AL_WEST", ifelse(AWAY_TEAM_ID=="OAK", "AL_WEST", ifelse(AWAY_TEAM_ID=="SEA", "AL_WEST", ifelse(AWAY_TEAM_ID=="SFN", "AL_WEST", "AL_WEST"))))))))))))))))))))))))))))))) %>%
  mutate(TeamMatch = ifelse(AWAY_DIV==HOME_DIV, "IntraDIV", ifelse(AWAY_LEAG==HOME_LEAG, "IntraLEAG", "InterLEAG"))) %>%
  mutate(YEAR=2012) %>%
  group_by(GAME_ID, BAT_HOME_ID) %>%
  mutate(total_batter=1:n(), pitch_count_p=cumsum(pitch_ct_bat), order=total_batter/9, order_ct=ifelse(order<=1, 1, ifelse(order<=2, 2, ifelse(order<=3, 3, ifelse(order<=4, 4, ifelse(order<=5, 5, ifelse(order<=6, 6, ifelse(order<=7,7,8)))))))) %>%
  ungroup() %>% 
  mutate(woba_evnt = ifelse(EVENT_CD==14, 0.72, ifelse(EVENT_CD==16, 0.75, ifelse(H_FL==1, 0.90, ifelse(EVENT_CD==18, 0.92, ifelse(H_FL==2, 1.24,ifelse(H_FL==3, 1.56, ifelse(H_FL==4, 1.95, 0))))))))

# Create data set for starting Pitchers & Relievers
starter_12 <- select(MLB_2012_A, GAME_ID, PIT_ID, order)  %>%
  filter(order <  0.2) %>% subset(select=-order)

# Event level data set of only starters
MLB2012_A_s_A <- merge(MLB_2012_A, starter_12, by=c("GAME_ID", "PIT_ID"))



# 2013 YEAR 2013 YEAR
MLB_2013_A <- select(MLB_2013, X1, GAME_ID, AWAY_TEAM_ID, INN_CT, BAT_HOME_ID, OUTS_CT, PITCH_SEQ_TX, BAT_ID, BAT_HAND_CD, PIT_ID, PIT_HAND_CD, EVENT_TX, PH_FL, BAT_FLD_CD, BAT_LINEUP_ID, EVENT_CD, BAT_EVENT_FL, AB_FL, H_FL, EVENT_OUTS_CT, RBI_CT, RUNS, Runs.Inning) %>%
  filter(BAT_EVENT_FL==TRUE) %>%
  mutate(pitch_ct_bat = str_length(PITCH_SEQ_TX))  %>%
  mutate(HOME_TEAM_ID=substr(GAME_ID, 1,3), HIT_binary=ifelse(H_FL==0, 0, 1))  %>%
  mutate(HOME_LEAG = ifelse(HOME_TEAM_ID=="ATL", "NL", ifelse(HOME_TEAM_ID=="FLO", "NL", ifelse(HOME_TEAM_ID=="NYN", "NL", ifelse(HOME_TEAM_ID=="PHI", "NL", ifelse(HOME_TEAM_ID=="WAS", "NL", ifelse(HOME_TEAM_ID=="CHN", "NL", ifelse(HOME_TEAM_ID=="CIN", "NL", ifelse(HOME_TEAM_ID=="MIL", "NL", ifelse(HOME_TEAM_ID=="PIT", "NL", ifelse(HOME_TEAM_ID=="SLN", "NL", ifelse(HOME_TEAM_ID=="ARI", "NL", ifelse(HOME_TEAM_ID=="SDN", "NL", ifelse(HOME_TEAM_ID=="COL", "NL", ifelse(HOME_TEAM_ID=="SFN", "NL", ifelse(HOME_TEAM_ID=="LAN", "NL", "AL")))))))))))))))) %>%
  mutate(AWAY_LEAG = ifelse(AWAY_TEAM_ID=="ATL", "NL", ifelse(AWAY_TEAM_ID=="FLO", "NL", ifelse(AWAY_TEAM_ID=="NYN", "NL", ifelse(AWAY_TEAM_ID=="PHI", "NL", ifelse(AWAY_TEAM_ID=="WAS", "NL", ifelse(AWAY_TEAM_ID=="", "NL", ifelse(AWAY_TEAM_ID=="CIN", "NL", ifelse(AWAY_TEAM_ID=="MIL", "NL", ifelse(AWAY_TEAM_ID=="PIT", "NL", ifelse(AWAY_TEAM_ID=="SLN", "NL", ifelse(AWAY_TEAM_ID=="ARI", "NL", ifelse(AWAY_TEAM_ID=="SDN", "NL", ifelse(AWAY_TEAM_ID=="COL", "NL", ifelse(AWAY_TEAM_ID=="SFN", "NL", ifelse(AWAY_TEAM_ID=="LAN", "NL", "AL")))))))))))))))) %>%
  mutate(HOME_DIV = ifelse(HOME_TEAM_ID=="ATL", "NL_East", ifelse(HOME_TEAM_ID=="FLO", "NL_East", ifelse(HOME_TEAM_ID=="NYN", "NL_East", ifelse(HOME_TEAM_ID=="PHI", "NL_East", ifelse(HOME_TEAM_ID=="WAS", "NL_East", ifelse(HOME_TEAM_ID=="CHN", "NL_CNTRL", ifelse(HOME_TEAM_ID=="CIN", "NL_CNTRL", ifelse(HOME_TEAM_ID=="MIL", "NL_CNTRL", ifelse(HOME_TEAM_ID=="PIT", "NL_CNTRL", ifelse(HOME_TEAM_ID=="SLN", "NL_CNTRL", ifelse(HOME_TEAM_ID=="ARI", "NL_WEST", ifelse(HOME_TEAM_ID=="SDN", "NL_WEST", ifelse(HOME_TEAM_ID=="COL", "NL_WEST", ifelse(HOME_TEAM_ID=="SFN", "NL_WEST", ifelse(HOME_TEAM_ID=="LAN", "NL_WEST", ifelse(HOME_TEAM_ID=="BAL", "AL_East", ifelse(HOME_TEAM_ID=="BOS", "AL_East", ifelse(HOME_TEAM_ID=="NYA", "AL_East", ifelse(HOME_TEAM_ID=="TOR", "AL_East", ifelse(HOME_TEAM_ID=="TBA", "AL_East", ifelse(HOME_TEAM_ID=="CHA", "AL_CNTRL", ifelse(HOME_TEAM_ID=="CLE", "AL_CNTRL", ifelse(HOME_TEAM_ID=="DET", "AL_CNTRL", ifelse(HOME_TEAM_ID=="KCA", "AL_CNTRL", ifelse(HOME_TEAM_ID=="MIN", "AL_CNTRL", ifelse(HOME_TEAM_ID=="ANA", "AL_WEST", ifelse(HOME_TEAM_ID=="HOU", "AL_WEST", ifelse(HOME_TEAM_ID=="OAK", "AL_WEST", ifelse(HOME_TEAM_ID=="SEA", "AL_WEST", ifelse(HOME_TEAM_ID=="SFN", "AL_WEST", "AL_WEST"))))))))))))))))))))))))))))))) %>%
  mutate(AWAY_DIV = ifelse(AWAY_TEAM_ID=="ATL", "NL_East", ifelse(AWAY_TEAM_ID=="FLO", "NL_East", ifelse(AWAY_TEAM_ID=="NYN", "NL_East", ifelse(AWAY_TEAM_ID=="PHI", "NL_East", ifelse(AWAY_TEAM_ID=="WAS", "NL_East", ifelse(AWAY_TEAM_ID=="CHN", "NL_CNTRL", ifelse(AWAY_TEAM_ID=="CIN", "NL_CNTRL", ifelse(AWAY_TEAM_ID=="MIL", "NL_CNTRL", ifelse(AWAY_TEAM_ID=="PIT", "NL_CNTRL", ifelse(AWAY_TEAM_ID=="SLN", "NL_CNTRL", ifelse(AWAY_TEAM_ID=="ARI", "NL_WEST", ifelse(AWAY_TEAM_ID=="SDN", "NL_WEST", ifelse(AWAY_TEAM_ID=="COL", "NL_WEST", ifelse(AWAY_TEAM_ID=="SFN", "NL_WEST", ifelse(AWAY_TEAM_ID=="LAN", "NL_WEST", ifelse(AWAY_TEAM_ID=="BAL", "AL_East", ifelse(AWAY_TEAM_ID=="BOS", "AL_East", ifelse(AWAY_TEAM_ID=="NYA", "AL_East", ifelse(AWAY_TEAM_ID=="TOR", "AL_East", ifelse(AWAY_TEAM_ID=="TBA", "AL_East", ifelse(AWAY_TEAM_ID=="CHA", "AL_CNTRL", ifelse(AWAY_TEAM_ID=="CLE", "AL_CNTRL", ifelse(AWAY_TEAM_ID=="DET", "AL_CNTRL", ifelse(AWAY_TEAM_ID=="KCA", "AL_CNTRL", ifelse(AWAY_TEAM_ID=="MIN", "AL_CNTRL", ifelse(AWAY_TEAM_ID=="ANA", "AL_WEST", ifelse(AWAY_TEAM_ID=="HOU", "AL_WEST", ifelse(AWAY_TEAM_ID=="OAK", "AL_WEST", ifelse(AWAY_TEAM_ID=="SEA", "AL_WEST", ifelse(AWAY_TEAM_ID=="SFN", "AL_WEST", "AL_WEST"))))))))))))))))))))))))))))))) %>%
  mutate(TeamMatch = ifelse(AWAY_DIV==HOME_DIV, "IntraDIV", ifelse(AWAY_LEAG==HOME_LEAG, "IntraLEAG", "InterLEAG"))) %>%
  mutate(YEAR=2013) %>%
  group_by(GAME_ID, BAT_HOME_ID) %>%
  mutate(total_batter=1:n(), pitch_count_p=cumsum(pitch_ct_bat), order=total_batter/9, order_ct=ifelse(order<=1, 1, ifelse(order<=2, 2, ifelse(order<=3, 3, ifelse(order<=4, 4, ifelse(order<=5, 5, ifelse(order<=6, 6, ifelse(order<=7,7,8)))))))) %>%
  ungroup() %>% 
  mutate(woba_evnt = ifelse(EVENT_CD==14, 0.72, ifelse(EVENT_CD==16, 0.75, ifelse(H_FL==1, 0.90, ifelse(EVENT_CD==18, 0.92, ifelse(H_FL==2, 1.24,ifelse(H_FL==3, 1.56, ifelse(H_FL==4, 1.95, 0))))))))

# Create data set for starting Pitchers & Relievers
starter_13 <- select(MLB_2013_A, GAME_ID, PIT_ID, order)  %>%
  filter(order <  0.2) %>% subset(select=-order)

# Event level data set of only starters
MLB2013_A_s_A <- merge(MLB_2013_A, starter_13, by=c("GAME_ID", "PIT_ID"))



# 2014 YEAR 2014 YEAR
MLB_2014_A <- select(MLB_2014, X1, GAME_ID, AWAY_TEAM_ID, INN_CT, BAT_HOME_ID, OUTS_CT, PITCH_SEQ_TX, BAT_ID, BAT_HAND_CD, PIT_ID, PIT_HAND_CD, EVENT_TX, PH_FL, BAT_FLD_CD, BAT_LINEUP_ID, EVENT_CD, BAT_EVENT_FL, AB_FL, H_FL, EVENT_OUTS_CT, RBI_CT, RUNS, Runs.Inning) %>%
  filter(BAT_EVENT_FL==TRUE) %>%
  mutate(pitch_ct_bat = str_length(PITCH_SEQ_TX))  %>%
  mutate(HOME_TEAM_ID=substr(GAME_ID, 1,3), HIT_binary=ifelse(H_FL==0, 0, 1))  %>%
  mutate(HOME_LEAG = ifelse(HOME_TEAM_ID=="ATL", "NL", ifelse(HOME_TEAM_ID=="FLO", "NL", ifelse(HOME_TEAM_ID=="NYN", "NL", ifelse(HOME_TEAM_ID=="PHI", "NL", ifelse(HOME_TEAM_ID=="WAS", "NL", ifelse(HOME_TEAM_ID=="CHN", "NL", ifelse(HOME_TEAM_ID=="CIN", "NL", ifelse(HOME_TEAM_ID=="MIL", "NL", ifelse(HOME_TEAM_ID=="PIT", "NL", ifelse(HOME_TEAM_ID=="SLN", "NL", ifelse(HOME_TEAM_ID=="ARI", "NL", ifelse(HOME_TEAM_ID=="SDN", "NL", ifelse(HOME_TEAM_ID=="COL", "NL", ifelse(HOME_TEAM_ID=="SFN", "NL", ifelse(HOME_TEAM_ID=="LAN", "NL", "AL")))))))))))))))) %>%
  mutate(AWAY_LEAG = ifelse(AWAY_TEAM_ID=="ATL", "NL", ifelse(AWAY_TEAM_ID=="FLO", "NL", ifelse(AWAY_TEAM_ID=="NYN", "NL", ifelse(AWAY_TEAM_ID=="PHI", "NL", ifelse(AWAY_TEAM_ID=="WAS", "NL", ifelse(AWAY_TEAM_ID=="", "NL", ifelse(AWAY_TEAM_ID=="CIN", "NL", ifelse(AWAY_TEAM_ID=="MIL", "NL", ifelse(AWAY_TEAM_ID=="PIT", "NL", ifelse(AWAY_TEAM_ID=="SLN", "NL", ifelse(AWAY_TEAM_ID=="ARI", "NL", ifelse(AWAY_TEAM_ID=="SDN", "NL", ifelse(AWAY_TEAM_ID=="COL", "NL", ifelse(AWAY_TEAM_ID=="SFN", "NL", ifelse(AWAY_TEAM_ID=="LAN", "NL", "AL")))))))))))))))) %>%
  mutate(HOME_DIV = ifelse(HOME_TEAM_ID=="ATL", "NL_East", ifelse(HOME_TEAM_ID=="FLO", "NL_East", ifelse(HOME_TEAM_ID=="NYN", "NL_East", ifelse(HOME_TEAM_ID=="PHI", "NL_East", ifelse(HOME_TEAM_ID=="WAS", "NL_East", ifelse(HOME_TEAM_ID=="CHN", "NL_CNTRL", ifelse(HOME_TEAM_ID=="CIN", "NL_CNTRL", ifelse(HOME_TEAM_ID=="MIL", "NL_CNTRL", ifelse(HOME_TEAM_ID=="PIT", "NL_CNTRL", ifelse(HOME_TEAM_ID=="SLN", "NL_CNTRL", ifelse(HOME_TEAM_ID=="ARI", "NL_WEST", ifelse(HOME_TEAM_ID=="SDN", "NL_WEST", ifelse(HOME_TEAM_ID=="COL", "NL_WEST", ifelse(HOME_TEAM_ID=="SFN", "NL_WEST", ifelse(HOME_TEAM_ID=="LAN", "NL_WEST", ifelse(HOME_TEAM_ID=="BAL", "AL_East", ifelse(HOME_TEAM_ID=="BOS", "AL_East", ifelse(HOME_TEAM_ID=="NYA", "AL_East", ifelse(HOME_TEAM_ID=="TOR", "AL_East", ifelse(HOME_TEAM_ID=="TBA", "AL_East", ifelse(HOME_TEAM_ID=="CHA", "AL_CNTRL", ifelse(HOME_TEAM_ID=="CLE", "AL_CNTRL", ifelse(HOME_TEAM_ID=="DET", "AL_CNTRL", ifelse(HOME_TEAM_ID=="KCA", "AL_CNTRL", ifelse(HOME_TEAM_ID=="MIN", "AL_CNTRL", ifelse(HOME_TEAM_ID=="ANA", "AL_WEST", ifelse(HOME_TEAM_ID=="HOU", "AL_WEST", ifelse(HOME_TEAM_ID=="OAK", "AL_WEST", ifelse(HOME_TEAM_ID=="SEA", "AL_WEST", ifelse(HOME_TEAM_ID=="SFN", "AL_WEST", "AL_WEST"))))))))))))))))))))))))))))))) %>%
  mutate(AWAY_DIV = ifelse(AWAY_TEAM_ID=="ATL", "NL_East", ifelse(AWAY_TEAM_ID=="FLO", "NL_East", ifelse(AWAY_TEAM_ID=="NYN", "NL_East", ifelse(AWAY_TEAM_ID=="PHI", "NL_East", ifelse(AWAY_TEAM_ID=="WAS", "NL_East", ifelse(AWAY_TEAM_ID=="CHN", "NL_CNTRL", ifelse(AWAY_TEAM_ID=="CIN", "NL_CNTRL", ifelse(AWAY_TEAM_ID=="MIL", "NL_CNTRL", ifelse(AWAY_TEAM_ID=="PIT", "NL_CNTRL", ifelse(AWAY_TEAM_ID=="SLN", "NL_CNTRL", ifelse(AWAY_TEAM_ID=="ARI", "NL_WEST", ifelse(AWAY_TEAM_ID=="SDN", "NL_WEST", ifelse(AWAY_TEAM_ID=="COL", "NL_WEST", ifelse(AWAY_TEAM_ID=="SFN", "NL_WEST", ifelse(AWAY_TEAM_ID=="LAN", "NL_WEST", ifelse(AWAY_TEAM_ID=="BAL", "AL_East", ifelse(AWAY_TEAM_ID=="BOS", "AL_East", ifelse(AWAY_TEAM_ID=="NYA", "AL_East", ifelse(AWAY_TEAM_ID=="TOR", "AL_East", ifelse(AWAY_TEAM_ID=="TBA", "AL_East", ifelse(AWAY_TEAM_ID=="CHA", "AL_CNTRL", ifelse(AWAY_TEAM_ID=="CLE", "AL_CNTRL", ifelse(AWAY_TEAM_ID=="DET", "AL_CNTRL", ifelse(AWAY_TEAM_ID=="KCA", "AL_CNTRL", ifelse(AWAY_TEAM_ID=="MIN", "AL_CNTRL", ifelse(AWAY_TEAM_ID=="ANA", "AL_WEST", ifelse(AWAY_TEAM_ID=="HOU", "AL_WEST", ifelse(AWAY_TEAM_ID=="OAK", "AL_WEST", ifelse(AWAY_TEAM_ID=="SEA", "AL_WEST", ifelse(AWAY_TEAM_ID=="SFN", "AL_WEST", "AL_WEST"))))))))))))))))))))))))))))))) %>%
  mutate(TeamMatch = ifelse(AWAY_DIV==HOME_DIV, "IntraDIV", ifelse(AWAY_LEAG==HOME_LEAG, "IntraLEAG", "InterLEAG"))) %>%
  mutate(YEAR=2014) %>%
  group_by(GAME_ID, BAT_HOME_ID) %>%
  mutate(total_batter=1:n(), pitch_count_p=cumsum(pitch_ct_bat), order=total_batter/9, order_ct=ifelse(order<=1, 1, ifelse(order<=2, 2, ifelse(order<=3, 3, ifelse(order<=4, 4, ifelse(order<=5, 5, ifelse(order<=6, 6, ifelse(order<=7,7,8)))))))) %>%
  ungroup() %>% 
  mutate(woba_evnt = ifelse(EVENT_CD==14, 0.72, ifelse(EVENT_CD==16, 0.75, ifelse(H_FL==1, 0.90, ifelse(EVENT_CD==18, 0.92, ifelse(H_FL==2, 1.24,ifelse(H_FL==3, 1.56, ifelse(H_FL==4, 1.95, 0))))))))

# Create data set for starting Pitchers & Relievers
starter_14 <- select(MLB_2014_A, GAME_ID, PIT_ID, order)  %>%
  filter(order <  0.2) %>% subset(select=-order)

# Event level data set of only starters
MLB2014_A_s_A <- merge(MLB_2014_A, starter_14, by=c("GAME_ID", "PIT_ID"))




# 2015 YEAR 2015 YEAR
MLB_2015_A <- select(MLB_2015, X1, GAME_ID, AWAY_TEAM_ID, INN_CT, BAT_HOME_ID, OUTS_CT, PITCH_SEQ_TX, BAT_ID, BAT_HAND_CD, PIT_ID, PIT_HAND_CD, EVENT_TX, PH_FL, BAT_FLD_CD, BAT_LINEUP_ID, EVENT_CD, BAT_EVENT_FL, AB_FL, H_FL, EVENT_OUTS_CT, RBI_CT, RUNS, Runs.Inning) %>%
  filter(BAT_EVENT_FL==TRUE) %>%
  mutate(pitch_ct_bat = str_length(PITCH_SEQ_TX))  %>%
  mutate(HOME_TEAM_ID=substr(GAME_ID, 1,3), HIT_binary=ifelse(H_FL==0, 0, 1))  %>%
  mutate(HOME_LEAG = ifelse(HOME_TEAM_ID=="ATL", "NL", ifelse(HOME_TEAM_ID=="FLO", "NL", ifelse(HOME_TEAM_ID=="NYN", "NL", ifelse(HOME_TEAM_ID=="PHI", "NL", ifelse(HOME_TEAM_ID=="WAS", "NL", ifelse(HOME_TEAM_ID=="CHN", "NL", ifelse(HOME_TEAM_ID=="CIN", "NL", ifelse(HOME_TEAM_ID=="MIL", "NL", ifelse(HOME_TEAM_ID=="PIT", "NL", ifelse(HOME_TEAM_ID=="SLN", "NL", ifelse(HOME_TEAM_ID=="ARI", "NL", ifelse(HOME_TEAM_ID=="SDN", "NL", ifelse(HOME_TEAM_ID=="COL", "NL", ifelse(HOME_TEAM_ID=="SFN", "NL", ifelse(HOME_TEAM_ID=="LAN", "NL", "AL")))))))))))))))) %>%
  mutate(AWAY_LEAG = ifelse(AWAY_TEAM_ID=="ATL", "NL", ifelse(AWAY_TEAM_ID=="FLO", "NL", ifelse(AWAY_TEAM_ID=="NYN", "NL", ifelse(AWAY_TEAM_ID=="PHI", "NL", ifelse(AWAY_TEAM_ID=="WAS", "NL", ifelse(AWAY_TEAM_ID=="", "NL", ifelse(AWAY_TEAM_ID=="CIN", "NL", ifelse(AWAY_TEAM_ID=="MIL", "NL", ifelse(AWAY_TEAM_ID=="PIT", "NL", ifelse(AWAY_TEAM_ID=="SLN", "NL", ifelse(AWAY_TEAM_ID=="ARI", "NL", ifelse(AWAY_TEAM_ID=="SDN", "NL", ifelse(AWAY_TEAM_ID=="COL", "NL", ifelse(AWAY_TEAM_ID=="SFN", "NL", ifelse(AWAY_TEAM_ID=="LAN", "NL", "AL")))))))))))))))) %>%
  mutate(HOME_DIV = ifelse(HOME_TEAM_ID=="ATL", "NL_East", ifelse(HOME_TEAM_ID=="FLO", "NL_East", ifelse(HOME_TEAM_ID=="NYN", "NL_East", ifelse(HOME_TEAM_ID=="PHI", "NL_East", ifelse(HOME_TEAM_ID=="WAS", "NL_East", ifelse(HOME_TEAM_ID=="CHN", "NL_CNTRL", ifelse(HOME_TEAM_ID=="CIN", "NL_CNTRL", ifelse(HOME_TEAM_ID=="MIL", "NL_CNTRL", ifelse(HOME_TEAM_ID=="PIT", "NL_CNTRL", ifelse(HOME_TEAM_ID=="SLN", "NL_CNTRL", ifelse(HOME_TEAM_ID=="ARI", "NL_WEST", ifelse(HOME_TEAM_ID=="SDN", "NL_WEST", ifelse(HOME_TEAM_ID=="COL", "NL_WEST", ifelse(HOME_TEAM_ID=="SFN", "NL_WEST", ifelse(HOME_TEAM_ID=="LAN", "NL_WEST", ifelse(HOME_TEAM_ID=="BAL", "AL_East", ifelse(HOME_TEAM_ID=="BOS", "AL_East", ifelse(HOME_TEAM_ID=="NYA", "AL_East", ifelse(HOME_TEAM_ID=="TOR", "AL_East", ifelse(HOME_TEAM_ID=="TBA", "AL_East", ifelse(HOME_TEAM_ID=="CHA", "AL_CNTRL", ifelse(HOME_TEAM_ID=="CLE", "AL_CNTRL", ifelse(HOME_TEAM_ID=="DET", "AL_CNTRL", ifelse(HOME_TEAM_ID=="KCA", "AL_CNTRL", ifelse(HOME_TEAM_ID=="MIN", "AL_CNTRL", ifelse(HOME_TEAM_ID=="ANA", "AL_WEST", ifelse(HOME_TEAM_ID=="HOU", "AL_WEST", ifelse(HOME_TEAM_ID=="OAK", "AL_WEST", ifelse(HOME_TEAM_ID=="SEA", "AL_WEST", ifelse(HOME_TEAM_ID=="SFN", "AL_WEST", "AL_WEST"))))))))))))))))))))))))))))))) %>%
  mutate(AWAY_DIV = ifelse(AWAY_TEAM_ID=="ATL", "NL_East", ifelse(AWAY_TEAM_ID=="FLO", "NL_East", ifelse(AWAY_TEAM_ID=="NYN", "NL_East", ifelse(AWAY_TEAM_ID=="PHI", "NL_East", ifelse(AWAY_TEAM_ID=="WAS", "NL_East", ifelse(AWAY_TEAM_ID=="CHN", "NL_CNTRL", ifelse(AWAY_TEAM_ID=="CIN", "NL_CNTRL", ifelse(AWAY_TEAM_ID=="MIL", "NL_CNTRL", ifelse(AWAY_TEAM_ID=="PIT", "NL_CNTRL", ifelse(AWAY_TEAM_ID=="SLN", "NL_CNTRL", ifelse(AWAY_TEAM_ID=="ARI", "NL_WEST", ifelse(AWAY_TEAM_ID=="SDN", "NL_WEST", ifelse(AWAY_TEAM_ID=="COL", "NL_WEST", ifelse(AWAY_TEAM_ID=="SFN", "NL_WEST", ifelse(AWAY_TEAM_ID=="LAN", "NL_WEST", ifelse(AWAY_TEAM_ID=="BAL", "AL_East", ifelse(AWAY_TEAM_ID=="BOS", "AL_East", ifelse(AWAY_TEAM_ID=="NYA", "AL_East", ifelse(AWAY_TEAM_ID=="TOR", "AL_East", ifelse(AWAY_TEAM_ID=="TBA", "AL_East", ifelse(AWAY_TEAM_ID=="CHA", "AL_CNTRL", ifelse(AWAY_TEAM_ID=="CLE", "AL_CNTRL", ifelse(AWAY_TEAM_ID=="DET", "AL_CNTRL", ifelse(AWAY_TEAM_ID=="KCA", "AL_CNTRL", ifelse(AWAY_TEAM_ID=="MIN", "AL_CNTRL", ifelse(AWAY_TEAM_ID=="ANA", "AL_WEST", ifelse(AWAY_TEAM_ID=="HOU", "AL_WEST", ifelse(AWAY_TEAM_ID=="OAK", "AL_WEST", ifelse(AWAY_TEAM_ID=="SEA", "AL_WEST", ifelse(AWAY_TEAM_ID=="SFN", "AL_WEST", "AL_WEST"))))))))))))))))))))))))))))))) %>%
  mutate(TeamMatch = ifelse(AWAY_DIV==HOME_DIV, "IntraDIV", ifelse(AWAY_LEAG==HOME_LEAG, "IntraLEAG", "InterLEAG"))) %>%
  mutate(YEAR=2015) %>%
  group_by(GAME_ID, BAT_HOME_ID) %>%
  mutate(total_batter=1:n(), pitch_count_p=cumsum(pitch_ct_bat), order=total_batter/9, order_ct=ifelse(order<=1, 1, ifelse(order<=2, 2, ifelse(order<=3, 3, ifelse(order<=4, 4, ifelse(order<=5, 5, ifelse(order<=6, 6, ifelse(order<=7,7,8)))))))) %>%
  ungroup() %>% 
  mutate(woba_evnt = ifelse(EVENT_CD==14, 0.72, ifelse(EVENT_CD==16, 0.75, ifelse(H_FL==1, 0.90, ifelse(EVENT_CD==18, 0.92, ifelse(H_FL==2, 1.24,ifelse(H_FL==3, 1.56, ifelse(H_FL==4, 1.95, 0))))))))

# Create data set for starting Pitchers & Relievers
starter_15 <- select(MLB_2015_A, GAME_ID, PIT_ID, order)  %>%
  filter(order <  0.2) %>% subset(select=-order)

# Event level data set of only starters
MLB2015_A_s_A <- merge(MLB_2015_A, starter_15, by=c("GAME_ID", "PIT_ID"))



# 2016 YEAR 2016 YEAR
MLB_2016_A <- select(MLB_2016, X1, GAME_ID, AWAY_TEAM_ID, INN_CT, BAT_HOME_ID, OUTS_CT, PITCH_SEQ_TX, BAT_ID, BAT_HAND_CD, PIT_ID, PIT_HAND_CD, EVENT_TX, PH_FL, BAT_FLD_CD, BAT_LINEUP_ID, EVENT_CD, BAT_EVENT_FL, AB_FL, H_FL, EVENT_OUTS_CT, RBI_CT, RUNS, Runs.Inning) %>%
  filter(BAT_EVENT_FL==TRUE) %>%
  mutate(pitch_ct_bat = str_length(PITCH_SEQ_TX))  %>%
  mutate(HOME_TEAM_ID=substr(GAME_ID, 1,3), HIT_binary=ifelse(H_FL==0, 0, 1))  %>%
  mutate(HOME_LEAG = ifelse(HOME_TEAM_ID=="ATL", "NL", ifelse(HOME_TEAM_ID=="FLO", "NL", ifelse(HOME_TEAM_ID=="NYN", "NL", ifelse(HOME_TEAM_ID=="PHI", "NL", ifelse(HOME_TEAM_ID=="WAS", "NL", ifelse(HOME_TEAM_ID=="CHN", "NL", ifelse(HOME_TEAM_ID=="CIN", "NL", ifelse(HOME_TEAM_ID=="MIL", "NL", ifelse(HOME_TEAM_ID=="PIT", "NL", ifelse(HOME_TEAM_ID=="SLN", "NL", ifelse(HOME_TEAM_ID=="ARI", "NL", ifelse(HOME_TEAM_ID=="SDN", "NL", ifelse(HOME_TEAM_ID=="COL", "NL", ifelse(HOME_TEAM_ID=="SFN", "NL", ifelse(HOME_TEAM_ID=="LAN", "NL", "AL")))))))))))))))) %>%
  mutate(AWAY_LEAG = ifelse(AWAY_TEAM_ID=="ATL", "NL", ifelse(AWAY_TEAM_ID=="FLO", "NL", ifelse(AWAY_TEAM_ID=="NYN", "NL", ifelse(AWAY_TEAM_ID=="PHI", "NL", ifelse(AWAY_TEAM_ID=="WAS", "NL", ifelse(AWAY_TEAM_ID=="", "NL", ifelse(AWAY_TEAM_ID=="CIN", "NL", ifelse(AWAY_TEAM_ID=="MIL", "NL", ifelse(AWAY_TEAM_ID=="PIT", "NL", ifelse(AWAY_TEAM_ID=="SLN", "NL", ifelse(AWAY_TEAM_ID=="ARI", "NL", ifelse(AWAY_TEAM_ID=="SDN", "NL", ifelse(AWAY_TEAM_ID=="COL", "NL", ifelse(AWAY_TEAM_ID=="SFN", "NL", ifelse(AWAY_TEAM_ID=="LAN", "NL", "AL")))))))))))))))) %>%
  mutate(HOME_DIV = ifelse(HOME_TEAM_ID=="ATL", "NL_East", ifelse(HOME_TEAM_ID=="FLO", "NL_East", ifelse(HOME_TEAM_ID=="NYN", "NL_East", ifelse(HOME_TEAM_ID=="PHI", "NL_East", ifelse(HOME_TEAM_ID=="WAS", "NL_East", ifelse(HOME_TEAM_ID=="CHN", "NL_CNTRL", ifelse(HOME_TEAM_ID=="CIN", "NL_CNTRL", ifelse(HOME_TEAM_ID=="MIL", "NL_CNTRL", ifelse(HOME_TEAM_ID=="PIT", "NL_CNTRL", ifelse(HOME_TEAM_ID=="SLN", "NL_CNTRL", ifelse(HOME_TEAM_ID=="ARI", "NL_WEST", ifelse(HOME_TEAM_ID=="SDN", "NL_WEST", ifelse(HOME_TEAM_ID=="COL", "NL_WEST", ifelse(HOME_TEAM_ID=="SFN", "NL_WEST", ifelse(HOME_TEAM_ID=="LAN", "NL_WEST", ifelse(HOME_TEAM_ID=="BAL", "AL_East", ifelse(HOME_TEAM_ID=="BOS", "AL_East", ifelse(HOME_TEAM_ID=="NYA", "AL_East", ifelse(HOME_TEAM_ID=="TOR", "AL_East", ifelse(HOME_TEAM_ID=="TBA", "AL_East", ifelse(HOME_TEAM_ID=="CHA", "AL_CNTRL", ifelse(HOME_TEAM_ID=="CLE", "AL_CNTRL", ifelse(HOME_TEAM_ID=="DET", "AL_CNTRL", ifelse(HOME_TEAM_ID=="KCA", "AL_CNTRL", ifelse(HOME_TEAM_ID=="MIN", "AL_CNTRL", ifelse(HOME_TEAM_ID=="ANA", "AL_WEST", ifelse(HOME_TEAM_ID=="HOU", "AL_WEST", ifelse(HOME_TEAM_ID=="OAK", "AL_WEST", ifelse(HOME_TEAM_ID=="SEA", "AL_WEST", ifelse(HOME_TEAM_ID=="SFN", "AL_WEST", "AL_WEST"))))))))))))))))))))))))))))))) %>%
  mutate(AWAY_DIV = ifelse(AWAY_TEAM_ID=="ATL", "NL_East", ifelse(AWAY_TEAM_ID=="FLO", "NL_East", ifelse(AWAY_TEAM_ID=="NYN", "NL_East", ifelse(AWAY_TEAM_ID=="PHI", "NL_East", ifelse(AWAY_TEAM_ID=="WAS", "NL_East", ifelse(AWAY_TEAM_ID=="CHN", "NL_CNTRL", ifelse(AWAY_TEAM_ID=="CIN", "NL_CNTRL", ifelse(AWAY_TEAM_ID=="MIL", "NL_CNTRL", ifelse(AWAY_TEAM_ID=="PIT", "NL_CNTRL", ifelse(AWAY_TEAM_ID=="SLN", "NL_CNTRL", ifelse(AWAY_TEAM_ID=="ARI", "NL_WEST", ifelse(AWAY_TEAM_ID=="SDN", "NL_WEST", ifelse(AWAY_TEAM_ID=="COL", "NL_WEST", ifelse(AWAY_TEAM_ID=="SFN", "NL_WEST", ifelse(AWAY_TEAM_ID=="LAN", "NL_WEST", ifelse(AWAY_TEAM_ID=="BAL", "AL_East", ifelse(AWAY_TEAM_ID=="BOS", "AL_East", ifelse(AWAY_TEAM_ID=="NYA", "AL_East", ifelse(AWAY_TEAM_ID=="TOR", "AL_East", ifelse(AWAY_TEAM_ID=="TBA", "AL_East", ifelse(AWAY_TEAM_ID=="CHA", "AL_CNTRL", ifelse(AWAY_TEAM_ID=="CLE", "AL_CNTRL", ifelse(AWAY_TEAM_ID=="DET", "AL_CNTRL", ifelse(AWAY_TEAM_ID=="KCA", "AL_CNTRL", ifelse(AWAY_TEAM_ID=="MIN", "AL_CNTRL", ifelse(AWAY_TEAM_ID=="ANA", "AL_WEST", ifelse(AWAY_TEAM_ID=="HOU", "AL_WEST", ifelse(AWAY_TEAM_ID=="OAK", "AL_WEST", ifelse(AWAY_TEAM_ID=="SEA", "AL_WEST", ifelse(AWAY_TEAM_ID=="SFN", "AL_WEST", "AL_WEST"))))))))))))))))))))))))))))))) %>%
  mutate(TeamMatch = ifelse(AWAY_DIV==HOME_DIV, "IntraDIV", ifelse(AWAY_LEAG==HOME_LEAG, "IntraLEAG", "InterLEAG"))) %>%
  mutate(YEAR=2016) %>%
  group_by(GAME_ID, BAT_HOME_ID) %>%
  mutate(total_batter=1:n(), pitch_count_p=cumsum(pitch_ct_bat), order=total_batter/9, order_ct=ifelse(order<=1, 1, ifelse(order<=2, 2, ifelse(order<=3, 3, ifelse(order<=4, 4, ifelse(order<=5, 5, ifelse(order<=6, 6, ifelse(order<=7,7,8)))))))) %>%
  ungroup() %>% 
  mutate(woba_evnt = ifelse(EVENT_CD==14, 0.72, ifelse(EVENT_CD==16, 0.75, ifelse(H_FL==1, 0.90, ifelse(EVENT_CD==18, 0.92, ifelse(H_FL==2, 1.24,ifelse(H_FL==3, 1.56, ifelse(H_FL==4, 1.95, 0))))))))

# Create data set for starting Pitchers & Relievers
starter_16 <- select(MLB_2016_A, GAME_ID, PIT_ID, order)  %>%
  filter(order <  0.2) %>% subset(select=-order)

# Event level data set of only starters
MLB2016_A_s_A <- merge(MLB_2016_A, starter_16, by=c("GAME_ID", "PIT_ID"))



# 2017 YEAR 2017 YEAR
MLB_2017_A <- select(MLB_2017, X1, GAME_ID, AWAY_TEAM_ID, INN_CT, BAT_HOME_ID, OUTS_CT, PITCH_SEQ_TX, BAT_ID, BAT_HAND_CD, PIT_ID, PIT_HAND_CD, EVENT_TX, PH_FL, BAT_FLD_CD, BAT_LINEUP_ID, EVENT_CD, BAT_EVENT_FL, AB_FL, H_FL, EVENT_OUTS_CT, RBI_CT, RUNS, Runs.Inning) %>%
  filter(BAT_EVENT_FL==TRUE) %>%
  mutate(pitch_ct_bat = str_length(PITCH_SEQ_TX))  %>%
  mutate(HOME_TEAM_ID=substr(GAME_ID, 1,3), HIT_binary=ifelse(H_FL==0, 0, 1))  %>%
  mutate(HOME_LEAG = ifelse(HOME_TEAM_ID=="ATL", "NL", ifelse(HOME_TEAM_ID=="FLO", "NL", ifelse(HOME_TEAM_ID=="NYN", "NL", ifelse(HOME_TEAM_ID=="PHI", "NL", ifelse(HOME_TEAM_ID=="WAS", "NL", ifelse(HOME_TEAM_ID=="CHN", "NL", ifelse(HOME_TEAM_ID=="CIN", "NL", ifelse(HOME_TEAM_ID=="MIL", "NL", ifelse(HOME_TEAM_ID=="PIT", "NL", ifelse(HOME_TEAM_ID=="SLN", "NL", ifelse(HOME_TEAM_ID=="ARI", "NL", ifelse(HOME_TEAM_ID=="SDN", "NL", ifelse(HOME_TEAM_ID=="COL", "NL", ifelse(HOME_TEAM_ID=="SFN", "NL", ifelse(HOME_TEAM_ID=="LAN", "NL", "AL")))))))))))))))) %>%
  mutate(AWAY_LEAG = ifelse(AWAY_TEAM_ID=="ATL", "NL", ifelse(AWAY_TEAM_ID=="FLO", "NL", ifelse(AWAY_TEAM_ID=="NYN", "NL", ifelse(AWAY_TEAM_ID=="PHI", "NL", ifelse(AWAY_TEAM_ID=="WAS", "NL", ifelse(AWAY_TEAM_ID=="", "NL", ifelse(AWAY_TEAM_ID=="CIN", "NL", ifelse(AWAY_TEAM_ID=="MIL", "NL", ifelse(AWAY_TEAM_ID=="PIT", "NL", ifelse(AWAY_TEAM_ID=="SLN", "NL", ifelse(AWAY_TEAM_ID=="ARI", "NL", ifelse(AWAY_TEAM_ID=="SDN", "NL", ifelse(AWAY_TEAM_ID=="COL", "NL", ifelse(AWAY_TEAM_ID=="SFN", "NL", ifelse(AWAY_TEAM_ID=="LAN", "NL", "AL")))))))))))))))) %>%
  mutate(HOME_DIV = ifelse(HOME_TEAM_ID=="ATL", "NL_East", ifelse(HOME_TEAM_ID=="FLO", "NL_East", ifelse(HOME_TEAM_ID=="NYN", "NL_East", ifelse(HOME_TEAM_ID=="PHI", "NL_East", ifelse(HOME_TEAM_ID=="WAS", "NL_East", ifelse(HOME_TEAM_ID=="CHN", "NL_CNTRL", ifelse(HOME_TEAM_ID=="CIN", "NL_CNTRL", ifelse(HOME_TEAM_ID=="MIL", "NL_CNTRL", ifelse(HOME_TEAM_ID=="PIT", "NL_CNTRL", ifelse(HOME_TEAM_ID=="SLN", "NL_CNTRL", ifelse(HOME_TEAM_ID=="ARI", "NL_WEST", ifelse(HOME_TEAM_ID=="SDN", "NL_WEST", ifelse(HOME_TEAM_ID=="COL", "NL_WEST", ifelse(HOME_TEAM_ID=="SFN", "NL_WEST", ifelse(HOME_TEAM_ID=="LAN", "NL_WEST", ifelse(HOME_TEAM_ID=="BAL", "AL_East", ifelse(HOME_TEAM_ID=="BOS", "AL_East", ifelse(HOME_TEAM_ID=="NYA", "AL_East", ifelse(HOME_TEAM_ID=="TOR", "AL_East", ifelse(HOME_TEAM_ID=="TBA", "AL_East", ifelse(HOME_TEAM_ID=="CHA", "AL_CNTRL", ifelse(HOME_TEAM_ID=="CLE", "AL_CNTRL", ifelse(HOME_TEAM_ID=="DET", "AL_CNTRL", ifelse(HOME_TEAM_ID=="KCA", "AL_CNTRL", ifelse(HOME_TEAM_ID=="MIN", "AL_CNTRL", ifelse(HOME_TEAM_ID=="ANA", "AL_WEST", ifelse(HOME_TEAM_ID=="HOU", "AL_WEST", ifelse(HOME_TEAM_ID=="OAK", "AL_WEST", ifelse(HOME_TEAM_ID=="SEA", "AL_WEST", ifelse(HOME_TEAM_ID=="SFN", "AL_WEST", "AL_WEST"))))))))))))))))))))))))))))))) %>%
  mutate(AWAY_DIV = ifelse(AWAY_TEAM_ID=="ATL", "NL_East", ifelse(AWAY_TEAM_ID=="FLO", "NL_East", ifelse(AWAY_TEAM_ID=="NYN", "NL_East", ifelse(AWAY_TEAM_ID=="PHI", "NL_East", ifelse(AWAY_TEAM_ID=="WAS", "NL_East", ifelse(AWAY_TEAM_ID=="CHN", "NL_CNTRL", ifelse(AWAY_TEAM_ID=="CIN", "NL_CNTRL", ifelse(AWAY_TEAM_ID=="MIL", "NL_CNTRL", ifelse(AWAY_TEAM_ID=="PIT", "NL_CNTRL", ifelse(AWAY_TEAM_ID=="SLN", "NL_CNTRL", ifelse(AWAY_TEAM_ID=="ARI", "NL_WEST", ifelse(AWAY_TEAM_ID=="SDN", "NL_WEST", ifelse(AWAY_TEAM_ID=="COL", "NL_WEST", ifelse(AWAY_TEAM_ID=="SFN", "NL_WEST", ifelse(AWAY_TEAM_ID=="LAN", "NL_WEST", ifelse(AWAY_TEAM_ID=="BAL", "AL_East", ifelse(AWAY_TEAM_ID=="BOS", "AL_East", ifelse(AWAY_TEAM_ID=="NYA", "AL_East", ifelse(AWAY_TEAM_ID=="TOR", "AL_East", ifelse(AWAY_TEAM_ID=="TBA", "AL_East", ifelse(AWAY_TEAM_ID=="CHA", "AL_CNTRL", ifelse(AWAY_TEAM_ID=="CLE", "AL_CNTRL", ifelse(AWAY_TEAM_ID=="DET", "AL_CNTRL", ifelse(AWAY_TEAM_ID=="KCA", "AL_CNTRL", ifelse(AWAY_TEAM_ID=="MIN", "AL_CNTRL", ifelse(AWAY_TEAM_ID=="ANA", "AL_WEST", ifelse(AWAY_TEAM_ID=="HOU", "AL_WEST", ifelse(AWAY_TEAM_ID=="OAK", "AL_WEST", ifelse(AWAY_TEAM_ID=="SEA", "AL_WEST", ifelse(AWAY_TEAM_ID=="SFN", "AL_WEST", "AL_WEST"))))))))))))))))))))))))))))))) %>%
  mutate(TeamMatch = ifelse(AWAY_DIV==HOME_DIV, "IntraDIV", ifelse(AWAY_LEAG==HOME_LEAG, "IntraLEAG", "InterLEAG"))) %>%
  mutate(YEAR=2017) %>%
  group_by(GAME_ID, BAT_HOME_ID) %>%
  mutate(total_batter=1:n(), pitch_count_p=cumsum(pitch_ct_bat), order=total_batter/9, order_ct=ifelse(order<=1, 1, ifelse(order<=2, 2, ifelse(order<=3, 3, ifelse(order<=4, 4, ifelse(order<=5, 5, ifelse(order<=6, 6, ifelse(order<=7,7,8)))))))) %>%
  ungroup() %>% 
  mutate(woba_evnt = ifelse(EVENT_CD==14, 0.72, ifelse(EVENT_CD==16, 0.75, ifelse(H_FL==1, 0.90, ifelse(EVENT_CD==18, 0.92, ifelse(H_FL==2, 1.24,ifelse(H_FL==3, 1.56, ifelse(H_FL==4, 1.95, 0))))))))

# Create data set for starting Pitchers & Relievers
starter_17 <- select(MLB_2017_A, GAME_ID, PIT_ID, order)  %>%
  filter(order <  0.2) %>% subset(select=-order)

# Event level data set of only starters
MLB2017_A_s_A <- merge(MLB_2017_A, starter_17, by=c("GAME_ID", "PIT_ID"))


# 2018 YEAR 2018 YEAR
MLB_2018_A <- select(MLB_2018, X1, GAME_ID, AWAY_TEAM_ID, INN_CT, BAT_HOME_ID, OUTS_CT, PITCH_SEQ_TX, BAT_ID, BAT_HAND_CD, PIT_ID, PIT_HAND_CD, EVENT_TX, PH_FL, BAT_FLD_CD, BAT_LINEUP_ID, EVENT_CD, BAT_EVENT_FL, AB_FL, H_FL, EVENT_OUTS_CT, RBI_CT, RUNS, Runs.Inning) %>%
  filter(BAT_EVENT_FL==TRUE) %>%
  mutate(pitch_ct_bat = str_length(PITCH_SEQ_TX))  %>%
  mutate(HOME_TEAM_ID=substr(GAME_ID, 1,3), HIT_binary=ifelse(H_FL==0, 0, 1))  %>%
  mutate(HOME_LEAG = ifelse(HOME_TEAM_ID=="ATL", "NL", ifelse(HOME_TEAM_ID=="FLO", "NL", ifelse(HOME_TEAM_ID=="NYN", "NL", ifelse(HOME_TEAM_ID=="PHI", "NL", ifelse(HOME_TEAM_ID=="WAS", "NL", ifelse(HOME_TEAM_ID=="CHN", "NL", ifelse(HOME_TEAM_ID=="CIN", "NL", ifelse(HOME_TEAM_ID=="MIL", "NL", ifelse(HOME_TEAM_ID=="PIT", "NL", ifelse(HOME_TEAM_ID=="SLN", "NL", ifelse(HOME_TEAM_ID=="ARI", "NL", ifelse(HOME_TEAM_ID=="SDN", "NL", ifelse(HOME_TEAM_ID=="COL", "NL", ifelse(HOME_TEAM_ID=="SFN", "NL", ifelse(HOME_TEAM_ID=="LAN", "NL", "AL")))))))))))))))) %>%
  mutate(AWAY_LEAG = ifelse(AWAY_TEAM_ID=="ATL", "NL", ifelse(AWAY_TEAM_ID=="FLO", "NL", ifelse(AWAY_TEAM_ID=="NYN", "NL", ifelse(AWAY_TEAM_ID=="PHI", "NL", ifelse(AWAY_TEAM_ID=="WAS", "NL", ifelse(AWAY_TEAM_ID=="", "NL", ifelse(AWAY_TEAM_ID=="CIN", "NL", ifelse(AWAY_TEAM_ID=="MIL", "NL", ifelse(AWAY_TEAM_ID=="PIT", "NL", ifelse(AWAY_TEAM_ID=="SLN", "NL", ifelse(AWAY_TEAM_ID=="ARI", "NL", ifelse(AWAY_TEAM_ID=="SDN", "NL", ifelse(AWAY_TEAM_ID=="COL", "NL", ifelse(AWAY_TEAM_ID=="SFN", "NL", ifelse(AWAY_TEAM_ID=="LAN", "NL", "AL")))))))))))))))) %>%
  mutate(HOME_DIV = ifelse(HOME_TEAM_ID=="ATL", "NL_East", ifelse(HOME_TEAM_ID=="FLO", "NL_East", ifelse(HOME_TEAM_ID=="NYN", "NL_East", ifelse(HOME_TEAM_ID=="PHI", "NL_East", ifelse(HOME_TEAM_ID=="WAS", "NL_East", ifelse(HOME_TEAM_ID=="CHN", "NL_CNTRL", ifelse(HOME_TEAM_ID=="CIN", "NL_CNTRL", ifelse(HOME_TEAM_ID=="MIL", "NL_CNTRL", ifelse(HOME_TEAM_ID=="PIT", "NL_CNTRL", ifelse(HOME_TEAM_ID=="SLN", "NL_CNTRL", ifelse(HOME_TEAM_ID=="ARI", "NL_WEST", ifelse(HOME_TEAM_ID=="SDN", "NL_WEST", ifelse(HOME_TEAM_ID=="COL", "NL_WEST", ifelse(HOME_TEAM_ID=="SFN", "NL_WEST", ifelse(HOME_TEAM_ID=="LAN", "NL_WEST", ifelse(HOME_TEAM_ID=="BAL", "AL_East", ifelse(HOME_TEAM_ID=="BOS", "AL_East", ifelse(HOME_TEAM_ID=="NYA", "AL_East", ifelse(HOME_TEAM_ID=="TOR", "AL_East", ifelse(HOME_TEAM_ID=="TBA", "AL_East", ifelse(HOME_TEAM_ID=="CHA", "AL_CNTRL", ifelse(HOME_TEAM_ID=="CLE", "AL_CNTRL", ifelse(HOME_TEAM_ID=="DET", "AL_CNTRL", ifelse(HOME_TEAM_ID=="KCA", "AL_CNTRL", ifelse(HOME_TEAM_ID=="MIN", "AL_CNTRL", ifelse(HOME_TEAM_ID=="ANA", "AL_WEST", ifelse(HOME_TEAM_ID=="HOU", "AL_WEST", ifelse(HOME_TEAM_ID=="OAK", "AL_WEST", ifelse(HOME_TEAM_ID=="SEA", "AL_WEST", ifelse(HOME_TEAM_ID=="SFN", "AL_WEST", "AL_WEST"))))))))))))))))))))))))))))))) %>%
  mutate(AWAY_DIV = ifelse(AWAY_TEAM_ID=="ATL", "NL_East", ifelse(AWAY_TEAM_ID=="FLO", "NL_East", ifelse(AWAY_TEAM_ID=="NYN", "NL_East", ifelse(AWAY_TEAM_ID=="PHI", "NL_East", ifelse(AWAY_TEAM_ID=="WAS", "NL_East", ifelse(AWAY_TEAM_ID=="CHN", "NL_CNTRL", ifelse(AWAY_TEAM_ID=="CIN", "NL_CNTRL", ifelse(AWAY_TEAM_ID=="MIL", "NL_CNTRL", ifelse(AWAY_TEAM_ID=="PIT", "NL_CNTRL", ifelse(AWAY_TEAM_ID=="SLN", "NL_CNTRL", ifelse(AWAY_TEAM_ID=="ARI", "NL_WEST", ifelse(AWAY_TEAM_ID=="SDN", "NL_WEST", ifelse(AWAY_TEAM_ID=="COL", "NL_WEST", ifelse(AWAY_TEAM_ID=="SFN", "NL_WEST", ifelse(AWAY_TEAM_ID=="LAN", "NL_WEST", ifelse(AWAY_TEAM_ID=="BAL", "AL_East", ifelse(AWAY_TEAM_ID=="BOS", "AL_East", ifelse(AWAY_TEAM_ID=="NYA", "AL_East", ifelse(AWAY_TEAM_ID=="TOR", "AL_East", ifelse(AWAY_TEAM_ID=="TBA", "AL_East", ifelse(AWAY_TEAM_ID=="CHA", "AL_CNTRL", ifelse(AWAY_TEAM_ID=="CLE", "AL_CNTRL", ifelse(AWAY_TEAM_ID=="DET", "AL_CNTRL", ifelse(AWAY_TEAM_ID=="KCA", "AL_CNTRL", ifelse(AWAY_TEAM_ID=="MIN", "AL_CNTRL", ifelse(AWAY_TEAM_ID=="ANA", "AL_WEST", ifelse(AWAY_TEAM_ID=="HOU", "AL_WEST", ifelse(AWAY_TEAM_ID=="OAK", "AL_WEST", ifelse(AWAY_TEAM_ID=="SEA", "AL_WEST", ifelse(AWAY_TEAM_ID=="SFN", "AL_WEST", "AL_WEST"))))))))))))))))))))))))))))))) %>%
  mutate(TeamMatch = ifelse(AWAY_DIV==HOME_DIV, "IntraDIV", ifelse(AWAY_LEAG==HOME_LEAG, "IntraLEAG", "InterLEAG"))) %>%
  mutate(YEAR=2018) %>%
  group_by(GAME_ID, BAT_HOME_ID) %>%
  mutate(total_batter=1:n(), pitch_count_p=cumsum(pitch_ct_bat), order=total_batter/9, order_ct=ifelse(order<=1, 1, ifelse(order<=2, 2, ifelse(order<=3, 3, ifelse(order<=4, 4, ifelse(order<=5, 5, ifelse(order<=6, 6, ifelse(order<=7,7,8)))))))) %>%
  ungroup() %>% 
  mutate(woba_evnt = ifelse(EVENT_CD==14, 0.72, ifelse(EVENT_CD==16, 0.75, ifelse(H_FL==1, 0.90, ifelse(EVENT_CD==18, 0.92, ifelse(H_FL==2, 1.24,ifelse(H_FL==3, 1.56, ifelse(H_FL==4, 1.95, 0))))))))

# Create data set for starting Pitchers & Relievers
starter_18 <- select(MLB_2018_A, GAME_ID, PIT_ID, order)  %>%
  filter(order <  0.2) %>% subset(select=-order)

# Event level data set of only starters
MLB2018_A_s_A <- merge(MLB_2018_A, starter_18, by=c("GAME_ID", "PIT_ID"))



# 2019 YEAR 2019 YEAR
MLB_2019_A <- select(MLB_2019, X1, GAME_ID, AWAY_TEAM_ID, INN_CT, BAT_HOME_ID, OUTS_CT, PITCH_SEQ_TX, BAT_ID, BAT_HAND_CD, PIT_ID, PIT_HAND_CD, EVENT_TX, PH_FL, BAT_FLD_CD, BAT_LINEUP_ID, EVENT_CD, BAT_EVENT_FL, AB_FL, H_FL, EVENT_OUTS_CT, RBI_CT, RUNS, Runs.Inning) %>%
  filter(BAT_EVENT_FL==TRUE) %>%
  mutate(pitch_ct_bat = str_length(PITCH_SEQ_TX))  %>%
  mutate(HOME_TEAM_ID=substr(GAME_ID, 1,3), HIT_binary=ifelse(H_FL==0, 0, 1))  %>%
  mutate(HOME_LEAG = ifelse(HOME_TEAM_ID=="ATL", "NL", ifelse(HOME_TEAM_ID=="FLO", "NL", ifelse(HOME_TEAM_ID=="NYN", "NL", ifelse(HOME_TEAM_ID=="PHI", "NL", ifelse(HOME_TEAM_ID=="WAS", "NL", ifelse(HOME_TEAM_ID=="CHN", "NL", ifelse(HOME_TEAM_ID=="CIN", "NL", ifelse(HOME_TEAM_ID=="MIL", "NL", ifelse(HOME_TEAM_ID=="PIT", "NL", ifelse(HOME_TEAM_ID=="SLN", "NL", ifelse(HOME_TEAM_ID=="ARI", "NL", ifelse(HOME_TEAM_ID=="SDN", "NL", ifelse(HOME_TEAM_ID=="COL", "NL", ifelse(HOME_TEAM_ID=="SFN", "NL", ifelse(HOME_TEAM_ID=="LAN", "NL", "AL")))))))))))))))) %>%
  mutate(AWAY_LEAG = ifelse(AWAY_TEAM_ID=="ATL", "NL", ifelse(AWAY_TEAM_ID=="FLO", "NL", ifelse(AWAY_TEAM_ID=="NYN", "NL", ifelse(AWAY_TEAM_ID=="PHI", "NL", ifelse(AWAY_TEAM_ID=="WAS", "NL", ifelse(AWAY_TEAM_ID=="", "NL", ifelse(AWAY_TEAM_ID=="CIN", "NL", ifelse(AWAY_TEAM_ID=="MIL", "NL", ifelse(AWAY_TEAM_ID=="PIT", "NL", ifelse(AWAY_TEAM_ID=="SLN", "NL", ifelse(AWAY_TEAM_ID=="ARI", "NL", ifelse(AWAY_TEAM_ID=="SDN", "NL", ifelse(AWAY_TEAM_ID=="COL", "NL", ifelse(AWAY_TEAM_ID=="SFN", "NL", ifelse(AWAY_TEAM_ID=="LAN", "NL", "AL")))))))))))))))) %>%
  mutate(HOME_DIV = ifelse(HOME_TEAM_ID=="ATL", "NL_East", ifelse(HOME_TEAM_ID=="FLO", "NL_East", ifelse(HOME_TEAM_ID=="NYN", "NL_East", ifelse(HOME_TEAM_ID=="PHI", "NL_East", ifelse(HOME_TEAM_ID=="WAS", "NL_East", ifelse(HOME_TEAM_ID=="CHN", "NL_CNTRL", ifelse(HOME_TEAM_ID=="CIN", "NL_CNTRL", ifelse(HOME_TEAM_ID=="MIL", "NL_CNTRL", ifelse(HOME_TEAM_ID=="PIT", "NL_CNTRL", ifelse(HOME_TEAM_ID=="SLN", "NL_CNTRL", ifelse(HOME_TEAM_ID=="ARI", "NL_WEST", ifelse(HOME_TEAM_ID=="SDN", "NL_WEST", ifelse(HOME_TEAM_ID=="COL", "NL_WEST", ifelse(HOME_TEAM_ID=="SFN", "NL_WEST", ifelse(HOME_TEAM_ID=="LAN", "NL_WEST", ifelse(HOME_TEAM_ID=="BAL", "AL_East", ifelse(HOME_TEAM_ID=="BOS", "AL_East", ifelse(HOME_TEAM_ID=="NYA", "AL_East", ifelse(HOME_TEAM_ID=="TOR", "AL_East", ifelse(HOME_TEAM_ID=="TBA", "AL_East", ifelse(HOME_TEAM_ID=="CHA", "AL_CNTRL", ifelse(HOME_TEAM_ID=="CLE", "AL_CNTRL", ifelse(HOME_TEAM_ID=="DET", "AL_CNTRL", ifelse(HOME_TEAM_ID=="KCA", "AL_CNTRL", ifelse(HOME_TEAM_ID=="MIN", "AL_CNTRL", ifelse(HOME_TEAM_ID=="ANA", "AL_WEST", ifelse(HOME_TEAM_ID=="HOU", "AL_WEST", ifelse(HOME_TEAM_ID=="OAK", "AL_WEST", ifelse(HOME_TEAM_ID=="SEA", "AL_WEST", ifelse(HOME_TEAM_ID=="SFN", "AL_WEST", "AL_WEST"))))))))))))))))))))))))))))))) %>%
  mutate(AWAY_DIV = ifelse(AWAY_TEAM_ID=="ATL", "NL_East", ifelse(AWAY_TEAM_ID=="FLO", "NL_East", ifelse(AWAY_TEAM_ID=="NYN", "NL_East", ifelse(AWAY_TEAM_ID=="PHI", "NL_East", ifelse(AWAY_TEAM_ID=="WAS", "NL_East", ifelse(AWAY_TEAM_ID=="CHN", "NL_CNTRL", ifelse(AWAY_TEAM_ID=="CIN", "NL_CNTRL", ifelse(AWAY_TEAM_ID=="MIL", "NL_CNTRL", ifelse(AWAY_TEAM_ID=="PIT", "NL_CNTRL", ifelse(AWAY_TEAM_ID=="SLN", "NL_CNTRL", ifelse(AWAY_TEAM_ID=="ARI", "NL_WEST", ifelse(AWAY_TEAM_ID=="SDN", "NL_WEST", ifelse(AWAY_TEAM_ID=="COL", "NL_WEST", ifelse(AWAY_TEAM_ID=="SFN", "NL_WEST", ifelse(AWAY_TEAM_ID=="LAN", "NL_WEST", ifelse(AWAY_TEAM_ID=="BAL", "AL_East", ifelse(AWAY_TEAM_ID=="BOS", "AL_East", ifelse(AWAY_TEAM_ID=="NYA", "AL_East", ifelse(AWAY_TEAM_ID=="TOR", "AL_East", ifelse(AWAY_TEAM_ID=="TBA", "AL_East", ifelse(AWAY_TEAM_ID=="CHA", "AL_CNTRL", ifelse(AWAY_TEAM_ID=="CLE", "AL_CNTRL", ifelse(AWAY_TEAM_ID=="DET", "AL_CNTRL", ifelse(AWAY_TEAM_ID=="KCA", "AL_CNTRL", ifelse(AWAY_TEAM_ID=="MIN", "AL_CNTRL", ifelse(AWAY_TEAM_ID=="ANA", "AL_WEST", ifelse(AWAY_TEAM_ID=="HOU", "AL_WEST", ifelse(AWAY_TEAM_ID=="OAK", "AL_WEST", ifelse(AWAY_TEAM_ID=="SEA", "AL_WEST", ifelse(AWAY_TEAM_ID=="SFN", "AL_WEST", "AL_WEST"))))))))))))))))))))))))))))))) %>%
  mutate(TeamMatch = ifelse(AWAY_DIV==HOME_DIV, "IntraDIV", ifelse(AWAY_LEAG==HOME_LEAG, "IntraLEAG", "InterLEAG"))) %>%
  mutate(YEAR=2019) %>%
  group_by(GAME_ID, BAT_HOME_ID) %>%
  mutate(total_batter=1:n(), pitch_count_p=cumsum(pitch_ct_bat), order=total_batter/9, order_ct=ifelse(order<=1, 1, ifelse(order<=2, 2, ifelse(order<=3, 3, ifelse(order<=4, 4, ifelse(order<=5, 5, ifelse(order<=6, 6, ifelse(order<=7,7,8)))))))) %>%
  ungroup() %>% 
  mutate(woba_evnt = ifelse(EVENT_CD==14, 0.72, ifelse(EVENT_CD==16, 0.75, ifelse(H_FL==1, 0.90, ifelse(EVENT_CD==18, 0.92, ifelse(H_FL==2, 1.24,ifelse(H_FL==3, 1.56, ifelse(H_FL==4, 1.95, 0))))))))

# Create data set for starting Pitchers & Relievers
starter_19 <- select(MLB_2019_A, GAME_ID, PIT_ID, order)  %>%
  filter(order <  0.2) %>% subset(select=-order)

# Event level data set of only starters
MLB2019_A_s_A <- merge(MLB_2019_A, starter_19, by=c("GAME_ID", "PIT_ID"))


# CREATE ALL 10 YEARS FILE
# ALL 10 YEARS  # ALL 10 YEARS  # ALL 10 YEARS
Starter_TTO_10_19 <- rbind(MLB2010_A_s_A, MLB2011_A_s_A , MLB2012_A_s_A , MLB2013_A_s_A, MLB2014_A_s_A, MLB2015_A_s_A, MLB2016_A_s_A , MLB2017_A_s_A , MLB2018_A_s_A, MLB2019_A_s_A)

hist(Starter_TTO_10_19$YEAR)

write.csv(Starter_TTO_10_19, "Starter_TTO_10_19.csv")


# Remove Pinch Hitters
Starter_TTO_10_19_noPH <- filter(Starter_TTO_10_19, PH_FL==FALSE)


################################
##### ANALYSIS ON ALL 10 YEARS###
#################################


Starter_TTO_10_19 <- read_csv(file = "Starter_TTO_10_19.csv")

ggplot(data=Starter_TTO_10_19) +
  geom_histogram(aes(x=order_ct), binwidth = 1, color="grey") + 
  ylab("Plate apprearances") + xlab("TTO") +
  facet_wrap(~YEAR)



#### Batter - Hitter pairings ######
MatchUp <- rbind(MLB_2010, MLB_2011, MLB_2012, MLB_2013, MLB_2014, MLB_2015, MLB_2016, MLB_2017, MLB_2018, MLB_2019) %>%
  select(BAT_ID, PIT_ID) %>%
  mutate(B_P_pairs=paste(BAT_ID, PIT_ID))

Matchup_sum <-as.data.frame(table(MatchUp$B_P_pairs)) %>%
  rename(B_P_pairs = Var1)

Matchup_sum2<- arrange(Matchup_sum, Freq) %>% 
  mutate(cum=cumsum(Freq)) %>%
  mutate(cum_tot=cum/1899811)

ggplot(Matchup_sum2) +
  geom_point(aes(x=Freq, y=cum_tot)) +
  ylab("Fraction of all plate apprearances, 2010-2019") + xlab("Times a Batter - Pitcher pair has faced off")

Starter_TTO_10_19_pair <- mutate (Starter_TTO_10_19, B_P_pairs=paste(BAT_ID, PIT_ID))

Starter_TTO_10_19_pairs <- merge(Starter_TTO_10_19_pair , Matchup_sum, by="B_P_pairs")

hist(Starter_TTO_10_19_pairs$Freq, breaks=120)

MLB2013_A_s_A <- merge(MLB_2013_A, starter_13, by=c("GAME_ID", "PIT_ID"))
#### Batter - Hitter pairings ######




# PLATE APPEARANCES over the lineup by times through  

# create averages across pitchers
BL_order_starter_2019 <- group_by(MLB2019_A_s_A, order_ct, BAT_LINEUP_ID) %>%
  summarise(total_AB=sum(AB_FL), total_PA=sum(BAT_EVENT_FL), total_bases=sum(H_FL), total_hits=sum(HIT_binary), total_wOBA=sum(woba_evnt)) %>%
  mutate(ABV=total_hits/total_AB, SLG=total_bases/total_AB, wOBA=total_wOBA/total_PA, total_PA_p = total_PA/4860) %>%
  filter(order_ct<5)

BL_totalbatter_starter_2019 <- group_by(MLB2019_A_s_A, total_batter) %>%
  summarise(total_AB=sum(AB_FL), total_PA=sum(BAT_EVENT_FL), total_bases=sum(H_FL), total_hits=sum(HIT_binary), total_wOBA=sum(woba_evnt)) %>%
  mutate(ABV=total_hits/total_AB, SLG=total_bases/total_AB, wOBA=total_wOBA/total_PA, total_PA_p = total_PA/4860)


BL_order_starter_2018 <- group_by(MLB2018_A_s_A, order_ct, BAT_LINEUP_ID) %>%
  summarise(total_AB=sum(AB_FL), total_PA=sum(BAT_EVENT_FL), total_bases=sum(H_FL), total_hits=sum(HIT_binary), total_wOBA=sum(woba_evnt)) %>%
  mutate(ABV=total_hits/total_AB, SLG=total_bases/total_AB, wOBA=total_wOBA/total_PA, total_PA_p = total_PA/4860) %>%
  filter(order_ct<5)

BL_totalbatter_starter_2018 <- group_by(MLB2018_A_s_A, total_batter) %>%
  summarise(total_AB=sum(AB_FL), total_PA=sum(BAT_EVENT_FL), total_bases=sum(H_FL), total_hits=sum(HIT_binary), total_wOBA=sum(woba_evnt)) %>%
  mutate(ABV=total_hits/total_AB, SLG=total_bases/total_AB, wOBA=total_wOBA/total_PA, total_PA_p = total_PA/4860)


BL_order_starter_2017 <- group_by(MLB2017_A_s_A, order_ct, BAT_LINEUP_ID) %>%
  summarise(total_AB=sum(AB_FL), total_PA=sum(BAT_EVENT_FL), total_bases=sum(H_FL), total_hits=sum(HIT_binary), total_wOBA=sum(woba_evnt)) %>%
  mutate(ABV=total_hits/total_AB, SLG=total_bases/total_AB, wOBA=total_wOBA/total_PA, total_PA_p = total_PA/4860) %>%
  filter(order_ct<5)

BL_totalbatter_starter_2017 <- group_by(MLB2017_A_s_A, total_batter) %>%
  summarise(total_AB=sum(AB_FL), total_PA=sum(BAT_EVENT_FL), total_bases=sum(H_FL), total_hits=sum(HIT_binary), total_wOBA=sum(woba_evnt)) %>%
  mutate(ABV=total_hits/total_AB, SLG=total_bases/total_AB, wOBA=total_wOBA/total_PA, total_PA_p = total_PA/4860)


BL_order_starter_2016 <- group_by(MLB2016_A_s_A, order_ct, BAT_LINEUP_ID) %>%
  summarise(total_AB=sum(AB_FL), total_PA=sum(BAT_EVENT_FL), total_bases=sum(H_FL), total_hits=sum(HIT_binary), total_wOBA=sum(woba_evnt)) %>%
  mutate(ABV=total_hits/total_AB, SLG=total_bases/total_AB, wOBA=total_wOBA/total_PA, total_PA_p = total_PA/4860) %>%
  filter(order_ct<5)

BL_totalbatter_starter_2016 <- group_by(MLB2016_A_s_A, total_batter) %>%
  summarise(total_AB=sum(AB_FL), total_PA=sum(BAT_EVENT_FL), total_bases=sum(H_FL), total_hits=sum(HIT_binary), total_wOBA=sum(woba_evnt)) %>%
  mutate(ABV=total_hits/total_AB, SLG=total_bases/total_AB, wOBA=total_wOBA/total_PA, total_PA_p = total_PA/4860)


BL_order_starter_2015 <- group_by(MLB2015_A_s_A, order_ct, BAT_LINEUP_ID) %>%
  summarise(total_AB=sum(AB_FL), total_PA=sum(BAT_EVENT_FL), total_bases=sum(H_FL), total_hits=sum(HIT_binary), total_wOBA=sum(woba_evnt)) %>%
  mutate(ABV=total_hits/total_AB, SLG=total_bases/total_AB, wOBA=total_wOBA/total_PA, total_PA_p = total_PA/4860) %>%
  filter(order_ct<5)

BL_totalbatter_starter_2015 <- group_by(MLB2015_A_s_A, total_batter) %>%
  summarise(total_AB=sum(AB_FL), total_PA=sum(BAT_EVENT_FL), total_bases=sum(H_FL), total_hits=sum(HIT_binary), total_wOBA=sum(woba_evnt)) %>%
  mutate(ABV=total_hits/total_AB, SLG=total_bases/total_AB, wOBA=total_wOBA/total_PA, total_PA_p = total_PA/4860)


BL_order_starter_2014 <- group_by(MLB2014_A_s_A, order_ct, BAT_LINEUP_ID) %>%
  summarise(total_AB=sum(AB_FL), total_PA=sum(BAT_EVENT_FL), total_bases=sum(H_FL), total_hits=sum(HIT_binary), total_wOBA=sum(woba_evnt)) %>%
  mutate(ABV=total_hits/total_AB, SLG=total_bases/total_AB, wOBA=total_wOBA/total_PA, total_PA_p = total_PA/4860) %>%
  filter(order_ct<5)

BL_totalbatter_starter_2014 <- group_by(MLB2014_A_s_A, total_batter) %>%
  summarise(total_AB=sum(AB_FL), total_PA=sum(BAT_EVENT_FL), total_bases=sum(H_FL), total_hits=sum(HIT_binary), total_wOBA=sum(woba_evnt)) %>%
  mutate(ABV=total_hits/total_AB, SLG=total_bases/total_AB, wOBA=total_wOBA/total_PA, total_PA_p = total_PA/4860)


BL_order_starter_2013 <- group_by(MLB2013_A_s_A, order_ct, BAT_LINEUP_ID) %>%
  summarise(total_AB=sum(AB_FL), total_PA=sum(BAT_EVENT_FL), total_bases=sum(H_FL), total_hits=sum(HIT_binary), total_wOBA=sum(woba_evnt)) %>%
  mutate(ABV=total_hits/total_AB, SLG=total_bases/total_AB, wOBA=total_wOBA/total_PA, total_PA_p = total_PA/4860) %>%
  filter(order_ct<5)

BL_totalbatter_starter_2013 <- group_by(MLB2013_A_s_A, total_batter) %>%
  summarise(total_AB=sum(AB_FL), total_PA=sum(BAT_EVENT_FL), total_bases=sum(H_FL), total_hits=sum(HIT_binary), total_wOBA=sum(woba_evnt)) %>%
  mutate(ABV=total_hits/total_AB, SLG=total_bases/total_AB, wOBA=total_wOBA/total_PA, total_PA_p = total_PA/4860)


BL_order_starter_2012 <- group_by(MLB2012_A_s_A, order_ct, BAT_LINEUP_ID) %>%
  summarise(total_AB=sum(AB_FL), total_PA=sum(BAT_EVENT_FL), total_bases=sum(H_FL), total_hits=sum(HIT_binary), total_wOBA=sum(woba_evnt)) %>%
  mutate(ABV=total_hits/total_AB, SLG=total_bases/total_AB, wOBA=total_wOBA/total_PA, total_PA_p = total_PA/4860) %>%
  filter(order_ct<5)

BL_totalbatter_starter_2012 <- group_by(MLB2012_A_s_A, total_batter) %>%
  summarise(total_AB=sum(AB_FL), total_PA=sum(BAT_EVENT_FL), total_bases=sum(H_FL), total_hits=sum(HIT_binary), total_wOBA=sum(woba_evnt)) %>%
  mutate(ABV=total_hits/total_AB, SLG=total_bases/total_AB, wOBA=total_wOBA/total_PA, total_PA_p = total_PA/4860)


BL_order_starter_2011 <- group_by(MLB2011_A_s_A, order_ct, BAT_LINEUP_ID) %>%
  summarise(total_AB=sum(AB_FL), total_PA=sum(BAT_EVENT_FL), total_bases=sum(H_FL), total_hits=sum(HIT_binary), total_wOBA=sum(woba_evnt)) %>%
  mutate(ABV=total_hits/total_AB, SLG=total_bases/total_AB, wOBA=total_wOBA/total_PA, total_PA_p = total_PA/4860) %>%
  filter(order_ct<5)

BL_totalbatter_starter_2011 <- group_by(MLB2011_A_s_A, total_batter) %>%
  summarise(total_AB=sum(AB_FL), total_PA=sum(BAT_EVENT_FL), total_bases=sum(H_FL), total_hits=sum(HIT_binary), total_wOBA=sum(woba_evnt)) %>%
  mutate(ABV=total_hits/total_AB, SLG=total_bases/total_AB, wOBA=total_wOBA/total_PA, total_PA_p = total_PA/4860)


BL_order_starter_2010 <- group_by(MLB2010_A_s_A, order_ct, BAT_LINEUP_ID) %>%
  summarise(total_AB=sum(AB_FL), total_PA=sum(BAT_EVENT_FL), total_bases=sum(H_FL), total_hits=sum(HIT_binary), total_wOBA=sum(woba_evnt)) %>%
  mutate(ABV=total_hits/total_AB, SLG=total_bases/total_AB, wOBA=total_wOBA/total_PA, total_PA_p = total_PA/4860) %>%
  filter(order_ct<5)

BL_totalbatter_starter_2010 <- group_by(MLB2010_A_s_A, total_batter) %>%
  summarise(total_AB=sum(AB_FL), total_PA=sum(BAT_EVENT_FL), total_bases=sum(H_FL), total_hits=sum(HIT_binary), total_wOBA=sum(woba_evnt)) %>%
  mutate(ABV=total_hits/total_AB, SLG=total_bases/total_AB, wOBA=total_wOBA/total_PA, total_PA_p = total_PA/4860)


BL_order_starter_ALL <- group_by(Starter_TTO_10_19, order_ct, BAT_LINEUP_ID) %>%
  summarise(total_AB=sum(AB_FL), total_PA=sum(BAT_EVENT_FL), total_bases=sum(H_FL), total_hits=sum(HIT_binary), total_wOBA=sum(woba_evnt)) %>%
  mutate(ABV=total_hits/total_AB, SLG=total_bases/total_AB, wOBA=total_wOBA/total_PA, total_PA_p = total_PA/4860) %>%
  filter(order_ct<5)

BL_totalbatter_starter_ALL <- group_by(Starter_TTO_10_19, total_batter) %>%
  summarise(total_AB=sum(AB_FL), total_PA=sum(BAT_EVENT_FL), total_bases=sum(H_FL), total_hits=sum(HIT_binary), total_wOBA=sum(woba_evnt)) %>%
  mutate(ABV=total_hits/total_AB, SLG=total_bases/total_AB, wOBA=total_wOBA/total_PA, total_PA_p = total_PA/48600)



BL_order_starter_ALL_noPH <- group_by(Starter_TTO_10_19_noPH, order_ct, BAT_LINEUP_ID) %>%
  summarise(total_AB=sum(AB_FL), total_PA=sum(BAT_EVENT_FL), total_bases=sum(H_FL), total_hits=sum(HIT_binary), total_wOBA=sum(woba_evnt)) %>%
  mutate(ABV=total_hits/total_AB, SLG=total_bases/total_AB, wOBA=total_wOBA/total_PA, total_PA_p = total_PA/4860) %>%
  filter(order_ct<5)

# all 10 years
BL_totalbatter_starter_ALL_noPH <- group_by(Starter_TTO_10_19_noPH, total_batter) %>%
  summarise(total_AB=sum(AB_FL), total_PA=sum(BAT_EVENT_FL), total_bases=sum(H_FL), total_hits=sum(HIT_binary), total_wOBA=sum(woba_evnt)) %>%
  mutate(ABV=total_hits/total_AB, SLG=total_bases/total_AB, wOBA=total_wOBA/total_PA, total_PA_p = total_PA/48600) %>%
  mutate(TTO = ifelse(total_batter<=9, 1, ifelse(total_batter <=18, 2, ifelse(total_batter<=27, 3, 4))))%>%
  merge(LINEUP, by= "total_batter")

BL_order_starter_ALL_noPH_orderct <- group_by(Starter_TTO_10_19_noPH, order_ct) %>%
  summarise(total_AB=sum(AB_FL), total_PA=sum(BAT_EVENT_FL), total_bases=sum(H_FL), total_hits=sum(HIT_binary), total_wOBA=sum(woba_evnt)) %>%
  mutate(ABV=total_hits/total_AB, SLG=total_bases/total_AB, wOBA=total_wOBA/total_PA, total_PA_p = total_PA/4860) 

# all 10 years by 3 years groups
Starter_TTO_10_19_noPH_3yrs <- mutate(Starter_TTO_10_19_noPH, YEAR1_3 = ifelse(YEAR >2014, 1, ifelse(YEAR>2017, 2, 3)))
                         

BL_totalbatter_starter_ALL_noPH_3yrs <- group_by(Starter_TTO_10_19_noPH_3yrs, total_batter, YEAR1_3) %>%
  summarise(total_AB=sum(AB_FL), total_PA=sum(BAT_EVENT_FL), total_bases=sum(H_FL), total_hits=sum(HIT_binary), total_wOBA=sum(woba_evnt)) %>%
  mutate(ABV=total_hits/total_AB, SLG=total_bases/total_AB, wOBA=total_wOBA/total_PA, total_PA_p = total_PA/48600) %>%
  mutate(TTO = ifelse(total_batter<=9, 1, ifelse(total_batter <=18, 2, ifelse(total_batter<=27, 3, 4))))%>%
  merge(LINEUP, by= "total_batter")

BL_order_starter_ALL_noPH_orderct_3yrs <- group_by(Starter_TTO_10_19_noPH_3yrs, order_ct) %>%
  summarise(total_AB=sum(AB_FL), total_PA=sum(BAT_EVENT_FL), total_bases=sum(H_FL), total_hits=sum(HIT_binary), total_wOBA=sum(woba_evnt)) %>%
  mutate(ABV=total_hits/total_AB, SLG=total_bases/total_AB, wOBA=total_wOBA/total_PA) 


LINEUP <- data.frame(total_batter=c(1:36),BAT_LINEUP_ID = c((1:9), (1:9), (1:9), (1:9)))

BL_totalbatter_starter_ALL_noPH_sum <- BL_totalbatter_starter_ALL_noPH %>%  
  group_by(BAT_LINEUP_ID) %>%
  filter(TTO<4) %>%
  summarise(wOBA_ave_LineUp=mean(wOBA), SLG_ave_LineUp=mean(SLG), ABV_ave_LineUp=mean(ABV))
          

TTO_sum <- merge(BL_totalbatter_starter_ALL_noPH, BL_totalbatter_starter_ALL_noPH_sum, by="BAT_LINEUP_ID") %>%
  mutate(delta_wOBA =wOBA - wOBA_ave_LineUp, delta_ABV = ABV - ABV_ave_LineUp, delta_SLG =SLG - SLG_ave_LineUp)

TTO_sum_9 <- filter(TTO_sum, !(BAT_LINEUP_ID %in% 9))

# Figure 3 - Batter count & wOBA SLG ABV
ggplot(TTO_sum_9) +
  geom_point(aes(x=total_batter, y=delta_wOBA), size = 5) +
  geom_point(aes(x=total_batter, y=delta_ABV), size = 3, color="#FF6600", shape=15) +
  geom_point(aes(x=total_batter, y=delta_SLG), size = 3, color="#FF00CC", shape=17) +
  ylab("delta (wOBA, ABV, SLG), 2010-2019") + 
  xlab("Batter sequence") +
  scale_x_continuous(limits = c(1, 26), breaks=c(1, 5, 10, 15, 20, 25))  +
  scale_y_continuous(limits = c(-.012, .012), breaks=c(-.01, -.005, 0, 0.005, 0.01))  +
  theme_bw() + theme(panel.grid.major = element_blank(), text = element_text(size=17), legend.position="top")
0#  geom_hline(yintercept = 0, color="grey")

  
# by three 3 year epochs
BL_totalbatter_starter_ALL_noPH_sum_3yrs <- BL_totalbatter_starter_ALL_noPH_3yrs %>%  
  group_by(BAT_LINEUP_ID, YEAR1_3) %>%
  filter(TTO<4) %>%
  summarise(wOBA_ave_LineUp=mean(wOBA), SLG_ave_LineUp=mean(SLG), ABV_ave_LineUp=mean(ABV))

TTO_sum_3yrs <- merge(BL_totalbatter_starter_ALL_noPH_3yrs, BL_totalbatter_starter_ALL_noPH_sum_3yrs, by="BAT_LINEUP_ID") %>%
  mutate(delta_wOBA =wOBA - wOBA_ave_LineUp, delta_ABV = ABV - ABV_ave_LineUp, delta_SLG =SLG - SLG_ave_LineUp)

TTO_sum_9_3x_yrs <- filter(TTO_sum_3yrs, !(BAT_LINEUP_ID %in% 9))

# Figure 3 - Batter count & wOBA SLG ABV
ggplot(TTO_sum_9_3x_yrs) +
  geom_point(aes(x=total_batter, y=delta_wOBA), size = 5) +
  geom_point(aes(x=total_batter, y=delta_ABV), size = 3, color="red", shape=15) +
  geom_point(aes(x=total_batter, y=delta_SLG), size = 3, color="blue", shape=17) +
  ylab("delta wOBA, ABV, SLG (2010-2019)") + 
  xlab("Batter count") +
  scale_x_continuous(limits = c(1, 26), breaks=c(1, 5, 10, 15, 20, 25))  +
  scale_y_continuous(limits = c(-.012, .012), breaks=c(-.01, -.005, 0, 0.005, 0.01))  +
  theme_bw() + theme(panel.grid.major = element_blank(), text = element_text(size=17), legend.position="top")
#  geom_hline(yintercept = 0, color="grey")


# %%%%%%% PRIMIARY PLOT %%%%%%
# Figure 2 STARTER total PLATE APPEARANCES over the lineup by times through
ggplot(subset(BL_order_starter_2018, order_ct %in% c(1,1))) +
  ylab("Plate appearances against starters") + 
  xlab("Batter lineup") +
  geom_point(aes(x=BAT_LINEUP_ID, y= total_PA), size=3) + scale_x_continuous(limits = c(1, 9), breaks=c(1:9))  +
  geom_point(data=subset(BL_order_starter_2018, order_ct %in% 2), aes(x=BAT_LINEUP_ID, y= total_PA), color="blue", size=3) +
  geom_point(data=subset(BL_order_starter_2018, order_ct %in% 3), aes(x=BAT_LINEUP_ID, y= total_PA), color="red", size=3) +  
  geom_point(data=subset(BL_order_starter_2018, order_ct %in% 4), aes(x=BAT_LINEUP_ID, y= total_PA), color="grey", size=3) +
  theme_bw() + theme(panel.grid.major = element_blank(), text = element_text(size=15), legend.position="top") +
  geom_point(data=subset(BL_order_starter_2019, order_ct %in% 3), aes(x=BAT_LINEUP_ID, y= total_PA), color="orangered2", size=2) +  
  geom_point(data=subset(BL_order_starter_2017, order_ct %in% 3), aes(x=BAT_LINEUP_ID, y= total_PA), color="red4", size=2) +
  geom_point(data=subset(BL_order_starter_2016, order_ct %in% 3), aes(x=BAT_LINEUP_ID, y= total_PA), color="red1", size=2) +   
  geom_point(data=subset(BL_order_starter_2015, order_ct %in% 3), aes(x=BAT_LINEUP_ID, y= total_PA), color="red4", size=2) +  
  geom_point(data=subset(BL_order_starter_2015, order_ct %in% 3), aes(x=BAT_LINEUP_ID, y= total_PA), color="red2", size=2) +
  geom_point(data=subset(BL_order_starter_2014, order_ct %in% 3), aes(x=BAT_LINEUP_ID, y= total_PA), color="red4", size=2) +
  geom_point(data=subset(BL_order_starter_2013, order_ct %in% 3), aes(x=BAT_LINEUP_ID, y= total_PA), color="red3", size=2) +
  geom_point(data=subset(BL_order_starter_2012, order_ct %in% 3), aes(x=BAT_LINEUP_ID, y= total_PA), color="red4", size=2) +
  geom_point(data=subset(BL_order_starter_2011, order_ct %in% 3), aes(x=BAT_LINEUP_ID, y= total_PA), color="red4", size=2) +
  geom_point(data=subset(BL_order_starter_2010, order_ct %in% 3), aes(x=BAT_LINEUP_ID, y= total_PA), color="rosybrown", size=2) +
  geom_hline(yintercept = 4860)



ggplot(subset(BL_order_starter_2015, order_ct %in% c(1,1))) +
  ylab("Plate appearances against starters (% of possible)") + 
  xlab("Batter lineup") +
  geom_point(aes(x=BAT_LINEUP_ID, y= total_PA_p), size=3) + scale_x_continuous(limits = c(1, 9), breaks=c(1:9))  +
  geom_point(data=subset(BL_order_starter_2015, order_ct %in% 2), aes(x=BAT_LINEUP_ID, y= total_PA_p), color="blue", size=3) +
  geom_point(data=subset(BL_order_starter_2015, order_ct %in% 4), aes(x=BAT_LINEUP_ID, y= total_PA_p), color="grey", size=3) +
  theme_bw() + theme(panel.grid.major = element_blank(), text = element_text(size=15), legend.position="top") +
  geom_point(data=subset(BL_order_starter_2019, order_ct %in% 3), aes(x=BAT_LINEUP_ID, y= total_PA_p), color="orangered2", size=3) +  
  geom_point(data=subset(BL_order_starter_2018, order_ct %in% 3), aes(x=BAT_LINEUP_ID, y= total_PA_p), color="red1", size=2) +
  geom_point(data=subset(BL_order_starter_2017, order_ct %in% 3), aes(x=BAT_LINEUP_ID, y= total_PA_p), color="red1", size=3) +
  geom_point(data=subset(BL_order_starter_2016, order_ct %in% 3), aes(x=BAT_LINEUP_ID, y= total_PA_p), color="red2", size=2) +   
  geom_point(data=subset(BL_order_starter_2015, order_ct %in% 3), aes(x=BAT_LINEUP_ID, y= total_PA_p), color="red2", size=3) +  
  geom_point(data=subset(BL_order_starter_2015, order_ct %in% 3), aes(x=BAT_LINEUP_ID, y= total_PA_p), color="red3", size=2) +
  geom_point(data=subset(BL_order_starter_2014, order_ct %in% 3), aes(x=BAT_LINEUP_ID, y= total_PA_p), color="red3", size=3) +
  geom_point(data=subset(BL_order_starter_2013, order_ct %in% 3), aes(x=BAT_LINEUP_ID, y= total_PA_p), color="red3", size=2) +
  geom_point(data=subset(BL_order_starter_2012, order_ct %in% 3), aes(x=BAT_LINEUP_ID, y= total_PA_p), color="red4", size=3) +
  geom_point(data=subset(BL_order_starter_2011, order_ct %in% 3), aes(x=BAT_LINEUP_ID, y= total_PA_p), color="red4", size=2) +
  geom_point(data=subset(BL_order_starter_2010, order_ct %in% 3), aes(x=BAT_LINEUP_ID, y= total_PA_p), color="rosybrown", size=3) +
  geom_hline(yintercept = 1, color="grey")



ggplot(subset(BL_order_starter_2015, order_ct %in% c(1,1))) +
  ylab("Plate appearances against starters (% of possible)") + 
  xlab("Batter lineup") +
  geom_point(aes(x=BAT_LINEUP_ID, y= total_PA_p), size=3) + scale_x_continuous(limits = c(1, 9), breaks=c(1:9))  +
  geom_point(data=subset(BL_order_starter_2015, order_ct %in% 2), aes(x=BAT_LINEUP_ID, y= total_PA_p), color="blue", size=3) +
  geom_point(data=subset(BL_order_starter_2015, order_ct %in% 4), aes(x=BAT_LINEUP_ID, y= total_PA_p), color="grey", size=3) +
  theme_bw() + theme(panel.grid.major = element_blank(), text = element_text(size=15), legend.position="top") +
  geom_point(data=subset(BL_order_starter_2019, order_ct %in% 3), aes(x=BAT_LINEUP_ID, y= total_PA_p), color="#CC00FF", size=3) +  
  geom_point(data=subset(BL_order_starter_2018, order_ct %in% 3), aes(x=BAT_LINEUP_ID, y= total_PA_p), color="#ff00FF", size=2) +
  geom_point(data=subset(BL_order_starter_2017, order_ct %in% 3), aes(x=BAT_LINEUP_ID, y= total_PA_p), color="#ff00CC", size=3) +
  geom_point(data=subset(BL_order_starter_2016, order_ct %in% 3), aes(x=BAT_LINEUP_ID, y= total_PA_p), color="#FF0099", size=2) +   
  geom_point(data=subset(BL_order_starter_2015, order_ct %in% 3), aes(x=BAT_LINEUP_ID, y= total_PA_p), color="#CC0066", size=3) +  
  geom_point(data=subset(BL_order_starter_2015, order_ct %in% 3), aes(x=BAT_LINEUP_ID, y= total_PA_p), color="#ff0033", size=2) +
  geom_point(data=subset(BL_order_starter_2014, order_ct %in% 3), aes(x=BAT_LINEUP_ID, y= total_PA_p), color="#CC0000", size=3) +
  geom_point(data=subset(BL_order_starter_2013, order_ct %in% 3), aes(x=BAT_LINEUP_ID, y= total_PA_p), color="#990000", size=2) +
  geom_point(data=subset(BL_order_starter_2012, order_ct %in% 3), aes(x=BAT_LINEUP_ID, y= total_PA_p), color="#CC3300", size=3) +
  geom_point(data=subset(BL_order_starter_2011, order_ct %in% 3), aes(x=BAT_LINEUP_ID, y= total_PA_p), color="#ff6600", size=2) +
  geom_point(data=subset(BL_order_starter_2010, order_ct %in% 3), aes(x=BAT_LINEUP_ID, y= total_PA_p), color="#CC9900", size=3) +
  geom_hline(yintercept = 1, color="grey")


ggplot(subset(BL_order_starter_2015, order_ct %in% c(1,1))) +
  ylab("Batters faces by starters per TTO") + 
  xlab("Batter lineup") + scale_y_continuous(limits = c(0, 9.2), breaks=c(0, 1, 2, 3, 4, 5, 6, 7,8, 9)) +
  geom_point(aes(x=BAT_LINEUP_ID, y= total_PA_p*9), size=3) + scale_x_continuous(limits = c(1, 9), breaks=c(1:9))  +
  geom_point(data=subset(BL_order_starter_2015, order_ct %in% 2), aes(x=BAT_LINEUP_ID, y= total_PA_p*9), color="blue", size=3) +
  geom_point(data=subset(BL_order_starter_2015, order_ct %in% 4), aes(x=BAT_LINEUP_ID, y= total_PA_p*9), color="grey", size=3) +
  theme_bw() + theme(panel.grid.major = element_blank(), text = element_text(size=15), legend.position="top") +
  geom_point(data=subset(BL_order_starter_2019, order_ct %in% 3), aes(x=BAT_LINEUP_ID, y= total_PA_p*9), color="#CC00FF", size=3) +  
  geom_point(data=subset(BL_order_starter_2018, order_ct %in% 3), aes(x=BAT_LINEUP_ID, y= total_PA_p*9), color="#ff00FF", size=2) +
  geom_point(data=subset(BL_order_starter_2017, order_ct %in% 3), aes(x=BAT_LINEUP_ID, y= total_PA_p*9), color="#ff00CC", size=3) +
  geom_point(data=subset(BL_order_starter_2016, order_ct %in% 3), aes(x=BAT_LINEUP_ID, y= total_PA_p*9), color="#FF0099", size=2) +   
  geom_point(data=subset(BL_order_starter_2015, order_ct %in% 3), aes(x=BAT_LINEUP_ID, y= total_PA_p*9), color="#CC0066", size=3) +  
  geom_point(data=subset(BL_order_starter_2015, order_ct %in% 3), aes(x=BAT_LINEUP_ID, y= total_PA_p*9), color="#ff0033", size=2) +
  geom_point(data=subset(BL_order_starter_2014, order_ct %in% 3), aes(x=BAT_LINEUP_ID, y= total_PA_p*9), color="#CC0000", size=3) +
  geom_point(data=subset(BL_order_starter_2013, order_ct %in% 3), aes(x=BAT_LINEUP_ID, y= total_PA_p*9), color="#990000", size=2) +
  geom_point(data=subset(BL_order_starter_2012, order_ct %in% 3), aes(x=BAT_LINEUP_ID, y= total_PA_p*9), color="#CC3300", size=3) +
  geom_point(data=subset(BL_order_starter_2011, order_ct %in% 3), aes(x=BAT_LINEUP_ID, y= total_PA_p*9), color="#ff6600", size=2) +
  geom_point(data=subset(BL_order_starter_2010, order_ct %in% 3), aes(x=BAT_LINEUP_ID, y= total_PA_p*9), color="#CC9900", size=3) +
  geom_hline(yintercept = 9, color="grey")


ggplot(subset(BL_order_starter_2015, order_ct %in% c(1,1))) +
  ylab("Batters faces by starters per TTO") + 
  geom_hline(yintercept = 9, color="grey") +
  xlab("Batter lineup") + scale_y_continuous(limits = c(0, 9.2), breaks=c(0, 1, 2, 3, 4, 5, 6, 7,8, 9)) +
  geom_point(aes(x=BAT_LINEUP_ID, y= total_PA_p*9), size=3) + scale_x_continuous(limits = c(1, 9), breaks=c(1:9))  +
  geom_point(data=subset(BL_order_starter_2015, order_ct %in% 2), aes(x=BAT_LINEUP_ID, y= total_PA_p*9), color="blue", size=3) +
  geom_point(data=subset(BL_order_starter_2015, order_ct %in% 4), aes(x=BAT_LINEUP_ID, y= total_PA_p*9), color="grey", size=3) +
  theme_bw() + theme(panel.grid.major = element_blank(), text = element_text(size=15), legend.position="top") +
  geom_point(data=subset(BL_order_starter_2019, order_ct %in% 3), aes(x=BAT_LINEUP_ID, y= total_PA_p*9), color="#CC00FF", size=3) +  
  geom_point(data=subset(BL_order_starter_2018, order_ct %in% 3), aes(x=BAT_LINEUP_ID, y= total_PA_p*9), color="#ff00FF", size=2) +
  geom_point(data=subset(BL_order_starter_2017, order_ct %in% 3), aes(x=BAT_LINEUP_ID, y= total_PA_p*9), color="#ff00CC", size=3) +
  geom_point(data=subset(BL_order_starter_2016, order_ct %in% 3), aes(x=BAT_LINEUP_ID, y= total_PA_p*9), color="#FF0099", size=2) +   
  geom_point(data=subset(BL_order_starter_2015, order_ct %in% 3), aes(x=BAT_LINEUP_ID, y= total_PA_p*9), color="#CC0066", size=3) +  
  geom_point(data=subset(BL_order_starter_2015, order_ct %in% 3), aes(x=BAT_LINEUP_ID, y= total_PA_p*9), color="#ff0033", size=2) +
  geom_point(data=subset(BL_order_starter_2014, order_ct %in% 3), aes(x=BAT_LINEUP_ID, y= total_PA_p*9), color="#CC0000", size=3) +
  geom_point(data=subset(BL_order_starter_2013, order_ct %in% 3), aes(x=BAT_LINEUP_ID, y= total_PA_p*9), color="#990000", size=2) +
  geom_point(data=subset(BL_order_starter_2012, order_ct %in% 3), aes(x=BAT_LINEUP_ID, y= total_PA_p*9), color="#CC3300", size=3) +
  geom_point(data=subset(BL_order_starter_2011, order_ct %in% 3), aes(x=BAT_LINEUP_ID, y= total_PA_p*9), color="#ff6600", size=2) +
  geom_point(data=subset(BL_order_starter_2010, order_ct %in% 3), aes(x=BAT_LINEUP_ID, y= total_PA_p*9), color="#CC9900", size=3) +
  geom_point(data=subset(BL_order_starter_2019, order_ct %in% 1), aes(x=BAT_LINEUP_ID, y= total_PA_p*9), color="#101010", size=3) +  
  geom_point(data=subset(BL_order_starter_2018, order_ct %in% 1), aes(x=BAT_LINEUP_ID, y= total_PA_p*9), color="#111111", size=2) +
  geom_point(data=subset(BL_order_starter_2017, order_ct %in% 1), aes(x=BAT_LINEUP_ID, y= total_PA_p*9), color="#555555", size=3) +
  geom_point(data=subset(BL_order_starter_2016, order_ct %in% 1), aes(x=BAT_LINEUP_ID, y= total_PA_p*9), color="#444444", size=2) +   
  geom_point(data=subset(BL_order_starter_2015, order_ct %in% 1), aes(x=BAT_LINEUP_ID, y= total_PA_p*9), color="#666666", size=3) +  
  geom_point(data=subset(BL_order_starter_2015, order_ct %in% 1), aes(x=BAT_LINEUP_ID, y= total_PA_p*9), color="#333333", size=2) +
  geom_point(data=subset(BL_order_starter_2014, order_ct %in% 1), aes(x=BAT_LINEUP_ID, y= total_PA_p*9), color="#222222", size=3) +
  geom_point(data=subset(BL_order_starter_2013, order_ct %in% 1), aes(x=BAT_LINEUP_ID, y= total_PA_p*9), color="#999999", size=2) +
  geom_point(data=subset(BL_order_starter_2012, order_ct %in% 1), aes(x=BAT_LINEUP_ID, y= total_PA_p*9), color="#666666", size=3) +
  geom_point(data=subset(BL_order_starter_2011, order_ct %in% 1), aes(x=BAT_LINEUP_ID, y= total_PA_p*9), color="#666666", size=2) +
  geom_point(data=subset(BL_order_starter_2010, order_ct %in% 1), aes(x=BAT_LINEUP_ID, y= total_PA_p*9), color="#111111", size=3)
  



ggplot(subset(BL_order_starter_ALL, order_ct %in% c(1,1))) +
  ylab("Plate appearances against starters") + 
  xlab("Batter lineup") +
  geom_point(aes(x=BAT_LINEUP_ID, y= wOBA), size=3) + scale_x_continuous(limits = c(1, 9), breaks=c(1:9))  +
  geom_point(data=subset(BL_order_starter_ALL, order_ct %in% 2), aes(x=BAT_LINEUP_ID, y= wOBA), color="blue", size=3) +
  geom_point(data=subset(BL_order_starter_ALL, order_ct %in% 3), aes(x=BAT_LINEUP_ID, y= wOBA), color="red", size=3) +  
  theme_bw() + theme(panel.grid.major = element_blank(), text = element_text(size=15), legend.position="top") +
  geom_point(data=subset(BL_order_starter_2019, order_ct %in% 3), aes(x=BAT_LINEUP_ID, y= wOBA), color="orangered2", size=2) +  
  geom_point(data=subset(BL_order_starter_2018, order_ct %in% 3), aes(x=BAT_LINEUP_ID, y= wOBA), color="orangered2", size=2) +  
  geom_point(data=subset(BL_order_starter_2017, order_ct %in% 3), aes(x=BAT_LINEUP_ID, y= wOBA), color="red4", size=2) +
  geom_point(data=subset(BL_order_starter_2016, order_ct %in% 3), aes(x=BAT_LINEUP_ID, y= wOBA), color="red1", size=2) +   
  geom_point(data=subset(BL_order_starter_2015, order_ct %in% 3), aes(x=BAT_LINEUP_ID, y= wOBA), color="red4", size=2) +  
  geom_point(data=subset(BL_order_starter_2015, order_ct %in% 3), aes(x=BAT_LINEUP_ID, y= wOBA), color="red2", size=2) +
  geom_point(data=subset(BL_order_starter_2014, order_ct %in% 3), aes(x=BAT_LINEUP_ID, y= wOBA), color="red4", size=2) +
  geom_point(data=subset(BL_order_starter_2013, order_ct %in% 3), aes(x=BAT_LINEUP_ID, y= wOBA), color="red3", size=2) +
  geom_point(data=subset(BL_order_starter_2012, order_ct %in% 3), aes(x=BAT_LINEUP_ID, y= wOBA), color="red4", size=2) +
  geom_point(data=subset(BL_order_starter_2011, order_ct %in% 3), aes(x=BAT_LINEUP_ID, y= wOBA), color="red4", size=2) +
  geom_point(data=subset(BL_order_starter_2010, order_ct %in% 3), aes(x=BAT_LINEUP_ID, y= wOBA), color="rosybrown", size=2) +
  geom_point(data=subset(BL_order_starter_2019, order_ct %in% 1), aes(x=BAT_LINEUP_ID, y= wOBA), color="grey", size=2) +  
  geom_point(data=subset(BL_order_starter_2018, order_ct %in% 1), aes(x=BAT_LINEUP_ID, y= wOBA), color="grey2", size=2) +  
  geom_point(data=subset(BL_order_starter_2017, order_ct %in% 1), aes(x=BAT_LINEUP_ID, y= wOBA), color="grey4", size=2) +
  geom_point(data=subset(BL_order_starter_2016, order_ct %in% 1), aes(x=BAT_LINEUP_ID, y= wOBA), color="grey1", size=2) +   
  geom_point(data=subset(BL_order_starter_2015, order_ct %in% 1), aes(x=BAT_LINEUP_ID, y= wOBA), color="grey", size=2) +  
  geom_point(data=subset(BL_order_starter_2015, order_ct %in% 1), aes(x=BAT_LINEUP_ID, y= wOBA), color="grey", size=2) +
  geom_point(data=subset(BL_order_starter_2014, order_ct %in% 1), aes(x=BAT_LINEUP_ID, y= wOBA), color="grey", size=2) +
  geom_point(data=subset(BL_order_starter_2013, order_ct %in% 1), aes(x=BAT_LINEUP_ID, y= wOBA), color="grey", size=2) +
  geom_point(data=subset(BL_order_starter_2012, order_ct %in% 1), aes(x=BAT_LINEUP_ID, y= wOBA), color="grey", size=2) +
  geom_point(data=subset(BL_order_starter_2011, order_ct %in% 1), aes(x=BAT_LINEUP_ID, y= wOBA), color="grey", size=2) +
  geom_point(data=subset(BL_order_starter_2010, order_ct %in% 1), aes(x=BAT_LINEUP_ID, y= wOBA), color="grey5", size=2)
  


ggplot(data=BL_totalbatter_starter_ALL_noPH) +
  ylab("raw wOBA (2010-2019)") + 
  xlab("Batter count") +
  geom_point(aes(x=total_batter, y= wOBA), size=5) + 
  scale_x_continuous(limits = c(1, 26), breaks=c(1:26))  +
  scale_y_continuous(limits = c(0.27, 0.4))  +
  theme_bw() + theme(panel.grid.major = element_blank(), text = element_text(size=15), legend.position="top") +
  geom_point(data=BL_totalbatter_starter_2019, aes(x=total_batter, y= wOBA), color="#FFCC00", size=2) +  
  geom_point(data=BL_totalbatter_starter_2018, aes(x=total_batter, y= wOBA), color="#FF9900", size=2) +  
  geom_point(data=BL_totalbatter_starter_2017, aes(x=total_batter, y= wOBA), color="#FF6600", size=2) +
  geom_point(data=BL_totalbatter_starter_2016, aes(x=total_batter, y= wOBA), color="#FF2200", size=2) +   
  geom_point(data=BL_totalbatter_starter_2015, aes(x=total_batter, y= wOBA), color="#FF0000", size=2) +  
  geom_point(data=BL_totalbatter_starter_2014, aes(x=total_batter, y= wOBA), color="#FF0023", size=2) +
  geom_point(data=BL_totalbatter_starter_2013, aes(x=total_batter, y= wOBA), color="#FF0046", size=2) +
  geom_point(data=BL_totalbatter_starter_2012, aes(x=total_batter, y= wOBA), color="#FF0069", size=2) +
  geom_point(data=BL_totalbatter_starter_2011, aes(x=total_batter, y= wOBA), color="#FF0099", size=2) +
  geom_point(data=BL_totalbatter_starter_2010, aes(x=total_batter, y= wOBA), color="#FF00CC", size=2)



geom_point(data=subset(BL_order_starter_2019, order_ct %in% 3), aes(x=BAT_LINEUP_ID, y= total_PA_p*9), color="#CC00FF", size=3) +  
  geom_point(data=subset(BL_order_starter_2018, order_ct %in% 3), aes(x=BAT_LINEUP_ID, y= total_PA_p*9), color="#ff00FF", size=2) +
  geom_point(data=subset(BL_order_starter_2017, order_ct %in% 3), aes(x=BAT_LINEUP_ID, y= total_PA_p*9), color="#ff00CC", size=3) +
  geom_point(data=subset(BL_order_starter_2016, order_ct %in% 3), aes(x=BAT_LINEUP_ID, y= total_PA_p*9), color="#FF0099", size=2) +   
  geom_point(data=subset(BL_order_starter_2015, order_ct %in% 3), aes(x=BAT_LINEUP_ID, y= total_PA_p*9), color="#CC0066", size=3) +  
  geom_point(data=subset(BL_order_starter_2015, order_ct %in% 3), aes(x=BAT_LINEUP_ID, y= total_PA_p*9), color="#ff0033", size=2) +
  geom_point(data=subset(BL_order_starter_2014, order_ct %in% 3), aes(x=BAT_LINEUP_ID, y= total_PA_p*9), color="#CC0000", size=3) +
  geom_point(data=subset(BL_order_starter_2013, order_ct %in% 3), aes(x=BAT_LINEUP_ID, y= total_PA_p*9), color="#990000", size=2) +
  geom_point(data=subset(BL_order_starter_2012, order_ct %in% 3), aes(x=BAT_LINEUP_ID, y= total_PA_p*9), color="#CC3300", size=3) +


ggplot(subset(BL_order_starter_ALL_noPH, order_ct %in% c(1,1))) +
  ylab("wOBA against starters, 2010-2019") + 
  xlab("Batter lineup") +
  geom_point(aes(x=BAT_LINEUP_ID, y= wOBA), size=3) + scale_x_continuous(limits = c(1, 9), breaks=c(1:9))  +
  geom_point(data=subset(BL_order_starter_ALL_noPH, order_ct %in% 2), aes(x=BAT_LINEUP_ID, y= wOBA), color="blue", size=3) +
  geom_point(data=subset(BL_order_starter_ALL_noPH, order_ct %in% 3), aes(x=BAT_LINEUP_ID, y= wOBA), color="red", size=3) +  
  theme_bw() + theme(panel.grid.major = element_blank(), text = element_text(size=15), legend.position="top")



# SURVIVAL PLOT  # SURVIVAL PLOT   # SURVIVAL PLOT
ggplot() +
  geom_hline(yintercept = 1, size=0.9) +
  geom_vline(xintercept = 9.5, color="grey", size=0.8) +  
  geom_vline(xintercept = 18.5, color="grey", size=0.8) +  
  geom_vline(xintercept = 27.5, color="grey", size=0.8) +    
  scale_x_continuous(limits = c(1, 35), breaks=c(1,9, 10, 18,19, 27,28, 34), expand = c(0, 0))  +
  scale_y_continuous(limits = c(0, 1.05), breaks=c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0), expand = c(0, 0))  +
  theme_bw() + theme(panel.grid.major = element_blank(), text = element_text(size=17), legend.position="top") +
  geom_line(data=BL_totalbatter_starter_2019, aes(x=total_batter, y= total_PA_p), color="#FF6600", size=1.2)  +
  geom_line(data=BL_totalbatter_starter_2018, aes(x=total_batter, y= total_PA_p), color="#aaaaaa", size=1.2) + 
  geom_line(data=BL_totalbatter_starter_2017, aes(x=total_batter, y= total_PA_p), color="#FF3000", size=1.2) +
  geom_line(data=BL_totalbatter_starter_2016, aes(x=total_batter, y= total_PA_p), color="#999999", size=1)  +
  geom_line(data=BL_totalbatter_starter_2015, aes(x=total_batter, y= total_PA_p), color="#FF1111", size=1.2)  +
  geom_line(data=BL_totalbatter_starter_2014, aes(x=total_batter, y= total_PA_p), color="#666666", size=1) +
  geom_line(data=BL_totalbatter_starter_2013, aes(x=total_batter, y= total_PA_p), color="#FF0060", size=1.1) +
  geom_line(data=BL_totalbatter_starter_2012, aes(x=total_batter, y= total_PA_p), color="#444444", size=0.9)  +
  geom_line(data=BL_totalbatter_starter_2011, aes(x=total_batter, y= total_PA_p), color="#FF00CC", size=1.)  +
  geom_line(data=BL_totalbatter_starter_2010, aes(x=total_batter, y= total_PA_p), color="black", size=0.8) +
  ylab("Starting pitcher survival probablity") + xlab("Batter sequence")

  


geom_smooth(data=subset(Starter_pulled_sum, YEAR %in% 2019), aes(x=total_batter, y= pulled_batter/4860), color="#FFCC00", size=2, se=FALSE, span = 0.17) +  
  geom_smooth(data=subset(Starter_pulled_sum, YEAR %in% 2018), aes(x=total_batter, y= pulled_batter/4860), color="#FF9900", size=1, se=FALSE, span = 0.17) +
  geom_smooth(data=subset(Starter_pulled_sum, YEAR %in% 2017), aes(x=total_batter, y= pulled_batter/4860), color="#FF6600", size=1, se=FALSE, span = 0.17) +  
  geom_smooth(data=subset(Starter_pulled_sum, YEAR %in% 2016), aes(x=total_batter, y= pulled_batter/4860), color="#FF0000", size=1, se=FALSE, span = 0.17) +
  geom_smooth(data=subset(Starter_pulled_sum, YEAR %in% 2015), aes(x=total_batter, y= pulled_batter/4860), color="#FFCC23", size=1, se=FALSE, span = 0.17) +  
  geom_smooth(data=subset(Starter_pulled_sum, YEAR %in% 2014), aes(x=total_batter, y= pulled_batter/4860), color="#FF0046", size=1, se=FALSE, span = 0.17) +
  geom_smooth(data=subset(Starter_pulled_sum, YEAR %in% 2013), aes(x=total_batter, y= pulled_batter/4860), color="#FF0060", size=1, se=FALSE, span = 0.17) +  
  geom_smooth(data=subset(Starter_pulled_sum, YEAR %in% 2012), aes(x=total_batter, y= pulled_batter/4860), color="#FF0099", size=1, se=FALSE, span = 0.17) +  
  geom_smooth(data=subset(Starter_pulled_sum, YEAR %in% 2011), aes(x=total_batter, y= pulled_batter/4860), color="#FF00CC", size=1, se=FALSE, span = 0.17) +  
  geom_smooth(data=subset(Starter_pulled_sum, YEAR %in% 2010), aes(x=total_batter, y= pulled_batter/4860), color="#FF00FF", size=1, se=FALSE, span = 0.17)   



# Survival plot zoom in with 50% lines
ggplot(data=BL_totalbatter_starter_2019) +
  geom_hline(yintercept = 1) +
  ylab("Starter survival probablity") + xlab("Batter sequence") +
  scale_x_continuous(limits = c(18, 32), breaks=c(1,9, 10, 18,19, 27,28, 34), expand = c(0, 0))  +
  scale_y_continuous(limits = c(0, 1.05), breaks=c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0), expand = c(0, 0))  +
  geom_line(aes(x=total_batter, y=total_PA_p), size=1, color="grey") + 
  theme_bw() + theme(panel.grid.major = element_blank(), text = element_text(size=15), legend.position="top") +
  geom_line(data=BL_totalbatter_starter_2018, aes(x=total_batter, y= total_PA_p), color="red", size=0.5)  +
  geom_line(data=BL_totalbatter_starter_2017, aes(x=total_batter, y= total_PA_p), color="grey9", size=1) +
  geom_line(data=BL_totalbatter_starter_2016, aes(x=total_batter, y= total_PA_p), color="red4", size=0.5)  +
  geom_line(data=BL_totalbatter_starter_2015, aes(x=total_batter, y= total_PA_p), color="red2", size=1)  +
  geom_line(data=BL_totalbatter_starter_2014, aes(x=total_batter, y= total_PA_p), color="red4", size=0.5) +
  geom_line(data=BL_totalbatter_starter_2013, aes(x=total_batter, y= total_PA_p), color="grey4", size=1) +
  geom_line(data=BL_totalbatter_starter_2012, aes(x=total_batter, y= total_PA_p), color="red4", size=0.5)  +
  geom_line(data=BL_totalbatter_starter_2011, aes(x=total_batter, y= total_PA_p), color="red4", size=1)  +
  geom_line(data=BL_totalbatter_starter_2010, aes(x=total_batter, y= total_PA_p), color="grey1", size=0.5) +
  geom_vline(xintercept=23.65) +
  geom_vline(xintercept=26.65) +
  geom_hline(yintercept=0.5)


######################################
###### when starters get pulled ######
######################################

# distribution of starter batter counts by year
Starter_TTO_10_19_GP_game <- Starter_TTO_10_19 %>%
  mutate(GAMEID_PITID = paste(GAME_ID, PIT_ID)) %>%
  group_by(GAMEID_PITID)

Starter_pulled <- top_n(Starter_TTO_10_19_GP_game, 1, total_batter)

Starter_pulled_sum <- Starter_pulled %>%
  group_by(YEAR, total_batter) %>%
  summarise(pulled=sum(total_batter)) %>%
  mutate(pulled_batter = pulled/total_batter)

hist(Starter_pulled$total_batter)

ggplot(Starter_pulled_sum) +
  geom_point(aes(x=total_batter, y=pulled_batter, color=YEAR))
 

ggplot(Starter_pulled_sum) +
  ylab("Starter survival probablity") + xlab("Batter sequence") +
  ylab("freq") +  xlab("Batter count") +
  scale_x_continuous(limits = c(1, 37), breaks=c(1:37))  +
  theme_bw() + theme(panel.grid.major = element_blank(), text = element_text(size=15), legend.position="top") +
  geom_point(data=subset(Starter_pulled_sum, YEAR %in% 2019), aes(x=total_batter, y= pulled_batter), color="#FFCC00", size=3, shape=17) +  
  geom_point(data=subset(Starter_pulled_sum, YEAR %in% 2018), aes(x=total_batter, y= pulled_batter), color="#FF9900", size=3, shape=17) +
  geom_point(data=subset(Starter_pulled_sum, YEAR %in% 2017), aes(x=total_batter, y= pulled_batter), color="#FF6600", size=2, shape=17) +  
  geom_point(data=subset(Starter_pulled_sum, YEAR %in% 2016), aes(x=total_batter, y= pulled_batter), color="#FF0000", size=2, shape=15) +
  geom_point(data=subset(Starter_pulled_sum, YEAR %in% 2015), aes(x=total_batter, y= pulled_batter), color="#FFCC23", size=2, shape=15) +  
  geom_point(data=subset(Starter_pulled_sum, YEAR %in% 2014), aes(x=total_batter, y= pulled_batter), color="#FF0046", size=2, shape=15) +
  geom_point(data=subset(Starter_pulled_sum, YEAR %in% 2013), aes(x=total_batter, y= pulled_batter), color="#FF0060", size=2) +  
  geom_point(data=subset(Starter_pulled_sum, YEAR %in% 2012), aes(x=total_batter, y= pulled_batter), color="#FF0099", size=2) +  
  geom_point(data=subset(Starter_pulled_sum, YEAR %in% 2011), aes(x=total_batter, y= pulled_batter), color="#FF00CC", size=2) +  
  geom_point(data=subset(Starter_pulled_sum, YEAR %in% 2010), aes(x=total_batter, y= pulled_batter), color="#FF00FF", size=2)   


ggplot(Starter_pulled_sum) +
  ylab("Starter survival probablity") + xlab("Batter sequence") +
  ylab("freq") +  xlab("Batter count") + ggtitle("When starters get pulled") +
  scale_x_continuous(limits = c(1, 37), breaks=c(1:37))  +
  theme_bw() + theme(panel.grid.major = element_blank(), text = element_text(size=15), legend.position="top") +
  geom_line(data=subset(Starter_pulled_sum, YEAR %in% 2019), aes(x=total_batter, y= pulled_batter), color="#FFCC00", size=2) +  
  geom_line(data=subset(Starter_pulled_sum, YEAR %in% 2018), aes(x=total_batter, y= pulled_batter), color="#FF9900", size=1) +
  geom_line(data=subset(Starter_pulled_sum, YEAR %in% 2017), aes(x=total_batter, y= pulled_batter), color="#FF6600", size=1) +  
  geom_line(data=subset(Starter_pulled_sum, YEAR %in% 2016), aes(x=total_batter, y= pulled_batter), color="#FF0000", size=1) +
  geom_line(data=subset(Starter_pulled_sum, YEAR %in% 2015), aes(x=total_batter, y= pulled_batter), color="#FFCC23", size=1) +  
  geom_line(data=subset(Starter_pulled_sum, YEAR %in% 2014), aes(x=total_batter, y= pulled_batter), color="#FF0046", size=1) +
  geom_line(data=subset(Starter_pulled_sum, YEAR %in% 2013), aes(x=total_batter, y= pulled_batter), color="#FF0060", size=1) +  
  geom_line(data=subset(Starter_pulled_sum, YEAR %in% 2012), aes(x=total_batter, y= pulled_batter), color="#FF0099", size=1) +  
  geom_line(data=subset(Starter_pulled_sum, YEAR %in% 2011), aes(x=total_batter, y= pulled_batter), color="#FF00CC", size=1) +  
  geom_smooth(data=subset(Starter_pulled_sum, YEAR %in% 2010), aes(x=total_batter, y= pulled_batter), color="#FF00FF", size=1, se=FALSE, span = 0.4)   



# Distribution of when starters are pulled
ggplot(Starter_pulled_sum) +
  ylab("Starter survival probablity") + xlab("Batter sequence") +
  ylab("density") +  xlab("Batter count") +
  scale_x_continuous(limits = c(1, 37), breaks = seq(0,37,3))  +
  theme_bw() + theme(panel.grid.major = element_blank(), text = element_text(size=15), legend.position="top") +
  geom_smooth(data=subset(Starter_pulled_sum, YEAR %in% 2019), aes(x=total_batter, y= pulled_batter/4860), color="#FFCC00", size=2, se=FALSE, span = 0.17) +  
  geom_smooth(data=subset(Starter_pulled_sum, YEAR %in% 2018), aes(x=total_batter, y= pulled_batter/4860), color="#FF9900", size=1, se=FALSE, span = 0.17) +
  geom_smooth(data=subset(Starter_pulled_sum, YEAR %in% 2017), aes(x=total_batter, y= pulled_batter/4860), color="#FF6600", size=1, se=FALSE, span = 0.17) +  
  geom_smooth(data=subset(Starter_pulled_sum, YEAR %in% 2016), aes(x=total_batter, y= pulled_batter/4860), color="#FF0000", size=1, se=FALSE, span = 0.17) +
  geom_smooth(data=subset(Starter_pulled_sum, YEAR %in% 2015), aes(x=total_batter, y= pulled_batter/4860), color="#FFCC23", size=1, se=FALSE, span = 0.17) +  
  geom_smooth(data=subset(Starter_pulled_sum, YEAR %in% 2014), aes(x=total_batter, y= pulled_batter/4860), color="#FF0046", size=1, se=FALSE, span = 0.17) +
  geom_smooth(data=subset(Starter_pulled_sum, YEAR %in% 2013), aes(x=total_batter, y= pulled_batter/4860), color="#FF0060", size=1, se=FALSE, span = 0.17) +  
  geom_smooth(data=subset(Starter_pulled_sum, YEAR %in% 2012), aes(x=total_batter, y= pulled_batter/4860), color="#FF0099", size=1, se=FALSE, span = 0.17) +  
  geom_smooth(data=subset(Starter_pulled_sum, YEAR %in% 2011), aes(x=total_batter, y= pulled_batter/4860), color="#FF00CC", size=1, se=FALSE, span = 0.17) +  
  geom_smooth(data=subset(Starter_pulled_sum, YEAR %in% 2010), aes(x=total_batter, y= pulled_batter/4860), color="#FF00FF", size=1, se=FALSE, span = 0.17)   


GAME1 <- filter(Starter_TTO_10_19, GAME_ID=="ANA201004050")
GAME1_bakes002 <- filter(Starter_TTO_10_19, GAME_ID=="ANA201004050", PIT_ID=="bakes002")
GAME1_weavj003 <- filter(Starter_TTO_10_19, GAME_ID=="ANA201004050", PIT_ID=="weavj003")



Pitchers_ALL_10_19 <- rbind(MLB_2010_A, MLB_2011_A, MLB_2012_A, MLB_2013_A, MLB_2014_A, MLB_2015_A, MLB_2016_A, MLB_2017_A, MLB_2018_A, MLB_2019_A) %>%
  select(RUNS, RBI_CT, YEAR, total_batter, pitch_count_p, order_ct, woba_evnt, INN_CT, OUTS_CT, H_FL, HIT_binary, BAT_EVENT_FL, AB_FL)



Pitchers_ALL_10_19_sum <- group_by(Pitchers_ALL_10_19, total_batter, YEAR) %>%
  summarise(total_AB=sum(AB_FL), total_PA=sum(BAT_EVENT_FL), total_bases=sum(H_FL), total_hits=sum(HIT_binary), total_wOBA=sum(woba_evnt), total_RBI=sum(RBI_CT) ) %>%
  mutate(ABV=total_hits/total_AB, SLG=total_bases/total_AB, wOBA=total_wOBA/total_PA, total_PA_p = total_PA/48600) %>% 
  filter(total_batter < 34)

# runs/year
Pitchers_ALL_10_19_sumYR <- group_by(Pitchers_ALL_10_19, YEAR) %>%
  summarise(total_AB=sum(AB_FL), total_RBI=sum(RBI_CT)) %>%
  mutate(RBI_factor = year_RBI_Ave/total_RBI)

# 10 years average RBI/yr
year_RBI_Ave <- sum(Pitchers_ALL_10_19_sumYR$total_RBI)/10
year_RBI_Ave 


# normalize RBI's by year => create RBI normalization factor by YEAR
Pitchers_ALL_10_19_sum_norm <- select(Pitchers_ALL_10_19_sum, total_RBI, YEAR, total_batter) %>%
  merge(Pitchers_ALL_10_19_sumYR, by="YEAR") %>%
  mutate(RBI_norm = total_RBI.x*RBI_factor)
  

Pitchers_ALL_10_19_sum_total_batter <- group_by(Pitchers_ALL_10_19_sum_norm, total_batter) %>%
  summarise(total_RBI10=mean(RBI_norm))

# data set to plot
Pitchers_ALL_10_19_sum_2 <- merge(Pitchers_ALL_10_19_sum_norm, Pitchers_ALL_10_19_sum_total_batter, by="total_batter") %>%
  mutate(delta_RBI=total_RBI10-RBI_norm) %>% 
  merge(Pitchers_ALL_10_19_sumYR, by="YEAR")



ggplot(Pitchers_ALL_10_19_sum) +
  geom_point(aes(x=total_batter, y=total_RBI)) +
  facet_wrap(~YEAR)


ggplot() +
  geom_point(data=subset(Pitchers_ALL_10_19_sum_2,Pitchers_ALL_10_19_sum_2$YEAR==2010), aes(x=total_batter, y=delta_RBI), color="#FF00FF", size=2) +
  geom_point(data=subset(Pitchers_ALL_10_19_sum_2,Pitchers_ALL_10_19_sum_2$YEAR==2011), aes(x=total_batter, y=delta_RBI), color="#FF00CC", size=2) + 
  geom_point(data=subset(Pitchers_ALL_10_19_sum_2,Pitchers_ALL_10_19_sum_2$YEAR==2012), aes(x=total_batter, y=delta_RBI), color="#FF0066", size=2) +
  geom_point(data=subset(Pitchers_ALL_10_19_sum_2,Pitchers_ALL_10_19_sum_2$YEAR==2013), aes(x=total_batter, y=delta_RBI), color="#FF0022", size=2) +
  geom_point(data=subset(Pitchers_ALL_10_19_sum_2,Pitchers_ALL_10_19_sum_2$YEAR==2014), aes(x=total_batter, y=delta_RBI), color="#FF0000", size=2) +
  geom_point(data=subset(Pitchers_ALL_10_19_sum_2,Pitchers_ALL_10_19_sum_2$YEAR==2015), aes(x=total_batter, y=delta_RBI), color="#FF1100", size=2) + 
  geom_point(data=subset(Pitchers_ALL_10_19_sum_2,Pitchers_ALL_10_19_sum_2$YEAR==2016), aes(x=total_batter, y=delta_RBI), color="#FF2200", size=2) +
  geom_point(data=subset(Pitchers_ALL_10_19_sum_2,Pitchers_ALL_10_19_sum_2$YEAR==2017), aes(x=total_batter, y=delta_RBI), color="#FF9900", size=2) +
  geom_point(data=subset(Pitchers_ALL_10_19_sum_2,Pitchers_ALL_10_19_sum_2$YEAR==2018), aes(x=total_batter, y=delta_RBI), color="#FFCC00", size=2) +
  geom_point(data=subset(Pitchers_ALL_10_19_sum_2,Pitchers_ALL_10_19_sum_2$YEAR==2019), aes(x=total_batter, y=delta_RBI), color="#FFFF00", size=2)  
  ylim(-90, 90)


  
  ggplot() +
    geom_point(data=subset(Pitchers_ALL_10_19_sum_2,Pitchers_ALL_10_19_sum_2$total_batter==18), aes(x=YEAR, y=delta_RBI), color="#FF00FF", size=3) +
    geom_point(data=subset(Pitchers_ALL_10_19_sum_2,Pitchers_ALL_10_19_sum_2$total_batter==19), aes(x=YEAR, y=delta_RBI), color="#FF00CC", size=3) + 
    geom_point(data=subset(Pitchers_ALL_10_19_sum_2,Pitchers_ALL_10_19_sum_2$total_batter==20), aes(x=YEAR, y=delta_RBI), color="#FF0066", size=3) +
    geom_point(data=subset(Pitchers_ALL_10_19_sum_2,Pitchers_ALL_10_19_sum_2$total_batter==21), aes(x=YEAR, y=delta_RBI), color="#FF0022", size=3) +
    geom_point(data=subset(Pitchers_ALL_10_19_sum_2,Pitchers_ALL_10_19_sum_2$total_batter==22), aes(x=YEAR, y=delta_RBI), color="#FF0000", size=3) +
    geom_point(data=subset(Pitchers_ALL_10_19_sum_2,Pitchers_ALL_10_19_sum_2$total_batter==23), aes(x=YEAR, y=delta_RBI), color="#FF1100", size=3) + 
    geom_point(data=subset(Pitchers_ALL_10_19_sum_2,Pitchers_ALL_10_19_sum_2$total_batter==24), aes(x=YEAR, y=delta_RBI), color="#FF2200", size=2) +
    geom_point(data=subset(Pitchers_ALL_10_19_sum_2,Pitchers_ALL_10_19_sum_2$total_batter==25), aes(x=YEAR, y=delta_RBI), color="#FF9900", size=2) +
    geom_point(data=subset(Pitchers_ALL_10_19_sum_2,Pitchers_ALL_10_19_sum_2$total_batter==26), aes(x=YEAR, y=delta_RBI), color="#FFCC00", size=2) +
    geom_point(data=subset(Pitchers_ALL_10_19_sum_2,Pitchers_ALL_10_19_sum_2$total_batter==27), aes(x=YEAR, y=delta_RBI), color="#FFFF00", size=2)  
  ylim(-90, 90)
  
  
  ggplot() +
    geom_point(data=subset(Pitchers_ALL_10_19_sum_2,Pitchers_ALL_10_19_sum_2$total_batter==1), aes(x=YEAR, y=delta_RBI), color="#FF00FF", size=3) +
    geom_point(data=subset(Pitchers_ALL_10_19_sum_2,Pitchers_ALL_10_19_sum_2$total_batter==2), aes(x=YEAR, y=delta_RBI), color="#FF00CC", size=3) + 
    geom_point(data=subset(Pitchers_ALL_10_19_sum_2,Pitchers_ALL_10_19_sum_2$total_batter==3), aes(x=YEAR, y=delta_RBI), color="#FF0066", size=3) +
    geom_point(data=subset(Pitchers_ALL_10_19_sum_2,Pitchers_ALL_10_19_sum_2$total_batter==4), aes(x=YEAR, y=delta_RBI), color="#FF0022", size=3) +
    geom_point(data=subset(Pitchers_ALL_10_19_sum_2,Pitchers_ALL_10_19_sum_2$total_batter==5), aes(x=YEAR, y=delta_RBI), color="#FF0000", size=3) +
    geom_point(data=subset(Pitchers_ALL_10_19_sum_2,Pitchers_ALL_10_19_sum_2$total_batter==6), aes(x=YEAR, y=delta_RBI), color="#FF1100", size=3) + 
    geom_point(data=subset(Pitchers_ALL_10_19_sum_2,Pitchers_ALL_10_19_sum_2$total_batter==7), aes(x=YEAR, y=delta_RBI), color="#FF2200", size=2) +
    geom_point(data=subset(Pitchers_ALL_10_19_sum_2,Pitchers_ALL_10_19_sum_2$total_batter==8), aes(x=YEAR, y=delta_RBI), color="#FF9900", size=2) +
    geom_point(data=subset(Pitchers_ALL_10_19_sum_2,Pitchers_ALL_10_19_sum_2$total_batter==9), aes(x=YEAR, y=delta_RBI), color="#FFCC00", size=2) +
    geom_point(data=subset(Pitchers_ALL_10_19_sum_2,Pitchers_ALL_10_19_sum_2$total_batter==10), aes(x=YEAR, y=delta_RBI), color="#FFFF00", size=2)  
  ylim(-90, 90)
 
 
  ggplot() +
    geom_smooth(data=subset(Pitchers_ALL_10_19_sum_2,Pitchers_ALL_10_19_sum_2$total_batter==11), aes(x=YEAR, y=delta_RBI), color="#FF00FF", size=2, se=FALSE, span = 0.5) +
    geom_smooth(data=subset(Pitchers_ALL_10_19_sum_2,Pitchers_ALL_10_19_sum_2$total_batter==12), aes(x=YEAR, y=delta_RBI), color="#FF00CC", size=2, se=FALSE, span = 0.5) + 
    geom_smooth(data=subset(Pitchers_ALL_10_19_sum_2,Pitchers_ALL_10_19_sum_2$total_batter==13), aes(x=YEAR, y=delta_RBI), color="#FF0066", size=2, se=FALSE, span = 0.5) +
    geom_smooth(data=subset(Pitchers_ALL_10_19_sum_2,Pitchers_ALL_10_19_sum_2$total_batter==14), aes(x=YEAR, y=delta_RBI), color="#FF0022", size=2, se=FALSE, span = 0.5) +
    geom_smooth(data=subset(Pitchers_ALL_10_19_sum_2,Pitchers_ALL_10_19_sum_2$total_batter==15), aes(x=YEAR, y=delta_RBI), color="#FF0000", size=2, se=FALSE, span = 0.5) +
    geom_smooth(data=subset(Pitchers_ALL_10_19_sum_2,Pitchers_ALL_10_19_sum_2$total_batter==16), aes(x=YEAR, y=delta_RBI), color="#FF1100", size=2, se=FALSE, span = 0.5) + 
    geom_smooth(data=subset(Pitchers_ALL_10_19_sum_2,Pitchers_ALL_10_19_sum_2$total_batter==17), aes(x=YEAR, y=delta_RBI), color="#FF2200", size=2, se=FALSE, span = 0.5) +
    geom_smooth(data=subset(Pitchers_ALL_10_19_sum_2,Pitchers_ALL_10_19_sum_2$total_batter==18), aes(x=YEAR, y=delta_RBI), color="#FF9900", size=2, se=FALSE, span = 0.5) +
    geom_smooth(data=subset(Pitchers_ALL_10_19_sum_2,Pitchers_ALL_10_19_sum_2$total_batter==19), aes(x=YEAR, y=delta_RBI), color="#FFCC00", size=2, se=FALSE, span = 0.5) +
    geom_smooth(data=subset(Pitchers_ALL_10_19_sum_2,Pitchers_ALL_10_19_sum_2$total_batter==20), aes(x=YEAR, y=delta_RBI), color="#FFFF00", size=2, se=FALSE, span = 0.5)  
  ylim(-90, 90)
  
  
  ggplot() +
    geom_smooth(data=subset(Pitchers_ALL_10_19_sum_2,Pitchers_ALL_10_19_sum_2$total_batter==16), aes(x=YEAR, y=delta_RBI), color="#FF00FF", size=2, se=FALSE, span = 0.9) +
    geom_smooth(data=subset(Pitchers_ALL_10_19_sum_2,Pitchers_ALL_10_19_sum_2$total_batter==17), aes(x=YEAR, y=delta_RBI), color="#FF00CC", size=2, se=FALSE, span = 0.9) + 
    geom_smooth(data=subset(Pitchers_ALL_10_19_sum_2,Pitchers_ALL_10_19_sum_2$total_batter==18), aes(x=YEAR, y=delta_RBI), color="#FF0066", size=2, se=FALSE, span = 0.9) +
    geom_smooth(data=subset(Pitchers_ALL_10_19_sum_2,Pitchers_ALL_10_19_sum_2$total_batter==19), aes(x=YEAR, y=delta_RBI), color="#FF0022", size=2, se=FALSE, span = 0.9) +
    geom_smooth(data=subset(Pitchers_ALL_10_19_sum_2,Pitchers_ALL_10_19_sum_2$total_batter==20), aes(x=YEAR, y=delta_RBI), color="#FF0000", size=2, se=FALSE, span = 0.9) +
    geom_smooth(data=subset(Pitchers_ALL_10_19_sum_2,Pitchers_ALL_10_19_sum_2$total_batter==21), aes(x=YEAR, y=delta_RBI), color="#FF1100", size=2, se=FALSE, span = 0.9) + 
    geom_smooth(data=subset(Pitchers_ALL_10_19_sum_2,Pitchers_ALL_10_19_sum_2$total_batter==22), aes(x=YEAR, y=delta_RBI), color="#FF2200", size=2, se=FALSE, span = 0.9) +
    geom_smooth(data=subset(Pitchers_ALL_10_19_sum_2,Pitchers_ALL_10_19_sum_2$total_batter==23), aes(x=YEAR, y=delta_RBI), color="#FF9900", size=2, se=FALSE, span = 0.9) +
    geom_smooth(data=subset(Pitchers_ALL_10_19_sum_2,Pitchers_ALL_10_19_sum_2$total_batter==24), aes(x=YEAR, y=delta_RBI), color="#FFCC00", size=2, se=FALSE, span = 0.9) +    
    geom_smooth(data=subset(Pitchers_ALL_10_19_sum_2,Pitchers_ALL_10_19_sum_2$total_batter==25), aes(x=YEAR, y=delta_RBI), color="#FFFF00", size=2, se=FALSE, span = 0.9)  
  ylim(-90, 90)
  
  
   
   ggplot() +
    geom_smooth(data=subset(Pitchers_ALL_10_19_sum_2,Pitchers_ALL_10_19_sum_2$total_batter==1), aes(x=YEAR, y=delta_RBI), color="#FF00FF", size=2, se=FALSE, span = 0.5) +
    geom_smooth(data=subset(Pitchers_ALL_10_19_sum_2,Pitchers_ALL_10_19_sum_2$total_batter==2), aes(x=YEAR, y=delta_RBI), color="#FF00CC", size=2, se=FALSE, span = 0.5) + 
    geom_smooth(data=subset(Pitchers_ALL_10_19_sum_2,Pitchers_ALL_10_19_sum_2$total_batter==3), aes(x=YEAR, y=delta_RBI), color="#FF0066", size=2, se=FALSE, span = 0.5) +
    geom_smooth(data=subset(Pitchers_ALL_10_19_sum_2,Pitchers_ALL_10_19_sum_2$total_batter==4), aes(x=YEAR, y=delta_RBI), color="#FF0022", size=2, se=FALSE, span = 0.5) +
    geom_smooth(data=subset(Pitchers_ALL_10_19_sum_2,Pitchers_ALL_10_19_sum_2$total_batter==5), aes(x=YEAR, y=delta_RBI), color="#FF0000", size=2, se=FALSE, span = 0.5) +
    geom_smooth(data=subset(Pitchers_ALL_10_19_sum_2,Pitchers_ALL_10_19_sum_2$total_batter==6), aes(x=YEAR, y=delta_RBI), color="#FF1100", size=2, se=FALSE, span = 0.5) + 
    geom_smooth(data=subset(Pitchers_ALL_10_19_sum_2,Pitchers_ALL_10_19_sum_2$total_batter==7), aes(x=YEAR, y=delta_RBI), color="#FF2200", size=2, se=FALSE, span = 0.5) +
    geom_smooth(data=subset(Pitchers_ALL_10_19_sum_2,Pitchers_ALL_10_19_sum_2$total_batter==8), aes(x=YEAR, y=delta_RBI), color="#FF9900", size=2, se=FALSE, span = 0.5) +
    geom_smooth(data=subset(Pitchers_ALL_10_19_sum_2,Pitchers_ALL_10_19_sum_2$total_batter==9), aes(x=YEAR, y=delta_RBI), color="#FFCC00", size=2, se=FALSE, span = 0.5) +
    geom_smooth(data=subset(Pitchers_ALL_10_19_sum_2,Pitchers_ALL_10_19_sum_2$total_batter==10), aes(x=YEAR, y=delta_RBI), color="#FFFF00", size=2, se=FALSE, span = 0.5)  
  ylim(-90, 90)
  
  
  
  ggplot() +
    geom_smooth(data=subset(Pitchers_ALL_10_19_sum_2,Pitchers_ALL_10_19_sum_2$total_batter==24), aes(x=YEAR, y=delta_RBI), color="#FF00FF", size=2, se=FALSE, span = 0.9) +
    geom_smooth(data=subset(Pitchers_ALL_10_19_sum_2,Pitchers_ALL_10_19_sum_2$total_batter==25), aes(x=YEAR, y=delta_RBI), color="#FF00CC", size=2, se=FALSE, span = 0.9) + 
    geom_smooth(data=subset(Pitchers_ALL_10_19_sum_2,Pitchers_ALL_10_19_sum_2$total_batter==26), aes(x=YEAR, y=delta_RBI), color="#FF0066", size=2, se=FALSE, span = 0.9) +
    geom_smooth(data=subset(Pitchers_ALL_10_19_sum_2,Pitchers_ALL_10_19_sum_2$total_batter==27), aes(x=YEAR, y=delta_RBI), color="#FF0022", size=2, se=FALSE, span = 0.9) +
    geom_smooth(data=subset(Pitchers_ALL_10_19_sum_2,Pitchers_ALL_10_19_sum_2$total_batter==28), aes(x=YEAR, y=delta_RBI), color="#FF0000", size=2, se=FALSE, span = 0.9) +
    geom_smooth(data=subset(Pitchers_ALL_10_19_sum_2,Pitchers_ALL_10_19_sum_2$total_batter==29), aes(x=YEAR, y=delta_RBI), color="#FF1100", size=2, se=FALSE, span = 0.9) + 
    geom_smooth(data=subset(Pitchers_ALL_10_19_sum_2,Pitchers_ALL_10_19_sum_2$total_batter==30), aes(x=YEAR, y=delta_RBI), color="#FF2200", size=2, se=FALSE, span = 0.9) +
    geom_smooth(data=subset(Pitchers_ALL_10_19_sum_2,Pitchers_ALL_10_19_sum_2$total_batter==31), aes(x=YEAR, y=delta_RBI), color="#FF9900", size=2, se=FALSE, span = 0.9) +
    geom_smooth(data=subset(Pitchers_ALL_10_19_sum_2,Pitchers_ALL_10_19_sum_2$total_batter==32), aes(x=YEAR, y=delta_RBI), color="#FFCC00", size=2, se=FALSE, span = 0.9) +    
    geom_smooth(data=subset(Pitchers_ALL_10_19_sum_2,Pitchers_ALL_10_19_sum_2$total_batter==33), aes(x=YEAR, y=delta_RBI), color="#FFFF00", size=2, se=FALSE, span = 0.9)  
  ylim(-90, 90)
  
  

ggplot() +
  geom_point(data=subset(Pitchers_ALL_10_19_sum,Pitchers_ALL_10_19_sum$YEAR==2010), aes(x=total_batter, y=total_RBI), color="#FF00FF", size=2) +
  geom_point(data=subset(Pitchers_ALL_10_19_sum,Pitchers_ALL_10_19_sum$YEAR==2011), aes(x=total_batter, y=total_RBI), color="#FF00CC", size=2) + 
  geom_point(data=subset(Pitchers_ALL_10_19_sum,Pitchers_ALL_10_19_sum$YEAR==2012), aes(x=total_batter, y=total_RBI), color="#FF0066", size=2) +
  geom_point(data=subset(Pitchers_ALL_10_19_sum,Pitchers_ALL_10_19_sum$YEAR==2013), aes(x=total_batter, y=total_RBI), color="#FF0022", size=2) +
  geom_point(data=subset(Pitchers_ALL_10_19_sum,Pitchers_ALL_10_19_sum$YEAR==2014), aes(x=total_batter, y=total_RBI), color="#FF0000", size=2) +
  geom_point(data=subset(Pitchers_ALL_10_19_sum,Pitchers_ALL_10_19_sum$YEAR==2015), aes(x=total_batter, y=total_RBI), color="#FF1100", size=2) + 
  geom_point(data=subset(Pitchers_ALL_10_19_sum,Pitchers_ALL_10_19_sum$YEAR==2016), aes(x=total_batter, y=total_RBI), color="#FF2200", size=2) +
  geom_point(data=subset(Pitchers_ALL_10_19_sum,Pitchers_ALL_10_19_sum$YEAR==2017), aes(x=total_batter, y=total_RBI), color="#FF9900", size=2) +
  geom_point(data=subset(Pitchers_ALL_10_19_sum,Pitchers_ALL_10_19_sum$YEAR==2018), aes(x=total_batter, y=total_RBI), color="#FFCC00", size=2) +
  geom_point(data=subset(Pitchers_ALL_10_19_sum,Pitchers_ALL_10_19_sum$YEAR==2019), aes(x=total_batter, y=total_RBI), color="#FFFF00", size=2) + 
  ylim(0, 650)





# LEAD OFF HR 
ggplot(subset(Pitchers_ALL_10_19_sum,Pitchers_ALL_10_19_sum$total_batter==1)) +
  geom_point(aes(x=YEAR, y=total_RBI), color="red", size=3) +
  ylab("lead off HR") +
  ylim(0, 210)

ggplot() +
  geom_point(data=subset(Pitchers_ALL_10_19_sum,Pitchers_ALL_10_19_sum$total_batter==1), aes(x=YEAR, y=total_RBI), color="red", size=3) +
  geom_point(data=subset(Pitchers_ALL_10_19_sum,Pitchers_ALL_10_19_sum$total_batter==2), aes(x=YEAR, y=total_RBI), size=3) + 
  geom_point(data=subset(Pitchers_ALL_10_19_sum,Pitchers_ALL_10_19_sum$total_batter==10), aes(x=YEAR, y=total_RBI), color="red", size=3) +
  geom_point(data=subset(Pitchers_ALL_10_19_sum,Pitchers_ALL_10_19_sum$total_batter==11), aes(x=YEAR, y=total_RBI), size=3) + 
  ylim(0, 650)


ggplot() +
  geom_point(data=subset(Pitchers_ALL_10_19_sum,Pitchers_ALL_10_19_sum$total_batter==7), aes(x=YEAR, y=total_RBI), color="red", size=3) +
  geom_point(data=subset(Pitchers_ALL_10_19_sum,Pitchers_ALL_10_19_sum$total_batter==8), aes(x=YEAR, y=total_RBI), size=3) + 
  ylim(0, 650)


