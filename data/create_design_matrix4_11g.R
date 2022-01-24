library(tidyverse)

### create moving avg. estimator for batter and pitcher quality

input_filename = "TTO_dataset_410.csv" #FIXME
output_filename = "TTO_dataset_411g.csv" #FIXME
E0 <- read_csv(input_filename)

#############################################################################
#### WOBA_AVG_BAT_19, WOBA_FINAL_BAT_19, WOBA_AVG_PIT_19, WOBA_FINAL_PIT_19, 
#### NUM_WOBA_APP_BAT, NUM_WOBA_APP_FINAL_BAT, NUM_WOBA_APP_PIT, NUM_WOBA_APP_FINAL_PIT,
#############################################################################

{
  E00 = E0 %>% 
      group_by(YEAR, PIT_ID) %>%
      arrange(DATE, row_idx) %>%
      mutate(
        NUM_WOBA_APP_PIT = cumsum(WOBA_APP),
        NUM_WOBA_APP_FINAL_PIT = NUM_WOBA_APP_PIT[n()],
        WOBA_AVG_PIT_19 = cumsum(EVENT_WOBA_19)/NUM_WOBA_APP_PIT,
        WOBA_FINAL_PIT_19 = WOBA_AVG_PIT_19[n()]
      ) %>%
      ungroup() %>%
      group_by(YEAR, BAT_ID) %>%
      arrange(DATE, row_idx) %>%
      mutate(
        NUM_WOBA_APP_BAT = cumsum(WOBA_APP),
        NUM_WOBA_APP_FINAL_BAT = NUM_WOBA_APP_BAT[n()],
        WOBA_AVG_BAT_19 = cumsum(EVENT_WOBA_19)/NUM_WOBA_APP_BAT,
        WOBA_FINAL_BAT_19 = WOBA_AVG_BAT_19[n()],
      ) %>%
      ungroup() 
}

################################################
########### PITCHER QUALITY FUNCTION ###########
################################################

get_PQ <- function(E00, sigma=0.125, tau=0.5) {

  ########### theta_bar_0 for pitchers ########### 
  {
    # prev_szn_avg_woba === pitcher's end-of-season avg. wOBA from the previous season
    pit_szns = E00 %>% group_by(YEAR, PIT_ID) %>%
      summarise(final_szn_woba = unique(WOBA_FINAL_PIT_19)) %>% 
      ungroup() %>%
      group_by(PIT_ID) %>%
      mutate(prev_szn_avg_woba = lag(final_szn_woba)) %>%
      ungroup() %>%
      arrange(PIT_ID,YEAR)
    pit_szns
    
    # prev_szn_avg_woba_allPit === for a given season y, the mean wOBA 
    szns = pit_szns %>% select(-c(final_szn_woba)) %>% drop_na() %>%
      group_by(YEAR) %>%
      summarise(prev_szn_avg_woba_allPit = median(prev_szn_avg_woba)) %>%
      ungroup()
    szns
    
    # insert prev_szn_avg_woba_allPit into pit_szns dataset 
    pit_szns1 = pit_szns %>% left_join(szns)
    pit_szns1
    
    # theta_bar_0 
    pit_szns2 = pit_szns1 %>% 
      mutate(pit_played_in_prev_szn = !is.na(prev_szn_avg_woba),
             theta_bar_0_pit = ifelse(pit_played_in_prev_szn, prev_szn_avg_woba, prev_szn_avg_woba_allPit)) 
    pit_szns2
    
    # theta_bar_0 dataframe
    theta_bar_0_df = pit_szns2 %>% select(YEAR,PIT_ID,theta_bar_0_pit)
    theta_bar_0_df
  }  

  E11 = E00 %>% 
    left_join(theta_bar_0_df) %>%
    filter(YEAR >= 2010) %>%
    group_by(YEAR, PIT_ID) %>% 
    mutate(
      top = (1/tau^2)*WOBA_AVG_PIT_19*(NUM_WOBA_APP_PIT-1) + (1/sigma^2)*theta_bar_0_pit,
      bottom = (NUM_WOBA_APP_PIT-1)/(tau^2) + (1/sigma^2),
      PQ = top/bottom # running avg. estimator
    ) %>%
    ungroup()
  
  R = E11 %>% select(-c(top,bottom))
  
  ### find examples of PIT_ID
  ######(R %>% filter(YEAR == 2019) %>% select(PIT_ID))$PIT_ID
    
  ### view korey kluber's running avg. estimator
  # ex1 = R %>% filter(PIT_ID == "klubc001" & YEAR == 2019) %>% 
  #   mutate(idx = row_number()) %>%
  #   select(row_idx,DATE,GAME_ID,INNING,PIT_ID,BAT_ID,EVENT_WOBA_19, #top,bottom,
  #          NUM_WOBA_APP_PIT,WOBA_AVG_PIT_19,theta_bar_0,PQ,WOBA_FINAL_PIT_19)
  # View(ex1)
  
  return(R)
}

####################################################
########### GET PITCHER & BATTER QUALITY ###########
####################################################

get_PQBQ <- function(full=TRUE, sigma=0.125, tau=0.5) {
  # get Pitcher Quality
  E1a = get_PQ(E00, sigma=sigma, tau=tau)
  
  # get Batter Quality by re-using the Pitcher Quality function
  E0b = E00 %>% select(row_idx,YEAR,GAME_ID,DATE,
                       BAT_ID, WOBA_FINAL_BAT_19, WOBA_AVG_BAT_19,
                       NUM_WOBA_APP_BAT, #NUM_WOBA_APP_FINAL_BAT,
                       WOBA_APP,EVENT_WOBA_19) 
  E00b = E0b %>% rename(
    PIT_ID = BAT_ID, 
    WOBA_FINAL_PIT_19 = WOBA_FINAL_BAT_19,
    WOBA_AVG_PIT_19 = WOBA_AVG_BAT_19,
    NUM_WOBA_APP_PIT = NUM_WOBA_APP_BAT
  )
  E1b = get_PQ(E00b, sigma=sigma, tau=tau) %>%
    rename(BQ = PQ,
           theta_bar_0_bat = theta_bar_0_pit,
           BAT_ID = PIT_ID,
           WOBA_FINAL_BAT_19 = WOBA_FINAL_PIT_19,
           WOBA_AVG_BAT_19 = WOBA_AVG_PIT_19,
           NUM_WOBA_APP_BAT = NUM_WOBA_APP_PIT) 
  X1 = E1a %>% left_join(E1b)
  # check columns with NA
  print("\n"); print(names(X1)[sapply(1:ncol(X1),fun <- function(i) {sum(is.na(X1[,i]))}) > 0]); print("\n");
  if (full) return(X1) else return(X1 %>% select(PQ,BQ))
}

X1 = get_PQBQ(full=TRUE, sigma=0.0357, tau=0.5) #sigma=0.0357
pb2 = get_PQBQ(full=FALSE, sigma=0.05, tau=0.5) %>% rename(BQ2=BQ, PQ2=PQ)
pb3 = get_PQBQ(full=FALSE, sigma=0.075, tau=0.5) %>% rename(BQ3=BQ, PQ3=PQ)
pb4 = get_PQBQ(full=FALSE, sigma=0.1, tau=0.5) %>% rename(BQ4=BQ, PQ4=PQ)
pb5 = get_PQBQ(full=FALSE, sigma=0.125, tau=0.5) %>% rename(BQ5=BQ, PQ5=PQ)
pb6 = get_PQBQ(full=FALSE, sigma=0.15, tau=0.5) %>% rename(BQ6=BQ, PQ6=PQ)
pb7 = get_PQBQ(full=FALSE, sigma=0.2, tau=0.5) %>% rename(BQ7=BQ, PQ7=PQ)
pb8 = get_PQBQ(full=FALSE, sigma=0.375, tau=0.5) %>% rename(BQ8=BQ, PQ8=PQ)
pb9 = get_PQBQ(full=FALSE, sigma=0.5, tau=0.5) %>% rename(BQ9=BQ, PQ9=PQ)
pb10 = get_PQBQ(full=FALSE, sigma=1, tau=0.5) %>% rename(BQ10=BQ, PQ10=PQ)

# sum(is.na(X1$BQ))
# which(is.na(X1$BQ))
# View(X1[(108696-10):(108696+10),])

# ex1p = X1 %>% filter(PIT_ID == "klubc001" & YEAR == 2019) %>%
#   mutate(idx = row_number()) %>%
#   select(row_idx,DATE,GAME_ID,INNING,PIT_ID,BAT_ID,EVENT_WOBA_19, #top,bottom,
#          NUM_WOBA_APP_PIT,WOBA_AVG_PIT_19,theta_bar_0_pit,PQ,WOBA_FINAL_PIT_19)
# View(ex1p)
# ex1b = X1 %>% filter(BAT_ID == "zobrb001" & YEAR == 2019) %>%
#   mutate(idx = row_number()) %>%
#   select(row_idx,DATE,GAME_ID,INNING,PIT_ID,BAT_ID,EVENT_WOBA_19, #top,bottom,
#          NUM_WOBA_APP_BAT,WOBA_AVG_BAT_19,theta_bar_0_bat,BQ,WOBA_FINAL_BAT_19)
# View(ex1b)

#############################################################################
#### get data ready for RSTAN
#### standardize the relevant columns, and remove unnecessary columns
#############################################################################

# keep relevant columns
X2 = bind_cols(X1,pb2,pb3,pb4,pb5,pb6,pb7,pb8,pb9,pb10) %>% 
          select(-c(row_idx, 
                      #PIT_ID, BAT_ID, WOBA_AVG_PIT_19, WOBA_AVG_BAT_19,
                      NUM_WOBA_APP_PIT, NUM_WOBA_APP_FINAL_PIT, 
                      NUM_WOBA_APP_BAT, NUM_WOBA_APP_FINAL_BAT))

# standardize the vector x to have mean 0 and s.d. 1/2
std <- function(x) {
  (x-mean(x))/(sd(x) * 2)
}

# standardize these columns
X3 = X2 %>% 
  group_by(YEAR) %>%
  mutate(std_EVENT_WOBA_19 = std(EVENT_WOBA_19),
         std_WOBA_FINAL_BAT_19 = std(WOBA_FINAL_BAT_19),
         std_WOBA_FINAL_PIT_19 = std(WOBA_FINAL_PIT_19),
         std_BQ = std(BQ),
         std_PQ = std(PQ),
         std_BQ2 = std(BQ2),
         std_PQ2 = std(PQ2),
         std_BQ3 = std(BQ3),
         std_PQ3 = std(PQ3),
         std_BQ4 = std(BQ4),
         std_PQ4 = std(PQ4),
         std_BQ5 = std(BQ5),
         std_PQ5 = std(PQ5),
         std_BQ6 = std(BQ6),
         std_PQ6 = std(PQ6),
         std_BQ7 = std(BQ7),
         std_PQ7 = std(PQ7),
         std_BQ8 = std(BQ8),
         std_PQ8 = std(PQ8),
         std_BQ9 = std(BQ9),
         std_PQ9 = std(PQ9),
         std_BQ10 = std(BQ10),
         std_PQ10 = std(PQ10),
         std_WOBA_CURR_BAT_19 = std(WOBA_AVG_BAT_19),
         std_WOBA_CURR_PIT_19 = std(WOBA_AVG_PIT_19)) %>%
  ungroup()

# hist checks
# hist(X3$std_PQ)
# hist(X3$std_BQ)


#### look at the trajectory of certain players, 
#### compare curr_avg_woba to BQ,PQ trajectories....
X3 %>% filter(BAT_ID == "suare001" & YEAR == 2019) %>% mutate(idx = row_number()) %>% ggplot() +
  geom_line(aes(x=idx, y=std_BQ), col="royalblue4") +
  geom_line(aes(x=idx, y=std_BQ2), col="royalblue3") +
  geom_line(aes(x=idx, y=std_BQ3), col="royalblue2") +
  geom_line(aes(x=idx, y=std_BQ4), col="royalblue1") +
  geom_line(aes(x=idx, y=std_BQ5), col="royalblue") +
  geom_line(aes(x=idx, y=std_BQ6), col="dodgerblue4") +
  geom_line(aes(x=idx, y=std_BQ7), col="dodgerblue3") +
  geom_line(aes(x=idx, y=std_BQ8), col="dodgerblue2") +
  geom_line(aes(x=idx, y=std_BQ9), col="dodgerblue1") +
  geom_line(aes(x=idx, y=std_BQ10), col="firebrick")+
  geom_line(aes(x=idx, y=std_WOBA_CURR_BAT_19), col="black", size=1.5) +
  geom_line(aes(x=idx, y=std_WOBA_FINAL_BAT_19), col="black", size=1.5)+
  ylim(c(-.75,0.75))

##suare001
##as_tibble(as.data.frame(table((X3 %>% filter(YEAR==2019))$BAT_ID))) %>% arrange(-Freq)
X3 %>% filter(BAT_ID == "zobrb001" & YEAR == 2019) %>% mutate(idx = row_number()) %>%
  #filter(row_number() <= 50) %>%
  # filter(row_number() >= 50) %>%
  ggplot() +
  geom_line(aes(x=idx, y=std_BQ), col="royalblue4") +
  geom_line(aes(x=idx, y=std_BQ2), col="royalblue3") +
  geom_line(aes(x=idx, y=std_BQ3), col="royalblue2") +
  geom_line(aes(x=idx, y=std_BQ4), col="royalblue1") +
  geom_line(aes(x=idx, y=std_BQ5), col="royalblue") +
  geom_line(aes(x=idx, y=std_BQ6), col="tomato4") +
  geom_line(aes(x=idx, y=std_BQ7), col="tomato3") +
  geom_line(aes(x=idx, y=std_BQ8), col="tomato2") +
  geom_line(aes(x=idx, y=std_BQ9), col="tomato1") +
  geom_line(aes(x=idx, y=std_BQ10), col="tomato")+
  geom_line(aes(x=idx, y=std_WOBA_CURR_BAT_19), col="black", size=1.5) +
  geom_line(aes(x=idx, y=std_WOBA_FINAL_BAT_19), col="black", size=1.5)+
  ylim(c(-1,0.75))



########### write csv ########### 
R = X3
write_csv(R, output_filename)
            



