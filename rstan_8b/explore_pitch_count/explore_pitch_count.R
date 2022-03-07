library(tidyverse)
# theme_set(theme_bw())
# theme_update(text = element_text(size=18))
# theme_update(plot.title = element_text(hjust = 0.5))

### load data
input_file = "../data/TTO_dataset_510.csv"  
D <- read_csv(input_file) #%>% drop_na() 
D <- D %>% filter(YEAR == 2019) %>% filter(BQ>0 & BQ<1 & PQ>0 & PQ<1 )
logit <- function(p) { log(p/(1-p)) }
X <- as.matrix(D %>% mutate(lBQ=logit(BQ), lPQ=logit(PQ)) %>% select(lBQ, lPQ, HAND_MATCH, BAT_HOME_IND)) 
# source("rstan8_main.R")

########################
### PITCH COUNT EDA ####
########################

D1 = D %>% group_by(GAME_ID,BAT_HOME_IND) %>% mutate(PITCH_CT = lag(PITCH_COUNT_CUMU,default=0)) %>% ungroup()
View( D1 %>% select(YEAR, DATE,GAME_ID,INNING, PIT_ID, BAT_ID, PITCH_COUNT_CUMU, PITCH_CT,
                    BATTER_SEQ_NUM, BATTER_IDX))

epc1 = D1 %>% group_by(BATTER_SEQ_NUM) %>% summarise(w = mean(EVENT_WOBA_19), p = mean(PITCH_CT)) %>%
  filter(row_number() <= 27) %>%
  ggplot(aes(x=p,y=w)) + geom_point() + geom_line() +
  xlab("mean pitch count") +
  ylab("mean woba")
epc1
# ggsave("woba_vs_pitchCount.png", epc1)

epc2 = D1 %>% group_by(BATTER_SEQ_NUM) %>% summarise(w = mean(EVENT_WOBA_19)) %>%
  filter(row_number() <= 27) %>%
  ggplot(aes(x=BATTER_SEQ_NUM,y=w)) + geom_point() + geom_line() +
  xlab("batter sequence number") +
  ylab("mean woba")
epc2
# ggsave("woba_vs_bsn.png", epc2)

epc3 = D1 %>% group_by(BATTER_SEQ_NUM) %>% summarise(p = mean(PITCH_CT)) %>%
  filter(row_number() <= 27) %>%
  ggplot(aes(x=BATTER_SEQ_NUM,y=p)) + geom_point() + geom_line() +
  xlab("batter sequence number") +
  ylab("mean pitch count")
epc3
# ggsave("pitchCount_vs_bsn.png", epc3)



