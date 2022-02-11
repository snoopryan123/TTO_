library(tidyverse)
library(fitdistrplus)


# ### load data
# input_file = "./data/TTO_dataset_510.csv"  
# D <- read_csv(input_file)
# 
# E <- D[,!startsWith(names(D), "std")] %>% 
#   select(-c(BQ,PQ,BQ2,PQ2)) %>%
#   filter(YEAR==2019)
# 
# OUTPUT_FILE = "data/2019_exploratory_mlb_data.csv"
# write_csv(E, OUTPUT_FILE)

D <- read_csv("data/2019_exploratory_mlb_data.csv")
D <- D %>% group_by(PIT_ID,GAME_ID) %>% mutate(num_games = n()) %>% ungroup()
D <- D %>% filter(num_games > 10)

### event woba histogram
D %>% #filter(EVENT_WOBA_19 >0) %>% 
  ggplot() + 
  geom_histogram(aes(x=EVENT_WOBA_19, y=..density..),
                 bins=100,colour = "black",fill="white") 

# 2019 ONLY
# pitcher
by_pit = D %>% group_by(PIT_ID) %>%
  summarise(mean_woba = mean(EVENT_WOBA_19))
by_pit

fit.gamma = fitdistrplus::fitdist(by_pit$mean_woba, distr="gamma", method="mle")
fit.gamma
fit.weib = fitdistrplus::fitdist(by_pit$mean_woba, distr="weibull", method="mle")
fit.weib
fit.cauchy = fitdistrplus::fitdist(by_pit$mean_woba, distr="cauchy", method="mle")
fit.cauchy
fit.normal = MASS::fitdistr(by_pit$mean_woba, "normal")
fit.normal

by_pit %>% ggplot() + 
  geom_histogram(aes(x=mean_woba, y=..density..),
                 bins=100,colour = "black",fill="white") +
  stat_function(fun = dnorm, size=1, aes(colour = "normal"),
                args = list(mean = fit.normal$estimate[[1]], 
                            sd = fit.normal$estimate[[2]])) +
  stat_function(fun = dgamma,size=1, aes(colour = "gamma"),
                args = list(shape = fit.gamma$estimate[[1]], 
                            rate = fit.gamma$estimate[[2]])) +
  stat_function(fun = dcauchy,size=1, aes(colour = "cauchy"),
                args = list(location = fit.cauchy$estimate[[1]], 
                            scale = fit.cauchy$estimate[[2]])) +
  stat_function(fun = dweibull,size=1, aes(colour = "weibull"),
                args = list(shape = fit.weib$estimate[[1]], 
                            scale = fit.weib$estimate[[2]])) +
  scale_colour_manual("Distributions", 
                      values = c("firebrick","dodgerblue", "darkgreen", "orange"))

# pitcher-game
by_pit_game = D %>% group_by(PIT_ID,GAME_ID) %>%
  summarise(mean_woba = mean(EVENT_WOBA_19)) %>%
  ungroup() %>% group_by(PIT_ID) %>%
  mutate(num_games = n()) %>% ungroup()
unique((by_pit_game %>% filter(num_games>30))$PIT_ID)

by_pit_game[by_pit_game$PIT_ID %in% c("verlj001","waina001","lestj001","coleg001"),] %>%
  ggplot() + 
  facet_wrap(~PIT_ID) + 
  #geom_density(aes(x=mean_woba))
  geom_histogram(aes(x=mean_woba),bins=70) 
  
  





