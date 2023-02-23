
source("A_getData.R") ### get observed data 

##############################################################

###
df.exits.0 = D0 %>%
  group_by(GAME_ID, PIT_ID) %>%
  mutate(
    last = row_number() == n(),
    woba_prev = ifelse(last, EVENT_WOBA_19[n()], NA),
    woba_prev2 = ifelse(last, EVENT_WOBA_19[max(n()-1, 1)], NA),
    woba_prev3 = ifelse(last, EVENT_WOBA_19[max(n()-2, 1)], NA),
    woba_gameMean = mean(EVENT_WOBA_19, na.rm=TRUE),
    pit_exit_idx = BATTER_SEQ_NUM[n()] + 1,
  ) %>%
  ungroup() %>%
  mutate(
    woba_prev123 = (woba_prev+woba_prev2+woba_prev3)/3
  )
### check
# A = df.exits.0 %>% select(GAME_ID, PIT_ID, EVENT_WOBA_19, BATTER_SEQ_NUM, last, pit_exit_idx, 
#                           woba_prev, woba_prev123, woba_gameMean)
# View(A)

df.exits = df.exits.0 %>%
  group_by(GAME_ID, PIT_ID) %>%
  summarise(
    YEAR = YEAR[n()],
    pq = PQ[n()],
    woba_prev = woba_prev[n()],
    woba_prev123 = woba_prev123[n()],
    woba_gameMean = woba_gameMean[n()],
    pit_exit_idx = pit_exit_idx[n()]
  ) %>%
  ungroup() %>%
  mutate(
    tto = floor((pit_exit_idx-1)/9)+1,
    pq_bin = cut(pq, quantile(pq,0:6/6)),
    woba_prev_bin = factor(woba_prev),
    woba_prev123_bin = cut(woba_prev123, quantile(woba_prev123, 0:3/3)),
    woba_gameMean_bin = cut(woba_gameMean, quantile(woba_gameMean,0:6/6)),
    woba_gameMean_bin2 = cut(woba_gameMean, quantile(woba_gameMean,0:16/16)),
  ) %>%
  filter(tto <= 3)
df.exits

##############################################################

p1 = df.exits %>%
  # filter(YEAR==2018) %>%
  ggplot() +
  geom_histogram(aes(x=pit_exit_idx), binwidth = 1) +
  scale_x_continuous(breaks=seq(3,40,by=3)) +
  geom_vline(aes(xintercept=9), linewidth=1) +
  geom_vline(aes(xintercept=18), linewidth=1) +
  xlab("batter number t at which the starting pitcher exits")
# p1
ggsave("selection_bias_explore_plot_1.png", p1, width=10, height=4)

p2 = df.exits %>%
  drop_na(pq_bin) %>%
  mutate(pq_bin = paste0("pitcher quality bin ", pq_bin)) %>%
  ggplot() +
  facet_wrap(~pq_bin) +
  geom_histogram(aes(x=pit_exit_idx), binwidth = 1) +
  scale_x_continuous(breaks=seq(3,40,by=3)) +
  geom_vline(aes(xintercept=9), linewidth=1) +
  geom_vline(aes(xintercept=18), linewidth=1) +
  xlab("batter number t at which the starting pitcher exits")
# p2
ggsave("selection_bias_explore_plot_2.png", p2, width=15, height=5)

p3 = df.exits %>%
  drop_na(woba_prev_bin) %>%
  mutate(woba_prev_bin = paste0("previous at-bat wOBA ", woba_prev_bin)) %>%
  ggplot() +
  facet_wrap(~woba_prev_bin) +
  geom_histogram(aes(x=pit_exit_idx), binwidth = 1) +
  scale_x_continuous(breaks=seq(3,40,by=3)) +
  geom_vline(aes(xintercept=9), linewidth=1) +
  geom_vline(aes(xintercept=18), linewidth=1) +
  xlab("batter number t at which the starting pitcher exits")
# p3
ggsave("selection_bias_explore_plot_3.png", p3, width=15, height=8)

p4 = df.exits %>%
  drop_na(woba_prev123_bin) %>%
  mutate(woba_prev123_bin = paste0("mean wOBA of previous 3 at-bats ", woba_prev123_bin)) %>%
  ggplot() +
  facet_wrap(~woba_prev123_bin) +
  geom_histogram(aes(x=pit_exit_idx), binwidth = 1) +
  scale_x_continuous(breaks=seq(3,40,by=3)) +
  geom_vline(aes(xintercept=9), linewidth=1) +
  geom_vline(aes(xintercept=18), linewidth=1) +
  xlab("batter number t at which the starting pitcher exits")
# p4
ggsave("selection_bias_explore_plot_4.png", p4, width=15, height=5)

p5 = df.exits %>%
  drop_na(woba_gameMean_bin) %>%
  mutate(woba_gameMean_bin = paste0("mean game wOBA ", woba_gameMean_bin)) %>%
  ggplot() +
  facet_wrap(~woba_gameMean_bin) +
  geom_histogram(aes(x=pit_exit_idx), binwidth = 1) +
  scale_x_continuous(breaks=seq(3,40,by=3)) +
  geom_vline(aes(xintercept=9), linewidth=1) +
  geom_vline(aes(xintercept=18), linewidth=1) +
  xlab("batter number t at which the starting pitcher exits")
# p5
ggsave("selection_bias_explore_plot_5.png", p5, width=15, height=5)

p6 = df.exits %>%
  drop_na(woba_gameMean_bin2) %>%
  mutate(woba_gameMean_bin2 = paste0("mean game wOBA ", woba_gameMean_bin2)) %>%
  ggplot() +
  facet_wrap(~woba_gameMean_bin2) +
  geom_histogram(aes(x=pit_exit_idx), binwidth = 1) +
  scale_x_continuous(breaks=seq(3,40,by=3)) +
  geom_vline(aes(xintercept=9), linewidth=1) +
  geom_vline(aes(xintercept=18), linewidth=1) +
  xlab("batter number t at which the starting pitcher exits")
# p6
ggsave("selection_bias_explore_plot_6.png", p6, width=15, height=10)




