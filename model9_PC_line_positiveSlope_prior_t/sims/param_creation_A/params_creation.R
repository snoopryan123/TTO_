library(tidyverse)
library(splines)
library(latex2exp)
theme_set(theme_bw())
theme_update(text = element_text(size=18))
theme_update(plot.title = element_text(hjust = 0.5))

### load data
input_file = "../../../data/TTO_dataset_510.csv"  
D <- read_csv(input_file)
D <- D %>% filter(YEAR == 2018) %>% filter(BQ>0 & BQ<1 & PQ>0 & PQ<1) %>% filter(ORDER_CT <= 3)
logit <- function(p) { log(p/(1-p)) }
categories = sort(unique(D$EVENT_WOBA_19))
num_categories = length(categories)
category_strings <- c("out","BB","HBP","1B","2B","3B","HR")

TOT_PITCHES = 108
# MAX_PITCHES = 200 #FIXME
### mean number of pitches per batter: 4
### equivalently, 108 pitches over 27 batters
### equivalently, 36 pitches over each TTO
# D %>% group_by(GAME_ID, PIT_ID) %>% 
#   mutate(num_pitches = diff(c(0,PITCH_COUNT_CUMU)) ) %>%
#   select(GAME_ID, PIT_ID, BAT_ID, INNING, PITCH_COUNT_CUMU, num_pitches) %>%
#   drop_na() %>% summarise(num_pitches = mean(num_pitches), .groups="drop") %>% 
#   summarise(num_pitches = mean(num_pitches))

######################################################################
### Explore a previous fit to find a reasonable line over b.s.n ####
######################################################################

fit <- readRDS("prev_fit_for_sim.rds") 
draws <- as.matrix(fit)

### alpha means: lined
### suppose a pitcher throws 100 pitches in a game
alpha_incpt_draws = draws[,str_detect(colnames(draws), "^alpha_incpt")]
# alpha_slope_draws = draws[,str_detect(colnames(draws), "^alpha_slope")]
alpha_incpt_draws_means = colMeans(alpha_incpt_draws)
# alpha_slope_draws_means = colMeans(alpha_slope_draws)
# alpha_slope_draws_means[2] = 0.001 # increase walk slope
# alpha_slope_draws_means[4] = 0.001 # increase single slope
alpha_tib = tibble()
alpha_tib_saveMe = tibble()
for (k in 1:7) {
  a0k = alpha_incpt_draws_means[endsWith(colnames(alpha_incpt_draws), paste0(k,"]"))]
  a1k = 0
  a_traj_k = unname(a0k + a1k*(1:TOT_PITCHES))
  alpha_tib_k = tibble(alpha = a_traj_k, k=k, c=category_strings[k], pc=1:TOT_PITCHES)
  alpha_tib = bind_rows(alpha_tib, alpha_tib_k)
  alpha_tib_saveMe = bind_rows(alpha_tib_saveMe,
    tibble(alpha_incpt = a0k, alpha_slope = a1k, k=k, c=category_strings[k]))
}
alpha_tib$alpha_line = alpha_tib$alpha
alpha_tib

# alpha_tib %>%
#   filter(k!=1) %>%
#   ggplot() +
#   facet_wrap(~c, scales="free")+
#   geom_point(aes(x=pc, y=alpha))

plot_true_alphaLine = alpha_tib %>%
  filter(k!=1) %>%
  ggplot(aes(x=pc, y=alpha_line)) +
  facet_wrap(~c, scales="free")+
  # geom_point(color="dodgerblue2", shape=21, size=2, stroke=1, fill="white") +
  geom_point(size=2) +
  geom_vline(aes(xintercept =  36), size=0.5, color="gray50") + #1.2
  geom_vline(aes(xintercept = 72), size=0.5, color="gray50") +
  ylab(TeX('simulated "true" $\\alpha$')) + 
  scale_x_continuous(name="pitch count", breaks=seq(0,108,12))
plot_true_alphaLine
ggsave(paste0("plot_sim", "_true_alphaLine_",".png"), plot_true_alphaLine, width=11, height=6)

### eta means
eta_draws = draws[,str_detect(colnames(draws), "^eta")]
eta_draws_means = colMeans(eta_draws)
eta_tib = tibble()
for (k in 1:7) {
  eta_means_k = eta_draws_means[endsWith(colnames(eta_draws), paste0(k,"]"))]
  eta_tib_k = tibble(eta = eta_means_k, k=k, c=category_strings[k], p=1:4)
  eta_tib = bind_rows(eta_tib, eta_tib_k)
}
eta_tib

# eta_tib %>%
#   filter(k!=1) %>%
#   ggplot() +
#   facet_wrap(~c, scales="free")+
#   geom_point(aes(x=p, y=eta))

### create betas
print(category_strings)
beta_tib = bind_rows(
  tibble(beta_2 = 0, beta_3 = 0, k = 1:7, sim_num = 1),
  tibble(k = 1:7, sim_num = 2,
   beta_3 = c(0, 0.1, 0, 0.2, 0.1, 0, 0.1)/1.5, 
   beta_2 = c(0, 0.1, 0, 0.2, 0.1, 0, 0.1)/3.25
  ),
  tibble(k = 1:7, sim_num = 3,
         # beta_3 = c(0, 0.1, 0, 0.2, 0.1, 0, 0.1), 
         beta_3 = c(0, 0.1, 0.1, 0.3, 0.1, 0.1, 0.15), 
         beta_2 = 0
  )
) %>% mutate(c = category_strings[k])
beta_tib

### save simulation parameters
write_csv(alpha_tib_saveMe, "params_sim_alpha.csv")
write_csv(eta_tib, "params_sim_eta.csv")
write_csv(beta_tib, "params_sim_beta.csv")

### corresponding probability means
# SIM_NUM = 1 ### no TTOP batter learning bumps
# SIM_NUM = 2 ### TTOP batter learning bumps consistent with Tango's effect size
# SIM_NUM = 3 ### absurdly huge 3TTOP batter learning bump
for (SIM_NUM in c(1,2,3)) {
  x_tilde = c(logit(0.315), logit(0.315), 1, 0)
  raw_p = tibble()
  for (kk in 1:7) {
    ### no line
    # alpha_k = (alpha_tib %>% filter(k == kk))$alpha
    ### yes line
    tt = TOT_PITCHES/3
    alpha_k = (alpha_tib %>% filter(k == kk))$alpha_line
    eta_k = (eta_tib %>% filter(k == kk))$eta
    beta_2k = (beta_tib %>% filter(k==kk & sim_num==SIM_NUM))$beta_2
    beta_3k = (beta_tib %>% filter(k==kk & sim_num==SIM_NUM))$beta_3
    beta_bumps = beta_2k * c(rep(0,tt), rep(1,tt), rep(0,tt)) +
                 beta_3k * c(rep(0,tt), rep(0,tt), rep(1,tt))
    raw_p_k = unname(exp(alpha_k + 
                         pracma::dot(x_tilde, eta_k) +
                         beta_bumps))
    raw_p = bind_rows(raw_p, 
              tibble(p = raw_p_k, k=kk, c=category_strings[kk], pc=1:TOT_PITCHES)
            )
  }
  p_tib = raw_p %>% group_by(pc) %>% mutate(p = p/sum(p)) %>% ungroup()
  
  plot_true_p = p_tib %>%
    filter(k!=1) %>%
    ggplot(aes(x=pc, y=p)) +
    facet_wrap(~c, scales="free")+
    # geom_point(color="dodgerblue2", shape=21, size=2, stroke=1, fill="white") +
    geom_point(size=2) +
    geom_vline(aes(xintercept =  36), size=0.5, color="gray50") + #1.2
    geom_vline(aes(xintercept = 72), size=0.5, color="gray50") +
    ylab('simulated "true" probability') + 
    scale_x_continuous(name="pitch count", breaks=seq(0,TOT_PITCHES,12))
  plot_true_p
  ggsave(paste0("plot_sim", SIM_NUM, "_true_p_",".png"), plot_true_p, width=11, height=6)
  
  ### corresponding xWOBA means
  xw_tib = p_tib %>%
    mutate(w = categories[k]) %>%
    mutate(xw = p*w) %>%
    group_by(pc) %>%
    summarise(xw = sum(xw)*1000)
  
  plot_true_xw = xw_tib %>%
    ggplot(aes(x=pc, y=xw)) +
    # geom_point(color="dodgerblue2", shape=21, size=2, stroke=1, fill="white") +
    geom_point(size=2) +
    geom_vline(aes(xintercept =  36), size=0.5, color="gray50") + #1.2
    geom_vline(aes(xintercept = 72), size=0.5, color="gray50") +
    ylab('simulated "true" expected wOBA') + 
    scale_x_continuous(name="pitch count", breaks=seq(0,TOT_PITCHES,12))
  plot_true_xw
  ggsave(paste0("plot_sim", SIM_NUM, "_true_xw_",".png"), plot_true_xw, width=7, height=6)
}













