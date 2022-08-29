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


######################################################################
### Explore a previous fit to find a reasonable spline over b.s.n ####
######################################################################

fit <- readRDS("prev_fit_for_sim.rds")
draws <- as.matrix(fit)

### alpha means
alpha_draws = draws[,str_detect(colnames(draws), "^alpha")]
alpha_draws_means = colMeans(alpha_draws)
alpha_tib = tibble()
for (k in 1:7) {
  alpha_means_k = alpha_draws_means[endsWith(colnames(alpha_draws), paste0(k,"]"))][1:27]
  alpha_tib_k = tibble(alpha = unname(alpha_means_k), k=k, c=category_strings[k], t=1:27)
  alpha_tib = bind_rows(alpha_tib, alpha_tib_k)
}
alpha_tib

alpha_tib %>%
  filter(k!=1) %>%
  ggplot() +
  facet_wrap(~c, scales="free")+
  geom_point(aes(x=t, y=alpha))

### alpha means: splined
alpha_spl = numeric()
for (kk in 1:7) {
  # alpha_spl_k0 = t(bbb) %*% matrix((alpha_tib %>% filter(k==kk))$alpha)
  # alpha_spl_k = as.numeric( t(alpha_spl_k0) %*% t(bbb) )
  alpha_spl_k = smooth.spline( (alpha_tib %>% filter(k==kk))$alpha, 
                               df=3)$y
  alpha_spl = c(alpha_spl, alpha_spl_k)
}
alpha_tib$alpha_spl = alpha_spl

plot_true_alphaSpl = alpha_tib %>%
  filter(k!=1) %>%
  ggplot(aes(x=t, y=alpha_spl)) +
  facet_wrap(~c, scales="free")+
  # geom_point(color="dodgerblue2", shape=21, size=2, stroke=1, fill="white") +
  geom_point(size=2) +
  geom_vline(aes(xintercept =  9), size=0.5, color="gray50") + #1.2
  geom_vline(aes(xintercept = 18), size=0.5, color="gray50") +
  ylab(TeX('simulated "true" $\\alpha$')) + 
  scale_x_continuous(name="batter sequence number, t", breaks=seq(0,27,3))
plot_true_alphaSpl
ggsave(paste0("plot_sim", "_true_alphaSpl_",".png"), plot_true_alphaSpl, width=11, height=6)

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
    beta_3 = c(0, 0, 0, 0.2, 0.1, 0, 0.1)/1.5, 
    beta_2 = c(0, 0, 0, 0.2, 0.1, 0, 0.1)/3.25
  )
) %>% mutate(c = category_strings[k])
beta_tib

### save simulation parameters
write_csv(alpha_tib %>% select(-alpha), "params_sim_alpha.csv")
write_csv(eta_tib, "params_sim_eta.csv")
write_csv(beta_tib, "params_sim_beta.csv")

### corresponding probability means
# SIM_NUM = 1 ### no TTOP batter learning bumps
# SIM_NUM = 2 ### yes TTOP batter learning bumps
for (SIM_NUM in c(1,2)) {
  x_tilde = c(logit(0.315), logit(0.315), 1, 0)
  raw_p = tibble()
  for (kk in 1:7) {
    ### no spl
    # alpha_k = (alpha_tib %>% filter(k == kk))$alpha
    ### yes spl
    alpha_k = (alpha_tib %>% filter(k == kk))$alpha_spl
    eta_k = (eta_tib %>% filter(k == kk))$eta
    beta_2k = (beta_tib %>% filter(k==kk & sim_num==SIM_NUM))$beta_2
    beta_3k = (beta_tib %>% filter(k==kk & sim_num==SIM_NUM))$beta_3
    beta_bumps = beta_2k * c(rep(0,9), rep(1,9), rep(0,9)) +
                 beta_3k * c(rep(0,9), rep(0,9), rep(1,9))
    raw_p_k = unname(exp(alpha_k + 
                         pracma::dot(x_tilde, eta_k) +
                         beta_bumps))
    raw_p = bind_rows(raw_p, 
              tibble(p = raw_p_k, k=kk, c=category_strings[kk], t=1:27)
            )
  }
  p_tib = raw_p %>% group_by(t) %>% mutate(p = p/sum(p)) %>% ungroup()
  
  plot_true_p = p_tib %>%
    filter(k!=1) %>%
    ggplot(aes(x=t, y=p)) +
    facet_wrap(~c, scales="free")+
    # geom_point(color="dodgerblue2", shape=21, size=2, stroke=1, fill="white") +
    geom_point(size=2) +
    geom_vline(aes(xintercept =  9), size=0.5, color="gray50") + #1.2
    geom_vline(aes(xintercept = 18), size=0.5, color="gray50") +
    ylab('simulated "true" probability') + 
    scale_x_continuous(name="batter sequence number, t", breaks=seq(0,27,3))
  plot_true_p
  ggsave(paste0("plot_sim", SIM_NUM, "_true_p_",".png"), plot_true_p, width=11, height=6)
  
  ### corresponding xWOBA means
  xw_tib = p_tib %>%
    mutate(w = categories[k]) %>%
    mutate(xw = p*w) %>%
    group_by(t) %>%
    summarise(xw = sum(xw)*1000)
  
  plot_true_xw = xw_tib %>%
    ggplot(aes(x=t, y=xw)) +
    # geom_point(color="dodgerblue2", shape=21, size=2, stroke=1, fill="white") +
    geom_point(size=2) +
    geom_vline(aes(xintercept =  9), size=0.5, color="gray50") + #1.2
    geom_vline(aes(xintercept = 18), size=0.5, color="gray50") +
    ylab('simulated "true" expected wOBA') + 
    scale_x_continuous(name="batter sequence number, t", breaks=seq(0,27,3))
  plot_true_xw
  ggsave(paste0("plot_sim", SIM_NUM, "_true_xw_",".png"), plot_true_xw, width=7, height=6)
}













