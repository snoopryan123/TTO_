library(tidyverse)
output_folder = './' #'./job_output/'

### load data
input_file = "../../data/TTO_dataset_411.csv"  
D <- read_csv(input_file)
D <- D %>% drop_na() 
#FIXME 
D <- D %>% filter(YEAR == 2019) 
X <- as.matrix(D %>% select(std_WOBA_FINAL_BAT_19, std_WOBA_FINAL_PIT_19, HAND_MATCH, BAT_HOME_IND)) 
OUTPUT_FILE = "rstan5_plots-1.R"
fit <- readRDS("../job_output/fit_rstan5_overall-1.R.rds") 

source("rstan5_plots_main.R")

#############################
########### PLOTS ###########
#############################

# plot full trajectory 
p = plot_bsn0(fit)
p
ggsave(paste0("./plot_",OUTPUT_FILE,".png"), p)

# plot 3 panel histogram B1->B2, B9->B10, B18->B19
p1 = plot_3hist_bsn1(fit)
p1
ggsave(paste0("./plot_3hist1_",OUTPUT_FILE,".png"), p1)




draws <- as_tibble(as.matrix(fit))
draws <- as_tibble(transform_back(draws))
tto1 = draws[paste0("alpha[",1:9,"]")]
tto2 = draws[paste0("alpha[",10:18,"]")]
tto3 = draws[paste0("alpha[",19:27,"]")]
tto1_means = tto1 %>% mutate(tto_means = rowMeans(.)) %>% select(tto_means)
tto2_means = tto2 %>% mutate(tto_means = rowMeans(.)) %>% select(tto_means)
tto3_means = tto3 %>% mutate(tto_means = rowMeans(.)) %>% select(tto_means)
tto1_means$tto = "1"
tto2_means$tto = "2"
tto3_means$tto = "3"
tto_means = bind_rows(tto1_means,tto2_means,tto3_means)
# labs <- c("first TTO",
#           "second TTO",
#           "third TTO")
labs <- c(bquote(paste("1st TTO: mean posterior dist. of {", alpha[k], ": k=1,...,9}"  )), 
          bquote(paste("2nd TTO: mean posterior dist. of {", alpha[k], ": k=10,...,18}"  )), 
          bquote(paste("3rd TTO: mean posterior dist. of {", alpha[k], ": k=19,...,27}"  )) )
p2 = tto_means %>% 
  ggplot(aes(tto_means, fill = tto)) + 
  geom_density(alpha = 0.2) +
  scale_fill_discrete(labels=labs, name="") +
  theme(#axis.title.y=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks.y=element_blank()) +
  xlab("") +
  #labs(title = "")
  labs(title=bquote(paste("Comparing the mean ", alpha, "posterior distributions in the \n first, second, and third times through the batting order")))
p2





