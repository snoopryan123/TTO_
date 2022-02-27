# TTO_
code for the "time through the order (TTO)" project,
including dataset creation and fully bayesian regression

./data_wrangling/   - scrape and clean Retrosheet data
./data/             - dataset to put into rstan

Good:
rstan_6 logistic regression for OBP
rstan_8a multinomial logistic regression for wOBA


Bad:
rstan_5 normal linear regression for wOBA
rstan_7a ordinal logistic regression for wOBA