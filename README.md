
# Reproducing the results from _A Bayesian Analysis of the Time Through the Order Penalty_ (`TTO`):

### Get data:
* download `retro_final_PA_1990-2020d.csv` (which was compiled in `TTO_/data_wrangling`), and `TTO_dataset_410.csv` and `TTO_dataset_510.csv` (which were compiled in `TTO_/data/create_design_matrix4_10.R` and `TTO_/data/create_design_matrix5_10.R`), from `https://upenn.app.box.com/v/retrosheet-pa-1990-2000`, and store these datasets in`TTO_/data`

### Fit the bayesian model used in the `TTO` paper to observed data:
* run the Bayesian model (the `Stan` file `TTO_/model_positive_slope_prior/tto9_PF_BL_linePosSlope.stan`) for multiple years on a cluster via the array job `TTO_/model_positive_slope_prior/run_obs_fitModel_obs_AJ.sh`, which runs `TTO_/model_positive_slope_prior/obs_fitModel.R`
* then, run `TTO_/model_positive_slope_prior/results_obs.R` and `TTO_/model_positive_slope_prior/results_obs2.R` to obtain the plots and results from the fitted models
* same process but in the folder `TTO_/model_pitcher_varying_decay` to use the model with pitcher-varying slopes
* same process but in the folder `TTO_/model_indicators` to use the indicator model (indicator at each batter sequence number 1,...,27)
* simulation study in `TTO_/model_positive_slope_prior/sims` 
