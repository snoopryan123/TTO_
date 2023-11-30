# TTO_
code for the `time through the order penalty (TTOP)` project,
including dataset creation and fully bayesian regression.

The final model used in the paper with positive slope prior is `model_positive_slope_prior/tto9_PF_BL_linePosSlope.stan`.  

Other stan files are also in the github, for example pitcher varying decay slopes in `model_pitcher_varying_decay/tto11.stan`.

The raw dataset used is `retro_final_PA_1990-2020d.csv` at https://upenn.app.box.com/v/retrosheet-pa-1990-2000.
Run `data/create_design_matrix4_10.R` and `data/create_design_matrix5_10.R` to create the dataset `TTO_dataset_510.csv` that is used to fit the models.
