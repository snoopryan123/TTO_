### understanding the code flow of the simulation
0. Find the "true" parameters, in `../param_creation`
1. `sim-x.R`  (x=1,...,25)
2. `sim_main.R`
3. `sim_config.R`  
set `SIM_NUM` (1 for no BL, 2 for yes BL)
set `YRS`, the years for the observed data matrices S,O,X
4. `../model9_getData.R`  
5. `sim_simulateData.R`  
6. `sim_fitModel.R`  
7. `../model9_importRstan.R`
8. `tto9_bsnBL.stan  