#!/bin/bash
for i in {4..7}
do
   ####echo "moving $i"
   mv ./job_output/fit_obs_model_SingleSplOnlyyrs_18_df${i}_.rds           ../../Dropbox/HPCC/fit_obs_model_SingleSplOnlyyrs_18_df${i}_.rds
   boxup
   mv ../../Dropbox/HPCC/fit_obs_model_SingleSplOnlyyrs_18_df${i}_.rds  ./job_output/fit_obs_model_SingleSplOnlyyrs_18_df${i}_.rds    
done

