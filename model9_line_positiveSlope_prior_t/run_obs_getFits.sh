#!/bin/bash
for i in {12..19}
do
   ####echo "moving $i"
   mv ./job_output/fit_obs_model_lineyrs_${i}_.rds           ../../Dropbox/HPCC/fit_obs_model_lineyrs_${i}_.rds
   boxup
   mv ../../Dropbox/HPCC/fit_obs_model_lineyrs_${i}_.rds  ./job_output/fit_obs_model_lineyrs_${i}_.rds   
done


