#!/bin/bash
for i in {1..25}
do
   ####echo "moving $i"
   mv ./job_output/fit_sim_model_bsnBL_$i.rds    ../../../Dropbox/HPCC/fit_sim_model_bsnBL_$i.rds
   mv ./job_output/y_sim_model_bsnBL_$i.rds    ../../../Dropbox/HPCC/y_sim_model_bsnBL_$i.rds 
   boxup
   mv ../../../Dropbox/HPCC/fit_sim_model_bsnBL_$i.rds   ./job_output/fit_sim_model_bsnBL_$i.rds
   mv ../../../Dropbox/HPCC/y_sim_model_bsnBL_$i.rds    ./job_output/y_sim_model_bsnBL_$i.rds 
done

