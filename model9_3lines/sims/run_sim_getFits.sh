#!/bin/bash
for i in {1..10}
do
   ####echo "moving $i"
   mv ./job_output/fit_sim1_model_bsnBL_$i.rds    ../../../Dropbox/HPCC/fit_sim1_model_bsnBL_$i.rds
   mv ./job_output/fit_sim2_model_bsnBL_$i.rds    ../../../Dropbox/HPCC/fit_sim2_model_bsnBL_$i.rds
   boxup
   mv ../../../Dropbox/HPCC/fit_sim1_model_bsnBL_$i.rds   ./job_output/fit_sim1_model_bsnBL_$i.rds
   mv ../../../Dropbox/HPCC/fit_sim2_model_bsnBL_$i.rds   ./job_output/fit_sim2_model_bsnBL_$i.rds
done
