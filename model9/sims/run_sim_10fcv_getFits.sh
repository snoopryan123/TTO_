#!/bin/bash
for i in {1..3}
do
   ####echo "moving $i"
   mv ./job_output/fit_sim1_model${i}_1.rds    ../../../Dropbox/HPCC/fit_sim1_model${i}_1.rds
   mv ./job_output/fit_sim2_model${i}_1.rds    ../../../Dropbox/HPCC/fit_sim2_model${i}_1.rds
   boxup
   mv ../../../Dropbox/HPCC/fit_sim1_model${i}_1.rds   ./job_output/fit_sim1_model${i}_1.rds
   mv ../../../Dropbox/HPCC/fit_sim2_model${i}_1.rds   ./job_output/fit_sim2_model${i}_1.rds
done
