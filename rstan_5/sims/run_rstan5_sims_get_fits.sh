#!/bin/bash
for i in {1..115}
do
   ####echo "moving bsn $i"
   mv ./job_output/fit_rstan5_sim1-$i.R.rds    ../../../Dropbox/HPCC/fit_rstan5_sim1-$i.R.rds
   mv ./job_output/epsilon_rstan5_sim1-$i.R.rds    ../../../Dropbox/HPCC/epsilon_rstan5_sim1-$i.R.rds
   mv ./job_output/y_rstan5_sim1-$i.R.rds    ../../../Dropbox/HPCC/y_rstan5_sim1-$i.R.rds
   boxup
   mv ../../../Dropbox/HPCC/fit_rstan5_sim1-$i.R.rds   ./job_output/fit_rstan5_sim1-$i.R.rds
   mv ../../../Dropbox/HPCC/epsilon_rstan5_sim1-$i.R.rds   ./job_output/epsilon_rstan5_sim1-$i.R.rds
   mv ../../../Dropbox/HPCC/y_rstan5_sim1-$i.R.rds   ./job_output/y_rstan5_sim1-$i.R.rds
done