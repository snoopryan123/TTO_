#!/bin/bash
for i in {1..25}
do
   ####echo "moving bsn $i"
   mv ./job_output/fit_rstan8_sim-$i.R.rds    ../../../Dropbox/HPCC/fit_rstan8_sim-$i.R.rds
   mv ./job_output/y_rstan8_sim-$i.R.rds    ../../../Dropbox/HPCC/y_rstan8_sim-$i.R.rds
   boxup
   mv ../../../Dropbox/HPCC/fit_rstan8_sim-$i.R.rds   ./job_output/fit_rstan8_sim-$i.R.rds
   mv ../../../Dropbox/HPCC/y_rstan8_sim-$i.R.rds   ./job_output/y_rstan8_sim-$i.R.rds
done