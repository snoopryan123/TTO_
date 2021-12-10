#!/bin/bash
for i in {1..25}
do
   ####echo "moving ubi $i"
   mv ./job_output/fit_rstan3_sim1_ubi-$i.R.rds    ../../Dropbox/HPCC/fit_rstan3_sim1_ubi-$i.R.rds
   mv ./job_output/epsilon_rstan3_sim1_ubi-$i.R.rds    ../../Dropbox/HPCC/epsilon_rstan3_sim1_ubi-$i.R.rds
   mv ./job_output/y_rstan3_sim1_ubi-$i.R.rds    ../../Dropbox/HPCC/y_rstan3_sim1_ubi-$i.R.rds
   boxup
   mv ../../Dropbox/HPCC/fit_rstan3_sim1_ubi-$i.R.rds   ./job_output/fit_rstan3_sim1_ubi-$i.R.rds
   mv ../../Dropbox/HPCC/epsilon_rstan3_sim1_ubi-$i.R.rds   ./job_output/epsilon_rstan3_sim1_ubi-$i.R.rds
   mv ../../Dropbox/HPCC/y_rstan3_sim1_ubi-$i.R.rds   ./job_output/y_rstan3_sim1_ubi-$i.R.rds
done