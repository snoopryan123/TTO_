#!/bin/bash
for i in {1..25}
do
   ####echo "moving bsn $i"
   mv ./job_output/fit_rstan3_sim1_bsn-$i.R.rds    ../../Dropbox/HPCC/fit_rstan3_sim1_bsn-$i.R.rds
   boxup
   mv ../../Dropbox/HPCC/fit_rstan3_sim1_bsn-$i.R.rds   ./job_output/fit_rstan3_sim1_bsn-$i.R.rds
done