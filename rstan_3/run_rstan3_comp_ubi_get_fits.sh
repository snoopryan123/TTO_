#!/bin/bash
for i in {1..10}
do
   echo "moving bsn $i"
   mv ./job_output/fit_rstan3_comp_ubi-$i.R.rds    ../../Dropbox/HPCC/fit_rstan3_comp_ubi-$i.R.rds
   boxup
   mv ../../Dropbox/HPCC/fit_rstan3_comp_ubi-$i.R.rds   ./job_output/fit_rstan3_comp_ubi-$i.R.rds
done