#!/bin/bash
for i in {1..4}
do
   ####echo "moving bsn $i"
   mv ./job_output/fit_rstan5_overall-$i.R.rds    ../../Dropbox/HPCC/fit_rstan5_overall-$i.R.rds  
   boxup
   mv ../../Dropbox/HPCC/fit_rstan5_overall-$i.R.rds    ./job_output/fit_rstan5_overall-$i.R.rds  
done