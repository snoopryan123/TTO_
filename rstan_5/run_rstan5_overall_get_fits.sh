#!/bin/bash
for i in {7..48}
do
   ####echo "moving bsn $i"
   mv ./job_output/fit_rstan5-$i.R.rds    ../../Dropbox/HPCC/fit_rstan5-$i.R.rds  
   boxup
   mv ../../Dropbox/HPCC/fit_rstan5-$i.R.rds    ./job_output/fit_rstan5-$i.R.rds  
done