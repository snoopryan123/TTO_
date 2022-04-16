#!/bin/bash
for i in {1..1}
do
   mv ./job_output/fit_rstan1-$i.R.rds    ../../../Dropbox/HPCC/fit_rstan1-$i.R.rds  
   boxup
   mv ../../../Dropbox/HPCC/fit_rstan1-$i.R.rds    ./job_output/fit_rstan1-$i.R.rds  
done