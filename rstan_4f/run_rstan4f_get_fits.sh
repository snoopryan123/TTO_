#!/bin/bash
for i in {1..61}
do
   ####echo "moving bsn $i"
   mv ./job_output/fit_rstan4f-$i.R.rds    ../../Dropbox/HPCC/fit_rstan4f-$i.R.rds  
   boxup
   mv ../../Dropbox/HPCC/fit_rstan4f-$i.R.rds    ./job_output/fit_rstan4f-$i.R.rds  
done