#!/bin/bash
for i in {1..20}
do
   ####echo "moving $i"
   mv ./job_output/fit_rstan8_comp-$i.R.rds    ../../../Dropbox/HPCC/fit_rstan8_comp-$i.R.rds
   boxup
   mv ../../../Dropbox/HPCC/fit_rstan8_comp-$i.R.rds   ./job_output/fit_rstan8_comp-$i.R.rds
done