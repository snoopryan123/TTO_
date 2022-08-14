#!/bin/bash
for i in {1..10}
do
   ####echo "moving $i"
   mv ./job_output/fit_rstan8_comp-$i_1.rds    ../../../Dropbox/HPCC/fit_rstan8_comp-$i_1.rds
   mv ./job_output/fit_rstan8_comp-$i_2.rds    ../../../Dropbox/HPCC/fit_rstan8_comp-$i_2.rds
   boxup
   mv ../../../Dropbox/HPCC/fit_rstan8_comp-$i_1.rds   ./job_output/fit_rstan8_comp-$i_1.rds
   mv ../../../Dropbox/HPCC/fit_rstan8_comp-$i_2.rds   ./job_output/fit_rstan8_comp-$i_2.rds
done