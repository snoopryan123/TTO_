#!/bin/bash
for i in {1..10}
do
   #### echo "moving $i"
   mv ./job_output/fit_ten_fold_cv_${i}_model_spl.rds    ../../../Dropbox/HPCC/fit_ten_fold_cv_${i}_model_spl.rds
   boxup
   mv     ../../../Dropbox/HPCC/fit_ten_fold_cv_${i}_model_spl.rds    ./job_output/fit_ten_fold_cv_${i}_model_spl.rds
done

