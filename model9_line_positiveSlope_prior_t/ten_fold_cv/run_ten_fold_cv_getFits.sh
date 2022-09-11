#!/bin/bash
for i in {1..10}
do
   #### echo "moving $i"
   mv ./job_output/fit_ten_fold_cv_${i}_model_bsnBL.rds    ../../../Dropbox/HPCC/fit_ten_fold_cv_${i}_model_bsnBL.rds
   boxup
   mv     ../../../Dropbox/HPCC/fit_ten_fold_cv_${i}_model_bsnBL.rds    ./job_output/fit_ten_fold_cv_${i}_model_bsnBL.rds
done

