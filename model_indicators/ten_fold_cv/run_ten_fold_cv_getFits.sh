#!/bin/bash
for i in {1..10}
do
   #### echo "moving $i"
   mv ./job_output/fit_ten_fold_cv_${i}_model_indicators.rds    ../../../.
   boxup
   mv     ../../../Dropbox/HPCC/fit_ten_fold_cv_${i}_model_indicators.rds    ./job_output/.
done

