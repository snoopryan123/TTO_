#!/bin/bash
for i in {1..10}
do
   #### echo "moving $i"
   ## mv ./job_output/fit_sim1_model_bsnBL_$i.rds    ../../../Dropbox/HPCC/fit_sim1_model_bsnBL_$i.rds
   ## mv ./job_output/fit_sim2_model_bsnBL_$i.rds    ../../../Dropbox/HPCC/fit_sim2_model_bsnBL_$i.rds

   ## mv ./job_output/fit_sim1_model_bsnBL_${i}_underlying_cubic.rds    ../../../Dropbox/HPCC/fit_sim1_model_bsnBL_${i}_underlying_cubic.rds
   ## mv ./job_output/fit_sim2_model_bsnBL_${i}_underlying_cubic.rds    ../../../Dropbox/HPCC/fit_sim2_model_bsnBL_${i}_underlying_cubic.rds

   mv ./job_output/fit_sim1_model_bsnBL_${i}_underlying_line.rds    ../../../Dropbox/HPCC/fit_sim1_model_bsnBL_${i}_underlying_line.rds
   mv ./job_output/fit_sim2_model_bsnBL_${i}_underlying_line.rds    ../../../Dropbox/HPCC/fit_sim2_model_bsnBL_${i}_underlying_line.rds
   mv ./job_output/fit_sim3_model_bsnBL_${i}_underlying_line.rds    ../../../Dropbox/HPCC/fit_sim3_model_bsnBL_${i}_underlying_line.rds

   mv ./job_output/fit_sim1A_model_bsnBL_${i}_underlying_line.rds    ../../../Dropbox/HPCC/fit_sim1A_model_bsnBL_${i}_underlying_line.rds
   mv ./job_output/fit_sim2A_model_bsnBL_${i}_underlying_line.rds    ../../../Dropbox/HPCC/fit_sim2A_model_bsnBL_${i}_underlying_line.rds
   mv ./job_output/fit_sim3A_model_bsnBL_${i}_underlying_line.rds    ../../../Dropbox/HPCC/fit_sim3A_model_bsnBL_${i}_underlying_line.rds


   boxup

   ## mv ../../../Dropbox/HPCC/fit_sim1_model_bsnBL_$i.rds   ./job_output/fit_sim1_model_bsnBL_$i.rds
   ## mv ../../../Dropbox/HPCC/fit_sim2_model_bsnBL_$i.rds   ./job_output/fit_sim2_model_bsnBL_$i.rds

   ## mv    ../../../Dropbox/HPCC/fit_sim1_model_bsnBL_${i}_underlying_cubic.rds    ./job_output/fit_sim1_model_bsnBL_${i}_underlying_cubic.rds 
   ## mv    ../../../Dropbox/HPCC/fit_sim2_model_bsnBL_${i}_underlying_cubic.rds    ./job_output/fit_sim2_model_bsnBL_${i}_underlying_cubic.rds 
  
   mv     ../../../Dropbox/HPCC/fit_sim1_model_bsnBL_${i}_underlying_line.rds    ./job_output/fit_sim1_model_bsnBL_${i}_underlying_line.rds
   mv     ../../../Dropbox/HPCC/fit_sim2_model_bsnBL_${i}_underlying_line.rds    ./job_output/fit_sim2_model_bsnBL_${i}_underlying_line.rds
   mv     ../../../Dropbox/HPCC/fit_sim3_model_bsnBL_${i}_underlying_line.rds    ./job_output/fit_sim3_model_bsnBL_${i}_underlying_line.rds

   mv     ../../../Dropbox/HPCC/fit_sim1A_model_bsnBL_${i}_underlying_line.rds    ./job_output/fit_sim1A_model_bsnBL_${i}_underlying_line.rds
   mv     ../../../Dropbox/HPCC/fit_sim2A_model_bsnBL_${i}_underlying_line.rds    ./job_output/fit_sim2A_model_bsnBL_${i}_underlying_line.rds
   mv     ../../../Dropbox/HPCC/fit_sim3A_model_bsnBL_${i}_underlying_line.rds    ./job_output/fit_sim3A_model_bsnBL_${i}_underlying_line.rds
done

