#!/bin/bash
#$ -N run_ten_fold_cv_results
#$ -j y
#$ -m e -M ryguy123@sas.upenn.edu 

## this command is to ask for multiple cores for running RStan files: [4]
#$ -pe openmp 1

#$ -o job_output/$JOB_NAME-$JOB_ID.log
## MORE RAM
#$ -l m_mem_free=10G

Rscript --vanilla results_ten_fold_cv.R