#!/bin/bash
#$ -N rstan2_yrs_AJ
#$ -j y
#$ -m e -M ryguy123@sas.upenn.edu 

## this command is to ask for multiple cores for running RStan files: [4]
#$ -pe openmp 1

## ARRAY JOB
#$ -t 1-13
#$ -o job_output/$JOB_NAME-$JOB_ID-$TASK_ID.log
## MORE RAM
##$ -l m_mem_free=10G

Rscript --vanilla rstan2_yrs-$SGE_TASK_ID.R


