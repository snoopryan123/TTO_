#!/bin/bash
#$ -N rstan3_10FoldCV-array-job-ubi.R
#$ -j y
#$ -m e -M ryguy123@sas.upenn.edu 

## this command is to ask for multiple cores for running RStan files: [4]
#$ -pe openmp 1

## ARRAY JOB
#$ -t 1-10
#$ -o job_output/$JOB_NAME-$JOB_ID-$TASK_ID.log
## MORE RAM
#$ -l m_mem_free=30G

Rscript --vanilla rstan3_comp_ubi-$SGE_TASK_ID.R