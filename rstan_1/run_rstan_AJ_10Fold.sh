#!/bin/bash
#$ -N rstan1_10FoldCV-array-job-1.R
#$ -j y
#$ -m e -M ryguy123@sas.upenn.edu 

## this command is to ask for multiple cores for running RStan files: [4]
#$ -pe openmp 1

## ARRAY JOB
#$ -t 1-5
#$ -o job_output/$JOB_NAME-$JOB_ID-$TASK_ID.log
## MORE RAM
#$ -l m_mem_free=30G

Rscript --vanilla rstan1_10FoldCVa-$SGE_TASK_ID.R


## Rscript --vanilla rstan1_10FoldCVa-$SGE_TASK_ID.R
## Rscript --vanilla rstan1_10FoldCVa-$TASK_ID.R
## Rscript --no-save < rstan1_10FoldCVa-$SGE_TASK_ID.R
## Rscript --no-save < rstan1_10FoldCVa-$TASK_ID.R
##$ -o job_output/$JOB_NAME-$JOB_ID-$TASK_ID.log
