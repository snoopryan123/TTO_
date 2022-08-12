#!/bin/bash
#$ -N rstan8_AJ.R
#$ -j y
#$ -m e -M ryguy123@sas.upenn.edu 

## this command is to ask for multiple cores for running RStan files: [4] [1]
#$ -pe openmp 4

## ARRAY JOB
#$ -t 1-3
#$ -o job_output/$JOB_NAME-$JOB_ID-$TASK_ID.log
## MORE RAM
#$ -l m_mem_free=10G

Rscript --vanilla rstan8-$SGE_TASK_ID.R