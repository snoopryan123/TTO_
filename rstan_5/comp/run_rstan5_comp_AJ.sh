#!/bin/bash
#$ -N rstan5_comp_AJ
#$ -j y
#$ -m e -M ryguy123@sas.upenn.edu 

## this command is to ask for multiple cores for running RStan files: [4]
#$ -pe openmp 1

## ARRAY JOB
#$ -t 1-40
#$ -o job_output/$JOB_NAME-$JOB_ID-$TASK_ID.log
## MORE RAM
####$ -l m_mem_free=10G

Rscript --vanilla rstan5_comp-$SGE_TASK_ID.R