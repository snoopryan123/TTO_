#!/bin/bash
#$ -N rstan4a_overall_bsn.R
#$ -j y
#$ -m e -M ryguy123@sas.upenn.edu 

## this command is to ask for multiple cores for running RStan files: [4]
#$ -pe openmp 1

## ARRAY JOB
#$ -t 1-14
#$ -o job_output/$JOB_NAME-$JOB_ID-$TASK_ID.log
## MORE RAM
####$ -l m_mem_free=10G

Rscript --vanilla rstan4a_overall_bsn-$SGE_TASK_ID.R