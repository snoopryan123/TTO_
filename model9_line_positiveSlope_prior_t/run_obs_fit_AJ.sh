#!/bin/bash
#$ -N model9_obs_AJ
#$ -j y
#$ -m e -M ryguy123@sas.upenn.edu 

## this command is to ask for multiple cores for running RStan files: [4]
#$ -pe openmp 4

## ARRAY JOB
#$ -t 18-18 #12-19
#$ -o job_output/$JOB_NAME-$JOB_ID-$TASK_ID.log
## MORE RAM
#$ -l m_mem_free=15G

Rscript --vanilla obs_fit-$SGE_TASK_ID.R