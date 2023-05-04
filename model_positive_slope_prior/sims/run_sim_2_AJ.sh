#!/bin/bash
#$ -N posSlopeSims_AJ2
#$ -j y
###$ -m e -M ryguy123@sas.upenn.edu 

## this command is to ask for multiple cores for running RStan files: [4]
#$ -pe openmp 1

## ARRAY JOB
#$ -t 1-225 ##1-25
#$ -o job_output/$JOB_NAME-$JOB_ID-$TASK_ID.log
## MORE RAM
####$ -l m_mem_free=5G

Rscript --vanilla sim_main.R ${SGE_TASK_ID} 2