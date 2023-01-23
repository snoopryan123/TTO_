#!/bin/bash
#$ -N obs_fit
#$ -j y
#$ -m e -M ryguy123@sas.upenn.edu 

## this command is to ask for multiple cores for running RStan files: 
#$ -pe openmp 1   #4

## ARRAY JOB
#$ -t 18-18   #12-19
#$ -o job_output/$JOB_NAME-$JOB_ID-$TASK_ID.log
## MORE RAM
#$ -l m_mem_free=10G

Rscript  --vanilla obs_fitModel.R ${SGE_TASK_ID}