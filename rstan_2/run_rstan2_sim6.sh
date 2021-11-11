#!/bin/bash
#$ -N rstan2_sim_6.R
#$ -j y
#$ -m e -M ryguy123@sas.upenn.edu 

#$ -o job_output/$JOB_NAME-$JOB_ID.log
## comment the above, and uncomment the two (2) below for an array job
##$ -t 1-10
##$ -o job_output/$JOB_NAME-$JOB_ID-$TASK_ID.log

## this command is to ask for multiple cores for running RStan files: [4]
#$ -pe openmp 1

## MORE RAM
#$ -l m_mem_free=10G

Rscript --vanilla rstan2_sim_6.R
