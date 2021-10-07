#!/bin/bash
#$ -N rstan_10_arp1_hpcc.R
#$ -j y
#$ -m e -M ryguy123@sas.upenn.edu 

#$ -o job_output/$JOB_NAME-$JOB_ID.log
## comment the above, and uncomment the two (2) below for an array job
##$ -t 1-10
##$ -o job_output/$JOB_NAME-$JOB_ID-$TASK_ID.log

## this command is to ask for multiple cores for running RStan files:
#$ -pe openmp 1
## comment the above, and uncomment the below, for 4 cores & 4 chains
##$ -pe openmp 4

Rscript --vanilla rstan_10_arp1_hpcc.R
## Rscript --no-save ex.R