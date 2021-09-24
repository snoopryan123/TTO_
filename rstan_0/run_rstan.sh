#!/bin/bash
#$ -N arp1_05_hpcc
#$ -j y
#$ -m e -M ryguy123@sas.upenn.edu 

#$ -o job_output/$JOB_NAME-$JOB_ID.log
## comment the above, and uncomment the two (2) below for an array job
##$ -t 1-10
##$ -o job_output/$JOB_NAME-$JOB_ID-$TASK_ID.log

## this command is to ask for multiple cores for running RStan files:
#$ -pe openmp 4

Rscript --vanilla rstan_05_arp1_hpcc.R
## Rscript --no-save ex.R