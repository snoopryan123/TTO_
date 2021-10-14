#!/bin/bash
#$ -N rstan1_10FoldCVa-7.R
#$ -j y
#$ -m e -M ryguy123@sas.upenn.edu 

#$ -o job_output/$JOB_NAME-$JOB_ID.log

## this command is to ask for multiple cores for running RStan files: [4]
#$ -pe openmp 1

## MORE RAM
#$ -l m_mem_free=30G

Rscript --vanilla rstan1_10FoldCVa-7.R
