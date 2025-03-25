#!/bin/bash

log_path='/home/mloprete/Documents/logs/'
log_name='pp_comparacionv3.log'
log_file=$log_path$(date +"%Y%m%d_%H%M")$log_name

cd /home/mloprete/Documents/pptree_comparacion

Rscript R/generate_modelsv3.R > $log_file 2>&1 &
