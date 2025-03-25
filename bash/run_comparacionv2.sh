#!/bin/bash

log_path='/home/mloprete/Documents/logs/'
log_name='pp_comparacionv2.log'
log_file=$log_path$(date +"%Y%m%d_%H%M")$log_name

Rscript generate_modelsv2.R > $log_file 2>&1 &
