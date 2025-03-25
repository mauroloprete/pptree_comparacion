#!/bin/bash

log_path='/home/mloprete/Documents/logs/'
log_name='pp_comparacionv3.log'
log_file="${log_path}$(date +"%Y%m%d_%H%M")_${log_name}"

cd /home/mloprete/Documents/pptree_comparacion

systemd-run --user --scope -p MemoryMax=30% --quiet --collect \
  Rscript R/generate_modelsv3.R > "$log_file" 2>&1 &

