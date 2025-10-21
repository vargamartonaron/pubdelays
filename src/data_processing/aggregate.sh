#!/bin/bash

DIRECTORY="/users/zsimi/pubdelays/data/raw_data/pudmed/jsons"
AGGREGATE_SCRIPT="/users/zsimi/pubdelays/src/data_processing/aggregate.R"
SBATCH_OPTIONS="--mem=10G --cpus-per-task=1"
JOB_NAME="aggregate"
source /users/zsimi/.bashrc

LOG_FILE="/users/zsimi/pubdelays/src/data_processing/${JOB_NAME}.out"

cd /users/zsimi/pubdelays/src/data_processing/

sbatch $SBATCH_OPTIONS --job-name="$JOB_NAME" --output="$LOG_FILE" --wrap="Rscript $AGGREGATE_SCRIPT"
