#!/bin/bash

DIRECTORY="/users/zsimi/pubdelays/data/processed_data/processed.csv"
ANALYSIS_SCRIPT="/users/zsimi/pubdelays/src/analysis/main_analysis.R"
SBATCH_OPTIONS="--mem=16G --cpus-per-task=1"
JOB_NAME="main analysis"
source /users/zsimi/.bashrc

LOG_FILE="/users/zsimi/pubdelays/src/analysis/${JOB_NAME}.out"

cd /users/zsimi/pubdelays/src/analysis/

sbatch $SBATCH_OPTIONS --job-name="$JOB_NAME" --output="$LOG_FILE" --wrap="Rscript $ANALYSIS_SCRIPT"