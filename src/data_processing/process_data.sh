#!/bin/bash

DIRECTORY="/users/zsimi/pubdelays/data/raw_data/pubmed/jsons"
R_SCRIPT="/users/zsimi/pubdelays/src/data_processing/process_data.R"
SBATCH_OPTIONS="--mem=12G --cpus-per-task=1"
JOB_NAME="process"
source /users/zsimi/.bashrc

FILES=()
while IFS= read -r -d '' FILE; do
    FILES+=("$FILE")
done < <(find "$DIRECTORY" -type f -name "*.xml.gz.json" -print0)

if [ ${#FILES[@]} -eq 0 ]; then
    echo "No files found in directory: $DIRECTORY"
    exit 1
fi

for ((INDEX=0; INDEX<${#FILES[@]}; INDEX++)); do
  FILE="${FILES[$INDEX]}"
  echo "Starting job for file: $FILE (Index: $INDEX)"

  cd /users/zsimi/pubdelays/src/data_processing/

    # Run the R script
  sbatch $SBATCH_OPTIONS --job-name="$JOB_NAME-$INDEX" --wrap="Rscript $R_SCRIPT $FILE"
done
