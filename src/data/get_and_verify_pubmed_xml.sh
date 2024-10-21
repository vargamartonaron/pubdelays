#!/bin/bash

# FTP server URL
FTP_URL="ftp://ftp.ncbi.nlm.nih.gov/pubmed/baseline/"

# Directory to store downloaded data
DOWNLOAD_DIR="$HOME/GitHub/pubdelays/Data/"

# Log file to record the report
LOG_FILE="$DOWNLOAD_DIR/download_report.txt"

# Ensure the download directory exists
mkdir -p $DOWNLOAD_DIR

# Start time of the script
START_TIME=$(date +%s)

# Use wget with FTP options to download all files and log any errors
# wget -r -P $DOWNLOAD_DIR ftp://ftp.ncbi.nlm.nih.gov/pubmed/baseline/

# Change to the download directory
cd $DOWNLOAD_DIR/ftp.ncbi.nlm.nih.gov/pubmed/baseline

# Run md5sum on all .md5 files and log the result
for file in *.md5; do
    if [ -f "$file" ]; then
	(md5sum -c "$file") >> "$LOG_FILE" 2>&1
    fi
done

# Calculate the time passed
END_TIME=$(date +%s)
ELAPSED_TIME=$((END_TIME - START_TIME))

# Log the time passed and a completion message
# echo "Download completed in $ELAPSED_TIME seconds." >> "$LOG_FILE"

# Optionally, you can remove the .md5 files if you don't need them
# rm -f *.md5

# Optionally, navigate back to the original directory
cd -
