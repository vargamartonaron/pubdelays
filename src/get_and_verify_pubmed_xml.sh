#!/bin/bash

SOURCE_DIR="/users/usumusu/pubdelays"

# FTP server URL
FTP_URL="https://ftp.ncbi.nlm.nih.gov/pubmed/baseline"

# Directory to store downloaded data
DOWNLOAD_DIR="../data"

# Log file to record the report
LOG_FILE="$DOWNLOAD_DIR/download_report.txt"

# Ensure the download directory exists
mkdir -p $DOWNLOAD_DIR

# Start time of the script
START_TIME=$(date +%s)

# Use wget with FTP options to download all files and log any errors
#curl -s https://ftp.ncbi.nlm.nih.gov/pubmed/baseline/ | grep -oP '(?<=href=")[^"]*\.(gz|md5)' | xargs -n 1 -P 4 -I {} curl -o "$DOWNLOAD_DIR/{}" https://ftp.ncbi.nlm.nih.gov/pubmed/baseline/{}

curl -s https://ftp.ncbi.nlm.nih.gov/pubmed/updatefiles/ | grep -oP '(?<=href=")[^"]*\.(gz|md5)' | xargs -n 1 -P 4 -I {} curl -o "$DOWNLOAD_DIR/{}" https://ftp.ncbi.nlm.nih.gov/pubmed/updatefiles/{}


# Change to the download directory
cd $DOWNLOAD_DIR

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
 echo "Download completed in $ELAPSED_TIME seconds." >> "$LOG_FILE"

# Optionally, you can remove the .md5 files if you don't need them
# rm -f *.md5

# Optionally, navigate back to the original directory
cd -
