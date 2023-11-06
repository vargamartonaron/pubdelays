#!/bin/bash

# Directory containing the .gz files
SOURCE_DIR="$HOME/GitHub/pubdelays/Data/ftp.ncbi.nlm.nih.gov/pubmed/baseline"

# Ensure the source directory exists
if [ ! -d "$SOURCE_DIR" ]; then
    echo "Source directory not found."
    exit 1
fi

# Loop through all .gz files in the directory
for gz_file in "$SOURCE_DIR"/*.gz; do
    if [ -f "$gz_file" ]; then
        # Extract the file without the .gz extension
        gunzip -d "$gz_file" 
        
        # Optionally, you can delete the compressed version
        # rm "$gz_file"
    fi
done
