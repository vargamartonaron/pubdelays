#!/bin/bash
#SBATCH --job-name=data_processing            # Job name
#SBATCH --output=output.txt       # Standard output and error log
#SBATCH --ntasks=1                   # Number of tasks (usually set to 1 for R scripts)
#SBATCH --cpus-per-task=8            # Number of CPU cores per task (adjust based on your needs)
#SBATCH --time=02:00:00              # Wall time (hh:mm:ss)
#SBATCH --mem=16G                     # Memory per node
#SBATCH --partition=hpc2019         # Partition to submit to (depends on your system)

# Load R module (depends on your cluster environment)
module load R/4.3.3

# Run the R script
Rscript process_data.R
