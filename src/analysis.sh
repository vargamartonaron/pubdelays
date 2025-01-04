#!/bin/bash
#SBATCH --job-name=analysis       # Job name
#SBATCH --output=output_%j.log         # Output file (%j is replaced by the job ID)
#SBATCH --error=error_%j.log           # Error file (%j is replaced by the job ID)
#SBATCH --ntasks=1                     # Number of tasks (1 for single-node jobs)
#SBATCH --cpus-per-task=8              # Number of CPUs per task
#SBATCH --mem=16G                      # Memory per node
#SBATCH --partition=hpc2019           # Partition (queue) name


# Navigate to the directory where the script is located (if not already there)
cd /users/usumusu/pubdelays/src
source /users/usumusu/.bashrc

# Run your R script
Rscript analysis.R                # Replace with your R script's name
