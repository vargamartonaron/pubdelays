#!/bin/bash
#SBATCH --job-name=validate_json
#SBATCH --partition=hpc2019
#SBATCH --nodes=1
#SBATCH --tasks-per-node=1
#SBATCH --cpus-per-task=5
# Load any necessary modules if required
# module load your_module

# Activate your virtual environment if needed
 source /users/usumusu/.bashrc

# Run the Python script
python3 validate_json.py
