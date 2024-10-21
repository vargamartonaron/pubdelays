#!/bin/bash
#SBATCH --job-name=validate_json_job
#SBATCH --partition=hpc2019
#SBATCH --nodes=1
#SBATCH --tasks-per-node=1
#SBATCH --cpus-per-task=1
# Load any necessary modules if required
# module load your_module

# Activate your virtual environment if needed
# source /path/to/your/venv/bin/activate

# Run the Python script
python3 validate_json.py
