#!/bin/bash
#SBATCH --job-name=parser
#SBATCH --partition=hpc2019
#SBATCH --nodes=1
#SBATCH --tasks-per-node=1
#SBATCH --cpus-per-task=10
# Load any necessary modules if required

# Activate your virtual environment if needed
cd /users/usumusu/pubdelays/src
source /users/usumusu/.bashrc

# Run the Python script
python3 xmls2json.py
