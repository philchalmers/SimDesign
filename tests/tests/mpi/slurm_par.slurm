#!/bin/bash
#SBATCH --job-name="SLURM test (parallel)"
#SBATCH --mail-type=ALL
#SBATCH --mail-user=chalmrp@yorku.ca
#SBATCH --output=/dev/null    
#SBATCH --time=00:30:00       
#SBATCH --mem-per-cpu=2G      
#SBATCH --cpus-per-task=5
#SBATCH --array=1-3

module load r
Rscript --vanilla slurm_test_par.R