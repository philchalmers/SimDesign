#!/bin/bash
#SBATCH --job-name="brms test"
#SBATCH --mail-type=ALL
#SBATCH --mail-user=somewhere@out.there
#SBATCH --time=1:00:00       
#SBATCH --mem-per-cpu=12G      
#SBATCH --cpus-per-task=4
#SBATCH --array=1-6         

module load r/4.5.0
Rscript --vanilla brms_example.R
