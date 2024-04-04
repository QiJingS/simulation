#!/bin/bash
#SBATCH -J susieR_horseshoe
#SBATCH -p long

#SBATCH -D /well/band/users/xdy671/function_file/additive/res_output/
#SBATCH -c 4
#SBATCH --array=1-66

module load R/4.1.0-foss-2021a

Rscript /well/band/users/xdy671/function_file/main.R ${SLURM_ARRAY_TASK_ID} 