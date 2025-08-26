#!/bin/bash
#SBATCH -D /work/thompsok

# ----------------------------------------------------------------------
# slurm arguments
# ----------------------------------------------------------------------

#SBATCH -J calculate_rarefied_richness
#SBATCH -t 9-00:00:00
#SBATCH --mem-per-cpu=200G
#SBATCH --array=1-27

# ----------------------------------------------------------------------
# setup job output/error reports
# ----------------------------------------------------------------------

#SBATCH -o /work/%u/Rarefied_Richness_Output/%x-%j-%a_log.txt


# ----------------------------------------------------------------------
# load required modules
# ----------------------------------------------------------------------
module load foss/2020b R

# ----------------------------------------------------------------------
# execute task
# ----------------------------------------------------------------------

# run code
code=/gpfs1/data/idiv_meyer/01_projects/Kim_T/R_Code/Rarefied_Richness/Calculating_Rarefied_Richness_Values_with_mobr.R

Rscript "$code" 