#!/bin/bash
#SBATCH -D /work/thompsok

# ----------------------------------------------------------------------
# slurm arguments
# ----------------------------------------------------------------------

#SBATCH -J relate_canopy_400mbuff_to_BBS_routes
#SBATCH -t 2-00:00:00
#SBATCH --mem-per-cpu=200G
#SBATCH --array=1-81

# ----------------------------------------------------------------------
# setup job output/error reports
# ----------------------------------------------------------------------

#SBATCH -o /work/%u/Canopy_Output/%x-%j-%a_log.txt


# ----------------------------------------------------------------------
# load required modules
# ----------------------------------------------------------------------
module load foss/2020b R

# ----------------------------------------------------------------------
# execute task
# ----------------------------------------------------------------------

# run code
code=/gpfs1/data/idiv_meyer/01_projects/Kim_T/R_Code/Predictor_Preparation/Relating_iGFC_to_BBS_routes_EVE_version_400mbuffer.R

Rscript "$code" 