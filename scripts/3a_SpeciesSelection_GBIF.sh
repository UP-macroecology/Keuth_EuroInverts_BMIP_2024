#!/bin/bash

#SBATCH --job-name=MCP_GBIF
#SBATCH --mail-type=ALL
#SBATCH --mail-user=keuth@uni-potsdam.de
#SBATCH --cpus-per-task=1
#SBATCH --time=96:00:00
#SBATCH --mem=100gb
#SBATCH --nodelist=ecoc9

cd ${SLURM_SUBMIT_DIR}
R CMD BATCH ./3a_SpeciesSelection_GBIF.R ./output-file-MCP_GBIF.Rout
