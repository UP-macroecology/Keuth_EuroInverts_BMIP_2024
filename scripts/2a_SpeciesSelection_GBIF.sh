#!/bin/bash

#SBATCH --job-name=MCP_GBIF
#SBATCH --mail-type=ALL
#SBATCH --mail-user=keuth@uni-potsdam.de
#SBATCH --cpus-per-task=40
#SBATCH --time=96:00:00
#SBATCH --mem=300gb
#SBATCH --nodelist=ecoc9

cd ${SLURM_SUBMIT_DIR}
R CMD BATCH ./2a_SpeciesSelection_GBIF.R ./output-file-MCP_GBIF.Rout
