#!/bin/bash
#SBATCH --time=10:00:00
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --job-name=job_mu1_0
#SBATCH --output=logs/%j.log
#SBATCH --mem=30GB
#SBATCH --partition=regular

module load R-bundle-CRAN/2023.12-foss-2023a
Rscript -e "devtools::install_github('Yangshen0325/specmutual')"
Rscript -e "devtools::install_github('Yangshen0325/DataAnlz')"

Rscript ~/DataAnlz/script/mu1_0/job_mu1_0.R
