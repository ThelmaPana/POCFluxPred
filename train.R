#--------------------------------------------------------------------------#
# Project: POCFluxPred
# Script purpose: Train a model
# Date: 02/06/2025
# Author: Thelma Panaïotis
#--------------------------------------------------------------------------#

args <- commandArgs(trailingOnly = TRUE)

if (length(args) < 1) {
  stop("Please provide the path to a config.yaml file as a command-line argument.")
}

config_path <- args[1]

source("R/run_experiment.R")

run_experiment(config_path)
