#--------------------------------------------------------------------------#
# Project: POCFluxPred
# Script purpose: Render quarto reports for experiments
# Date: 02/06/2025
# Author: Thelma Panaïotis
#--------------------------------------------------------------------------#

library(fs)
library(glue)
library(optparse)
library(yaml)


## Options ----
#--------------------------------------------------------------------------#
# Define options
option_list <- list(
  make_option(c("--include"), type = "character", default = NULL,
              help = "Comma-separated list of experiment IDs to render reports for (e.g., 'exp_01,exp_03')"),
  make_option(c("--refresh"), action = "store_true", default = FALSE,
              help = "Refresh Quarto cache"),
  make_option(c("--no-cache"), dest = "no_cache", action = "store_true", default = FALSE,
              help = "Disable Quarto cache")
)

if (interactive()) {
  # Simulate command line arguments when running interactively (in RStudio or R console)
  command_args <- c(
    #"--no-cache",
    "--include=exp_04,exp_05",
    "--refresh"
  )
} else {
  # When running from terminal, take the real command line arguments
  command_args <- commandArgs(trailingOnly = TRUE)
}

# Then pass command_args to OptionParser
opts <- parse_args(OptionParser(option_list = option_list), args = command_args)


## List directories of experiments ----
#--------------------------------------------------------------------------#
# Parse list of experiments to include 
include_list <- strsplit(opts$include, ",")[[1]] %||% character()

# List directory of experiments, excluding the demo experiment
experiment_dirs <- dir_ls("experiments", type = "directory", regexp = "^experiments/exp_[0-9]+$")

# Keep only directories for which reports should be rendered
filtered_experiment_dirs <- experiment_dirs[path_file(experiment_dirs) %in% include_list]


## Render reports ----
#--------------------------------------------------------------------------#
# Loop over experiments and generate reports
for (dir in filtered_experiment_dirs) {
  exp_name <- path_file(dir)
  
  cat("Generating report for", exp_name, "\n")
  
  # Copy quarto report to target directory
  file_copy("R/report_experiment.qmd", path(dir, "report_experiment.qmd"), overwrite = TRUE)  
  
  # Build render command
  cmd <- glue(
    "quarto render report_experiment.qmd ",
    "--to html ",
    "--execute ",
    "--execute-param config_path='config.yaml' ",
    "--output report.html ",
    if (opts$no_cache) {"--no-cache"} else {""},
    if (opts$refresh) {"--cache-refresh"} else {""}
  )
  
  # Move to target dir and render document
  old <- setwd(dir)
  system(cmd)
  setwd(old)
  
  # Remove copied quarto report
  file_delete(path(dir, "report_experiment.qmd"))
  
  cat("Rendered report for", path_file(dir), "\n")
}

