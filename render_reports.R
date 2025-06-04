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

# Define options
option_list <- list(
  make_option(c("--refresh"), type = "character", default = NULL,
              help = "Comma-separated list of experiment IDs to force refresh (e.g., 'exp_01,exp_03')"),
  make_option(c("--no-cache"), dest = "no_cache", action = "store_true", default = FALSE,
              help = "Disable Quarto cache")
)

opt_parser <- OptionParser(option_list = option_list)
opts <- parse_args(opt_parser)

# Parse refresh list
refresh_list <- strsplit(opts$refresh, ",")[[1]] %||% character()

# List directory of experiments, excluding the demo experiment
experiment_dirs <- dir_ls("experiments", type = "directory", regexp = "^experiments/exp_[0-9]+$")

# Loop over experiments and generate reports
for (dir in experiment_dirs) {
  exp_name <- path_file(dir)
  force_refresh <- exp_name %in% refresh_list
  
  cat("Generating report for", exp_name, "\n")
  
  # Copy quarto report to target directory
  file_copy("R/report_experiment.qmd", path(dir, "report_experiment.qmd"), overwrite = TRUE)  
  
  # Clean cache if forced refresh
  if (force_refresh) {
    cat("Clearing cache for", exp_name, "\n")
    cache_path <- path(dir, "_quarto")
    if (dir_exists(cache_path)) {dir_delete(cache_path)}
  }
  
  # Build render command
  cmd <- glue(
    "quarto render report_experiment.qmd ",
    "--to html ",
    "--execute ",
    "--execute-param config_path='config.yaml' ",
    "--output report.html ",
    if (opts$no_cache) {"--no-cache"} else {""}
  )
  
  # Move to target dir and render document
  old <- setwd(dir)
  system(cmd)
  setwd(old)
  
  # Remove copied quarto report
  file_delete(path(dir, "report_experiment.qmd"))
  
  cat("Rendered report for", path_file(dir), "\n")
}

