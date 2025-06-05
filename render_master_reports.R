#--------------------------------------------------------------------------#
# Project: POCFluxPred
# Script purpose: Render quarto master report for all experiments
# Date: 04/06/2025
# Author: Thelma Panaïotis
#--------------------------------------------------------------------------#

library(fs)
library(glue)
library(optparse)
library(yaml)
library(stringr)


## Options ----
#--------------------------------------------------------------------------#
# Define options
option_list <- list(
  make_option(c("--no-cache"), dest = "no_cache", action = "store_true", default = FALSE,
              help = "Disable Quarto cache"),
  make_option(c("--refresh"), action = "store_true", default = FALSE,
              help = "Refresh Quarto cache"),
  make_option(c("--exclude"), type = "character", default = NULL,
              help = "Comma-separated list of experiments to exclude from report")
)

if (interactive()) {
  # Simulate command line arguments when running interactively (in RStudio or R console)
  command_args <- c(
    "--no-cache"
    #"--refresh",
    #"--exclude=exp_04"
  )
} else {
  # When running from terminal, take the real command line arguments
  command_args <- commandArgs(trailingOnly = TRUE)
}

# Then pass command_args to OptionParser
opts <- parse_args(OptionParser(option_list = option_list), args = command_args)


## List directories of experiments ----
#--------------------------------------------------------------------------#
# List directory of experiments, excluding the demo experiment
experiment_dirs <- dir_ls(here::here("experiments"), type = "directory", regexp = "exp_[0-9]+$")
experiment_names <- path_file(experiment_dirs)

# Exclude experiments if provided in opts$exclude
if (!is.null(opts$exclude)) {
  exclude_vec <- str_split(opts$exclude, ",")[[1]]
  # Keep only experiments NOT in exclude_vec
  experiment_names <- setdiff(experiment_names, exclude_vec)
}

# Now reconstruct full paths of experiments to keep
filtered_experiment_dirs <- path("experiments", experiment_names)


## Prepare quarto command ----
#--------------------------------------------------------------------------#
# Build the Quarto render command arguments
args <- c(
  "render",
  "reports/master_report.qmd",
  "--to", "html",
  "--execute",
  glue("--execute-param included_experiments='{paste(experiment_names, collapse = ',')}'")
)

# Add --no-cache option if requested
if (opts$no_cache) {
  args <- c(args, "--no-cache")
}
# Add --cache-refresh option if requested
if (opts$refresh) {
  args <- c(args, "--cache-refresh")
}


## Execute quarto command ----
#--------------------------------------------------------------------------#
system2("quarto", args)


