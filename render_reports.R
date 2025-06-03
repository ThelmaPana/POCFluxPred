#--------------------------------------------------------------------------#
# Project: POCFluxPred
# Script purpose: Render quarto reports for experiments
# Date: 02/06/2025
# Author: Thelma Panaïotis
#--------------------------------------------------------------------------#

library(fs)
library(glue)

# List directory of experiments, excluding the demo experiment
experiment_dirs <- dir_ls("experiments", type = "directory", regexp = "^experiments/exp_[0-9]+$")

# Loop over experiments and generate reports
for (dir in experiment_dirs) {
  
  cat("Generating report for", path_file(dir), "\n")
  
  # Copy quarto report to target directory
  file_copy("R/report_experiment.qmd", path(dir, "report_experiment.qmd"), overwrite = TRUE)  
  
  cmd <- glue(
    "quarto render report_experiment.qmd ",
    "--to html ",
    "--execute ",
    "--execute-param config_path='config.yaml' ",
    "--output report.html "
  )
  
  # Move to target dir and render document
  old <- setwd(dir)
  system(cmd)
  setwd(old)
  
  cat("Rendered report for", path_file(dir), "\n")
  
  # Remove copied quarto report
  file_delete(path(dir, "report_experiment.qmd"))
}

