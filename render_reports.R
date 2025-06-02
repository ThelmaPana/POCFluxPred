#--------------------------------------------------------------------------#
# Project: POCFluxPred
# Script purpose: Render quarto reports for experiments
# Date: 02/06/2025
# Author: Thelma Panaïotis
#--------------------------------------------------------------------------#

library(fs)
library(glue)

experiment_dirs <- dir_ls("experiments", type = "directory")

for (dir in experiment_dirs) {
  # Copy quarto report to target directory
  file_copy("report_experiment.qmd", path(dir, "report_experiment.qmd"))  
  
  
  cmd <- glue(
    "quarto render report_experiment.qmd ",
    "--to html ",
    "--execute ",
    "--execute-param config_path='config.yaml' ",
    "--output report.html "
  )
  
  # Move to target dir and render document
  old = setwd(dir)
  system(cmd)
  setwd(old)
  
  cat("✅ Rendered report for", exp_name, "\n")
  
  # Remove copied quarto report
  file_delete(path(dir, "report_experiment.qmd"))
}
