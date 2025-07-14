##############################################################################
# FILE NAME: 04b_regressions_alldata
# AUTHOR: Zoe Mitchell 
# PURPOSE: This script runs regressions and creates regression tables for the
# EPA PM2.5 Monitors project (for all data)
# UPDATED: 06-23-2025
##############################################################################

#### Regression and Table ####

  # Fit models for Original and Updated
  original_model <- lm(Monitor_PM2.5 ~ Satellite, data = dplyr::filter(monitor_satellite_long_alldata, Source == "Original"))
  updated_model  <- lm(Monitor_PM2.5 ~ Satellite, data = dplyr::filter(monitor_satellite_long_alldata, Source == "Updated"))

  # Save regression table
  output_file <- file.path(output_dir, "regression_results_table_alldata.tex")
  
  stargazer(original_model, updated_model,
            title = "Regression of Monitor PM2.5 on Satellite PM2.5",
            column.labels = c("Original", "Updated"),
            covariate.labels = "Satellite PM2.5",
            dep.var.labels = "Monitor PM2.5",
            type = "latex",
            out = output_file)
  
  cat("LaTeX regression table saved to", output_file, "\n")
  