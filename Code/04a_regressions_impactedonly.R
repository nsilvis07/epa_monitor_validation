##############################################################################
# FILE NAME: 04a_regressions_impactedonly
# AUTHOR: Zoe Mitchell 
# PURPOSE: This script runs regressions and creates regression tables for the
# EPA PM2.5 Monitors project (for impacted monitors only)
# UPDATED: 06-16-2025
##############################################################################

  # Fit models for Original and Updated
  original_model <- lm(Monitor_PM2.5 ~ Satellite, data = dplyr::filter(monitor_satellite_long_impacted, Source == "Original"))
  updated_model  <- lm(Monitor_PM2.5 ~ Satellite, data = dplyr::filter(monitor_satellite_long_impacted, Source == "Updated"))

  # Save regression table
  output_file <- file.path(output_dir, "regression_results_table_impacted.tex")
  
  stargazer(original_model, updated_model,
            title = "Regression of Monitor PM2.5 on Satellite PM2.5: Impacted Monitors Only",
            column.labels = c("Original", "Updated"),
            covariate.labels = "Satellite PM2.5",
            dep.var.labels = "Monitor PM2.5",
            type = "latex",
            out = output_file)
  
  cat("LaTeX regression table saved to", output_file, "\n")
  
  