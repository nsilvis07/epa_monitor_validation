##############################################################################
# FILE NAME: 04a_regressions_impactedonly
# AUTHOR: Zoe Mitchell 
# PURPOSE: This script runs regressions and creates regression tables for the
# EPA PM2.5 Monitors project (for impacted monitors only)
# UPDATED: 06-16-2025
##############################################################################

# Regression and Table

  # Fit linear models by source
  original_model_impacted <- lm(Monitor_PM2.5 ~ Satellite, data = filter(monitor_satellite_long_impacted, Source == "Original_monitor_data_impacted"))
  updated_model_impacted <- lm(Monitor_PM2.5 ~ Satellite, data = filter(monitor_satellite_long_impacted, Source == "Updated_monitor_data_impacted"))
  
  # Output regression table in LaTeX
  stargazer(
    original_model_impacted, updated_model_impacted,
    type = "latex",
    title = "Regression Results for Original and Updated Data: Impacted Monitors Only",
    label = "tab:regression",
    dep.var.labels = c("Monitor PM2.5"),
    column.labels = c("Original Data", "Updated Data"),
    covariate.labels = c("Satellite PM2.5", "Intercept"),
    omit.stat = c("f", "ser"),
    out = file.path(output_dir, "regression_results_table_impacted.tex")
  )
  cat("LaTeX regression table saved to", file.path(output_dir, "regression_results_table_impacted.tex"), "
")

  