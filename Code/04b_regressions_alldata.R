##############################################################################
# FILE NAME: 04b_regressions_alldata
# AUTHOR: Zoe Mitchell 
# PURPOSE: This script runs regressions and creates regression tables for the
# EPA PM2.5 Monitors project (for all data)
# UPDATED: 06-23-2025
##############################################################################

#### Regression and Table ####

  # Fit linear models by source
  original_model_alldata <- lm(Monitor_PM2.5 ~ Satellite, data = filter(monitor_satellite_long_alldata, Source == "monitor_data_original"))
  updated_model_alldata <- lm(Monitor_PM2.5 ~ Satellite, data = filter(monitor_satellite_long_alldata, Source == "monitor_data_updated"))
  
  # Output regression table in LaTeX
  stargazer(
    original_model_alldata, updated_model_alldata,
    type = "latex",
    title = "Regression Results for Original and Updated Data",
    label = "tab:regression",
    dep.var.labels = c("Monitor PM2.5"),
    column.labels = c("Original Data", "Updated Data"),
    covariate.labels = c("Satellite PM2.5", "Intercept"),
    omit.stat = c("f", "ser"),
    out = file.path(output_dir, "regression_results_table_alldata.tex")
  )
  cat("LaTeX regression table saved to", file.path(output_dir, "regression_results_table_alldata.tex"), "
")
  