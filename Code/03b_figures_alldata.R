##############################################################################
# FILE NAME: 03b_figures_alldata
# AUTHOR: Zoe Mitchell 
# PURPOSE: This script generates the figures for the EPA PM2.5 Monitors project
# (for all data)
# UPDATED: 06-23-2025
##############################################################################

###### Call Functions to Generate Figures ######

# ---- Density Plots ----

pct99_alldata <- quantile(monitor_data_combined$arithmetic_mean, 0.99, na.rm = TRUE)
alldata_99 <- filter(monitor_data_combined, arithmetic_mean <= pct99_alldata)
plot_density(alldata_99, "density_plot_99_alldata.png")
  
# ---- Annual Averages Bar Plot ----

  plot_annual_averages(monitor_data_combined, "bar_plot_alldata.png")
  
# ---- Annual Differences Bar Plot ----

plot_annual_differences(monitor_data_combined, "differences_bar_plot_alldata.png")

# ---- QQ Plot ----

original_values_alldata <- quantile(monitor_data_combined$arithmetic_mean[monitor_data_combined$method_type == "Original"], probs = seq(0, 1, 0.01), na.rm = TRUE)
updated_values_alldata <- quantile(monitor_data_combined$arithmetic_mean[monitor_data_combined$method_type == "Updated"], probs = seq(0, 1, 0.01), na.rm = TRUE)
plot_qq_alldata(original_values_alldata, updated_values_alldata, "qq_plot_alldata.png")
  
# --- Scatter Plot ---

plot_scatter_alldata(monitor_satellite_long_alldata,   "scatter_plot_alldata.png")
                                c(cbPalette[6], cbPalette[7])

