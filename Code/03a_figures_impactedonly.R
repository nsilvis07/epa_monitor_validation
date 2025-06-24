##############################################################################
# FILE NAME: 03a_figures_impactedonly
# AUTHOR: Zoe Mitchell 
# PURPOSE: This script generates the figures for the EPA PM2.5 Monitors project
# (for impacted monitors only) 
# UPDATED: 06-16-2025
##############################################################################

###### Call Functions to Generate Figures ######

# ---- Density Plots ----

pct99_impacted <- quantile(monitor_data_impacted$arithmetic_mean, 0.99, na.rm=TRUE)
impacted_99 <- filter(monitor_data_impacted, arithmetic_mean <= pct99_impacted)
plot_density(impacted_99, "density_plot_99_impacted.png")
  
# ---- Annual Averages Bar Plot ----
  
  plot_annual_averages(monitor_data_impacted, "bar_plot_impacted.png")

# ---- Annual Differences Bar Plot ----


plot_annual_differences(monitor_data_impacted, "differences_bar_plot_impacted.png")

  
# ---- QQ Plot ----

original_values_impacted <- quantile(monitor_data_impacted$arithmetic_mean[monitor_data_impacted$method_type=="Original"], probs=seq(0,1,0.01), na.rm=TRUE)
updated_values_impacted <- quantile(monitor_data_impacted$arithmetic_mean[monitor_data_impacted$method_type=="Updated"], probs=seq(0,1,0.01), na.rm=TRUE)
plot_qq(original_values_impacted, updated_values_impacted, "qq_plot_impacted.png")
 
# --- Scatter Plot ---

plot_scatter(monitor_satellite_long_impacted,   "scatter_plot.png")
                  c(cbPalette[6], cbPalette[7])


#  --- Bar chart to investigate share of erroneous monitors by year ---

# Calculate share of unique site_id values with method_code 236 or 238 by year
share_by_year <- monitor_data_original %>%
  group_by(Year) %>%
  summarize(
    total_sites = n_distinct(site_id),
    sites_236_238 = n_distinct(site_id[method_code %in% c(236, 238)]),
    share_236_238 = sites_236_238 / total_sites
  )

# Plot the share by year
share236or238_plot <- ggplot(share_by_year, aes(x = Year, y = share_236_238)) +
  geom_col(fill = "#0072B2") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    x = "Year",
    y = "Share of Monitors (%)"
  ) +
  theme_classic(base_family = "Times", base_size = 25) # Match function figures

# Save graph
ggsave(
  filename = file.path(output_dir, "share236or238_bar_plot.png"),
  plot = share236or238_plot,
  width = 15,
  height = 10,
  dpi = 300
)


