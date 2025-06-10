
rm(list = ls())

# Required Libraries
library(ggplot2)
library(sp)
library(dplyr)
library(raster)
library(stargazer)
library(tidyr)
library(ggplot2)
library(cowplot) 
library(grid)  
library(gtable)
library(tibble)


# Step 1: Process Monitor Data
process_monitor_data <- function(data, year) {
  # Separate Old and New Monitors
  old_monitor_data <- data[data$Method.Code %in% c(236, 238), ]
  new_monitor_data <- data[data$Method.Code %in% c(736, 738), ]
  
  # Add Year and Type Columns
  old_monitor_data$Year <- year
  old_monitor_data$Type <- "Old"
  
  new_monitor_data$Year <- year
  new_monitor_data$Type <- "New"
  
  rbind(old_monitor_data, new_monitor_data)  # Combine Old and New
}

create_custom_legend <- function(output_filename, legend_labels, legend_colors, legend_linetypes = NULL) {
  # Create data for the legend
  legend_data <- data.frame(
    label = factor(legend_labels, levels = legend_labels),  # Define legend entries
    color = legend_colors,  # Assign colors to legend entries
    linetype = legend_linetypes %||% "solid",  # Default to solid if no linetypes provided
    x = seq_along(legend_labels),  # Dummy x values for legend plotting
    y = 1  # Dummy y values for legend plotting
  )
  
  # Create the legend plot
  legend_plot <- ggplot(legend_data, aes(x = x, y = y, linetype = label, color = label, group = label)) +
    geom_line(size = 1.5) +  # Draw lines for the legend
    scale_linetype_manual(values = if (!is.null(legend_linetypes)) legend_linetypes else rep("solid", length(legend_labels))) +
    scale_color_manual(values = legend_colors) +  # Set colors in the legend
    guides(color = guide_legend(title = NULL, nrow = 1),  # Customize legend appearance
           linetype = guide_legend(title = NULL, nrow = 1)) +
    theme_void() +  # Use a void theme to remove unnecessary elements
    theme(
      legend.position = "bottom",  # Position the legend at the bottom
      legend.key.width = unit(2, "cm"),  # Set width of legend keys
      legend.key.height = unit(1, "cm"),  # Set height of legend keys
      legend.text = element_text(size = 20, color = "black", family = "CMU Serif"),  # Customize legend text appearance
      plot.margin = margin(0, 0, 0, 0, "cm")  # Set plot margins
    )
  
  # Extract and save the legend
  legend_grob <- gtable_filter(ggplot_gtable(ggplot_build(legend_plot)), "guide-box")  # Extract legend as a grob
  
  png(output_filename, width = 2600, height = 400, res = 300)  # Define output path and size for the legend
  grid.draw(legend_grob)  # Draw the legend
  dev.off()  # Close the graphics device
}


# Load Monitor Data for Each Year
years <- 2017:2023
file_paths <- paste0("~/The Lab Dropbox/Nick Silvis/Air Quality Project/Data/daily_88101_", years, ".csv")

monitor_data <- do.call(rbind, lapply(seq_along(years), function(i) {
  data <- read.csv(file_paths[i])
  process_monitor_data(data, years[i])
}))

# Add a 'Year' column
monitor_data$Year <- as.numeric(substr(monitor_data$Date.Local, 1, 4))

# Calculate the 95th percentile for Arithmetic.Mean
percentile_95 <- quantile(monitor_data$Arithmetic.Mean, 0.95, na.rm = TRUE)
percentile_99 <- quantile(monitor_data$Arithmetic.Mean, 0.99, na.rm = TRUE)

# Filter monitor data to the 95th percentile
monitor_data_99 <- monitor_data %>%
  filter(Arithmetic.Mean <= percentile_99)

# Combine all old and new monitor data into single dataframes
all_old_data <- monitor_data_99 %>% filter(Type == "Old")
all_new_data <- monitor_data_99 %>% filter(Type == "New")

# Filter monitor data to the 95th percentile
monitor_data_95 <- monitor_data %>%
  filter(Arithmetic.Mean <= percentile_95)

# Combine all old and new monitor data into single dataframes
all_old_data <- monitor_data_95 %>% filter(Type == "Old")
all_new_data <- monitor_data_95 %>% filter(Type == "New")

# Step 2: Load Satellite Data
satellite_files <- list(
  "2017" = "~/The Lab Dropbox/Nick Silvis/Air Quality Project/Data/V5GL04.HybridPM25.NorthAmerica.201701-201712.rda",
  "2018" = "~/The Lab Dropbox/Nick Silvis/Air Quality Project/Data/V5GL04.HybridPM25.NorthAmerica.201801-201812.rda",
  "2019" = "~/The Lab Dropbox/Nick Silvis/Air Quality Project/Data/V5GL04.HybridPM25.NorthAmerica.201901-201912.rda",
  "2020" = "~/The Lab Dropbox/Nick Silvis/Air Quality Project/Data/V5GL04.HybridPM25.NorthAmerica.202001-202012.rda",
  "2021" = "~/The Lab Dropbox/Nick Silvis/Air Quality Project/Data/V5GL04.HybridPM25.NorthAmerica.202101-202112.rda"
)

satellite_data_list <- lapply(satellite_files, function(file) {
  get(load(file))
})

# Prepare satellite data as data frames
satellite_data_list <- lapply(satellite_data_list, function(sat_data) {
  sat_df <- as.data.frame(sat_data, xy = TRUE)
  colnames(sat_df) <- c("Longitude", "Latitude", "Satellite.PM2.5")
  sat_df
})


# Step 3: Density Plots
# Combine all old and new monitor data into single dataframes
all_old_data <- monitor_data %>% filter(Type == "Old")
all_new_data <- monitor_data %>% filter(Type == "New")


# Predefined color palette
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", 
               "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


# Density plot with legend included
density_plot_99<- ggplot(monitor_data_99, aes(x = Arithmetic.Mean, fill = Type, color = Type)) +
  geom_density(alpha = 0.3, linewidth = 0.8) +  # Density plot with semi-transparent fills
  scale_color_manual(values = c("Old" = cbPalette[6], "New" = cbPalette[7])) +  # Use cbPalette for line colors
  scale_fill_manual(values = c("Old" = cbPalette[6], "New" = cbPalette[7])) +  # Use cbPalette for fill colors
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +  # Start y-axis at 0
  labs(
    x = "PM2.5 Concentration (Arithmetic Mean)",
    y = "Density",
    fill = "Monitor Type",
    color = "Monitor Type"
  ) +
  theme_classic() +
  theme(
    legend.position = "none",  
    text = element_text(size = 25, family = "CMU Serif"),
    axis.line.x = element_line(color = "black", size = 0.5, lineend = "square"),
    axis.line.y = element_line(color = "black", size = 0.5, lineend = "square"),
    axis.ticks = element_blank(),
    axis.text = element_text(size = 25, color = "black"),
    axis.text.y = element_text(margin = margin(r = 10)),
    axis.text.x = element_text(margin = margin(t = 10)),
    panel.grid.major.y = element_line(color = "grey80", linewidth = 0.75, linetype = "dotted"),
    plot.margin = margin(1, 1, 1, 1, "cm")
  )

# Save the plot 
ggsave(
  filename = "~/The Lab Dropbox/Nick Silvis/Air Quality Project/Output/density_plot_99.png",
  plot = density_plot_99,
  width = 15,
  height = 10,
  dpi = 300
)

# Density plot for data restricted to 95th percentile with legend
density_plot_95 <- ggplot(monitor_data_95, aes(x = Arithmetic.Mean, fill = Type, color = Type)) +
  geom_density(alpha = 0.3, linewidth = 0.8) +  # Density plot with semi-transparent fills
  scale_color_manual(values = c("Old" = cbPalette[6], "New" = cbPalette[7])) +  # Use cbPalette for line colors
  scale_fill_manual(values = c("Old" = cbPalette[6], "New" = cbPalette[7])) +  # Use cbPalette for fill colors
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +  # Start y-axis at 0
  labs(
    x = "PM2.5 Concentration (Arithmetic Mean)",
    y = "Density",
    fill = "Monitor Type",
    color = "Monitor Type"
  ) +
  theme_classic() +
  theme(
    legend.position = "none",
    text = element_text(size = 25, family = "CMU Serif"),
    axis.line.x = element_line(color = "black", size = 0.5, lineend = "square"),
    axis.line.y = element_line(color = "black", size = 0.5, lineend = "square"),
    axis.ticks = element_blank(),
    axis.text = element_text(size = 25, color = "black"),
    axis.text.y = element_text(margin = margin(r = 10)),
    axis.text.x = element_text(margin = margin(t = 10)),
    panel.grid.major.y = element_line(color = "grey80", linewidth = 0.75, linetype = "dotted"),
    plot.margin = margin(1, 1, 1, 1, "cm")
  )

# Save the plot
ggsave(
  filename = "~/The Lab Dropbox/Nick Silvis/Air Quality Project/Output/density_plot_95.png",
  plot = density_plot_95,
  width = 15,
  height = 10,
  dpi = 300
)



# Ensure monitor_data is a data frame for dplyr operations
monitor_data_df <- as.data.frame(monitor_data)


# Bar Plot Function for Annual Averages
plot_annual_averages <- function(data, output_filename) {
  annual_averages <- data %>%
    group_by(Year, Type) %>%
    summarize(Average_PM2.5 = mean(Arithmetic.Mean, na.rm = TRUE), .groups = "drop")
  
  p <- ggplot(annual_averages, aes(x = Year, y = Average_PM2.5, fill = Type)) +
    geom_bar(stat = "identity", position = "dodge") +  # Side-by-side bars
    scale_fill_manual(values = c("Old" = cbPalette[6], "New" = cbPalette[7])) +  # Use cbPalette for bar colors
    scale_y_continuous(
      breaks = seq(0, max(annual_averages$Average_PM2.5, na.rm = TRUE), by = 1),
      expand = c(0, 0)
    ) +  # Adjust y-axis breaks
    labs(
      x = "Year",
      y = "Average PM2.5 Concentration (µg/m³)",
      fill = "Monitor Type"
    ) +
    theme_classic() +  # Use classic theme for a clean appearance
    theme(
      legend.position = "none",
      text = element_text(size = 25, family = "CMU Serif"),
      axis.line.x = element_line(color = "black", size = 0.5, lineend = "square"),
      axis.line.y = element_line(color = "black", size = 0.5, lineend = "square"),
      axis.ticks = element_blank(),
      axis.text = element_text(size = 25, color = "black"),
      axis.text.y = element_text(margin = margin(r = 10)),
      axis.text.x = element_text(margin = margin(t = 10)),
      panel.grid.major.y = element_line(color = "grey80", linewidth = 0.75, linetype = "dotted"),
      plot.margin = margin(1, 1, 1, 1, "cm")
    )
  
  ggsave(
    filename = output_filename,
    plot = p,
    width = 15,
    height = 10,
    dpi = 300
  )
  
  return(p)
}

# Bar Plot Function for Annual Averages
plot_annual_averages <- function(data, output_filename) {
  annual_averages <- data %>%
    group_by(Year, Type) %>%
    summarize(Average_PM2.5 = mean(Arithmetic.Mean, na.rm = TRUE), .groups = "drop")
  
  p <- ggplot(annual_averages, aes(x = Year, y = Average_PM2.5, fill = Type)) +
    geom_bar(stat = "identity", position = "dodge") +  # Side-by-side bars
    scale_fill_manual(values = c("Old" = cbPalette[6], "New" = cbPalette[7])) +  # Use cbPalette for bar colors
    scale_y_continuous(
      breaks = seq(0, max(annual_averages$Average_PM2.5, na.rm = TRUE), by = 1),
      expand = c(0, 0)
    ) +  # Adjust y-axis breaks
    labs(
      x = "Year",
      y = "Average PM2.5 Concentration (µg/m³)",
      fill = "Monitor Type"
    ) +
    theme_classic() +  # Use classic theme for a clean appearance
    theme(
      legend.position = "none",
      text = element_text(size = 25, family = "CMU Serif"),
      axis.line.x = element_line(color = "black", size = 0.5, lineend = "square"),
      axis.line.y = element_line(color = "black", size = 0.5, lineend = "square"),
      axis.ticks = element_blank(),
      axis.text = element_text(size = 25, color = "black"),
      axis.text.y = element_text(margin = margin(r = 10)),
      axis.text.x = element_text(margin = margin(t = 10)),
      panel.grid.major.y = element_line(color = "grey80", linewidth = 0.75, linetype = "dotted"),
      plot.margin = margin(1, 1, 1, 1, "cm")
    )
  
  ggsave(
    filename = output_filename,
    plot = p,
    width = 15,
    height = 10,
    dpi = 300
  )
  
  return(p)
}

# Bar Plot Function for Annual Differences
plot_annual_differences <- function(data, output_filename) {
  annual_differences <- data %>%
    group_by(Year) %>%
    summarize(
      Difference = mean(Arithmetic.Mean[Type == "New"], na.rm = TRUE) -
        mean(Arithmetic.Mean[Type == "Old"], na.rm = TRUE),
      .groups = "drop"
    )
  
  y_breaks <- seq(-2, 0, by = 0.1)  # Customize y-axis breaks
  
  p <- ggplot(annual_differences, aes(x = Year, y = Difference)) +
    geom_bar(stat = "identity", fill = cbPalette[6]) +  # Use cbPalette for bar fill
    geom_hline(yintercept = 0, linetype = "dashed", color = cbPalette[6]) +  # Reference line with cbPalette
    scale_y_continuous(
      breaks = y_breaks,
      expand = c(0, 0)
    ) +
    labs(
      x = "Year",
      y = "Difference in PM2.5 (µg/m³)"
    ) +
    theme_classic() +  # Use classic theme
    theme(
      legend.position = "none",
      text = element_text(size = 25, family = "CMU Serif"),
      axis.line.x = element_line(color = "black", size = 0.5, lineend = "square"),
      axis.line.y = element_line(color = "black", size = 0.5, lineend = "square"),
      axis.ticks = element_blank(),
      axis.text = element_text(size = 25, color = "black"),
      axis.text.y = element_text(margin = margin(r = 10)),
      axis.text.x = element_text(margin = margin(t = 10)),
      plot.margin = margin(1, 1, 1, 1, "cm")
    )
  
  ggsave(
    filename = output_filename,
    plot = p,
    width = 15,
    height = 10,
    dpi = 300
  )
  
  return(p)
}

# Example usage
bar_plot <- plot_annual_averages(
  monitor_data_df, 
  "~/The Lab Dropbox/Nick Silvis/Air Quality Project/Output/bar_plot.png"
)
print(bar_plot)

diff_bar_plot <- plot_annual_differences(
  monitor_data_df, 
  "~/The Lab Dropbox/Nick Silvis/Air Quality Project/Output/differences_bar_plot.png"
)
print(diff_bar_plot)



# Generate the annual differences plot
diff_bar_plot <- plot_annual_differences(
  monitor_data_df, 
  "~/The Lab Dropbox/Nick Silvis/Air Quality Project/Output/differences_bar_plot.png"
)
print(diff_bar_plot)





# File paths for the monitor and satellite data
monitor_files <- paste0("~/The Lab Dropbox/Nick Silvis/Air Quality Project/Data/daily_88101_", 2017:2022, ".csv")
satellite_files <- list(
  "2017" = "~/The Lab Dropbox/Nick Silvis/Air Quality Project/Data/V5GL04.HybridPM25.NorthAmerica.201701-201712.rda",
  "2018" = "~/The Lab Dropbox/Nick Silvis/Air Quality Project/Data/V5GL04.HybridPM25.NorthAmerica.201801-201812.rda",
  "2019" = "~/The Lab Dropbox/Nick Silvis/Air Quality Project/Data/V5GL04.HybridPM25.NorthAmerica.201901-201912.rda",
  "2020" = "~/The Lab Dropbox/Nick Silvis/Air Quality Project/Data/V5GL04.HybridPM25.NorthAmerica.202001-202012.rda",
  "2021" = "~/The Lab Dropbox/Nick Silvis/Air Quality Project/Data/V5GL04.HybridPM25.NorthAmerica.202101-202112.rda",
  "2022" = "~/The Lab Dropbox/Nick Silvis/Air Quality Project/Data/V5GL04.HybridPM25.NorthAmerica.202201-202212.rda"
)

# Step 1: Process Monitor Data
process_monitor_data <- function(data, year) {
  # Label Old and New monitors
  old_monitor_data <- data[data$Method.Code %in% c(236, 238), ] %>%
    mutate(Year = year, Old = Arithmetic.Mean) %>%
    dplyr::select(Longitude, Latitude, Year, Old)
  
  new_monitor_data <- data[data$Method.Code %in% c(736, 738), ] %>%
    mutate(Year = year, New = Arithmetic.Mean) %>%
    dplyr::select(Longitude, Latitude, Year, New)
  
  # Combine old and new monitors by grouping and summarizing
  combined_data <- bind_rows(old_monitor_data, new_monitor_data) %>%
    group_by(Longitude, Latitude, Year) %>%
    summarize(
      Old = mean(Old, na.rm = TRUE),
      New = mean(New, na.rm = TRUE),
      .groups = "drop"  # Avoid unnecessary grouping
    )
  
  return(combined_data)
}

# Initialize a list to store results
overlayed_data_list <- list()

# Loop through each year
for (year in 2017:2022) {
  print(paste("Processing Year:", year))
  
  # Load monitor data
  monitor_data <- read.csv(monitor_files[year - 2016])  # Adjust index to match year
  monitor_data <- process_monitor_data(monitor_data, year)
  
  # Ensure Longitude, Latitude are numeric
  monitor_data$Longitude <- as.numeric(monitor_data$Longitude)
  monitor_data$Latitude <- as.numeric(monitor_data$Latitude)
  
  # Load satellite data
  load(satellite_files[[as.character(year)]])
  sat_raster <- raster(get(load(satellite_files[[as.character(year)]])))
  
  # Ensure CRS is set
  crs(sat_raster) <- CRS("+proj=longlat +datum=WGS84")
  
  # Convert monitor data to spatial points
  coordinates(monitor_data) <- ~ Longitude + Latitude
  proj4string(monitor_data) <- CRS("+proj=longlat +datum=WGS84")
  
  # Rasterize monitor data
  monitor_raster_old <- rasterize(
    monitor_data,
    sat_raster, # Match satellite raster resolution and extent
    field = "Old",
    fun = mean,
    na.rm = TRUE
  )
  
  monitor_raster_new <- rasterize(
    monitor_data,
    sat_raster,
    field = "New",
    fun = mean,
    na.rm = TRUE
  )
  
  # Stack the rasters
  stacked_rasters <- stack(monitor_raster_old, monitor_raster_new, sat_raster)
  names(stacked_rasters) <- c("Old_Monitor_Data", "New_Monitor_Data_Data", "Satellite")
  
  # Extract data to dataframe
  overlayed_data <- as.data.frame(stacked_rasters, xy = TRUE, na.rm = TRUE)
  overlayed_data$Year <- year
  
  # Append to the results list
  overlayed_data_list[[as.character(year)]] <- overlayed_data
}

# Combine all years' data into one dataframe
final_data <- bind_rows(overlayed_data_list)

# Filter data for scatter plots and regression
final_data_long <- final_data %>%
  pivot_longer(cols = c("Old_Monitor_Data", "New_Monitor_Data_Data"), names_to = "Source", values_to = "Monitor_PM2.5") %>%
  filter(!is.na(Monitor_PM2.5) & !is.na(Satellite))

# Predefined color palette
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", 
               "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# Updated Scatter Plot with Correct Colors
scatter_plot <- ggplot(final_data_long, aes(x = Satellite, y = Monitor_PM2.5, color = Source)) +
  geom_point(alpha = 0.6) +  # Scatter points with transparency
  geom_smooth(method = "lm", se = FALSE, aes(group = Source, color = Source)) +  # Regression lines
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +  # 45-degree reference line
  scale_color_manual(
    values = c("Old_Monitor_Data" = cbPalette[6], "New_Monitor_Data_Data" = cbPalette[7]),  # Correct color assignment
    labels = c("Old Monitor Data", "New Monitor Data")  # Add custom labels for clarity
  ) +
  labs(
    x = "Satellite PM2.5 (µg/m³)",
    y = "Monitor PM2.5 (µg/m³)",
    color = "Data Source"
  ) +
  theme_classic() +  # Apply a clean theme
  theme(
    text = element_text(size = 25, family = "CMU Serif"),
    axis.line.x = element_line(color = "black", size = 0.5, lineend = "square"),
    axis.line.y = element_line(color = "black", size = 0.5, lineend = "square"),
    axis.ticks = element_blank(),
    axis.text = element_text(size = 25, color = "black"),
    axis.text.y = element_text(margin = margin(r = 10)),
    axis.text.x = element_text(margin = margin(t = 10)),
    legend.position = "none",  # Legend will be saved separately
    plot.margin = margin(1, 1, 1, 1, "cm")
  )

# Save the Scatter Plot without Legend
ggsave(
  filename = "~/The Lab Dropbox/Nick Silvis/Air Quality Project/Output/scatter_plot.png",
  plot = scatter_plot,
  width = 15,
  height = 10,
  dpi = 300
)

create_custom_legend(
  output_filename = "~/The Lab Dropbox/Nick Silvis/Air Quality Project/Output/legend.png",
  legend_labels = c("Old Monitor Data", "New Monitor Data"),
  legend_colors = c(cbPalette[6], cbPalette[7])
)




# Define quantile probabilities
quantile_probs <- seq(0, 1, 0.01)

# Compute quantiles for Old and New monitors
old_quantiles <- quantile(monitor_data_df$Arithmetic.Mean[monitor_data_df$Type == "Old"], 
                          probs = quantile_probs, na.rm = TRUE)
new_quantiles <- quantile(monitor_data_df$Arithmetic.Mean[monitor_data_df$Type == "New"], 
                          probs = quantile_probs, na.rm = TRUE)

# Combine into a tibble for QQ plot
qq_comparison <- tibble(
  old_quantiles = old_quantiles,
  new_quantiles = new_quantiles,
  quantile_prob = quantile_probs
)

# Check the contents of qq_comparison
print(qq_comparison)

# QQ Plot Function
plot_qq <- function(old_quantiles, new_quantiles, output_filename) {
  qq_comparison_percentiles <- tibble(
    Old_Percentile = ecdf(old_quantiles)(old_quantiles) * 100,  # Percentiles for old quantiles
    New_Percentile = ecdf(new_quantiles)(new_quantiles) * 100   # Percentiles for new quantiles
  )
  
  p <- ggplot(qq_comparison_percentiles, aes(x = Old_Percentile, y = New_Percentile)) +
    geom_point(alpha = 0.9, color = "blue", size = 4) +  # Blue dots, larger size
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +  # Reference line
    labs(
      x = "Old Monitor Percentile",
      y = "New Monitor Percentile"
    ) +
    scale_x_continuous(limits = c(0, 100)) +
    scale_y_continuous(limits = c(0, 100)) +
    theme_classic() +  # Standardized theme
    theme(
      text = element_text(size = 25, family = "CMU Serif"),
      axis.line.x = element_line(color = "black", size = 0.5, lineend = "square"),
      axis.line.y = element_line(color = "black", size = 0.5, lineend = "square"),
      axis.ticks = element_blank(),
      axis.text = element_text(size = 25, color = "black"),
      axis.text.y = element_text(margin = margin(r = 10)),
      axis.text.x = element_text(margin = margin(t = 10)),
      plot.margin = margin(1, 1, 1, 1, "cm")
    )
  
  ggsave(
    filename = output_filename,
    plot = p,
    width = 15,
    height = 10,
    dpi = 300
  )
  
  return(p)
}

# QQ Plot Example
qq_plot <- plot_qq(
  old_quantiles, 
  new_quantiles, 
  "~/The Lab Dropbox/Nick Silvis/Air Quality Project/Output/qq_plot.png"
)
print(qq_plot)






# Regression Analysis for Old and New Monitors
old_data <- final_data_long %>% filter(Source == "Old_Monitor_Data")
new_data <- final_data_long %>% filter(Source == "New_Monitor_Data_Data")

if (nrow(old_data) > 0) {
  old_model <- lm(Monitor_PM2.5 ~ Satellite, data = old_data)
  cat("Regression Summary for Old Monitors:\n")
  print(summary(old_model))  # Print the full regression table
}

if (nrow(new_data) > 0) {
  new_model <- lm(Monitor_PM2.5 ~ Satellite, data = new_data)
  cat("Regression Summary for New Monitors:\n")
  print(summary(new_model))  # Print the full regression table
}

# Extract Coefficients and Standard Errors
coef_old <- coef(summary(old_model))["Satellite", "Estimate"]
se_old <- coef(summary(old_model))["Satellite", "Std. Error"]

coef_new <- coef(summary(new_model))["Satellite", "Estimate"]
se_new <- coef(summary(new_model))["Satellite", "Std. Error"]

# Perform T-Tests
t_old <- (coef_old - 1) / se_old
t_new <- (coef_new - 1) / se_new

# Calculate P-Values
p_value_old <- 2 * pt(-abs(t_old), df = old_model$df.residual)  # Two-tailed test
p_value_new <- 2 * pt(-abs(t_new), df = new_model$df.residual)  # Two-tailed test

# Print Results
cat("T-Test for Old Monitor Coefficient:\n")
cat("  Coefficient:", coef_old, "\n")
cat("  Standard Error:", se_old, "\n")
cat("  T-Statistic:", t_old, "\n")
cat("  P-Value:", p_value_old, "\n\n")

cat("T-Test for New Monitor Coefficient:\n")
cat("  Coefficient:", coef_new, "\n")
cat("  Standard Error:", se_new, "\n")
cat("  T-Statistic:", t_new, "\n")
cat("  P-Value:", p_value_new, "\n")

# LaTeX Table with Stargazer
stargazer(
  old_model, new_model,
  type = "latex",
  title = "Regression Results for Old and New Monitors",
  label = "Table 1",
  dep.var.labels = c("Monitor PM2.5"),
  column.labels = c("Old Monitors", "New Monitors"),
  covariate.labels = c("Satellite", "Constant"),
  omit.stat = c("f", "ser"),
  notes = "$^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01",
  notes.align = "l",
  out = "~/The Lab Dropbox/Nick Silvis/Air Quality Project/Output/regression_results_table.tex"
)

cat("LaTeX table saved to '~/The Lab Dropbox/Nick Silvis/Air Quality Project/Output/regression_results_table.tex'\n")














####### Maps ###########
library(tigris)
library(sf)

# Load US state boundaries
us_states <- states(cb = TRUE)  # Use cb = TRUE for simplified boundaries

# Filter for contiguous US
contiguous_us_states <- us_states %>%
  filter(!STUSPS %in% c("AK", "HI", "PR", "VI", "GU", "MP", "AS", "UM"))  # Exclude non-contiguous regions

# Filter overlap data for 2017
overlay_2017 <- overlayed_data_list[["2017"]]

# Ensure overlay_2017 is an sf object
overlay_2017_sf <- st_as_sf(overlay_2017, coords = c("x", "y"), crs = "+proj=longlat +datum=WGS84")

plot_map <- function(data_sf, value_column, title, output_filename, boundaries_sf) {
  p <- ggplot() +
    geom_sf(data = data_sf, aes(color = !!sym(value_column)), size = 0.5) +  # Plot spatial data points
    geom_sf(data = boundaries_sf, fill = NA, color = "black", size = 0.3) +  # Add boundaries
    scale_color_viridis_c(option = "C", name = "PM2.5 (µg/m³)", limits = c(1.5, 17.5)) +  # Color scale
    labs(title = title) +
    coord_sf(xlim = c(-125, -66), ylim = c(25, 50)) +  # Focus on contiguous US
    theme_classic() +  # Apply classic theme
    theme(
      text = element_text(size = 25, family = "CMU Serif"),
      axis.line.x = element_line(color = "black", size = 0.5, lineend = "square"),
      axis.line.y = element_line(color = "black", size = 0.5, lineend = "square"),
      axis.ticks = element_blank(),
      axis.text = element_text(size = 25, color = "black"),
      axis.text.y = element_text(margin = margin(r = 10)),
      axis.text.x = element_text(margin = margin(t = 10)),
      panel.grid.major.y = element_line(color = "grey80", linewidth = 0.75, linetype = "dotted"),
      plot.margin = margin(1, 1, 1, 1, "cm")
    )
  
  print(p)  # Display the plot
  
  ggsave(
    filename = output_filename,
    plot = p,
    width = 20,
    height = 10,
    units = "in",
    dpi = 300
  )
}

create_custom_legend_map <- function(output_filename) {
  legend_data <- data.frame(
    type = factor(c("Monitor Data", "Satellite Data"), levels = c("Monitor Data", "Satellite Data")),
    color = c("#0072B2", "#D55E00"),
    x = 1:2,
    y = 1
  )
  
  legend_plot <- ggplot(legend_data, aes(x = x, y = y, color = type)) +
    geom_point(size = 4) +
    scale_color_manual(values = legend_data$color) +
    guides(color = guide_legend(title = NULL, nrow = 1)) +
    theme_void() +
    theme(
      legend.position = "bottom",
      legend.key.width = unit(2, "cm"),
      legend.key.height = unit(1, "cm"),
      legend.text = element_text(size = 20, family = "CMU Serif"),
      plot.margin = margin(0, 0, 0, 0, "cm")
    )
  
  legend_grob <- gtable_filter(ggplot_gtable(ggplot_build(legend_plot)), "guide-box")
  
  png(output_filename, width = 2600, height = 400, res = 300)
  grid.draw(legend_grob)
  dev.off()
}

# Generate monitor map
plot_map(
  data_sf = overlay_2017_sf,
  value_column = "Old_Monitor_Data",
  title = "Monitor Data for 2017",
  output_filename = "~/The Lab Dropbox/Nick Silvis/Air Quality Project/Output/monitor_map.png",
  boundaries_sf = contiguous_us_states
)

# Create custom legend
create_custom_legend_map(
  output_filename = "~/The Lab Dropbox/Nick Silvis/Air Quality Project/Output/map_legend.png"
)






















library(sf)
library(ggplot2)
library(dplyr)
library(tigris)

# Load the satellite data
satellite_data <- load("~/The Lab Dropbox/Nick Silvis/Air Quality Project/Data/V5GL04.HybridPM25.NorthAmerica.201701-201712.rda")



# The loaded object should be a data frame
# Replace "HybridPM25" with the actual object name after loading
satellite_df <- try1_grid  # Replace with the correct name if different
head(satellite_df)  # Inspect the first few rows to confirm the structure


# Convert to sf object
satellite_sf <- st_as_sf(
  satellite_df,
  coords = c("Longitude", "Latitude"),
  crs = 4326,  # WGS84 CRS
  remove = FALSE
)

# Load US state boundaries
us_states <- states(cb = TRUE)  # Use cb = TRUE for simplified boundaries
contiguous_us_states <- us_states %>%
  filter(!STUSPS %in% c("AK", "HI", "PR", "VI", "GU", "MP", "AS", "UM"))  # Exclude non-contiguous regions

# Plot the satellite data
plot_satellite_map <- function(satellite_data_sf, state_boundaries_sf, output_filename) {
  p <- ggplot() +
    geom_sf(data = satellite_data_sf, aes(color = Satellite_PM2.5), size = 0.5) +  # Plot satellite data
    geom_sf(data = state_boundaries_sf, fill = NA, color = "black", size = 0.3) +  # Add state boundaries
    scale_color_viridis_c(option = "C", name = "PM2.5 (µg/m³)", limits = c(1.5, 17.5)) +  # Color scale
    labs(title = "Satellite PM2.5 Data for 2017 (Full Dataset)") +
    coord_sf(xlim = c(-125, -66), ylim = c(25, 50)) +  # Focus on contiguous US
    theme_classic() +
    theme(
      text = element_text(size = 25, family = "CMU Serif"),
      axis.line.x = element_line(color = "black", size = 0.5, lineend = "square"),
      axis.line.y = element_line(color = "black", size = 0.5, lineend = "square"),
      axis.ticks = element_blank(),
      axis.text = element_text(size = 15, color = "black"),
      panel.grid.major.y = element_line(color = "grey80", linewidth = 0.75, linetype = "dotted"),
      plot.margin = margin(1, 1, 1, 1, "cm")
    )
  
  # Save the plot
  ggsave(
    filename = output_filename,
    plot = p,
    width = 15,
    height = 10,
    dpi = 300
  )
  
  print(p)  # Display the plot
}

# Generate and save the satellite map
plot_satellite_map(
  satellite_data_sf = satellite_sf,
  state_boundaries_sf = contiguous_us_states,
  output_filename = "~/The Lab Dropbox/Nick Silvis/Air Quality Project/Output/satellite_map_sf.png"
)
