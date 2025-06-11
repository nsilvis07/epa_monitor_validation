#### Monitor vs Satellite PM2.5 Analysis —— Updated to match original outputs
# Author: Nick Silvis
# Date Created: 2025-06-10
# Updated: 2025-06-10

# Purpose: Compare original and updated ground monitors with satellite PM2.5 estimates,
# preserving exact plotting behavior from the original monolithic script.

rm(list = ls())

# Libraries
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  ggplot2, dplyr, tidyr, tibble, raster, sp, sf, tigris,
  cowplot, grid, gtable, stargazer, smoothr, readr, janitor
)

###### Directories & Parameters ######
main_dir   <- "~/The Lab Dropbox/Nick Silvis/EPA PM2.5 Monitor Update Verification"
data_dir   <- file.path(main_dir, "Data")
output_dir <- file.path(main_dir, "Output")
years      <- 2017:2022

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
               "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# File Paths
monitor_files <- paste0(data_dir, "/daily_88101_", years, ".csv")
satellite_files <- setNames(
  file.path(data_dir,
            paste0("V5GL04.HybridPM25.NorthAmerica.", years, "01-", years, "12.rda")),
  years
)

###### Defining Functions ######

# Process Monitor Data
process_original_and_updated_monitor_data_restricted <- function(data, year) {
  # Separate Original and New Monitors
  original_monitor_data_restricted <- data[data$Method.Code %in% c(236, 238), ]
  updated_monitor_data_restricted <- data[data$Method.Code %in% c(736, 738), ]
  
  # Add Year and Type Columns
  original_monitor_data_restricted$Year <- year
  original_monitor_data_restricted$Type <- "Original"
  
  updated_monitor_data_restricted$Year <- year
  updated_monitor_data_restricted$Type <- "New"
  
  rbind(original_monitor_data_restricted, updated_monitor_data_restricted)  # Combine Original and New
}

# Reads in Satellite Data
sat_list <- lapply(satellite_files, function(f) {
  sat_obj <- get(load(f))
  df <- as.data.frame(sat_obj, xy=TRUE)
  colnames(df) <- c("Longitude","Latitude","Satellite.PM2.5")
  df
})

# Density plot with legend
plot_density <- function(df, fn) {
  df <- mutate(df, Type=factor(method_type, levels=c("Original","New")))
  p <- ggplot(df, aes(arithmetic_mean, fill=method_type, color=method_type)) +
    geom_density(alpha=0.3, linewidth=0.8) +
    scale_color_manual(values=c(Original=cbPalette[6],New=cbPalette[7])) +
    scale_fill_manual(values=c(Original=cbPalette[6],New=cbPalette[7])) +
    theme_classic(base_family="CMU Serif", base_size=25) +
    theme(
      legend.position   = "bottom",
      legend.key.width  = unit(2,"cm"),
      legend.key.height = unit(1,"cm"),
      legend.text       = element_text(size=20)
    ) +
    labs(x="PM2.5 Concentration (Arithmetic Mean)", y="Density")
  ggsave(file.path(output_dir, fn), p, width=15, height=10, dpi=300)
}

# Annual Averages & Differences Plot
plot_annual_averages <- function(df, fn) {
  # compute annual averages
  avg <- df %>%
    group_by(Year, method_type) %>%
    summarize(Average_PM2.5 = mean(arithmetic_mean, na.rm = TRUE),
              .groups = "drop")
  
  # determine y-axis upper limit (round up to next even)
  y_max <- ceiling(max(avg$Average_PM2.5, na.rm = TRUE) / 2) * 2
  
  p <- ggplot(avg, aes(x = Year, y = Average_PM2.5, fill = method_type)) +
    geom_col(position = "dodge") +
    scale_fill_manual(values = c(Original = cbPalette[6], New = cbPalette[7])) +
    scale_y_continuous(
      breaks = seq(0, y_max, by = 2),
      limits = c(0, y_max)
    ) +
    labs(x = "Year", y = "Average PM2.5 (µg/m³)") +
    theme_classic(base_family = "CMU Serif", base_size = 25) +
    theme(
      legend.position = "none",
      panel.grid.major.y = element_line(color = "grey80", linewidth = 0.75, linetype = "dotted")
    )
  
  ggsave(file.path(output_dir, fn), p, width = 15, height = 10, dpi = 300)
}

plot_annual_differences <- function(df, fn) {
  # compute yearly updated–original differences
  diff <- df %>%
    group_by(Year) %>%
    summarize(
      Difference =
        mean(arithmetic_mean[method_type == "New"], na.rm = TRUE) -
        mean(arithmetic_mean[method_type == "Original"], na.rm = TRUE),
      .groups = "drop"
    )
  
  # get limits for both positive and negative, rounded out to nearest multiple of 2
  y_min <- floor(min(diff$Difference, na.rm = TRUE) / 2) * 2
  y_max <- ceiling(max(diff$Difference, na.rm = TRUE) / 2) * 2
  
  p <- ggplot(diff, aes(x = Year, y = Difference)) +
    geom_col(fill = cbPalette[6]) +
    geom_hline(yintercept = 0, linetype = "dashed", color = cbPalette[6]) +
    scale_y_continuous(
      breaks = seq(y_min, y_max, by = 2),
      limits = c(y_min, y_max)
    ) +
    labs(x = "Year", y = "New – Original PM2.5 (µg/m³)") +
    theme_classic(base_family = "CMU Serif", base_size = 25) +
    theme(
      legend.position = "none",
      panel.grid.major.y = element_line(color = "grey80", linewidth = 0.75, linetype = "dotted")
    )
  
  ggsave(file.path(output_dir, fn), p, width = 15, height = 10, dpi = 300)
}


# QQ plot
plot_qq <- function(original_values, updated_values, fn) {
  df <- tibble(
    OriginalPct = ecdf(original_values)(original_values)*100,
    NewPct = ecdf(updated_values)(updated_values)*100
  )
  p <- ggplot(df, aes(OriginalPct, NewPct)) +
    geom_point(size=3, alpha=0.9, color="blue") +
    geom_abline(slope=1, intercept=0, linetype="dashed") +
    coord_equal(xlim=c(0,100), ylim=c(0,100)) +
    labs(x="Original Monitor Percentile", y="New Monitor Percentile") +
    theme_classic(base_family="CMU Serif", base_size=25)
  ggsave(file.path(output_dir, fn), p, width=15, height=10, dpi=300)
}

# Scatter Plot with Line of Best Fit
plot_scatter <- function(df, fn) {
  p <- ggplot(df, aes(x=Satellite, y=Monitor_PM2.5, color=Source)) +
    geom_point(alpha=0.6) +
    geom_smooth(method="lm", se=FALSE,
                aes(group=Source, color=Source)) +
    geom_abline(slope=1, intercept=0, linetype="dashed",color="black") +
    scale_color_manual(values=c(Original_monitor_data_restricted=cbPalette[6],New_monitor_data_restricted=cbPalette[7]),
                       labels=c("Original Monitor","New Monitor")) +
    labs(x="Satellite PM2.5 (µg/m³)", y="Monitor PM2.5 (µg/m³)") +
    theme_classic(base_family="CMU Serif", base_size=25) +
    theme(legend.position="none")
  ggsave(file.path(output_dir, fn), p, width=15, height=10, dpi=300)
}

###### Load & combine monitor data ######
# Read & bind all years (2017–2023)
years      <- 2017:2023
file_paths <- paste0(data_dir, "/daily_88101_", years, ".csv")

monitor_all <- bind_rows(lapply(file_paths, read_csv, show_col_types = FALSE))

# Clean up column names 
monitor_all <- monitor_all %>%
  clean_names()

# Build a site-level ID 
monitor_all <- monitor_all %>%
  mutate(
    state_code  = as.integer(state_code),
    county_code = as.integer(county_code),
    site_num    = as.integer(site_num),
    site_id     = sprintf("%02d%03d%04d", state_code, county_code, site_num)
  )

# Filter to just the T640/T640X FEM codes & flag original vs updated
original_codes <- c(236, 238)
updated_codes <- c(736, 738)

updated_monitors <- monitor_all %>%
  filter(method_code %in% c(original_codes, updated_codes)) %>%
  mutate(method_type = if_else(method_code %in% original_codes, "Original", "New"))

# Identify only the “updated” sites (ever had both original & updated) 
updated_site_ids <- updated_monitors %>%
  group_by(site_id) %>%
  filter(any(method_type == "Original") & any(method_type == "New")) %>%
  distinct(site_id) %>%
  pull()

monitor_data_restricted <- updated_monitors %>%
  filter(site_id %in% updated_site_ids)

# Check counts
total_sites   <- n_distinct(updated_monitors$site_id)
updated_sites <- length(updated_site_ids)
dropped_sites <- total_sites - updated_sites

cat(
  "Sites in 4-code subset:   ", total_sites,   "\n",
  "Sites w/ both Original & New: ", updated_sites, "\n",
  "Sites dropped by filter: ", dropped_sites, "\n",
  "Rows in updated dataset: ", nrow(monitor_data_restricted), "\n",
  "Rows in original dataset: ", nrow(updated_monitors), "\n"
)

# Add a 'Year' column
monitor_data_restricted$Year <- as.numeric(substr(monitor_data_restricted$date_local, 1, 4))

overlayed_satellite_monitor_data_list <- list()
for (yr in 2017:2022) {
  mon_sub <- filter(monitor_data_restricted, Year == yr)
  
  sat_r <- raster(get(load(satellite_files[[as.character(yr)]])))
  crs(sat_r) <- "+proj=longlat +datum=WGS84"
  
  original_pts <- filter(mon_sub, method_type == "Original")
  updated_pts <- filter(mon_sub, method_type == "New")
  
  coordinates(original_pts) <- ~ longitude + latitude
  proj4string(original_pts) <- CRS("+proj=longlat +datum=WGS84")
  coordinates(updated_pts) <- ~ longitude + latitude
  proj4string(updated_pts) <- CRS("+proj=longlat +datum=WGS84")
  
  # Rasterize each
  r_original <- rasterize(original_pts, sat_r, field = "arithmetic_mean",
                     fun = mean, na.rm = TRUE)
  r_updated <- rasterize(updated_pts, sat_r, field = "arithmetic_mean",
                     fun = mean, na.rm = TRUE)
  
  # Stack and extract to dataframe
  stk <- stack(r_original, r_updated, sat_r)
  names(stk) <- c("Original_monitor_data_restricted", "New_monitor_data_restricted", "Satellite")
  
  df <- as.data.frame(stk, xy = TRUE, na.rm = TRUE)
  df$Year <- yr
  overlayed_satellite_monitor_data_list[[as.character(yr)]] <- df
}
monitor_satellite_data <- bind_rows(overlayed_satellite_monitor_data_list)
monitor_satellite_long <- pivot_longer(monitor_satellite_data,
                           cols = c("Original_monitor_data_restricted", "New_monitor_data_restricted"),
                           names_to = "Source", values_to = "Monitor_PM2.5")


###### Manipulate Data ######

# Trim percentiles
pct95 <- quantile(monitor_data_restricted$arithmetic_mean, 0.95, na.rm=TRUE)
pct99 <- quantile(monitor_data_restricted$arithmetic_mean, 0.99, na.rm=TRUE)
monitor_95 <- filter(monitor_data_restricted, arithmetic_mean <= pct95)
monitor_99 <- filter(monitor_data_restricted, arithmetic_mean <= pct99)

###### Plots and Table Generation ######

plot_density(monitor_99,    "density_plot_99.png")
plot_density(monitor_95,    "density_plot_95.png")
plot_annual_averages(monitor_data_restricted, "bar_plot.png")
plot_annual_differences(monitor_data_restricted, "differences_bar_plot.png")
original_values <- quantile(monitor_data_restricted$arithmetic_mean[monitor_data_restricted$method_type=="Original"], probs=seq(0,1,0.01), na.rm=TRUE)
updated_values <- quantile(monitor_data_restricted$arithmetic_mean[monitor_data_restricted$method_type=="New"], probs=seq(0,1,0.01), na.rm=TRUE)
plot_qq(original_values, updated_values,      "qq_plot.png")
plot_scatter(monitor_satellite_long,   "scatter_plot.png")
                  c(cbPalette[6], cbPalette[7])
  
# Regression and Table
  # Fit linear models by Source
  original_model <- lm(Monitor_PM2.5 ~ Satellite, data = filter(monitor_satellite_long, Source == "Original_monitor_data_restricted"))
  updated_model <- lm(Monitor_PM2.5 ~ Satellite, data = filter(monitor_satellite_long, Source == "New_monitor_data_restricted"))
  
  # Output regression table in LaTeX
  stargazer(
    original_model, updated_model,
    type = "latex",
    title = "Regression Results for Original and New Monitors",
    label = "tab:regression",
    dep.var.labels = c("Monitor PM2.5"),
    column.labels = c("Original Monitors", "New Monitors"),
    covariate.labels = c("Satellite PM2.5", "Intercept"),
    omit.stat = c("f", "ser"),
    notes = "$^{*}p<0.1; ^{**}p<0.05; ^{***}p<0.01$",
    notes.align = "l",
    out = file.path(output_dir, "regression_results_table.tex")
  )
  cat("LaTeX regression table saved to", file.path(output_dir, "regression_results_table.tex"), "
")

  
  
  
  
  
