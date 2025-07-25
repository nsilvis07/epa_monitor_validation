##############################################################################
# FILE NAME: 00_setup 
# AUTHOR: Zoe Mitchell 
# PURPOSE: This script sets the working directories, sets the functions, etc.
# neccessary to run scripts 01 - 03
# UPDATED: 06-18-2025
##############################################################################

# ---- Clear environment ----

rm(list = ls())

# ---- Install neccessary packages ----

if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  ggplot2, dplyr, tidyr, tibble, raster, sp, sf, tigris, sandwich,
  cowplot, grid, gtable, stargazer, smoothr, janitor, readr, units, scales, terra, ncdf4, stringr, systemfit, car, lmtest
)

# ---- Set working directories ----

main_dir   <- "/Users/loaner/The Lab Dropbox/Zoe Mitchell/EPA PM2.5 Monitor Update Verification"
data_dir   <- file.path(main_dir, "Data")
nc_dir <- file.path(data_dir, "Annual-selected")
output_dir <- file.path(main_dir, "Output")

# ---- Set parameters ----

years_monitors <- 2017:2023
years_satellite <- 2017:2022

original_codes <- c(236, 238) 
updated_codes <- c(736, 738) 

###### Set File Paths ######

# ---- Monitor data files ----

monitor_files <- paste0(data_dir, "/daily_88101_", years_monitors, ".csv")

# ---- Satellite data files ----

# Build list of file paths
satellite_files <- file.path(nc_dir, paste0("V5GL04.HybridPM25.NorthAmerica.", years_satellite, "01-", years_satellite, "12.nc"))

# Name each individual file in the list with the corresponding year
# This will allow us to easily reference the files by year later on
satellite_files <- setNames(satellite_files, as.character(years_satellite))

# ---- Set color palettes ----

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
               "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

source_colors <- c(
  "Original_monitor_data_impacted" = cbPalette[6],
  "New_monitor_data_impacted" = cbPalette[7],
  "monitor_data_original" = cbPalette[6],
  "monitor_data_updated" = cbPalette[7]
)


###### Build Functions ######

# ---- Overlay ground-level data with satellite data ----

overlay_monitor_satellite <- function(monitor_data, satellite_files, years,
                                      original_label = "Original",
                                      updated_label = "Updated",
                                      out_names = c("monitor_data_original", "monitor_data_updated", "Satellite")) {
  result_list <- list()
  
  for (yr in years) {
    mon_sub <- dplyr::filter(monitor_data, Year == yr)
    
    # Load satellite raster from .nc file
    nc_path <- satellite_files[[as.character(yr)]]
    sat_r_terra <- terra::rast(nc_path)
    
    # Convert to raster::RasterLayer for compatibility with raster functions
    sat_r <- raster::raster(sat_r_terra)
    raster::crs(sat_r) <- "+proj=longlat +datum=WGS84"
    
    # Convert monitor data to spatial points
    original_pts <- dplyr::filter(mon_sub, method_type == original_label)
    updated_pts  <- dplyr::filter(mon_sub, method_type == updated_label)
    
    sp::coordinates(original_pts) <- ~ longitude + latitude
    sp::proj4string(original_pts) <- sp::CRS("+proj=longlat +datum=WGS84")
    
    sp::coordinates(updated_pts) <- ~ longitude + latitude
    sp::proj4string(updated_pts) <- sp::CRS("+proj=longlat +datum=WGS84")
    
    # Rasterize monitor values onto the satellite grid
    r_original <- raster::rasterize(original_pts, sat_r, field = "arithmetic_mean", fun = mean, na.rm = TRUE)
    r_updated  <- raster::rasterize(updated_pts,  sat_r, field = "arithmetic_mean", fun = mean, na.rm = TRUE)
    
    # Stack all layers
    stk <- raster::stack(r_original, r_updated, sat_r)
    names(stk) <- out_names
    
    # Convert to dataframe
    df <- as.data.frame(stk, xy = TRUE, na.rm = TRUE)
    df$Year <- yr
    result_list[[as.character(yr)]] <- df
  }
  
  dplyr::bind_rows(result_list)
}


# ---- Density plot with legend (impacted and all data) ----
plot_density <- function(df, fn) {
  df <- mutate(df, Type=factor(method_type, levels=c("Original","Updated")))
  p <- ggplot(df, aes(arithmetic_mean, fill=method_type, color=method_type)) +
    geom_density(alpha=0.3, linewidth=0.8) +
    scale_color_manual(values=c(Original=cbPalette[6],Updated=cbPalette[7])) +
    scale_fill_manual(values=c(Original=cbPalette[6],Updated=cbPalette[7])) +
    theme_classic(base_family="Times", base_size=25) +
    theme(
      legend.position   = "bottom",
      legend.key.width  = unit(2,"cm"),
      legend.key.height = unit(1,"cm"),
      legend.text       = element_text(size=20)
    ) +
    labs(x="PM2.5 Concentration (Arithmetic Mean)", y="Density")
  ggsave(file.path(output_dir, fn), p, width=15, height=10, dpi=300)
}

# ---- Annual averages & differences plot (impacted and all data) ----
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
    scale_fill_manual(values = c(Original = cbPalette[6], Updated = cbPalette[7])) +
    scale_y_continuous(
      breaks = seq(0, y_max, by = 2),
      limits = c(0, y_max)
    ) +
    labs(x = "Year", y = "Average PM2.5 (µg/m³)") +
    theme_classic(base_family = "Times", base_size = 25) +
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
        mean(arithmetic_mean[method_type == "Updated"], na.rm = TRUE) -
        mean(arithmetic_mean[method_type == "Original"], na.rm = TRUE),
      .groups = "drop"
    )
  
  # get limits for both positive and negative, rounded out to nearest multiple
  # of 2
  y_min <- floor(min(diff$Difference, na.rm = TRUE) / 2) * 2
  y_max <- ceiling(max(diff$Difference, na.rm = TRUE) / 2) * 2
  
  p <- ggplot(diff, aes(x = Year, y = Difference)) +
    geom_col(fill = cbPalette[6]) +
    geom_hline(yintercept = 0, linetype = "dashed", color = cbPalette[6]) +
    scale_y_continuous(
      breaks = seq(y_min, y_max, by = 0.5),
      limits = c(y_min, y_max)
    ) +
    labs(x = "Year", y = "Updated – Original PM2.5 (µg/m³)") +
    theme_classic(base_family = "Times", base_size = 25) +
    theme(
      legend.position = "none",
      panel.grid.major.y = element_line(color = "grey80", linewidth = 0.75, linetype = "dotted")
    )
  
  ggsave(file.path(output_dir, fn), p, width = 15, height = 10, dpi = 300)
}


# ---- QQ plot (impacted data only) ----
plot_qq <- function(original_values, updated_values, fn) {
  df <- tibble(
    OriginalPct = ecdf(original_values)(original_values)*100,
    UpdatedPct = ecdf(updated_values)(updated_values)*100
  )
  p <- ggplot(df, aes(OriginalPct, UpdatedPct)) +
    geom_point(size=3, alpha=0.9, color="blue") +
    geom_abline(slope=1, intercept=0, linetype="dashed") +
    coord_equal(xlim=c(0,100), ylim=c(0,100)) +
    labs(x="Original Monitor Percentile", y="Updated Monitor Percentile") +
    theme_classic(base_family="Times", base_size=25)
  ggsave(file.path(output_dir, fn), p, width=15, height=10, dpi=300)
}

# ---- QQ plot (all data only) ----
plot_qq_alldata <- function(original_values_alldata, updated_values_alldata, fn) {
  df <- tibble(
    OriginalPct = ecdf(original_values_alldata)(original_values_alldata)*100,
    UpdatedPct = ecdf(updated_values_alldata)(updated_values_alldata)*100
  )
  p <- ggplot(df, aes(OriginalPct, UpdatedPct)) +
    geom_point(size=3, alpha=0.9, color="blue") +
    geom_abline(slope=1, intercept=0, linetype="dashed") +
    coord_equal(xlim=c(0,100), ylim=c(0,100)) +
    labs(x="Original Data Percentile", y="Updated Data Percentile") +
    theme_classic(base_family="Times", base_size=25)
  ggsave(file.path(output_dir, fn), p, width=15, height=10, dpi=300)
}


# ---- Scatter plot with line of best fit ----
plot_scatter <- function(df, fn) {
  p <- ggplot(df, aes(x=Satellite, y=Monitor_PM2.5, color=Source)) +
    geom_point(alpha=0.6) +
    geom_smooth(method="lm", se=FALSE,
                aes(group=Source, color=Source)) +
    geom_abline(slope=1, intercept=0, linetype="dashed",color="black") +
    scale_color_manual(values=c(Original_monitor_data_impacted=cbPalette[6],Updated_monitor_data_impacted=cbPalette[7]),
                       labels=c("Original Monitor","Updated Monitor")) +
    labs(x="Satellite PM2.5 (µg/m³)", y="Monitor PM2.5 (µg/m³)") +
    theme_classic(base_family="Times", base_size=25) +
    theme(legend.position="none")
  ggsave(file.path(output_dir, fn), p, width=15, height=10, dpi=300)
}

# ---- Scatter Plot with Line of Best Fit (adapted for all data) ----
plot_scatter_alldata <- function(df, fn) {
  p <- ggplot(df, aes(x=Satellite, y=Monitor_PM2.5, color=Source)) +
    geom_point(alpha=0.6) +
    geom_smooth(method="lm", se=FALSE,
                aes(group=Source, color=Source)) +
    geom_abline(slope=1, intercept=0, linetype="dashed",color="black") +
    scale_color_manual(values=c(monitor_data_original=cbPalette[6],monitor_data_updated=cbPalette[7]),
                       labels=c("Original Data","Updated Data")) +
    labs(x="Satellite PM2.5 (µg/m³)", y="Monitor PM2.5 (µg/m³)") +
    theme_classic(base_family="Times", base_size=25) +
    theme(legend.position="none")
  ggsave(file.path(output_dir, fn), p, width=15, height=10, dpi=300)
}

