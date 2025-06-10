# Monitor vs Satellite PM2.5 Analysis

# Author: Nick Silvis
# Date Created: 2025-06-10
# Last Modified: 2025-06-10


# Purpose: This script compares old and new ground monitor PM2.5 readings with satellite-
# derived PM2.5 estimates across the contiguous United States from 2017 to 2022.
# It includes data processing, percentile trimming, overlay and raster alignment,
# density plots, bar plots, QQ plots, and regressions. 


# Load Required Libraries Using pacman

if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  ggplot2, dplyr, tidyr, tibble, raster, sp, sf, tigris, cowplot,
  grid, gtable, stargazer, smoothr
)


####### User-Defined Parameters ######

years <- 2017:2023

###### Defining Directories ######


main_dir <- "~/The Lab Dropbox/Nick Silvis/EPA PM2.5 Monitor Update Verification"

data_dir <- file.path(paste0(main_dir, "/Data"))

output_dir <- file.path(paste0(main_dir, "/Output"))

###### Reading in Data ######

monitor_files <- paste0(data_dir, "/daily_88101_", years, ".csv")

satellite_files <- list(
  "2017" = file.path(data_dir, "V5GL04.HybridPM25.NorthAmerica.201701-201712.rda"),
  "2018" = file.path(data_dir, "V5GL04.HybridPM25.NorthAmerica.201801-201812.rda"),
  "2019" = file.path(data_dir, "V5GL04.HybridPM25.NorthAmerica.201901-201912.rda"),
  "2020" = file.path(data_dir, "V5GL04.HybridPM25.NorthAmerica.202001-202012.rda"),
  "2021" = file.path(data_dir, "V5GL04.HybridPM25.NorthAmerica.202101-202112.rda"),
  "2022" = file.path(data_dir, "V5GL04.HybridPM25.NorthAmerica.202201-202212.rda")
)

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", 
               "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

###### Define Functions ######

# Processes monitor data

process_monitor_data <- function(df, yr) {
  old <- df[df$Method.Code %in% c(236, 238), ]
  new <- df[df$Method.Code %in% c(736, 738), ]
  old$Year <- new$Year <- yr
  old$Type <- "Old"; new$Type <- "New"
  rbind(old, new)
}


create_custom_legend <- function(filename, labels, colours, linetypes = NULL) {
  leg_dat <- data.frame(label = factor(labels, levels = labels),
                        colour = colours,
                        linetype = linetypes %||% "solid",
                        x = seq_along(labels), y = 1)
  p <- ggplot(leg_dat, aes(x, y, colour = label, linetype = label)) +
    geom_line(linewidth = 1.5) +
    scale_colour_manual(values = colours) +
    scale_linetype_manual(values = if (is.null(linetypes))
      rep("solid", length(labels)) else linetypes) +
    guides(colour = guide_legend(title = NULL, nrow = 1),
           linetype = guide_legend(title = NULL, nrow = 1)) +
    theme_void(base_family = "CMU Serif") +
    theme(legend.position   = "bottom",
          legend.key.width  = unit(2, "cm"),
          legend.key.height = unit(1, "cm"),
          legend.text       = element_text(size = 20))
  g <- gtable_filter(ggplot_gtable(ggplot_build(p)), "guide-box")
  png(file.path(output_dir, filename), width = 2600, height = 400, res = 300)
  grid.draw(g); dev.off()
}

create_custom_legend_map <- function(filename) {
  create_custom_legend(
    filename,
    labels   = c("Monitor Data", "Satellite Data"),
    colours  = c("#0072B2", "#D55E00")
  )
}

# Creates density plot

plot_density <- function(df, filename) {
  p <- ggplot(df, aes(Arithmetic.Mean, fill = Type, colour = Type)) +
    geom_density(alpha = 0.3, linewidth = 0.8) +
    scale_colour_manual(values = c(Old = cbPalette[6], New = cbPalette[7])) +
    scale_fill_manual(values   = c(Old = cbPalette[6], New = cbPalette[7])) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
    labs(x = "PM2.5 Concentration (µg/m³)", y = "Density") +
    theme_classic(base_family = "CMU Serif", base_size = 25) +
    theme(axis.ticks = element_blank(),
          panel.grid.major.y = element_line(colour = "grey80",
                                            linewidth = 0.75, linetype = "dotted"))
  ggsave(file.path(output_dir, filename), p, width = 15, height = 10, dpi = 300)
  invisible(p)
}

# Creates annual averages plot

plot_annual_averages <- function(df, filename) {
  avg <- df %>% group_by(Year, Type) %>%
    summarise(Average_PM2.5 = mean(Arithmetic.Mean, na.rm = TRUE), .groups = "drop")
  p <- ggplot(avg, aes(Year, Average_PM2.5, fill = Type)) +
    geom_col(position = "dodge") +
    scale_fill_manual(values = c(Old = cbPalette[6], New = cbPalette[7])) +
    labs(x = "Year", y = "Average PM2.5 (µg/m³)") +
    theme_classic(base_family = "CMU Serif", base_size = 25) +
    theme(legend.position = "none",
          axis.ticks = element_blank(),
          panel.grid.major.y = element_line(colour = "grey80",
                                            linewidth = 0.75, linetype = "dotted"))
  ggsave(file.path(output_dir, filename), p, width = 15, height = 10, dpi = 300)
  invisible(p)
}

# Creates annual differences plot

plot_annual_differences <- function(df, filename) {
  diff <- df %>% group_by(Year) %>%
    summarise(Difference =
                mean(Arithmetic.Mean[Type == "New"], na.rm = TRUE) -
                mean(Arithmetic.Mean[Type == "Old"], na.rm = TRUE),
              .groups = "drop")
  p <- ggplot(diff, aes(Year, Difference)) +
    geom_col(fill = cbPalette[6]) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = cbPalette[6]) +
    labs(x = "Year", y = "New – Old PM2.5 (µg/m³)") +
    theme_classic(base_family = "CMU Serif", base_size = 25) +
    theme(legend.position = "none",
          axis.ticks = element_blank(),
          panel.grid.major.y = element_line(colour = "grey80",
                                            linewidth = 0.75, linetype = "dotted"))
  ggsave(file.path(output_dir, filename), p, width = 15, height = 10, dpi = 300)
  invisible(p)
}

# Creates QQ plot

plot_qq <- function(old_q, new_q, filename) {
  qq_df <- tibble(Old = old_q, New = new_q)
  p <- ggplot(qq_df, aes(Old, New)) +
    geom_point(size = 3, alpha = 0.9, colour = "blue") +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    labs(x = "Old Monitor Quantiles (µg/m³)",
         y = "New Monitor Quantiles (µg/m³)") +
    theme_classic(base_family = "CMU Serif", base_size = 25)
  ggsave(file.path(output_dir, filename), p, width = 15, height = 10, dpi = 300)
  invisible(p)
}

# Creates scatter plot with lines of best fit

plot_scatter_with_lm <- function(df, filename) {
  p <- ggplot(df, aes(Satellite, Monitor_PM2.5, colour = Source)) +
    geom_point(alpha = 0.6) +
    geom_smooth(method = "lm", se = FALSE) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    scale_colour_manual(values = c(Old_Monitor_Data = cbPalette[6],
                                   New_Monitor_Data_Data = cbPalette[7]),
                        labels  = c("Old Monitor Data", "New Monitor Data")) +
    labs(x = "Satellite PM2.5 (µg/m³)", y = "Monitor PM2.5 (µg/m³)") +
    theme_classic(base_family = "CMU Serif", base_size = 25) +
    theme(legend.position = "none")
  ggsave(file.path(output_dir, filename), p, width = 15, height = 10, dpi = 300)
  invisible(p)
}

# Plots diagnostic maps

plot_map <- function(data_sf, value_column, title, filename, boundaries_sf) {
  p <- ggplot() +
    geom_sf(data = data_sf, aes(colour = !!sym(value_column)), size = 0.5) +
    geom_sf(data = boundaries_sf, fill = NA, colour = "black", linewidth = 0.3) +
    scale_colour_viridis_c(option = "C", name = "PM2.5 (µg/m³)",
                           limits = c(1.5, 17.5)) +
    coord_sf(xlim = c(-125, -66), ylim = c(25, 50)) +
    labs(title = title) +
    theme_classic(base_family = "CMU Serif", base_size = 25) +
    theme(axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.grid.major =
            element_line(colour = "grey80", linewidth = 0.75, linetype = "dotted"))
  ggsave(file.path(output_dir, filename), p, width = 20, height = 10,
         dpi = 300, units = "in")
  invisible(p)
}


plot_satellite_map <- function(sat_sf, boundaries_sf, filename) {
  p <- ggplot() +
    geom_sf(data = sat_sf, aes(colour = Satellite.PM2.5), size = 0.5) +
    geom_sf(data = boundaries_sf, fill = NA, colour = "black", linewidth = 0.3) +
    scale_colour_viridis_c(option = "C", name = "PM2.5 (µg/m³)",
                           limits = c(1.5, 17.5)) +
    coord_sf(xlim = c(-125, -66), ylim = c(25, 50)) +
    labs(title = "Satellite PM2.5 (full year)") +
    theme_classic(base_family = "CMU Serif", base_size = 25) +
    theme(axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.grid.major =
            element_line(colour = "grey80", linewidth = 0.75, linetype = "dotted"))
  ggsave(file.path(output_dir, filename), p, width = 15, height = 10, dpi = 300)
  invisible(p)
}


###### Read in and Process Monitor Data ######

# Load TIGER/CB 2023 state boundaries 

us_states <- st_read(
  file.path(data_dir, "cb_2023_us_state_500k", "cb_2023_us_state_500k.shp"),
  quiet = TRUE 
)

# Keep contiguous-U.S. states only

contiguous_us_states <- us_states %>%
  dplyr::filter(!STUSPS %in% c("AK", "HI", "PR", "VI", "GU", "MP", "AS", "UM"))

# Reads in monitor data

monitor_data <- do.call(rbind, lapply(seq_along(years), function(i) {
  df <- read.csv(monitor_files[i])
  process_monitor_data(df, years[i])
}))

# Makes the year column of monitor_data numeric

monitor_data$Year <- as.numeric(substr(monitor_data$Date.Local, 1, 4))

# Read and Format Satellite Data

satellite_data_list <- lapply(satellite_files, function(file) {
  get(load(file))
})

satellite_data_list <- lapply(satellite_data_list, function(sat_data) {
  sat_df <- as.data.frame(sat_data, xy = TRUE)
  colnames(sat_df) <- c("Longitude", "Latitude", "Satellite.PM2.5")
  sat_df
})

# Build overlayed_data_list  →  final_data  →  final_data_long

overlayed_data_list <- list()

years_available <- intersect(
  years[file.exists(monitor_files)],
  as.numeric(names(satellite_files)[file.exists(unlist(satellite_files))])
)

for (yr in years_available) {
  message("Processing Year: ", yr)
  
  ## ---------- MONITOR DATA ----------
  mon_path <- monitor_files[years == yr]
  mon_df   <- tryCatch(read.csv(mon_path), error = \(e) NULL)
  if (is.null(mon_df) || nrow(mon_df) == 0) {
    warning("Skipping ", yr, " – monitor file missing or empty.")
    next
  }
  mon_df  <- process_monitor_data(mon_df, yr)
  
  # ensure numeric lon/lat
  mon_df$Longitude <- as.numeric(mon_df$Longitude)
  mon_df$Latitude  <- as.numeric(mon_df$Latitude)
  
  ## ---------- SATELLITE RASTER ----------
  sat_obj <- get(load(satellite_files[[as.character(yr)]]))     # loads raster*
  sat_rast <- raster::raster(sat_obj)                           # coerce to raster
  raster::crs(sat_rast) <- "+proj=longlat +datum=WGS84"
  
  ## ---------- RASTERIZE MONITOR MEANS ----------
  sp::coordinates(mon_df) <- ~ Longitude + Latitude
  sp::proj4string(mon_df) <- sp::CRS("+proj=longlat +datum=WGS84")
  
  r_old <- raster::rasterize(mon_df, sat_rast, field = "Arithmetic.Mean",
                             subset = mon_df$Type == "Old", fun = mean, na.rm = TRUE)
  r_new <- raster::rasterize(mon_df, sat_rast, field = "Arithmetic.Mean",
                             subset = mon_df$Type == "New", fun = mean, na.rm = TRUE)
  
  ## ---------- STACK & EXTRACT ----------
  stk <- raster::stack(r_old, r_new, sat_rast)
  names(stk) <- c("Old_Monitor_Data", "New_Monitor_Data_Data", "Satellite")
  
  df_yr <- as.data.frame(stk, xy = TRUE, na.rm = TRUE)
  df_yr$Year <- yr
  overlayed_data_list[[as.character(yr)]] <- df_yr
}

# Combine all years
final_data <- dplyr::bind_rows(overlayed_data_list)

# Long form used by scatter / regression
final_data_long <- final_data |>
  tidyr::pivot_longer(
    cols      = c("Old_Monitor_Data", "New_Monitor_Data_Data"),
    names_to  = "Source",
    values_to = "Monitor_PM2.5"
  ) |>
  dplyr::filter(!is.na(Monitor_PM2.5), !is.na(Satellite))


###### Main Analyses (Density Plots, Overlay, Regression) ######

# Trim to 95th and 99th percentiles

percentile_95 <- quantile(monitor_data$Arithmetic.Mean, 0.95, na.rm = TRUE)
percentile_99 <- quantile(monitor_data$Arithmetic.Mean, 0.99, na.rm = TRUE)

monitor_data_95 <- monitor_data %>% filter(Arithmetic.Mean <= percentile_95)
monitor_data_99 <- monitor_data %>% filter(Arithmetic.Mean <= percentile_99)



# Density plots
plot_density(monitor_data_95, "density_plot_95.png")
plot_density(monitor_data_99, "density_plot_99.png")

# Yearly bar plot + New–Old difference plot
plot_annual_averages    (monitor_data, "bar_plot.png")
plot_annual_differences (monitor_data, "differences_bar_plot.png")

# QQ plot (Old vs New quantiles)
quantile_probs <- seq(0, 1, 0.01)
old_q <- quantile(monitor_data$Arithmetic.Mean[monitor_data$Type == "Old"],
                  probs = quantile_probs, na.rm = TRUE)
new_q <- quantile(monitor_data$Arithmetic.Mean[monitor_data$Type == "New"],
                  probs = quantile_probs, na.rm = TRUE)
plot_qq(old_q, new_q, "qq_plot.png")

# Scatter + regression lines
scatter_plot <- plot_scatter_with_lm(final_data_long, "scatter_plot.png")  # returns ggplot (optional)
create_custom_legend("legend.png",
                     labels  = c("Old Monitor Data", "New Monitor Data"),
                     colours = c(cbPalette[6], cbPalette[7]))

# Regression table 

# Run regressions and t-tests
old_model <- lm(Monitor_PM2.5 ~ Satellite, data = final_data_long %>% filter(Source == "Old_Monitor_Data"))
new_model <- lm(Monitor_PM2.5 ~ Satellite, data = final_data_long %>% filter(Source == "New_Monitor_Data_Data"))

# Extract regression details
coef_old <- coef(summary(old_model))["Satellite", "Estimate"]
se_old <- coef(summary(old_model))["Satellite", "Std. Error"]
coef_new <- coef(summary(new_model))["Satellite", "Estimate"]
se_new <- coef(summary(new_model))["Satellite", "Std. Error"]

t_old <- (coef_old - 1) / se_old
t_new <- (coef_new - 1) / se_new

p_value_old <- 2 * pt(-abs(t_old), df = old_model$df.residual)
p_value_new <- 2 * pt(-abs(t_new), df = new_model$df.residual)

cat("Old Monitor:\nCoef =", coef_old, "SE =", se_old, "t =", t_old, "p =", p_value_old, "\n")
cat("New Monitor:\nCoef =", coef_new, "SE =", se_new, "t =", t_new, "p =", p_value_new, "\n")

# Output regression table
stargazer(
  old_model, new_model,
  type = "latex",
  title = "Regression Results for Old and New Monitors",
  label = "tab:regression",
  dep.var.labels = "Monitor PM2.5",
  column.labels = c("Old Monitors", "New Monitors"),
  covariate.labels = c("Satellite", "Constant"),
  omit.stat = c("f", "ser"),
  notes = "$^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01",
  notes.align = "l",
  out = file.path(output_dir, "regression_results_table.tex")
)









###### Filtering to only monitors with updated data #####
library(dplyr)
library(readr)


all_monitors <- monitor_data

# Step 2: Create SiteID using underscore-style column names
all_monitors <- all_monitors %>%
  mutate(SiteID = sprintf("%02d%03d%04d", State.Code, County.Code, Site.Num))

# Step 3: Identify updated monitors (those that appear with both old and new method codes)
old_codes <- c(236, 238)
new_codes <- c(736, 738)

site_method_flags <- all_monitors %>%
  filter(Method.Code %in% c(old_codes, new_codes)) %>%
  distinct(SiteID, Method.Code) %>%
  mutate(Method.Type = case_when(
    Method.Code %in% old_codes ~ "Old",
    Method.Code %in% new_codes ~ "New"
  )) %>%
  distinct(SiteID, Method.Type) %>%
  count(SiteID) %>%
  filter(n == 2)  # Only monitors with both Old and New

# Step 4: Filter full dataset to updated monitors only
updated_monitors <- all_monitors %>%
  filter(SiteID %in% site_method_flags$SiteID)

# Step 5: (Optional) Check how many updated sites there are
cat("Number of updated monitor sites:", length(unique(updated_monitors$SiteID)), "\n")
