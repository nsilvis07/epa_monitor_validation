#### Monitor vs Satellite PM2.5 Analysis —— Updated to match original outputs
# Author: Nick Silvis
# Date Created: 2025-06-10
# Updated: 2025-06-10

# Purpose: Compare old and new ground monitors with satellite PM2.5 estimates,
# preserving exact plotting behavior from the original monolithic script.

rm(list = ls())

#### Libraries
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  ggplot2, dplyr, tidyr, tibble, raster, sp, sf, tigris,
  cowplot, grid, gtable, stargazer, smoothr
)

#### Directories & Parameters
main_dir   <- "~/The Lab Dropbox/Nick Silvis/EPA PM2.5 Monitor Update Verification"
data_dir   <- file.path(main_dir, "Data")
output_dir <- file.path(main_dir, "Output")
years      <- 2017:2022

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
               "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

#### File Paths
monitor_files <- paste0(data_dir, "/daily_88101_", years, ".csv")
satellite_files <- setNames(
  file.path(data_dir,
            paste0("V5GL04.HybridPM25.NorthAmerica.", years, "01-", years, "12.rda")),
  years
)

#### Helper: process & aggregate monitor data by station-year
# Step 1: Process Monitor Data
process_old_and_updated_monitor_data_restricted <- function(data, year) {
  # Separate Old and New Monitors
  old_monitor_data_restricted <- data[data$Method.Code %in% c(236, 238), ]
  new_monitor_data_restricted <- data[data$Method.Code %in% c(736, 738), ]
  
  # Add Year and Type Columns
  old_monitor_data_restricted$Year <- year
  old_monitor_data_restricted$Type <- "Old"
  
  new_monitor_data_restricted$Year <- year
  new_monitor_data_restricted$Type <- "New"
  
  rbind(old_monitor_data_restricted, new_monitor_data_restricted)  # Combine Old and New
}

#### Load & combine monitor data
# Load Monitor Data for Each Year
years <- 2017:2022

file_paths <- paste0(data_dir, "/daily_88101_", years, ".csv")

monitor_data_restricted <- do.call(rbind, lapply(seq_along(years), function(i) {
  data <- read.csv(file_paths[i])
  process_old_and_updated_monitor_data_restricted(data, years[i])
}))

# Add a 'Year' column
monitor_data_restricted$Year <- as.numeric(substr(monitor_data_restricted$Date.Local, 1, 4))

#### Trim percentiles
pct95 <- quantile(monitor_data_restricted$Arithmetic.Mean, 0.95, na.rm=TRUE)
pct99 <- quantile(monitor_data_restricted$Arithmetic.Mean, 0.99, na.rm=TRUE)
monitor_95 <- filter(monitor_data_restricted, Arithmetic.Mean <= pct95)
monitor_99 <- filter(monitor_data_restricted, Arithmetic.Mean <= pct99)

#### Satellite data → Data frames list
sat_list <- lapply(satellite_files, function(f) {
  sat_obj <- get(load(f))
  df <- as.data.frame(sat_obj, xy=TRUE)
  colnames(df) <- c("Longitude","Latitude","Satellite.PM2.5")
  df
})

#### Build overlayed_data_list
overlayed_data_list <- list()
for (yr in years) {
  mon_sub <- filter(monitor_data_restricted, Year == yr)
  
  # Load satellite raster
  sat_r <- raster(get(load(satellite_files[[as.character(yr)]])))
  crs(sat_r) <- "+proj=longlat +datum=WGS84"
  
  # Split into Old/New and convert to Spatial points
  old_pts <- filter(mon_sub, Type == "Old")
  new_pts <- filter(mon_sub, Type == "New")
  
  coordinates(old_pts) <- ~ Longitude + Latitude
  proj4string(old_pts) <- CRS("+proj=longlat +datum=WGS84")
  coordinates(new_pts) <- ~ Longitude + Latitude
  proj4string(new_pts) <- CRS("+proj=longlat +datum=WGS84")
  
  # Rasterize each
  r_old <- rasterize(old_pts, sat_r, field = "Arithmetic.Mean",
                     fun = mean, na.rm = TRUE)
  r_new <- rasterize(new_pts, sat_r, field = "Arithmetic.Mean",
                     fun = mean, na.rm = TRUE)
  
  # Stack and extract to dataframe
  stk <- stack(r_old, r_new, sat_r)
  names(stk) <- c("Old_monitor_data_restricted", "New_monitor_data_restricted", "Satellite")
  
  df <- as.data.frame(stk, xy = TRUE, na.rm = TRUE)
  df$Year <- yr
  overlayed_data_list[[as.character(yr)]] <- df
}
final_data <- bind_rows(overlayed_data_list)
final_long <- pivot_longer(final_data,
                           cols = c("Old_monitor_data_restricted", "New_monitor_data_restricted"),
                           names_to = "Source", values_to = "Monitor_PM2.5")

#### Plot Functions

# Density with legend
plot_density <- function(df, fn) {
  df <- mutate(df, Type=factor(Type, levels=c("Old","New")))
  p <- ggplot(df, aes(Arithmetic.Mean, fill=Type, color=Type)) +
    geom_density(alpha=0.3, linewidth=0.8) +
    scale_color_manual(values=c(Old=cbPalette[6],New=cbPalette[7])) +
    scale_fill_manual(values=c(Old=cbPalette[6],New=cbPalette[7])) +
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

# Annual averages & differences
plot_annual_averages <- function(df, fn) {
  avg <- df %>% group_by(Year, Type) %>%
    summarize(Average_PM2.5=mean(Arithmetic.Mean, na.rm=TRUE), .groups="drop")
  p <- ggplot(avg, aes(x=Year, y=Average_PM2.5, fill=Type)) +
    geom_col(position="dodge") +
    scale_fill_manual(values=c(Old=cbPalette[6],New=cbPalette[7])) +
    labs(x="Year", y="Average PM2.5 (µg/m³)") +
    theme_classic(base_family="CMU Serif", base_size=25) +
    theme(legend.position="none",
          panel.grid.major.y=element_line(color="grey80",linewidth=0.75,linetype="dotted"))
  ggsave(file.path(output_dir, fn), p, width=15, height=10, dpi=300)
}
plot_annual_differences <- function(df, fn) {
  diff <- df %>% group_by(Year) %>%
    summarize(Difference=
                mean(Arithmetic.Mean[Type=="New"], na.rm=TRUE)-
                mean(Arithmetic.Mean[Type=="Old"], na.rm=TRUE), .groups="drop")
  p <- ggplot(diff, aes(x=Year, y=Difference)) +
    geom_col(fill=cbPalette[6]) +
    geom_hline(yintercept=0, linetype="dashed",color=cbPalette[6]) +
    labs(x="Year", y="New – Old PM2.5 (µg/m³)") +
    theme_classic(base_family="CMU Serif", base_size=25) +
    theme(legend.position="none",
          panel.grid.major.y=element_line(color="grey80",linewidth=0.75,linetype="dotted"))
  ggsave(file.path(output_dir, fn), p, width=15, height=10, dpi=300)
}

# QQ plot in percentile space
tplot_qq <- function(old_q, new_q, fn) {
  df <- tibble(
    OldPct = ecdf(old_q)(old_q)*100,
    NewPct = ecdf(new_q)(new_q)*100
  )
  p <- ggplot(df, aes(OldPct, NewPct)) +
    geom_point(size=3, alpha=0.9, color="blue") +
    geom_abline(slope=1, intercept=0, linetype="dashed") +
    coord_equal(xlim=c(0,100), ylim=c(0,100)) +
    labs(x="Old Monitor Percentile", y="New Monitor Percentile") +
    theme_classic(base_family="CMU Serif", base_size=25)
  ggsave(file.path(output_dir, fn), p, width=15, height=10, dpi=300)
}

# Scatter + per-source regression
tplot_scatter <- function(df, fn) {
  p <- ggplot(df, aes(x=Satellite, y=Monitor_PM2.5, color=Source)) +
    geom_point(alpha=0.6) +
    geom_smooth(method="lm", se=FALSE,
                aes(group=Source, color=Source)) +
    geom_abline(slope=1, intercept=0, linetype="dashed",color="black") +
    scale_color_manual(values=c(Old_monitor_data_restricted=cbPalette[6],New_monitor_data_restricted=cbPalette[7]),
                       labels=c("Old Monitor","New Monitor")) +
    labs(x="Satellite PM2.5 (µg/m³)", y="Monitor PM2.5 (µg/m³)") +
    theme_classic(base_family="CMU Serif", base_size=25) +
    theme(legend.position="none")
  ggsave(file.path(output_dir, fn), p, width=15, height=10, dpi=300)
}

#### Execute Plots
plot_density(monitor_99,    "density_plot_99.png")
plot_density(monitor_95,    "density_plot_95.png")
plot_annual_averages(monitor_data_restricted, "bar_plot.png")
plot_annual_differences(monitor_data_restricted, "differences_bar_plot.png")
old_q <- quantile(monitor_data_restricted$Arithmetic.Mean[monitor_data_restricted$Type=="Old"], probs=seq(0,1,0.01), na.rm=TRUE)
new_q <- quantile(monitor_data_restricted$Arithmetic.Mean[monitor_data_restricted$Type=="New"], probs=seq(0,1,0.01), na.rm=TRUE)
tplot_qq(old_q, new_q,      "qq_plot.png")
tplot_scatter(final_long,   "scatter_plot.png")
                  c(cbPalette[6], cbPalette[7])
  
  # ---- Regression Analysis & Table via Stargazer ----
  # Fit linear models by Source
  old_model <- lm(Monitor_PM2.5 ~ Satellite, data = filter(final_long, Source == "Old_monitor_data_restricted"))
  new_model <- lm(Monitor_PM2.5 ~ Satellite, data = filter(final_long, Source == "New_monitor_data_restricted"))
  
  # Output regression table in LaTeX
  stargazer(
    old_model, new_model,
    type = "latex",
    title = "Regression Results for Old and New Monitors",
    label = "tab:regression",
    dep.var.labels = c("Monitor PM2.5"),
    column.labels = c("Old Monitors", "New Monitors"),
    covariate.labels = c("Satellite PM2.5", "Intercept"),
    omit.stat = c("f", "ser"),
    notes = "$^{*}p<0.1; ^{**}p<0.05; ^{***}p<0.01$",
    notes.align = "l",
    out = file.path(output_dir, "regression_results_table.tex")
  )
  cat("LaTeX regression table saved to", file.path(output_dir, "regression_results_table.tex"), "
")

  
  
  
  
  
# Filters to only monitors that were updated

# Read in the full monitor data
  years <- 2017:2022
  file_paths <- paste0(data_dir, "/daily_88101_", years, ".csv")
  
  monitor_data_full <- do.call(rbind, lapply(seq_along(years), function(i) {
    df <- read.csv(file_paths[i])
    df$Year <- years[i]
    df
  }))
  
  #### compute monitors per site
  count_site_rows <- function(df) {
    df %>%
      mutate(
        SiteID = sprintf("%02d%03d%04d",
                         State.Code, County.Code, Site.Num)
      ) %>%
      group_by(SiteID) %>%
      tally(name = "rows") %>%
      ungroup()
  }
  
  full_counts     <- count_site_rows(monitor_data_full)      %>% rename(full_rows = rows)
  restricted_counts <- count_site_rows(monitor_data_restricted) %>% rename(restricted_rows = rows)
  
  #### 4. Join and compare ####
  comparison <- full_counts %>%
    full_join(restricted_counts, by = "SiteID") %>%
    replace_na(list(full_rows = 0, restricted_rows = 0)) %>%
    mutate(
      dropped_rows = full_rows - restricted_rows,
      kept_pct    = restricted_rows / full_rows * 100
    )
  
  # Summary:
  total_full     <- sum(full_counts$full_rows)
  total_restrict <- sum(restricted_counts$restricted_rows)
  total_dropped  <- total_full - total_restrict
  
  cat("Total rows in full data:            ", total_full,     "\n")
  cat("Total rows in restricted data:      ", total_restrict, "\n")
  cat("Total rows dropped by restriction:  ", total_dropped,  "\n")