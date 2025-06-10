#### Monitor vs Satellite PM2.5 Analysis —— Updated to match original outputs
# Author: Nick Silvis
# Date Created: 2025-06-10
# Updated: 2025-06-10

# Purpose: Compare old and new ground monitors with satellite PM2.5 estimates,
#          preserving exact plotting behavior from the original monolithic script.

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
process_monitor_data <- function(df, yr) {
  df %>%
    filter(Method.Code %in% c(236,238,736,738)) %>%
    mutate(Type = ifelse(Method.Code %in% c(236,238), "Old", "New"),
           Year = yr) %>%
    group_by(Longitude, Latitude, Year, Type) %>%
    summarize(Arithmetic.Mean = mean(Arithmetic.Mean, na.rm=TRUE), .groups="drop")
}

#### Load & combine monitor data
monitor_data <- bind_rows(
  lapply(seq_along(years), function(i) {
    df <- read.csv(monitor_files[i])
    process_monitor_data(df, years[i])
  })
)

#### Trim percentiles
pct95 <- quantile(monitor_data$Arithmetic.Mean, 0.95, na.rm=TRUE)
pct99 <- quantile(monitor_data$Arithmetic.Mean, 0.99, na.rm=TRUE)
monitor_95 <- filter(monitor_data, Arithmetic.Mean <= pct95)
monitor_99 <- filter(monitor_data, Arithmetic.Mean <= pct99)

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
  mon_sub <- filter(monitor_data, Year == yr)
  
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
  names(stk) <- c("Old_Monitor_Data", "New_Monitor_Data", "Satellite")
  
  df <- as.data.frame(stk, xy = TRUE, na.rm = TRUE)
  df$Year <- yr
  overlayed_data_list[[as.character(yr)]] <- df
}
final_data <- bind_rows(overlayed_data_list)
final_long <- pivot_longer(final_data,
                           cols = c("Old_Monitor_Data", "New_Monitor_Data"),
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
    scale_color_manual(values=c(Old_Monitor_Data=cbPalette[6],New_Monitor_Data=cbPalette[7]),
                       labels=c("Old Monitor","New Monitor")) +
    labs(x="Satellite PM2.5 (µg/m³)", y="Monitor PM2.5 (µg/m³)") +
    theme_classic(base_family="CMU Serif", base_size=25) +
    theme(legend.position="none")
  ggsave(file.path(output_dir, fn), p, width=15, height=10, dpi=300)
}

#### Execute Plots
plot_density(monitor_99,    "density_plot_99.png")
plot_density(monitor_95,    "density_plot_95.png")
plot_annual_averages(monitor_data, "bar_plot.png")
plot_annual_differences(monitor_data, "differences_bar_plot.png")
old_q <- quantile(monitor_data$Arithmetic.Mean[monitor_data$Type=="Old"], probs=seq(0,1,0.01), na.rm=TRUE)
new_q <- quantile(monitor_data$Arithmetic.Mean[monitor_data$Type=="New"], probs=seq(0,1,0.01), na.rm=TRUE)
tplot_qq(old_q, new_q,      "qq_plot.png")
tplot_scatter(final_long,   "scatter_plot.png")
                  c(cbPalette[6], cbPalette[7]))
  
  # ---- Regression Analysis & Table via Stargazer ----
  # Fit linear models by Source
  old_model <- lm(Monitor_PM2.5 ~ Satellite, data = filter(final_long, Source == "Old_Monitor_Data"))
  new_model <- lm(Monitor_PM2.5 ~ Satellite, data = filter(final_long, Source == "New_Monitor_Data"))
  
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
  
  all_monitors <- monitor_data_df %>%
    mutate(
      SiteID = sprintf("%02d%03d%04d",
                       State.Code,
                       County.Code,
                       Site.Num)
    )
  
  # Figure out which sites have both old & new methods
  old_codes <- c(236, 238)
  new_codes <- c(736, 738)
  
  site_method_flags <- all_monitors %>%
    filter(Method.Code %in% c(old_codes, new_codes)) %>%
    distinct(SiteID, Method.Code) %>%
    mutate(Method.Type = if_else(
      Method.Code %in% old_codes, "Old", "New"
    )) %>%
    distinct(SiteID, Method.Type) %>%
    count(SiteID) %>%
    filter(n == 2)  # only sites with both Old & New ever
  
  # Count original monitors
  num_orig_monitors <- nrow(all_monitors)
  
  # Filter to only monitors at updated sites
  monitor_data_updated_only <- all_monitors %>%
    filter(SiteID %in% site_method_flags$SiteID)
  
  # Count updated monitors
  num_updated_monitors <- nrow(monitor_data_updated_only)
  
  # Compute dropped
  num_dropped_monitors <- num_orig_monitors - num_updated_monitors
  
  # Print everything
  cat("Total monitors (rows) originally:", num_orig_monitors, "\n")
  cat("Monitors at updated sites:       ", num_updated_monitors, "\n")
  cat("Monitors dropped:                ", num_dropped_monitors, "\n")
  