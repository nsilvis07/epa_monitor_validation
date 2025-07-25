##############################################################################
# FILE NAME: 01_load_data
# AUTHOR: Zoe Mitchell 
# PURPOSE: This script loads the EPA and satellite data needed to run scripts
# 02-04.
# UPDATED: 06-23-2025
# #############################################################################


###### Load and Combine Monitor Data ###### 

# ---- Generate monitor_all data frame by loading and combining all (for all
# years and method codes) PM2.5 monitor data ----
monitor_all <- bind_rows(lapply(monitor_files, read_csv, show_col_types = FALSE)) %>%
  clean_names() %>% # clean up column names
  filter(arithmetic_mean > 0) %>% # drop negative PM2.5 values
  mutate(
    Year = as.numeric(substr(date_local, 1, 4)), # extract year from date_local
    # build a site-level ID by concatenating the state ID, county ID, and site
    # number
    state_code  = as.integer(state_code),
    county_code = as.integer(county_code),
    site_num    = as.integer(site_num),
    site_id     = sprintf("%02d%03d%04d", state_code, county_code, site_num)
  )

###### Load Satellite Data ######

# Build list of file paths
satellite_files <- file.path(nc_dir, paste0("V5GL04.HybridPM25.NorthAmerica.", years_satellite, "01-", years_satellite, "12.nc"))
names(satellite_files) <- as.character(years_satellite)

# Initialize list
sat_list <- list()

# Loop through files and store in sat_list
for (i in seq_along(satellite_files)) {
  sat_data <- satellite_files[i]
  sat <- rast(sat_data)
  df_sat <- as.data.frame(sat, xy = TRUE)
  colnames(df_sat) <- c("Longitude", "Latitude", "Satellite.PM2.5")
  df_sat$Year <- years_satellite[i]
  sat_list[[i]] <- df_sat
}

