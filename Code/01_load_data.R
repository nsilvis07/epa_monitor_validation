##############################################################################
# FILE NAME: 01_load_data
# AUTHOR: Zoe Mitchell 
# PURPOSE: This script loads the EPA and satellite data needed to run scripts
# 02-04.
# UPDATED: 06-23-2025
# #############################################################################


###### Load and Combine Monitor Data ###### 

# ---- Generate monitor_all data frame by loading and combining ALL (for all
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

# Read in satellite data
sat_list <- lapply(satellite_files, function(f) {
  sat_obj <- get(load(f)) # load in satellite data
  df <- as.data.frame(sat_obj, xy=TRUE) # convert raster to data frame
  colnames(df) <- c("Longitude","Latitude","Satellite.PM2.5") # rename column names for clarity
  df
})