##############################################################################
# FILE NAME: 02b_clean_alldata
# AUTHOR: Zoe Mitchell 
# PURPOSE: This script loads and cleans the EPA PM2.5 monitors data, defines
#separate data frames for specific analysis purposes and overlays these data
#frames with satellite data (all data).
# UPDATED: 06-23-2025
# #############################################################################

###### Process Monitor Data ###### 

# ---- Generate monitor_data_combined data frame, which combines the entire
# original and updated datasets ----

# Exclude method codes 736 and 738 to obtain the "original" data 
monitor_data_original <- monitor_all %>%
  filter(!method_code %in% updated_codes)

# Exclude method codes 236 and 238 to obtain the "updated" data
monitor_data_updated <- monitor_all %>%
  filter(!method_code %in% original_codes)

# Add a column (data_type) to each data frame that indicates whether the data
# is original or updated
monitor_data_original <- monitor_data_original %>%
  mutate(method_type = "Original")

monitor_data_updated <- monitor_data_updated %>%
  mutate(method_type = "Updated")

# Combine the two data frames to facilitate plotting
monitor_data_combined <- bind_rows(monitor_data_original, monitor_data_updated)


###### Overlay Ground-Level PM2.5 Monitor Data with Satellite Data ######

# ---- Call function to overlay data ----

monitor_satellite_data_alldata <- overlay_monitor_satellite(
  monitor_data = monitor_data_combined,
  satellite_files = satellite_files,
  years = 2017:2022,
  original_label = "Original",
  updated_label = "Updated"
)

# ---- Pivot from wide to long format for easier plotting ----
# This will allow us to have a single column for monitor PM2.5 values, a single
# column for satellite PM2.5, and a single column identifying the source of the
# monitor data
monitor_satellite_long_alldata <- monitor_satellite_data_alldata %>%
  dplyr::rename(
    Monitor_Original = monitor_data_original,
    Monitor_Updated = monitor_data_updated
  ) %>%
  tidyr::pivot_longer(
    cols = c(Monitor_Original, Monitor_Updated),
    names_to = "Source", # include column source to differentiate original and updated data
    values_to = "Monitor_PM2.5" # rename for clarity
  ) %>%
  dplyr::mutate(
    Source = dplyr::recode(Source,
                           Monitor_Original = "Original", # rename for clarity
                           Monitor_Updated = "Updated")
  ) %>%
  dplyr::filter(!is.na(Satellite), !is.na(Monitor_PM2.5)) %>%
  dplyr::select(x, y, Year, Source, Monitor_PM2.5, Satellite)



