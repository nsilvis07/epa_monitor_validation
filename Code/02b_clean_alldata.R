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
  monitor_data_combined, satellite_files, 2017:2022,
  original_label = "Original", updated_label = "Updated",
  out_names = c("monitor_data_original", "monitor_data_updated", "Satellite")
)

monitor_satellite_long_alldata <- pivot_longer(
  monitor_satellite_data_alldata,
  cols = c("monitor_data_original", "monitor_data_updated"),
  names_to = "Source", values_to = "Monitor_PM2.5"
)