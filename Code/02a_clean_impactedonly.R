##############################################################################
# FILE NAME: 02a_clean_impactedonly
# AUTHOR: Zoe Mitchell 
# PURPOSE: This script cleans the EPA PM2.5 monitors data, defines
#separate data frames for specific analysis purposes and overlays these data
#frames with satellite data (impacted monitors only).
# UPDATED: 06-16-2025
# #############################################################################


###### Process Monitor Data ###### 

# ---- Generate monitor_data_impacted data frame, which includes both original
# and updated readings for the impacted monitors ----

impacted_monitors <- monitor_all %>%
  # Filter to monitors that were impacted by miscalibration
  filter(method_code %in% c(original_codes, updated_codes)) %>% 
  # Within the full data for impacted monitors, distinguish between original
  # (incorrect: 236/238) and updated (correct: 736/738) readings
  mutate(method_type = if_else(method_code %in% original_codes, "Original", "Updated"))

# Identify only the impacted sites (method_type = 236/238 --> method_type =
# 736/738) 
impacted_site_ids <- impacted_monitors %>%
  group_by(site_id) %>%
  filter(any(method_type == "Original") & any(method_type == "Updated")) %>%
  distinct(site_id) %>%
  pull()

# Store the full data (original and updated versions) for impacted monitors in
# new data frame
monitor_data_impacted <- impacted_monitors %>%
  filter(site_id %in% impacted_site_ids)

# Check counts
total_sites   <- n_distinct(impacted_monitors$site_id)
impacted_sites <- length(impacted_site_ids)
dropped_sites <- total_sites - impacted_sites

cat(
  "Sites in 4-code subset:   ", total_sites,   "\n",
  "Sites w/ both Original & Updated: ", impacted_sites, "\n",
  "Sites dropped by filter: ", dropped_sites, "\n",
  "Rows in updated dataset: ", nrow(monitor_data_impacted), "\n",
  "Rows in original dataset: ", nrow(impacted_monitors), "\n"
)

###### Overlay Ground-Level PM2.5 Monitor Data with Satellite Data ######

# ---- Call function to overlay data ----

monitor_satellite_data_impacted <- overlay_monitor_satellite(
  monitor_data_impacted, satellite_files, 2017:2022,
  original_label = "Original", updated_label = "Updated",
  out_names = c("Original_monitor_data_impacted", "Updated_monitor_data_impacted", "Satellite")
)

monitor_satellite_long_impacted <- pivot_longer(
  monitor_satellite_data_impacted,
  cols = c("Original_monitor_data_impacted", "Updated_monitor_data_impacted"),
  names_to = "Source", values_to = "Monitor_PM2.5"
)




