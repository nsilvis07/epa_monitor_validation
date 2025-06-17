# EPA PM2.5 Monitor Verification

*Nick Silvis and Zoe Mitchell*  
*June 2025*

# Project Description

Between 2017 and 2023, a sizable fraction (up to 30%) of the EPA's roughly 1,000 PM2.5 monitors were producing upward biased readings. Given that ground-level PM2.5 readings are used as a benchmark to verify satellite data, these inaccurate readings have possible consequences beyond the EPA's datasets. This project seeks to understand the extent and implications of this miscalibration by comparing the EPA's original (incorrect) and updated (corrected) datasets and examining them against satellite data.

# File and Directory Structure



* *GitHub*: Code and README
* *DropBox* : Data and Output



# Data Sources

This project uses two primary datasets:

1. *EPA PM2.5 Daily Average Datasets*: These consists of both the original (incorrect) and updated (corrected) readings for the impacted (initially incorrect) and unimpacted (never-incorrect) monitors. There are five individual datasets for each year, which we combine to form one large dataset.
2. *Satellite Data* **Ask Nick for more details on this**

# Methodology

* *Negative values:* There were roughly 21,000 negative values for PM2.5, all concentrated amongst the never-incorrect monitors. We dropped these observations as PM2.5, measured by volume (µg/m³), can never be negative.

# Data Description

Below are descriptions of the vectors, columns, and data frames that we defined. Within each category, the item is listed in the order in which it was defined.

**Vectors**

* *years_monitors*: contains the range of years (2017-2023) for which we have EPA monitor data.
* *years_satellite*: contains the range of years (2017-2022) for which we have satellite data.
* *original_codes*: refers to the method_codes  that indicate original, incorrect data (236 and 238).
* *updated_codes*: refers to the method_codes that indicate updated, corrected data (736 and 738).
* *impacted_site_ids*: a list of the site ids corresponding to all originally incorrect monitors.

**Columns**

* *site_id*: a unique 9 digit site identifier, constructed by concatenating the monitor's state ID, county ID, and site number.
* *method_type*: distinguishes between original and updated data based on the presence of method_code 236 or 238 (indicating incorrect data) or method_code 736 or 738 (indicating corrected data).

**Data Frames**

* *monitor_all*: contains data on every monitor for all five years (formed by binding the individaul EPA datasets from 2017-2023). Originally incorrect monitors are listed twice, once with method_code 236 or 238 (marking the incorrect measurement) and again with method_code 736 or 738 (marking the corrected measurement).  
* *impacted_monitors*: contains both original and updated data for any originally incorrect monitor.
* *original_monitor_data_impacted*: contains the original data, only for impacted monitors.
* *updated_monitor_data_impated*: contains the updated data, only for impacted monitors.
* *monitor_combined*: derived from monitor_all; like impacted_monitors, but column method_type separates between "original" and "updated" data. 
* *monitor_satellite_long_impacted*: impacted_monitors overlayed with satellite data.
* *monitor_satellite_long_alldata*: monitor_data_combined overlayed with satellite data.
