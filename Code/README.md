# EPA PM2.5 Monitor Verification

*Nick Silvis and Zoe Mitchell*  
*June 2025*

# Project Description

Between 2017 and 2023, a sizable fraction (up to 30%) of the EPA's over  1,000 PM2.5 monitors were producing upward biased readings. Given that ground-level PM2.5 readings are used as a benchmark to verify satellite data, these inaccurate readings have possible consequences beyond the EPA's datasets. This project seeks to understand the extent and implications of this miscalibration by comparing the EPA's original (incorrect) and updated (corrected) datasets and examining them against satellite data.

# File and Directory Structure


# Data Sources

This project uses two primary datasets:

1. *EPA PM2.5 Daily Average Datasets*: These consists of both the original (incorrect) and updated (corrected) readings for the impacted (initially incorrect) and unimpacted (never-incorrect) monitors. There are five individual datasets for each year, which we combine to form one large dataset.
2. *Satellite Data* *Ask Nick for more details on this

# Methodology

* *Negative values:* There were roughly 21,000 negative values for PM2.5, all concentrated amongst the never-incorrect monitors. We dropped these observations as PM2.5, measured by volume (µg/m³), can never be negative.

# Data Description

Below are descriptions of the data frames, vectors, and columns that we defined (within each category, the item is listed in the order in which it was defined).

Data frames:

* *monitor_all*: contains data on every monitor for all five years (formed by binding the individaul EPA datasets from 2017-2023). Originally incorrect monitors are listed twice, once with method_code 236 or 238 (marking the incorrect measurement) and again with method_code 736 or 738 (marking the corrected measurement).  
* *impacted_monitors*: contains both original and updated data for any originally incorrect monitor.
* 

Vectors:

* *impacted_site_ids*: a list of the site ids of all originally incorrect monitors.

Columns:

* *site_id*: a unique monitor identifier, constructed by concatenating the monitor's state ID, county ID, and site number.
