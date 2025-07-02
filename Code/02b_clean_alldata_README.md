# README for 02b_clean_alldata.R
*Author: Zoe Mitchell*  
*Last updated: July 1, 2025*

---

This script processes and cleans the full EPA PM2.5 monitor dataset, including both impacted (originally incorrect) and unimpacted monitors. It should be run after `01_load_data.R` and before any analysis or visualization scripts that use all monitor data.

---

## Purpose

- Filters and prepares the complete monitor dataset (impacted and unimpacted monitors).
- Separates and labels original and updated data for all monitors.
- Performs additional cleaning and formatting for the combined dataset.
- Creates data frames for original, updated, and combined monitor data across all sites.
- Prepares the full monitor dataset for overlay with satellite data and for downstream analysis.

---

## Usage

1. **Ensure `00_setup.R` and `01_load_data.R` have been run** to set up the environment and load the data.
2. **Open `02b_clean_alldata.R` in RStudio or your preferred R environment.**
3. **Run the entire script** (recommended: `Cmd + Shift + Enter` on Mac).
4. Proceed to run subsequent scripts for analysis and visualization of all monitor data.

---

## Key Components

### Data Filtering

- Includes both impacted and unimpacted monitors in the analysis.
- Separates and labels original and updated data for all monitors.

### Data Preparation

- Cleans and formats the full monitor dataset for analysis.
- Prepares combined data frames for comparison and plotting.
- Ensures all data is ready for overlay with satellite data.

### Output

- Saves cleaned and prepared full monitor data frames to the output directory for use in later scripts.

---

## Notes

- **Edit file paths** in `00_setup.R` if your folder structure is different.
- This script assumes all necessary data has been loaded and cleaned by previous scripts.
- All downstream scripts using the full monitor dataset assume this cleaning step has been completed.
- For questions about data sources or structure, see the main project README.

---