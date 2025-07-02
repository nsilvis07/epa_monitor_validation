# README for 02a_clean_impactedonly.R
*Author: Zoe Mitchell*  
*Last updated: July 1, 2025*

---

This script processes and cleans the subset of EPA PM2.5 monitor data corresponding to impacted (originally incorrect) monitors only. It should be run after `01_load_data.R` and before any analysis or visualization scripts that focus on impacted monitors.

---

## Purpose

- Filters the loaded data to include only impacted monitors (those with originally incorrect readings).
- Separates original and updated data for these monitors.
- Performs additional cleaning and preparation specific to the impacted subset.
- Creates data frames for original, updated, and combined impacted monitor data.
- Prepares the impacted monitor data for overlay with satellite data and for downstream analysis.

---

## Usage

1. **Ensure `00_setup.R` and `01_load_data.R` have been run** to set up the environment and load the data.
2. **Open `02a_clean_impactedonly.R` in RStudio or your preferred R environment.**
3. **Run the entire script** (recommended: `Cmd + Shift + Enter` on Mac).
4. Proceed to run subsequent scripts for analysis and visualization of impacted monitor data.

---

## Key Components

### Data Filtering

- Selects only those monitors identified as impacted (using `impacted_site_ids`).
- Separates and labels original and updated data for these monitors.

### Data Preparation

- Cleans and formats the impacted monitor data for analysis.
- Prepares combined data frames for comparison and plotting.
- Ensures all data is ready for overlay with satellite data.

### Output

- Saves cleaned and prepared impacted monitor data frames to the output directory for use in later scripts.

---

## Notes

- **Edit file paths** in `00_setup.R` if your folder structure is different.
- This script assumes all necessary data has been loaded and cleaned by previous scripts.
- All downstream scripts focusing on impacted monitors assume this cleaning step has been completed.
- For questions about data sources or structure, see the main project README.

---