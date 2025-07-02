# README for 01_load_data.R
*Author: Zoe Mitchell*  
*Last updated: July 1, 2025*

---

This script loads, cleans, and prepares the EPA PM2.5 monitor and satellite datasets for analysis in the EPA PM2.5 Monitor Verification project. It should be run after `00_setup.R` and before any analysis or visualization scripts.

---

## Purpose

- Loads EPA PM2.5 daily average data for all years and monitors.
- Loads satellite PM2.5 data for the relevant years.
- Cleans and combines datasets, including:
  - Removing negative PM2.5 values.
  - Creating unique site identifiers.
  - Merging original and updated monitor data.
  - Preparing data frames for impacted and unimpacted monitors.
- Saves cleaned and combined data frames for downstream analysis.

---

## Usage

1. **Ensure `00_setup.R` has been run** to set up the environment and directories.
2. **Open `01_load_data.R` in RStudio or your preferred R environment.**
3. **Run the entire script** (recommended: `Cmd + Shift + Enter` on Mac).
4. Proceed to run subsequent scripts (`02_*.R`, `03_*.R`, etc.) for analysis and visualization.

---

## Key Components

### Data Loading

- Reads in EPA monitor data files for each year (2017–2023).
- Loads satellite PM2.5 data files for each year (2017–2022).

### Data Cleaning and Preparation

- Drops negative PM2.5 values (which are not physically meaningful).
- Constructs `site_id` by concatenating state, county, and site numbers.
- Identifies and separates impacted (originally incorrect) and unimpacted monitors.
- Merges original and updated data for impacted monitors.
- Prepares combined data frames for analysis and plotting.

### Output

- Saves cleaned and combined data frames to the output directory for use in later scripts.

---

## Notes

- **Edit file paths** in `00_setup.R` if your folder structure is different.
- This script assumes all raw data files are present in the specified `data_dir`.
- All downstream scripts assume this data loading and cleaning step has been completed.
- For questions about data sources or structure, see the main project README.

---