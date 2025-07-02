# README for 03b_figures_alldata.R
*Author: Zoe Mitchell*  
*Last updated: July 1, 2025*

---

This script generates figures and summary tables for the full EPA PM2.5 monitor dataset, including both impacted (originally incorrect) and unimpacted monitors. It should be run after `02b_clean_alldata.R` and is intended for visualizing and summarizing results across all monitors.

---

## Purpose

- Produces density plots, annual averages, and other visualizations for all monitors.
- Generates summary tables comparing original and updated data across the full dataset.
- Saves all figures and tables to the output directory for reporting and further analysis.

---

## Usage

1. **Ensure `00_setup.R`, `01_load_data.R`, and `02b_clean_alldata.R` have been run** to set up the environment and prepare the full monitor data.
2. **Open `03b_figures_alldata.R` in RStudio or your preferred R environment.**
3. **Run the entire script** (recommended: `Cmd + Shift + Enter` on Mac).
4. Use the generated figures and tables for reporting or further analysis.

---

## Key Components

### Figure Generation

- Density plots comparing original and updated PM2.5 values for all monitors.
- Annual average plots and difference plots for the full dataset.
- Additional visualizations as needed for the combined data.

### Table Generation

- Summary statistics and comparison tables for original vs. updated data across all monitors.

### Output

- All figures and tables are saved to the output directory specified in `00_setup.R`.

---

## Notes

- **Edit file paths** in `00_setup.R` if your folder structure is different.
- This script assumes all necessary data has been prepared by previous scripts.
- For questions about data sources, structure, or interpretation, see the main project README.

---