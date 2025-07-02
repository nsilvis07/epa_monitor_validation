# README for 03a_figures_impactedonly.R
*Author: Zoe Mitchell*  
*Last updated: July 1, 2025*

---

This script generates figures and summary tables for the subset of EPA PM2.5 monitor data corresponding to impacted (originally incorrect) monitors only. It should be run after `02a_clean_impactedonly.R` and is intended for visualizing and summarizing results specific to impacted monitors.

---

## Purpose

- Produces density plots, annual averages, and other visualizations for impacted monitors.
- Generates summary tables comparing original and updated data for impacted monitors.
- Saves all figures and tables to the output directory for reporting and further analysis.

---

## Usage

1. **Ensure `00_setup.R`, `01_load_data.R`, and `02a_clean_impactedonly.R` have been run** to set up the environment and prepare the impacted monitor data.
2. **Open `03a_figures_impactedonly.R` in RStudio or your preferred R environment.**
3. **Run the entire script** (recommended: `Cmd + Shift + Enter` on Mac).
4. Use the generated figures and tables for reporting or further analysis.

---

## Key Components

### Figure Generation

- Density plots comparing original and updated PM2.5 values for impacted monitors.
- Annual average plots and difference plots for impacted monitors.
- Additional visualizations as needed for the impacted subset.

### Table Generation

- Summary statistics and comparison tables for original vs. updated data.

### Output

- All figures and tables are saved to the output directory specified in `00_setup.R`.

---

## Notes

- **Edit file paths** in `00_setup.R` if your folder structure is different.
- This script assumes all necessary data has been prepared by previous scripts.
- For questions about data sources, structure, or interpretation, see the main project README.

---