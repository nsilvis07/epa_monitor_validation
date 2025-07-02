# README for 04a_regressions_impactedonly.R
*Author: Zoe Mitchell*  
*Last updated: July 1, 2025*

---

This script runs regression analyses and generates regression tables for the subset of EPA PM2.5 monitor data corresponding to impacted (originally incorrect) monitors only. It should be run after `03a_figures_impactedonly.R` and is intended for statistical modeling and reporting focused on impacted monitors.

---

## Purpose

- Fits linear regression models relating monitor PM2.5 values to satellite PM2.5 values for impacted monitors.
- Runs separate regressions for original and updated monitor data.
- Generates a LaTeX regression table comparing results for original and updated data.
- Saves the regression table to the output directory for inclusion in reports or manuscripts.

---

## Usage

1. **Ensure `00_setup.R`, `01_load_data.R`, `02a_clean_impactedonly.R`, and `03a_figures_impactedonly.R` have been run** to prepare the impacted monitor data.
2. **Open `04a_regressions_impactedonly.R` in RStudio or your preferred R environment.**
3. **Run the entire script** (recommended: `Cmd + Shift + Enter` on Mac).
4. Use the generated regression table for reporting or further analysis.

---

## Key Components

### Regression Analysis

- Fits linear models for both original and updated impacted monitor data using satellite PM2.5 as a predictor.
- Uses the `lm()` function for model fitting.

### Table Generation

- Outputs a formatted LaTeX regression table using the `stargazer` package.
- Table includes results for both original and updated data side by side.

### Output

- The LaTeX regression table is saved to the output directory specified in `00_setup.R`.

---

## Notes

- **Edit file paths** in `00_setup.R` if your folder structure is different.
- This script assumes all necessary data has been prepared by previous scripts.
- For questions about data sources, structure, or interpretation, see the main project README.

---