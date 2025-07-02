# README for 00_setup.R
*Author: Zoe Mitchell*  
*Last updated: July 1, 2025*

---

This script initializes the environment for the EPA PM2.5 Monitor Verification project. It should be run before any other analysis scripts (01–03) to ensure all dependencies, directories, parameters, and functions are correctly set up.

---

## Purpose

- Clears the R environment.
- Installs and loads all required R packages.
- Sets working and data/output directories.
- Defines key parameters, color palettes, and file paths.
- Provides reusable functions for overlaying monitor and satellite data, and for generating all figures and tables used in the analysis.

---

## Usage

1. **Open `00_setup.R` in RStudio or your preferred R environment.**
2. **Run the entire script** (recommended: `Cmd + Shift + Enter` on Mac).
3. Proceed to run subsequent scripts (`01_*.R`, `02_*.R`, etc.) for data processing and analysis.

---

## Key Components

### Package Management

- Uses `pacman` to install and load all required packages:
  - Data wrangling: `dplyr`, `tidyr`, `tibble`, `janitor`, `readr`
  - Spatial: `raster`, `sp`, `sf`, `tigris`, `smoothr`, `units`
  - Plotting: `ggplot2`, `cowplot`, `grid`, `gtable`, `scales`
  - Reporting: `stargazer`

### Directory and File Paths

- `main_dir`: Main project directory (edit if your folder structure differs).
- `data_dir`, `output_dir`: Subdirectories for data and output.
- `monitor_files`, `satellite_files`: Lists of file paths for EPA and satellite datasets.

### Parameters

- `years_monitors`, `years_satellite`: Year ranges for monitor and satellite data.
- `original_codes`, `updated_codes`: Method codes for original and updated monitor data.

### Color Palettes

- `cbPalette`: Colorblind-friendly palette for plots.
- `source_colors`: Named colors for different data sources.

### Functions

Reusable functions for:
- Overlaying monitor and satellite data (`overlay_monitor_satellite`)
- Plotting density, annual averages, annual differences, QQ plots, and scatter plots

All plots are saved to the `output_dir`.

---

## Notes

- **Edit directory paths** at the top of the script if your folder structure is different.
- All downstream scripts assume this setup has been run.
- For questions about data sources or structure, see the main project README.

---

