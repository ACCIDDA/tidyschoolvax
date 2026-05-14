# tidyschoolvax

An R package to standardize and combine school-level vaccination data for the United States. The project aims to assess age-specific susceptibility and clustering in non-vaccinated populations at the county and school district levels, supporting public health preparedness and outbreak response efforts.

## Project Overview

tidyschoolvax processes and analyzes vaccination coverage data from multiple U.S. states to create standardized immunity profiles. The project is particularly focused on measles vaccination coverage using MMR (Measles, Mumps, and Rubella) vaccine data from kindergarten enrollment records.

## Installation

Install the development version from GitHub:

```r
# Install remotes if not already installed
install.packages("remotes")

# Install tidyschoolvax from GitHub
remotes::install_github("ACCIDDA/tidyschoolvax")
```

Alternatively, using `devtools`:

```r
install.packages("devtools")
devtools::install_github("ACCIDDA/tidyschoolvax")
```

To install with vignettes built:

```r
remotes::install_github("ACCIDDA/tidyschoolvax", build_vignettes = TRUE)
```

## Environment Setup

The geocoding and school status lookup steps require a Google Maps API key with the Geocoding API and Places API enabled. Set your key in your R session or `.Renviron` file before running the preprocessing pipeline:

```r
# Set for the current session
Sys.setenv(GOOGLEGEO_API_KEY = "YOUR_GOOGLE_API_KEY")

# Or add to ~/.Renviron (persistent across sessions):
# GOOGLEGEO_API_KEY=YOUR_GOOGLE_API_KEY
```

Never hard-code API keys in scripts or commit them to version control.

## Vignettes

The package includes a vignette that walks through the complete preprocessing pipeline from data download through geocoding:

```r
# List available vignettes
vignette(package = "tidyschoolvax")

# Open the preprocessing orchestration vignette
vignette("preprocessing-orchestration", package = "tidyschoolvax")
```

The vignette covers:

1. **Configuration** – specifying state, vaccine type, and API key
2. **Agnostic data download** – GreatSchools.org and CDC VaxView data
3. **State-specific download** – kindergarten vaccination and DOE/DOA school data
4. **School standardization** – name matching, unique ID assignment, and geocoding
5. **State-specific cleaning** – applying state-level data cleaning rules
6. **Final formatting and DQA** – formatting, data quality checks, and output generation

## Quick Start

```r
library(tidyschoolvax)

# 1. Set up directory paths for a state
paths <- setup_paths(project_root = here::here(), state = "ca")
list2env(paths, envir = environment())

# 2. Download state-agnostic data (GreatSchools.org, CDC VaxView)
download_agnostic_data(
  state            = "ca",
  state_name       = "California",
  greatschools_dir = greatschools_dir,
  vaxview_dir      = vaxview_dir
)

# 3. Standardize school names, assign unique IDs, and geocode
school_vax_joined <- standardize_schools(
  state_id         = "ca",
  kinder_dir       = kinder_dir,
  greatschools_dir = greatschools_dir,
  doe_dir          = doe_dir,
  state_school_dir = state_school_dir,
  temp_data_dir    = temp_data_dir,
  state_geo_dir    = state_geo_dir,
  state_dir        = state_dir,
  addr_source_pref = "kinder"
)

# 4. Apply final formatting and DQA checks
run_final_formatting(
  state                = "ca",
  vaccine_type_to_keep = "mmr",
  temp_data_dir        = temp_data_dir,
  clean_data_dir       = clean_data_dir,
  vaxview_dir          = vaxview_dir,
  general_data_dir     = general_data_dir,
  state_dir            = state_dir,
  outputs_data_dir     = outputs_data_dir
)
```

See `vignette("preprocessing-orchestration", package = "tidyschoolvax")` for a complete, annotated walkthrough.

## Key Functions

### Path Setup

| Function | Description |
|---|---|
| `setup_paths()` | Build the standard directory tree for a given state |

### Data Download

| Function | Description |
|---|---|
| `download_agnostic_data()` | Download GreatSchools.org and CDC VaxView data |
| `scrape_greatschools_schools()` | Scrape school listings from GreatSchools.org |
| `combine_vaxview_data()` / `save_vaxview_parquet()` | Load and save CDC VaxView data |

### Preprocessing and Standardization

| Function | Description |
|---|---|
| `standardize_schools()` | Match school names across sources, assign IDs, and geocode |
| `clean_kinder_data()` | Clean and standardize kindergarten vaccination data |
| `clean_doe_data()` | Clean Department of Education school roster data |
| `clean_greatschools_data()` | Clean GreatSchools.org data |
| `standardize_kinder_format()` | Standardize column formats for kindergarten data |
| `run_final_formatting()` | Apply final formatting rules and DQA checks |

### Data Quality Assurance (DQA)

| Function | Description |
|---|---|
| `run_dqa_checks()` | Run all DQA checks and return a summary and detailed results |
| `check_duplicates()` | Identify duplicate school-year records |
| `check_negative_values()` | Find records with negative values |
| `check_exceeding_enrollment_values()` | Find counts that exceed enrollment |
| `check_coverage_outliers()` | Detect unrealistic coverage percentages |
| `check_enrollment_deviation()` | Flag unusual year-over-year enrollment changes |
| `check_vaccination_deviation()` | Flag unusual year-over-year vaccination count changes |
| `check_extreme_outliers()` | Identify likely data entry errors |
| `generate_dqa_summary()` | Write a summary CSV of DQA results |
| `generate_detailed_dqa_report()` | Write per-check detail files of flagged records |

### Utilities

| Function | Description |
|---|---|
| `clean_state_data()` | Format raw state data to standardized column requirements |
| `standardized_school_name()` / `standardized_county_name()` | Normalize name strings |
| `match_locations()` | Match schools across data sources by string distance |
| `get_geo_info()` | Geocode addresses via the Google Geocoding API |
| `check_expected_files()` | Verify that expected output files exist after each step |
| `create_state_download_script()` | Scaffold a new state-specific download script |

## Repository Structure

```
tidyschoolvax/
├── R/                      # Package source functions
├── man/                    # Auto-generated function documentation
├── vignettes/              # Package vignettes
│   └── preprocessing-orchestration.Rmd
├── tests/                  # Unit tests (testthat)
├── inst/templates/         # State-specific script templates
├── 00_preprocessing/       # State-specific preprocessing scripts and raw data
│   └── states/             # Per-state subdirectories (ca/, md/, nc/, ...)
├── 01_model/               # Statistical modeling and analysis
├── 02_visualization/       # Data visualization and reporting
├── DESCRIPTION
└── NAMESPACE
```

## Data Sources

The package currently supports preprocessing of vaccination coverage data from:

- **California** (CA)
- **Maryland** (MD)
- **North Carolina** (NC) – in development

Data includes:
- Kindergarten MMR vaccine coverage by school year
- School-level and county-level aggregations
- GreatSchools.org school reference data
- CDC VaxView national vaccination coverage data

## Project Context

This work supports ACCIDDA's (Academic Consortium for COVID-19 and Infectious Disease Data Analysis) efforts in infectious disease outbreak preparedness, particularly for measles. The project aims to provide rapid assessment capabilities for:

- Age-specific susceptibility by county
- Clustering in non-vaccinated populations
- School catchment area analysis
- Support for outbreak response modeling

## License

MIT License:

Copyright (c) tidyschoolvax contributors

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.

## Contributing

[Bug reports](https://github.com/ACCIDDA/tidyschoolvax/issues) and [pull requests](https://github.com/ACCIDDA/tidyschoolvax/pulls) are welcome on GitHub.

## Contact

For questions or contributions, please contact the ACCIDDA team or open an issue on GitHub.
