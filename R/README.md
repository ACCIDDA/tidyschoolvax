# Modular Preprocessing Functions

This directory contains modular R functions for preprocessing and quality assurance of state-level school vaccination data. These functions are designed to be package-friendly and reusable across different states.

## Overview

The modular preprocessing workflow consists of three main stages:

1. **State-Specific Data Cleaning**: Standardize data formats and validate column types
2. **Generalized DQA Checks**: Run automated data quality assurance checks
3. **Report Generation**: Create summary reports of identified issues

## Functions

### 1. State-Specific Data Cleaning

#### `clean_state_data()`

Formats raw state-specific data to meet standardized requirements.

**Purpose:**
- Ensures required columns exist
- Validates and coerces column data types
- Removes rows with zero/missing enrollment
- Filters invalid vaccination counts

**Parameters:**
- `state_mmr`: Data frame with raw state data
- `required_cols`: Vector of required column names
- `optional_cols`: Vector of optional column names
- `expected_classes`: Named list of expected data types

**Returns:** Cleaned data frame with standardized columns

**Example:**
```r
cleaned_data <- clean_state_data(
  state_mmr = raw_state_data,
  required_cols = c("year", "school_id", "enrollment", "current", 
                    "med_exempt", "rel_exempt", "delayed"),
  optional_cols = c("school_type")
)
```

### 2. Data Quality Assurance (DQA) Checks

Individual check functions that can be used independently or via the wrapper function.

#### `check_duplicates(data, core_cols = NULL)`

Identifies duplicate school_id-year combinations and determines if they have identical or conflicting data.

**Returns:** List with `identical_dupes` and `conflicting_dupes` data frames

#### `check_negative_values(data, cols_to_check = c("current", "delayed", "med_exempt", "rel_exempt", "enrollment"))`

Finds records with negative values in numeric columns.

**Returns:** Data frame of rows with negative values

#### `check_exceeding_enrollment_values(data, cols_to_check = c("current", "delayed", "med_exempt", "rel_exempt"))`

Identifies records where individual counts exceed total enrollment.

**Returns:** Data frame of rows with values exceeding enrollment

#### `check_coverage_outliers(data, coverage_threshold = 1.05)`

Detects unrealistic coverage percentages and cases where sum of counts exceeds enrollment.

**Returns:** List with `coverage_outliers` and `over_coverage` data frames

#### `check_enrollment_deviation(data, deviation_threshold = 0.5)`

Finds schools where enrollment deviates significantly from historical average.

**Returns:** List with two components:
- `flagged`: Data frame of rows with enrollment deviations >50% (default), including `mean_enrollment` and `pct_diff_from_mean` columns
- `school_history`: Data frame with all years of data for flagged schools (for context when reviewing issues)

#### `check_vaccination_deviation(data, deviation_threshold = 0.5)`

Finds schools where vaccination counts deviate significantly from historical average.

**Returns:** List with two components:
- `flagged`: Data frame of rows with vaccination count deviations >50% (default), including `mean_current` and `pct_diff_from_mean` columns
- `school_history`: Data frame with all years of data for flagged schools (for context when reviewing issues)

#### `check_extreme_outliers(data, multiplier = 10)`

Identifies likely typos or extreme outliers (e.g., enrollment 10x typical for school).

**Returns:** Data frame of rows with extreme outlier values

### 3. Wrapper and Report Functions

#### `run_dqa_checks()`

Executes multiple DQA checks sequentially and captures results.

**Parameters:**
- `data`: Data frame to check
- `checks`: Vector of check names or "all" (default)
- `deviation_threshold`: Threshold for deviation checks (default: 0.5)
- `coverage_threshold`: Threshold for coverage checks (default: 1.05)
- `outlier_multiplier`: Multiplier for outlier detection (default: 10)
- `output_dir`: Directory to save individual check results (optional)
- `state`: State abbreviation for file naming (optional)

**Returns:** List with:
- `summary`: Data frame summarizing issue counts, with columns: `check`, `issue_count`, `action_type` ("immediate_change" or "requires_review")
- `results`: Named list of detailed results
- `data_cleaned`: Input data with duplicates resolved

**Example:**
```r
dqa_results <- run_dqa_checks(
  data = cleaned_data,
  checks = "all",
  output_dir = "output/dqa",
  state = "ca"
)
```

#### `generate_dqa_summary()`

Creates a summary report of DQA check results.

**Parameters:**
- `dqa_results`: Output from `run_dqa_checks()`
- `output_path`: Full path for report file (.csv, .rds, or .xlsx)
- `state`: State abbreviation (optional)
- `include_timestamp`: Include timestamp in report (default: TRUE)

**Example:**
```r
generate_dqa_summary(
  dqa_results = dqa_results,
  output_path = "output/ca_dqa_summary.csv",
  state = "ca"
)
```

#### `generate_detailed_dqa_report()`

Creates detailed reports with all flagged records from each check.

**Parameters:**
- `dqa_results`: Output from `run_dqa_checks()`
- `output_dir`: Directory for detailed reports
- `state`: State abbreviation (optional)
- `consolidate`: Create single file with all issues (default: FALSE)
- `format`: Output format - "csv", "rds", or "xlsx" (default: "csv")

**Notes for deviation checks:**
- For enrollment and vaccination deviation checks, two files are saved:
  - `*_enrollment_deviation_flagged.*`: Only the rows that deviate from the school mean
  - `*_enrollment_deviation_school_history.*`: All years for the flagged schools (for context)

**Example:**
```r
generate_detailed_dqa_report(
  dqa_results = dqa_results,
  output_dir = "output/dqa_details",
  state = "ca",
  consolidate = TRUE
)
```

## Standard Workflow

### Complete Example

```r
# 1. Load packages
library(tidyverse)
library(data.table)

# 2. Source functions (or load package when available)
source("R/clean_state_data.R")
source("R/dqa_checks.R")
source("R/run_dqa_checks.R")
source("R/generate_dqa_summary.R")

# 3. Load state-specific data (after initial formatting)
state_mmr <- readRDS("data/ca/ca_mmr_clean_full_columns.RDS")

# 4. Clean and standardize data
cleaned_data <- clean_state_data(state_mmr)

# 5. Run DQA checks
dqa_results <- run_dqa_checks(
  data = cleaned_data,
  checks = "all",
  output_dir = "data/ca/dqa_checks",
  state = "ca"
)

# 6. Generate summary reports
generate_dqa_summary(
  dqa_results = dqa_results,
  output_path = "data/ca/ca_dqa_summary.csv",
  state = "ca"
)

# 7. Generate detailed reports
generate_detailed_dqa_report(
  dqa_results = dqa_results,
  output_dir = "data/ca/dqa_details",
  state = "ca",
  consolidate = TRUE
)

# 8. Access cleaned data for further processing
final_data <- dqa_results$data_cleaned

# 9. Save for process model
state_mmr_dt <- as.data.table(final_data)
saveRDS(state_mmr_dt, "data/ca/ca_cleaned_data.RDS")
```

## Data Requirements

### Input Data Format

The input data (before `clean_state_data()`) should contain:

**Required columns:**
- `year`: Integer, kindergarten starting year
- `school_id`: Integer, unique school identifier
- `school_name`: Character, school name
- `cnty_id`: Integer, county identifier
- `school_county`: Character, county name
- `enrollment`: Integer, total students enrolled
- `current`: Integer, students with current MMR status
- `med_exempt`: Integer, students with medical exemption
- `rel_exempt`: Integer, students with religious/personal exemption
- `delayed`: Integer, students without current status or exemption

**Optional columns:**
- `school_type`: Character (e.g., "public", "private")

### Output Data Format

After cleaning and DQA checks, the data will have:
- All required columns with validated types
- No rows with zero/missing enrollment
- No rows with zero current vaccination count
- Duplicate rows merged (when identical)
- All validation issues documented in DQA reports

## DQA Check Types

### 1. Duplicates
- **What it checks**: School_id-year combinations that appear multiple times
- **Action**: Merges identical duplicates; flags conflicting duplicates for review

### 2. Negative Values
- **What it checks**: Any negative numbers in enrollment or count fields
- **Action**: Flags all rows with negative values

### 3. Exceeding Enrollment
- **What it checks**: Individual counts (current, exempt, delayed) > enrollment
- **Action**: Flags all rows where any single count exceeds enrollment

### 4. Coverage Outliers
- **What it checks**: 
  - MMR coverage < 0% or > 105%
  - Sum of all counts > enrollment
- **Action**: Flags unrealistic coverage rates and over-counting

### 5. Enrollment Deviation
- **What it checks**: School enrollment differs >50% from school's historical average
- **Action**: Flags potential data entry errors or major changes

### 6. Vaccination Deviation
- **What it checks**: Vaccination count differs >50% from school's historical average
- **Action**: Flags potential data entry errors or major changes

### 7. Extreme Outliers
- **What it checks**: Enrollment values >10x school's median enrollment
- **Action**: Flags likely typos or data errors

## Customization

### Adjust Thresholds

You can customize thresholds for your specific needs:

```r
dqa_results <- run_dqa_checks(
  data = cleaned_data,
  deviation_threshold = 0.3,  # 30% instead of 50%
  coverage_threshold = 1.02,   # 102% instead of 105%
  outlier_multiplier = 5       # 5x instead of 10x
)
```

### Run Specific Checks

To run only certain checks:

```r
dqa_results <- run_dqa_checks(
  data = cleaned_data,
  checks = c("negatives", "coverage_outliers", "duplicates")
)
```

### Add Custom Checks

To add a new check, create a function following the pattern:

```r
check_custom <- function(data, ...) {
  # Perform check
  issues <- data[condition, ]
  
  # Print message
  if (nrow(issues) > 0) {
    message("Found ", nrow(issues), " issues.")
  }
  
  # Return flagged rows
  return(issues)
}
```

Then add it to `run_dqa_checks()` or call it independently.

## Post-DQA Manual Adjustments

After running DQA checks:

1. **Review Reports**: Check CSV files in the output directory
2. **Prioritize Issues**: Focus on conflicting duplicates, negative values, and coverage outliers
3. **Investigate**: Look into the flagged records to determine root cause
4. **Correct Data**: Fix issues in the source data or document decisions
5. **Re-run Checks**: Verify that corrections resolved the issues
6. **Document**: Keep a log of manual adjustments for reproducibility

## Integration with Package

These functions are designed to be included in an R package structure:

```
tidyschoolvax/
├── R/
│   ├── clean_state_data.R
│   ├── dqa_checks.R
│   ├── run_dqa_checks.R
│   └── generate_dqa_summary.R
├── man/           # Generated documentation (roxygen2)
├── tests/         # Unit tests (testthat)
├── DESCRIPTION    # Package metadata
├── NAMESPACE      # Package namespace
└── README.md      # Package documentation
```

## Dependencies

Required packages:
- `base` (no additional installation needed)
- `utils` (for write.csv, etc.)

Optional packages:
- `writexl` (for Excel output)
- `data.table` (for efficient data handling)

## Future Enhancements

Potential improvements:
- [ ] Add visualization functions for DQA results
- [ ] Implement automated correction for common issues
- [ ] Add batch processing for multiple states
- [ ] Create interactive Shiny app for DQA review
- [ ] Add unit tests for all functions
- [ ] Create vignettes with detailed examples

## Support

For questions or issues:
- Check the example workflow: `00_preprocessing/example_modular_workflow.R`
- Review function documentation (roxygen2 headers)
- Contact the ACCIDDA team

## Citation

If you use these functions in your research, please cite:
```
ACCIDDA Team (2026). tidyschoolvax: Modular Preprocessing Functions
for School Vaccination Data. GitHub repository.
```
