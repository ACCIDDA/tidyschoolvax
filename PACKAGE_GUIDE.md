# Converting to an R Package

This document explains how to convert the current repository structure into a formal R package.

## Current Structure

```
tidyschoolvax/
├── R/                          # ✅ Package functions (ready)
│   ├── clean_state_data.R
│   ├── dqa_checks.R
│   ├── run_dqa_checks.R
│   └── generate_dqa_summary.R
├── tests/                      # ✅ Test scripts
│   └── test_modular_functions.R
├── DESCRIPTION                 # ✅ Package metadata (created)
├── NAMESPACE                   # ✅ Package namespace (created)
└── .gitignore                  # ✅ Ignores build artifacts
```

## Steps to Complete Package Conversion

### 1. Install Required Development Tools

```r
install.packages(c("devtools", "roxygen2", "testthat", "usethis"))
```

### 2. Initialize Package Structure (Optional)

If you want to formalize the package structure further:

```r
library(usethis)

# Add LICENSE file
use_mit_license("ACCIDDA Team")

# Add NEWS.md for tracking changes
use_news_md()

# Set up testthat for unit testing
use_testthat()

# Add package-level documentation
use_package_doc()
```

### 3. Document Functions

The functions already have roxygen2 documentation headers. To generate the documentation:

```r
library(devtools)
library(roxygen2)

# Generate documentation
document()

# This will create/update:
# - man/*.Rd files (help documentation)
# - NAMESPACE (if you use @export tags)
```

### 4. Convert Tests to testthat Format

Move the test script to the proper testthat structure:

```r
library(usethis)

# Create testthat structure
use_testthat()

# Move tests/test_modular_functions.R to tests/testthat/
# Rename to tests/testthat/test-modular-functions.R
```

Example testthat format:

```r
# tests/testthat/test-clean-state-data.R
test_that("clean_state_data handles valid input", {
  test_data <- data.frame(
    year = 2020L,
    school_id = 1L,
    school_name = "Test School",
    cnty_id = 1L,
    school_county = "Test County",
    enrollment = 100L,
    current = 90L,
    med_exempt = 2L,
    rel_exempt = 3L,
    delayed = 5L
  )
  
  result <- clean_state_data(test_data)
  
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
  expect_true(all(c("year", "school_id", "enrollment") %in% names(result)))
})
```

### 5. Build and Check Package

```r
library(devtools)

# Check package for errors
check()

# Build package
build()

# Install locally
install()

# Load and test
library(tidyschoolvax)
?clean_state_data
```

### 6. Add Vignettes (Optional)

Create detailed tutorials for users:

```r
library(usethis)

# Create a vignette
use_vignette("preprocessing-workflow", "Complete Preprocessing Workflow")
```

Example vignette content:

```r
---
title: "Complete Preprocessing Workflow"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{Complete Preprocessing Workflow}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---

# Introduction

This vignette demonstrates the complete workflow for preprocessing 
state-level school vaccination data using tidyschoolvax.

# Loading Data

...
```

### 7. Set Up Continuous Integration (Optional)

Add GitHub Actions for automated testing:

```r
library(usethis)

# Add GitHub Actions for R CMD check
use_github_action("check-standard")

# Add GitHub Actions for test coverage
use_github_action("test-coverage")
```

### 8. Create pkgdown Website (Optional)

Generate a website for your package documentation:

```r
library(usethis)

# Set up pkgdown
use_pkgdown()

# Build site locally
pkgdown::build_site()

# Set up GitHub Actions to auto-deploy
use_github_action("pkgdown")
```

## Using the Package

### Installation from GitHub

Once published, users can install with:

```r
# Install devtools if not already installed
install.packages("devtools")

# Install tidyschoolvax from GitHub
devtools::install_github("ACCIDDA/tidyschoolvax")
```

### Basic Usage

```r
library(tidyschoolvax)

# Load your data
state_data <- readRDS("data/ca/ca_mmr_clean_full_columns.RDS")

# Clean data
cleaned <- clean_state_data(state_data)

# Run DQA checks
results <- run_dqa_checks(cleaned, state = "ca")

# Generate report
generate_dqa_summary(results, "output/summary.csv", state = "ca")
```

## Package Development Workflow

### Making Changes

1. **Edit functions** in `R/`
2. **Update documentation** in roxygen2 headers
3. **Run `document()`** to update help files
4. **Run `check()`** to test for issues
5. **Run tests** with `test()`
6. **Commit changes** to git

### Adding New Functions

```r
library(usethis)

# Create a new R file
use_r("new_function_name")

# Add roxygen2 documentation
#' @title New Function
#' @description Does something useful
#' @param x Input parameter
#' @return Output description
#' @export
new_function <- function(x) {
  # Implementation
}

# Update documentation
document()

# Add tests
use_test("new_function_name")
```

### Version Management

Update version in DESCRIPTION file following semantic versioning:

- **Major** (1.0.0): Breaking changes
- **Minor** (0.1.0): New features, backwards compatible
- **Patch** (0.0.1): Bug fixes

```r
library(usethis)

# Increment version
use_version("minor")  # or "major" or "patch"
```

## Package Quality Checklist

Before releasing:

- [ ] All functions have roxygen2 documentation
- [ ] All functions have examples
- [ ] Package passes `R CMD check` with no errors
- [ ] Unit tests cover main functionality
- [ ] README.md is clear and comprehensive
- [ ] NEWS.md documents all changes
- [ ] Version number is updated
- [ ] All dependencies are listed in DESCRIPTION

## Advanced Features

### S3 Methods

Create custom classes for DQA results:

```r
# R/dqa_results.R
#' @export
print.dqa_results <- function(x, ...) {
  cat("DQA Results Summary\n")
  cat("===================\n")
  print(x$summary)
  invisible(x)
}

#' @export
summary.dqa_results <- function(object, ...) {
  # Custom summary
}
```

### Exported Data

Include example datasets:

```r
library(usethis)

# Add example data
use_data(example_state_data, overwrite = TRUE)

# Document the data
use_r("data")
```

Then in `R/data.R`:

```r
#' Example State Vaccination Data
#'
#' A sample dataset demonstrating the expected input format.
#'
#' @format A data frame with X rows and Y columns:
#' \describe{
#'   \item{year}{Kindergarten starting year}
#'   \item{school_id}{Unique school identifier}
#'   ...
#' }
"example_state_data"
```

### Package Options

Set default options:

```r
# R/zzz.R (runs on package load)
.onLoad <- function(libname, pkgname) {
  op <- options()
  op.usimmunity <- list(
    usimmunity.deviation_threshold = 0.5,
    usimmunity.coverage_threshold = 1.05
  )
  toset <- !(names(op.usimmunity) %in% names(op))
  if(any(toset)) options(op.usimmunity[toset])
  
  invisible()
}
```

## Resources

- [R Packages book](https://r-pkgs.org/) by Hadley Wickham
- [Writing R Extensions](https://cran.r-project.org/doc/manuals/r-release/R-exts.html) (official guide)
- [usethis documentation](https://usethis.r-lib.org/)
- [roxygen2 documentation](https://roxygen2.r-lib.org/)
- [testthat documentation](https://testthat.r-lib.org/)

## Support

For questions about package development:
- Open an issue on GitHub
- Contact the ACCIDDA development team
- Review existing R packages for examples
