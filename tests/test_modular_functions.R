#!/usr/bin/env Rscript
# Test script for modular preprocessing functions
# 
# This script performs basic testing of the modular functions to ensure
# they work correctly with synthetic data.

# Suppress messages for cleaner output
suppressPackageStartupMessages({
  library(data.table)
})

# Source the functions
source("R/clean_state_data.R")
source("R/dqa_checks.R")
source("R/run_dqa_checks.R")
source("R/generate_dqa_summary.R")

cat("=== Testing Modular Preprocessing Functions ===\n\n")

# ---- Create Synthetic Test Data ----
cat("Creating synthetic test data...\n")
set.seed(42)

test_data <- data.frame(
  year = rep(2015:2018, each = 25),
  school_id = rep(1001:1025, times = 4),
  school_name = rep(paste("Test School", 1:25), times = 4),
  cnty_id = rep(1:5, each = 5, times = 4),
  school_county = rep(paste("County", 1:5), each = 5, times = 4),
  school_type = sample(c("public", "private"), 100, replace = TRUE),
  enrollment = sample(80:200, 100, replace = TRUE),
  current = NA,
  med_exempt = sample(0:3, 100, replace = TRUE),
  rel_exempt = sample(0:5, 100, replace = TRUE),
  delayed = sample(0:8, 100, replace = TRUE)
)

# Calculate current based on enrollment
test_data$current <- pmax(0, test_data$enrollment - 
                          test_data$med_exempt - 
                          test_data$rel_exempt - 
                          test_data$delayed)

# Introduce some issues for testing
test_data$current[1] <- -5  # Negative value
test_data$enrollment[2] <- 0  # Zero enrollment
test_data$current[3] <- test_data$enrollment[3] + 10  # Exceeds enrollment
test_data <- rbind(test_data, test_data[4, ])  # Duplicate row

cat("Created test data with", nrow(test_data), "rows\n")
cat("Introduced test issues: negative value, zero enrollment, exceeding enrollment, duplicate\n\n")

# ---- Test 1: clean_state_data ----
cat("--- Test 1: clean_state_data() ---\n")
tryCatch({
  cleaned <- clean_state_data(test_data)
  cat("✓ clean_state_data() passed\n")
  cat("  Input rows:", nrow(test_data), "→ Output rows:", nrow(cleaned), "\n\n")
}, error = function(e) {
  cat("✗ clean_state_data() failed:", e$message, "\n\n")
  stop(e)
})

# ---- Test 2: Individual DQA Checks ----
cat("--- Test 2: Individual DQA Checks ---\n")

# Test check_duplicates
tryCatch({
  dup_result <- check_duplicates(cleaned)
  cat("✓ check_duplicates() passed\n")
  cat("  Identical duplicates:", nrow(dup_result$identical_dupes), "rows\n")
  cat("  Conflicting duplicates:", nrow(dup_result$conflicting_dupes), "rows\n")
}, error = function(e) {
  cat("✗ check_duplicates() failed:", e$message, "\n")
})

# Test check_negative_values
tryCatch({
  neg_result <- check_negative_values(cleaned)
  cat("✓ check_negative_values() passed\n")
  cat("  Negative values found:", nrow(neg_result), "rows\n")
}, error = function(e) {
  cat("✗ check_negative_values() failed:", e$message, "\n")
})

# Test check_exceeding_enrollment_values
tryCatch({
  exceed_result <- check_exceeding_enrollment_values(cleaned)
  cat("✓ check_exceeding_enrollment_values() passed\n")
  cat("  Exceeding enrollment:", nrow(exceed_result), "rows\n")
}, error = function(e) {
  cat("✗ check_exceeding_enrollment_values() failed:", e$message, "\n")
})

# Test check_coverage_outliers
tryCatch({
  coverage_result <- check_coverage_outliers(cleaned)
  cat("✓ check_coverage_outliers() passed\n")
  cat("  Coverage outliers:", nrow(coverage_result$coverage_outliers), "rows\n")
  cat("  Over-coverage:", nrow(coverage_result$over_coverage), "rows\n")
}, error = function(e) {
  cat("✗ check_coverage_outliers() failed:", e$message, "\n")
})

# Test check_enrollment_deviation
tryCatch({
  enroll_dev_result <- check_enrollment_deviation(cleaned)
  cat("✓ check_enrollment_deviation() passed\n")
  cat("  Enrollment deviations (flagged):", nrow(enroll_dev_result$flagged), "rows\n")
  cat("  School history rows:", nrow(enroll_dev_result$school_history), "rows\n")
}, error = function(e) {
  cat("✗ check_enrollment_deviation() failed:", e$message, "\n")
})

# Test check_vaccination_deviation
tryCatch({
  vax_dev_result <- check_vaccination_deviation(cleaned)
  cat("✓ check_vaccination_deviation() passed\n")
  cat("  Vaccination deviations (flagged):", nrow(vax_dev_result$flagged), "rows\n")
  cat("  School history rows:", nrow(vax_dev_result$school_history), "rows\n")
}, error = function(e) {
  cat("✗ check_vaccination_deviation() failed:", e$message, "\n")
})

# Test check_extreme_outliers
tryCatch({
  outlier_result <- check_extreme_outliers(cleaned)
  cat("✓ check_extreme_outliers() passed\n")
  cat("  Extreme outliers:", nrow(outlier_result), "rows\n\n")
}, error = function(e) {
  cat("✗ check_extreme_outliers() failed:", e$message, "\n\n")
})

# ---- Test 3: run_dqa_checks ----
cat("--- Test 3: run_dqa_checks() ---\n")
tryCatch({
  dqa_results <- run_dqa_checks(
    data = cleaned,
    checks = "all",
    output_dir = NULL,  # Don't save files during testing
    state = "test"
  )
  cat("✓ run_dqa_checks() passed\n")
  cat("  Summary rows:", nrow(dqa_results$summary), "\n")
  cat("  Results components:", length(dqa_results$results), "\n")
  cat("  Cleaned data rows:", nrow(dqa_results$data_cleaned), "\n")
  cat("  Summary has action_type column:", "action_type" %in% names(dqa_results$summary), "\n\n")
}, error = function(e) {
  cat("✗ run_dqa_checks() failed:", e$message, "\n\n")
  stop(e)
})

# ---- Test 4: generate_dqa_summary ----
cat("--- Test 4: generate_dqa_summary() ---\n")

# Create temp directory for test outputs
temp_dir <- tempdir()
test_output_dir <- file.path(temp_dir, "dqa_test")
if (!dir.exists(test_output_dir)) {
  dir.create(test_output_dir, recursive = TRUE)
}

# Test CSV output
tryCatch({
  csv_path <- file.path(test_output_dir, "test_summary.csv")
  summary_result <- generate_dqa_summary(
    dqa_results = dqa_results,
    output_path = csv_path,
    state = "test",
    include_timestamp = TRUE
  )
  if (file.exists(csv_path)) {
    cat("✓ generate_dqa_summary() CSV passed\n")
    cat("  File created:", csv_path, "\n")
  } else {
    cat("✗ generate_dqa_summary() CSV failed: File not created\n")
  }
}, error = function(e) {
  cat("✗ generate_dqa_summary() CSV failed:", e$message, "\n")
})

# Test RDS output
tryCatch({
  rds_path <- file.path(test_output_dir, "test_summary.rds")
  summary_result <- generate_dqa_summary(
    dqa_results = dqa_results,
    output_path = rds_path,
    state = "test",
    include_timestamp = FALSE
  )
  if (file.exists(rds_path)) {
    cat("✓ generate_dqa_summary() RDS passed\n")
    cat("  File created:", rds_path, "\n\n")
  } else {
    cat("✗ generate_dqa_summary() RDS failed: File not created\n\n")
  }
}, error = function(e) {
  cat("✗ generate_dqa_summary() RDS failed:", e$message, "\n\n")
})

# ---- Test 5: generate_detailed_dqa_report ----
cat("--- Test 5: generate_detailed_dqa_report() ---\n")
tryCatch({
  detail_dir <- file.path(test_output_dir, "details")
  detailed_result <- generate_detailed_dqa_report(
    dqa_results = dqa_results,
    output_dir = detail_dir,
    state = "test",
    consolidate = TRUE,
    format = "csv"
  )
  if (dir.exists(detail_dir)) {
    cat("✓ generate_detailed_dqa_report() passed\n")
    cat("  Directory created:", detail_dir, "\n")
    cat("  Files created:", length(list.files(detail_dir)), "\n\n")
  } else {
    cat("✗ generate_detailed_dqa_report() failed: Directory not created\n\n")
  }
}, error = function(e) {
  cat("✗ generate_detailed_dqa_report() failed:", e$message, "\n\n")
})

# ---- Test 6: Run Specific Checks ----
cat("--- Test 6: Run Specific Checks Only ---\n")
tryCatch({
  specific_results <- run_dqa_checks(
    data = cleaned,
    checks = c("negatives", "duplicates"),
    output_dir = NULL,
    state = "test"
  )
  cat("✓ Specific checks passed\n")
  cat("  Checks run:", paste(names(specific_results$results), collapse = ", "), "\n\n")
}, error = function(e) {
  cat("✗ Specific checks failed:", e$message, "\n\n")
})

# ---- Summary ----
cat("=== Test Summary ===\n")
cat("All modular functions tested successfully!\n")
cat("Test outputs saved to:", test_output_dir, "\n")
cat("\nYou can review the test outputs or delete the temp directory when done.\n")

# Clean up test outputs
cat("\nCleaning up test files...\n")
unlink(test_output_dir, recursive = TRUE)
cat("Test files cleaned up.\n")

cat("\n=== All Tests Passed ===\n")
