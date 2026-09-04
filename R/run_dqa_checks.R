#' Run Data Quality Assurance Checks
#'
#' @description
#' \strong{Deprecated:} orchestrates the legacy \code{check_*()} family in
#' \code{dqa_checks.R}, all of which are superseded by the \code{dqa_check_*()}
#' family in \code{final_format_utils.R} that \code{\link{run_final_formatting}}
#' actually calls in the live pipeline. Kept for backward compatibility; see
#' the file header of \code{dqa_checks.R} for the full replacement mapping.
#'
#' Wrapper function that executes a series of data quality assurance (DQA) checks
#' on vaccination data. Captures any issues into an output log for reporting.
#'
#' @param data A data frame containing school vaccination data
#' @param checks Character vector specifying which checks to run. 
#'   Options: "duplicates", "negatives", "exceeding_enrollment", "coverage_outliers",
#'   "enrollment_deviation", "vaccination_deviation", "extreme_outliers".
#'   Default: "all" (runs all checks)
#' @param deviation_threshold Numeric value for deviation checks (default: 0.5)
#' @param coverage_threshold Numeric value for coverage checks (default: 1.05)
#' @param outlier_multiplier Numeric value for extreme outlier detection (default: 10)
#' @param output_dir Optional directory path to save individual check results as CSV files
#' @param state Optional state abbreviation for file naming (e.g., "ca", "md")
#'
#' @return A list containing:
#'   \itemize{
#'     \item summary: A data frame summarizing the number of issues found in each check
#'     \item results: A named list containing the detailed results from each check
#'     \item data_cleaned: The input data with duplicates resolved (if applicable)
#'   }
#'
#' @details
#' The function runs the following checks in sequence:
#' \enumerate{
#'   \item check_duplicates: Identifies duplicate school_id-year combinations
#'   \item check_negative_values: Finds negative values in numeric columns
#'   \item check_exceeding_enrollment_values: Identifies values exceeding enrollment
#'   \item check_coverage_outliers: Detects unrealistic coverage percentages
#'   \item check_enrollment_deviation: Finds enrollment deviations from school averages
#'   \item check_vaccination_deviation: Finds vaccination count deviations
#'   \item check_extreme_outliers: Identifies extreme outlier values
#' }
#'
#' @examples
#' \dontrun{
#' # Run all checks
#' dqa_results <- run_dqa_checks(state_data)
#' 
#' # Run specific checks only
#' dqa_results <- run_dqa_checks(state_data, 
#'                                checks = c("negatives", "coverage_outliers"))
#' 
#' # Save results to files
#' dqa_results <- run_dqa_checks(state_data, 
#'                                output_dir = "output/dqa",
#'                                state = "ca")
#' }
#'
#' @export
run_dqa_checks <- function(data,
                          checks = "all",
                          deviation_threshold = 0.5,
                          coverage_threshold = 1.05,
                          outlier_multiplier = 10,
                          output_dir = NULL,
                          state = NULL) {
  
  message("\n========================================")
  message("Running Data Quality Assurance Checks")
  message("========================================\n")
  
  # Define all available checks
  all_checks <- c("duplicates", "negatives", "exceeding_enrollment", 
                  "coverage_outliers", "enrollment_deviation", 
                  "vaccination_deviation", "extreme_outliers")
  
  # Determine which checks to run
  if (length(checks) == 1 && checks[1] == "all") {
    checks_to_run <- all_checks
  } else {
    checks_to_run <- intersect(checks, all_checks)
    invalid_checks <- setdiff(checks, all_checks)
    if (length(invalid_checks) > 0) {
      warning("Invalid check names: ", paste(invalid_checks, collapse = ", "))
    }
  }
  
  # Initialize results storage
  results <- list()
  summary_data <- data.frame(
    check = character(),
    issue_count = integer(),
    action_type = character(),
    stringsAsFactors = FALSE
  )
  
  # Create output directory if specified
  if (!is.null(output_dir) && !dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
    message("Created output directory: ", output_dir)
  }
  
  # Helper function to save results
  save_check_result <- function(result, check_name) {
    if (!is.null(output_dir) && !is.null(state)) {
      filename <- file.path(output_dir, paste0(state, "_", check_name, ".csv"))
      if (is.data.frame(result) && nrow(result) > 0) {
        write.csv(result, filename, row.names = FALSE)
        message("  Saved results to: ", filename)
      } else if (is.list(result) && !is.data.frame(result)) {
        # Handle named list results (e.g., flagged + school_history, duplicates, coverage_outliers)
        for (subname in names(result)) {
          if (is.data.frame(result[[subname]]) && nrow(result[[subname]]) > 0) {
            sub_filename <- file.path(output_dir, 
                                     paste0(state, "_", check_name, "_", subname, ".csv"))
            write.csv(result[[subname]], sub_filename, row.names = FALSE)
            message("  Saved results to: ", sub_filename)
          }
        }
      }
    }
  }
  
  # Run each check
  data_cleaned <- data
  
  # 1. Check duplicates
  if ("duplicates" %in% checks_to_run) {
    message("\n--- Check 1: Duplicates ---")
    dup_result <- check_duplicates(data_cleaned)
    results$duplicates <- dup_result
    
    # Merge identical duplicates
    if (nrow(dup_result$identical_dupes) > 0) {
      n_before <- nrow(data_cleaned)
      data_cleaned <- unique(data_cleaned)
      n_after <- nrow(data_cleaned)
      message("  Merged ", n_before - n_after, " identical duplicate rows.")
    }
    
    issue_count <- nrow(dup_result$conflicting_dupes)
    summary_data <- rbind(summary_data, 
                         data.frame(check = "duplicates_conflicting", 
                                   issue_count = issue_count,
                                   action_type = "requires_review"))
    save_check_result(dup_result$conflicting_dupes, "duplicate_conflicts")
  }
  
  # 2. Check negative values
  if ("negatives" %in% checks_to_run) {
    message("\n--- Check 2: Negative Values ---")
    neg_result <- check_negative_values(data_cleaned)
    results$negatives <- neg_result
    summary_data <- rbind(summary_data, 
                         data.frame(check = "negative_values", 
                                   issue_count = nrow(neg_result),
                                   action_type = "requires_review"))
    save_check_result(neg_result, "negatives")
  }
  
  # 3. Check values exceeding enrollment
  if ("exceeding_enrollment" %in% checks_to_run) {
    message("\n--- Check 3: Values Exceeding Enrollment ---")
    exceed_result <- check_exceeding_enrollment_values(data_cleaned)
    results$exceeding_enrollment <- exceed_result
    summary_data <- rbind(summary_data, 
                         data.frame(check = "exceeding_enrollment", 
                                   issue_count = nrow(exceed_result),
                                   action_type = "requires_review"))
    save_check_result(exceed_result, "exceeding_enrollment")
  }
  
  # 4. Check coverage outliers
  if ("coverage_outliers" %in% checks_to_run) {
    message("\n--- Check 4: Coverage Outliers ---")
    coverage_result <- check_coverage_outliers(data_cleaned, coverage_threshold)
    results$coverage_outliers <- coverage_result
    summary_data <- rbind(summary_data, 
                         data.frame(check = "coverage_outliers", 
                                   issue_count = nrow(coverage_result$coverage_outliers),
                                   action_type = "requires_review"))
    summary_data <- rbind(summary_data, 
                         data.frame(check = "over_coverage", 
                                   issue_count = nrow(coverage_result$over_coverage),
                                   action_type = "requires_review"))
    save_check_result(coverage_result, "coverage_outliers")
  }
  
  # 5. Check enrollment deviation
  if ("enrollment_deviation" %in% checks_to_run) {
    message("\n--- Check 5: Enrollment Deviation ---")
    enroll_dev_result <- check_enrollment_deviation(data_cleaned, deviation_threshold)
    results$enrollment_deviation <- enroll_dev_result
    summary_data <- rbind(summary_data, 
                         data.frame(check = "enrollment_deviation", 
                                   issue_count = nrow(enroll_dev_result$flagged),
                                   action_type = "requires_review"))
    save_check_result(enroll_dev_result, "enrollment_deviation")
  }
  
  # 6. Check vaccination deviation
  if ("vaccination_deviation" %in% checks_to_run) {
    message("\n--- Check 6: Vaccination Count Deviation ---")
    vax_dev_result <- check_vaccination_deviation(data_cleaned, deviation_threshold)
    results$vaccination_deviation <- vax_dev_result
    summary_data <- rbind(summary_data, 
                         data.frame(check = "vaccination_deviation", 
                                   issue_count = nrow(vax_dev_result$flagged),
                                   action_type = "requires_review"))
    save_check_result(vax_dev_result, "vaccination_deviation")
  }
  
  # 7. Check extreme outliers
  if ("extreme_outliers" %in% checks_to_run) {
    message("\n--- Check 7: Extreme Outliers ---")
    outlier_result <- check_extreme_outliers(data_cleaned, outlier_multiplier)
    results$extreme_outliers <- outlier_result
    summary_data <- rbind(summary_data, 
                         data.frame(check = "extreme_outliers", 
                                   issue_count = nrow(outlier_result),
                                   action_type = "requires_review"))
    save_check_result(outlier_result, "extreme_outliers")
  }
  
  # Print summary
  message("\n========================================")
  message("DQA Checks Summary")
  message("========================================")
  message(sprintf("%-35s %12s %20s", "Check", "Flags Found", "Action Type"))
  message(strrep("-", 70))
  for (i in seq_len(nrow(summary_data))) {
    message(sprintf("%-35s %12d %20s",
                    summary_data$check[i],
                    summary_data$issue_count[i],
                    summary_data$action_type[i]))
  }
  message(strrep("-", 70))
  message(sprintf("%-35s %12d", "TOTAL FLAGS", sum(summary_data$issue_count)))
  
  # Return results
  return(list(
    summary = summary_data,
    results = results,
    data_cleaned = data_cleaned
  ))
}
