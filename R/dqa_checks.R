# ==============================================================================
# DEPRECATED — legacy, base-R DQA implementation.
#
# Every check in this file is functionally superseded by a dplyr-based
# equivalent in final_format_utils.R, which is what the live pipeline
# (inst/templates/preprocessing-orchestration.R -> run_final_formatting())
# actually calls:
#   check_duplicates()                -> dqa_check_duplicates()
#   check_negative_values()           -> dqa_check_negatives()
#   check_exceeding_enrollment_values()-> dqa_check_too_high()
#   check_coverage_outliers()         -> dqa_check_coverage_outliers()
#   check_enrollment_deviation()      -> dqa_check_enrollment_deviation()
#   check_vaccination_deviation()     -> dqa_check_current_deviation()
#   check_extreme_outliers()          -> dqa_check_extreme_outliers()
#
# This file (plus run_dqa_checks.R and generate_dqa_summary.R, which
# orchestrate it) is reachable only from inst/scripts/test_modular_functions.R
# and its own unit tests — not from the orchestration template. Kept for
# backward compatibility with any external caller and its existing tests
# rather than deleted outright; new code should use the final_format_utils.R
# functions above instead.
# ==============================================================================


#' Check for Duplicate Records
#'
#' @description
#' \strong{Deprecated:} superseded by \code{\link{dqa_check_duplicates}} in
#' \code{final_format_utils.R}, which the live pipeline actually calls. Kept
#' for backward compatibility; see the file header of \code{dqa_checks.R} for
#' the full list of replacements.
#'
#' Identifies duplicate school_id-year combinations in the dataset and determines
#' whether duplicates are identical or have conflicting data.
#'
#' @param data A data frame containing school vaccination data
#' @param core_cols Character vector of core columns to check for consistency.
#'   If NULL (default), uses first 10 columns excluding grouping variables.
#'
#' @return A list with two data frames:
#'   \itemize{
#'     \item identical_dupes: Duplicate records with identical core data
#'     \item conflicting_dupes: Duplicate records with conflicting core data
#'   }
#'
#' @details
#' Duplicates are identified based on school_id and year combinations. The function
#' distinguishes between exact duplicates (all core columns identical) and 
#' conflicting duplicates (core columns differ).
#'
#' @examples
#' \dontrun{
#' dup_results <- check_duplicates(state_data)
#' }
#'
#' @export
check_duplicates <- function(data, core_cols = NULL) {
  
  if (is.null(core_cols)) {
    # Define core columns (first 10, including school_type if present)
    core_cols <- colnames(data)[1:min(10, ncol(data))]
    # Remove the grouping columns from core_cols
    core_cols <- setdiff(core_cols, c("school_id", "year"))
  }
  
  # Check for duplicates
  dupes <- data[duplicated(data[, c("school_id", "year")]) | 
                duplicated(data[, c("school_id", "year")], fromLast = TRUE), ]
  
  if (nrow(dupes) == 0) {
    message("No duplicate school_id-year combinations found.")
    return(list(identical_dupes = data.frame(), conflicting_dupes = data.frame()))
  }
  
  # Analyze duplicates
  dup_summary <- aggregate(
    . ~ school_id + year, 
    data = dupes[, c("school_id", "year", core_cols)],
    FUN = function(x) length(unique(x)) == 1,
    simplify = FALSE
  )
  
  # Identify which groups are identical vs conflicting
  identical_mask <- apply(dup_summary[, -(1:2), drop = FALSE], 1, all)
  
  identical_ids <- dup_summary[identical_mask, c("school_id", "year")]
  conflicting_ids <- dup_summary[!identical_mask, c("school_id", "year")]
  
  # Extract actual rows
  identical_dupes <- merge(dupes, identical_ids, by = c("school_id", "year"))
  conflicting_dupes <- merge(dupes, conflicting_ids, by = c("school_id", "year"))
  
  if (nrow(identical_dupes) > 0) {
    message("Found ", nrow(unique(identical_ids)), " sets of identical duplicates.")
  }
  
  if (nrow(conflicting_dupes) > 0) {
    message("Found ", nrow(unique(conflicting_ids)), " sets of conflicting duplicates.")
  }
  
  return(list(
    identical_dupes = identical_dupes,
    conflicting_dupes = conflicting_dupes
  ))
}


#' Check for Negative Values
#'
#' @description
#' \strong{Deprecated:} superseded by \code{\link{dqa_check_negatives}}; see
#' the file header of \code{dqa_checks.R}.
#'
#' Identifies records with negative values in numeric columns such as
#' enrollment, current vaccinations, exemptions, or delayed counts.
#'
#' @param data A data frame containing school vaccination data
#' @param cols_to_check Character vector of column names to check for negative values.
#'   Default: c("current", "delayed", "med_exempt", "rel_exempt", "enrollment")
#'
#' @return A data frame containing rows with negative values in checked columns
#'
#' @examples
#' \dontrun{
#' negatives <- check_negative_values(state_data)
#' }
#'
#' @export
check_negative_values <- function(data, 
                                  cols_to_check = c("current", "delayed", 
                                                   "med_exempt", "rel_exempt", 
                                                   "enrollment")) {
  
  # Filter columns that exist in data
  cols_present <- intersect(cols_to_check, colnames(data))
  
  if (length(cols_present) == 0) {
    warning("None of the specified columns to check are present in the data.")
    return(data.frame())
  }
  
  # Check for negative values
  negative_mask <- rep(FALSE, nrow(data))
  for (col in cols_present) {
    negative_mask <- negative_mask | (data[[col]] < 0 & !is.na(data[[col]]))
  }
  
  negatives <- data[negative_mask, ]
  
  if (nrow(negatives) > 0) {
    message("Found negative values in ", nrow(negatives), " rows.")
  } else {
    message("No negative values found.")
  }
  
  return(negatives)
}


#' Check for Values Exceeding Enrollment
#'
#' @description
#' \strong{Deprecated:} superseded by \code{\link{dqa_check_too_high}}; see
#' the file header of \code{dqa_checks.R}.
#'
#' Identifies records where current vaccinations, exemptions, or delayed counts
#' individually exceed total enrollment.
#'
#' @param data A data frame containing school vaccination data
#' @param cols_to_check Character vector of column names to check against enrollment.
#'   Default: c("current", "delayed", "med_exempt", "rel_exempt")
#'
#' @return A data frame containing rows where checked values exceed enrollment
#'
#' @examples
#' \dontrun{
#' exceeding <- check_exceeding_enrollment_values(state_data)
#' }
#'
#' @export
check_exceeding_enrollment_values <- function(data,
                                              cols_to_check = c("current", "delayed",
                                                               "med_exempt", "rel_exempt")) {
  
  if (!"enrollment" %in% colnames(data)) {
    stop("Column 'enrollment' not found in data.")
  }
  
  # Filter columns that exist in data
  cols_present <- intersect(cols_to_check, colnames(data))
  
  if (length(cols_present) == 0) {
    warning("None of the specified columns to check are present in the data.")
    return(data.frame())
  }
  
  # Check for values exceeding enrollment
  exceeding_mask <- rep(FALSE, nrow(data))
  for (col in cols_present) {
    exceeding_mask <- exceeding_mask | (data[[col]] > data$enrollment & !is.na(data[[col]]))
  }
  
  too_high <- data[exceeding_mask, ]
  
  if (nrow(too_high) > 0) {
    message("Found ", nrow(too_high), " rows with values exceeding enrollment.")
  } else {
    message("No values exceeding enrollment found.")
  }
  
  return(too_high)
}



#' Check for Coverage Outliers
#'
#' @description
#' \strong{Deprecated:} superseded by \code{\link{dqa_check_coverage_outliers}}
#' / \code{\link{dqa_check_over_coverage}}; see the file header of
#' \code{dqa_checks.R}.
#'
#' Identifies records with unrealistic MMR coverage (<0 or >105 percent) and cases
#' where current + delayed + exemptions exceed enrollment.
#'
#' @param data A data frame containing school vaccination data
#' @param coverage_threshold Numeric value for maximum acceptable coverage (default: 1.05 for 105 percent)
#'
#' @return A list with two data frames:
#'   \itemize{
#'     \item coverage_outliers: Records with coverage outside acceptable range
#'     \item over_coverage: Records where sum of counts exceeds enrollment
#'   }
#'
#' @examples
#' \dontrun{
#' coverage_issues <- check_coverage_outliers(state_data)
#' }
#'
#' @export
#' 
check_coverage_outliers <- function(data, coverage_threshold = 1.05) {
  
  # Calculate MMR coverage if not present
  if (!"mmr_coverage" %in% colnames(data)) {
    if ("current" %in% colnames(data) && "enrollment" %in% colnames(data)) {
      data$mmr_coverage <- data$current / data$enrollment
    } else {
      warning("Cannot calculate MMR coverage: 'current' or 'enrollment' column missing.")
      return(list(coverage_outliers = data.frame(), over_coverage = data.frame()))
    }
  }
  
  # Check for unrealistic coverage
  coverage_outliers <- data[!is.na(data$mmr_coverage) & 
                           (data$mmr_coverage < 0 | data$mmr_coverage > coverage_threshold), ]
  
  if (nrow(coverage_outliers) > 0) {
    message("Found ", nrow(coverage_outliers), 
            " rows with unrealistic MMR coverage (<0 or >", 
            coverage_threshold * 100, "%).")
  } else {
    message("No coverage outliers found.")
  }
  
  # Check for over-coverage (sum exceeds enrollment)
  required_cols <- c("enrollment", "current", "delayed", "med_exempt", "rel_exempt")
  if (all(required_cols %in% colnames(data))) {
    over_coverage <- data[!is.na(data$enrollment) & 
                         (data$current + data$delayed + data$med_exempt + data$rel_exempt) > 
                         data$enrollment, ]
    
    if (nrow(over_coverage) > 0) {
      message("Found ", nrow(over_coverage), 
              " rows where current + delayed + exempt exceed enrollment.")
    } else {
      message("No over-coverage issues found.")
    }
  } else {
    warning("Cannot check over-coverage: required columns missing.")
    over_coverage <- data.frame()
  }
  
  return(list(
    coverage_outliers = coverage_outliers,
    over_coverage = over_coverage
  ))
}


#' Check for Enrollment Deviation
#'
#' @description
#' \strong{Deprecated:} superseded by
#' \code{\link{dqa_check_enrollment_deviation}}; see the file header of
#' \code{dqa_checks.R}.
#'
#' Identifies schools where enrollment in a given year deviates significantly
#' from that school's historical average enrollment.
#'
#' @param data A data frame containing school vaccination data
#' @param deviation_threshold Numeric value for maximum acceptable deviation 
#'   (default: 0.5 for 50 percent)
#'
#' @return A list with two components:
#'   \itemize{
#'     \item flagged: Data frame containing rows with enrollment deviating
#'       significantly from school's historical average (includes mean_enrollment
#'       and pct_diff_from_mean columns)
#'     \item school_history: Data frame containing all years of data for the
#'       schools that had flagged rows (useful for context when reviewing issues)
#'   }
#'
#' @examples
#' \dontrun{
#' enrollment_dev <- check_enrollment_deviation(state_data)
#' nrow(enrollment_dev$flagged)
#' }
#'
#' @export
#' 
check_enrollment_deviation <- function(data, deviation_threshold = 0.5) {
  
  if (!"school_id" %in% colnames(data) || !"enrollment" %in% colnames(data)) {
    warning("Cannot check enrollment deviation: 'school_id' or 'enrollment' column missing.")
    return(list(flagged = data.frame(), school_history = data.frame()))
  }
  
  # Calculate mean enrollment per school
  school_means <- aggregate(enrollment ~ school_id, data = data, FUN = mean, na.rm = TRUE)
  names(school_means)[2] <- "mean_enrollment"
  
  # Merge back to original data
  data_with_mean <- merge(data, school_means, by = "school_id", all.x = TRUE)
  
  # Calculate percent difference
  data_with_mean$pct_diff_from_mean <- 
    (data_with_mean$enrollment - data_with_mean$mean_enrollment) / 
    data_with_mean$mean_enrollment
  
  # Filter to significant deviations
  enrollment_deviation <- data_with_mean[
    !is.na(data_with_mean$pct_diff_from_mean) & 
    abs(data_with_mean$pct_diff_from_mean) > deviation_threshold, 
  ]
  
  if (nrow(enrollment_deviation) > 0) {
    message("Found ", nrow(enrollment_deviation),
            " rows with enrollment differing >", deviation_threshold * 100,
            "% from that school's mean across years.")
  } else {
    message("No significant enrollment deviations found.")
  }
  
  # Get all years of data for flagged schools (for context)
  flagged_school_ids <- unique(enrollment_deviation$school_id)
  school_history <- data_with_mean[data_with_mean$school_id %in% flagged_school_ids, ]
  
  return(list(flagged = enrollment_deviation, school_history = school_history))
}


#' Check for Vaccination Count Deviation
#'
#' @description
#' \strong{Deprecated:} superseded by \code{\link{dqa_check_current_deviation}};
#' see the file header of \code{dqa_checks.R}.
#'
#' Identifies schools where current vaccination count in a given year deviates
#' significantly from that school's historical average.
#'
#' @param data A data frame containing school vaccination data
#' @param deviation_threshold Numeric value for maximum acceptable deviation
#'   (default: 0.5 for 50 percent)
#'
#' @return A list with two components:
#'   \itemize{
#'     \item flagged: Data frame containing rows with vaccination counts deviating
#'       significantly from school's historical average (includes mean_current
#'       and pct_diff_from_mean columns)
#'     \item school_history: Data frame containing all years of data for the
#'       schools that had flagged rows (useful for context when reviewing issues)
#'   }
#'
#' @examples
#' \dontrun{
#' vaccination_dev <- check_vaccination_deviation(state_data)
#' nrow(vaccination_dev$flagged)
#' }
#'
#' @export
check_vaccination_deviation <- function(data, deviation_threshold = 0.5) {
  
  if (!"school_id" %in% colnames(data) || !"current" %in% colnames(data)) {
    warning("Cannot check vaccination deviation: 'school_id' or 'current' column missing.")
    return(list(flagged = data.frame(), school_history = data.frame()))
  }
  
  # Calculate mean current vaccinations per school
  school_means <- aggregate(current ~ school_id, data = data, FUN = mean, na.rm = TRUE)
  names(school_means)[2] <- "mean_current"
  
  # Merge back to original data
  data_with_mean <- merge(data, school_means, by = "school_id", all.x = TRUE)
  
  # Calculate percent difference
  data_with_mean$pct_diff_from_mean <- 
    (data_with_mean$current - data_with_mean$mean_current) / 
    data_with_mean$mean_current
  
  # Filter to significant deviations
  current_deviation <- data_with_mean[
    !is.na(data_with_mean$pct_diff_from_mean) & 
    abs(data_with_mean$pct_diff_from_mean) > deviation_threshold, 
  ]
  
  if (nrow(current_deviation) > 0) {
    message("Found ", nrow(current_deviation),
            " rows with 'current' differing >", deviation_threshold * 100,
            "% from that school's average across years.")
  } else {
    message("No significant vaccination count deviations found.")
  }
  
  # Get all years of data for flagged schools (for context)
  flagged_school_ids <- unique(current_deviation$school_id)
  school_history <- data_with_mean[data_with_mean$school_id %in% flagged_school_ids, ]
  
  return(list(flagged = current_deviation, school_history = school_history))
}


#' Check for Extreme Outliers
#'
#' @description
#' \strong{Deprecated:} superseded by \code{\link{dqa_check_extreme_outliers}};
#' see the file header of \code{dqa_checks.R}.
#'
#' Identifies likely typos or extreme outliers, such as enrollment values
#' that are 10x the school's typical enrollment.
#'
#' @param data A data frame containing school vaccination data
#' @param multiplier Numeric value for outlier detection threshold
#'   (default: 10 for 10x median enrollment)
#'
#' @return A data frame containing rows with extreme enrollment values
#'
#' @examples
#' \dontrun{
#' extreme_outliers <- check_extreme_outliers(state_data)
#' }
#'
#' @export
check_extreme_outliers <- function(data, multiplier = 10) {
  
  if (!"school_id" %in% colnames(data) || !"enrollment" %in% colnames(data)) {
    warning("Cannot check extreme outliers: 'school_id' or 'enrollment' column missing.")
    return(data.frame())
  }
  
  # Calculate median enrollment per school
  median_enrollment <- aggregate(enrollment ~ school_id, data = data, 
                                FUN = median, na.rm = TRUE)
  names(median_enrollment)[2] <- "median_enroll"
  
  # Merge back to original data
  data_with_median <- merge(data, median_enrollment, by = "school_id", all.x = TRUE)
  
  # Filter to extreme outliers
  outliers <- data_with_median[
    data_with_median$enrollment > multiplier * data_with_median$median_enroll &
    !is.na(data_with_median$enrollment), 
  ]
  
  if (nrow(outliers) > 0) {
    message("Found ", nrow(outliers),
            " rows with extreme enrollment values (", multiplier,
            "x that school's median across years).")
  } else {
    message("No extreme outliers found.")
  }
  
  return(outliers)
}
