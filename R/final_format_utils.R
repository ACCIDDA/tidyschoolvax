# ==============================================================================
# FINAL FORMATTING UTILITY FUNCTIONS
# ==============================================================================
# State-agnostic utility functions for final data formatting and DQA checks.
# These functions are applied after state-specific cleaning (step 03) has been
# completed and prepare the data for the modelling pipeline.


# ------------------------------------------------------------------------------
# DATA STANDARDISATION
# ------------------------------------------------------------------------------

#' Standardise Kindergarten Data Format
#'
#' @description
#' Renames and type-coerces columns from a raw state data frame to the
#' canonical column names expected by the rest of the preprocessing pipeline.
#' Required columns are mapped to their canonical names; optional columns are
#' included when present.
#'
#' @param df A data frame of raw kindergarten vaccination data.
#' @param cnty_id_col Column name for county ID (optional).
#' @param school_id_col Column name for school ID (required).
#' @param year_col Column name for year (required).
#' @param enrollment_col Column name for enrollment count (required).
#' @param current_col Column name for current-vaccination count (required).
#' @param med_exempt_col Column name for medical-exemption count (required).
#' @param rel_exempt_col Column name for religious-exemption count (required).
#' @param delayed_col Column name for delayed-vaccination count (optional).
#' @param school_name_col Column name for school name (optional).
#' @param county_name_col Column name for county name (optional).
#' @param vaccine_type_col Column name for vaccine type (optional).
#' @param school_type_col Column name for school type (optional).
#' @param school_level_col Column name for school level (optional).
#' @param level_code_col Column name for level code (optional).
#' @param excluded_note_col Column name for exclusion notes (optional).
#' @param addr_clean_col Column name for cleaned address (optional).
#' @param city_col Column name for city (optional).
#' @param zip_col Column name for ZIP code (optional).
#' @param state_col Column name for state abbreviation (optional).
#' @param business_status_col Column name for business/operational status (optional).
#' @param lat_col Column name for latitude (optional).
#' @param lon_col Column name for longitude (optional).
#'
#' @return A \code{data.table} with canonical column names.
#'
#' @importFrom data.table data.table setcolorder
#' @export
standardize_kinder_format <- function(
    df,
    cnty_id_col      = NULL,
    school_id_col,
    year_col,
    enrollment_col,
    current_col,
    med_exempt_col,
    rel_exempt_col,
    delayed_col      = NULL,
    school_name_col  = NULL,
    county_name_col  = NULL,
    vaccine_type_col = NULL,
    school_type_col  = NULL,
    school_level_col = NULL,
    level_code_col   = NULL,
    excluded_note_col = NULL,
    addr_clean_col   = NULL,
    city_col         = NULL,
    zip_col          = NULL,
    state_col        = NULL,
    business_status_col = NULL,
    lat_col          = NULL,
    lon_col          = NULL
) {
  req <- c(
    school_id_col, year_col, enrollment_col,
    current_col, med_exempt_col, rel_exempt_col
  )
  missing <- setdiff(req, names(df))
  if (length(missing) > 0) {
    stop("Missing columns in df: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  
  to_int <- function(x) {
    x <- as.character(x)
    x <- gsub(",", "", x)
    x <- gsub("%", "", x)
    x <- trimws(x)
    x[x == ""] <- NA_character_
    suppressWarnings(as.integer(as.numeric(x)))
  }
  
  to_year <- function(x) {
    x <- as.character(x)
    y <- suppressWarnings(as.integer(x))
    if (all(is.na(y))) {
      m <- regexpr("\\b(19|20)\\d{2}\\b", x)
      y <- ifelse(m > 0, as.integer(regmatches(x, m)), NA_integer_)
    }
    y
  }
  
  to_id <- function(x) {
    x_chr <- as.character(x)
    numeric_like <- grepl("^\\s*-?\\d+\\s*$", x_chr) & !is.na(x_chr)
    if (all(!numeric_like, na.rm = TRUE)) {
      ux <- sort(unique(x_chr[!is.na(x_chr)]))
      out <- match(x_chr, ux)
      return(as.integer(out))
    } else {
      out <- rep(NA_integer_, length(x_chr))
      out[numeric_like] <- suppressWarnings(as.integer(x_chr[numeric_like]))
      return(out)
    }
  }
  
  cleaned_data <- data.table::data.table(
    school_id  = to_id(df[[school_id_col]]),
    year       = to_year(df[[year_col]]),
    enrollment = to_int(df[[enrollment_col]]),
    current    = to_int(df[[current_col]]),
    med_exempt = to_int(df[[med_exempt_col]]),
    rel_exempt = to_int(df[[rel_exempt_col]])
  )
  
  if (!is.null(cnty_id_col) && cnty_id_col %in% names(df)) {
    cleaned_data[, cnty_id := to_id(df[[cnty_id_col]])]
  }
  
  add_char_col <- function(dt, out_name, col_arg) {
    if (!is.null(col_arg) && col_arg %in% names(df)) {
      dt[, (out_name) := as.character(df[[col_arg]])]
    }
  }
  
  add_char_col(cleaned_data, "school_name",     school_name_col)
  add_char_col(cleaned_data, "county_name",     county_name_col)
  add_char_col(cleaned_data, "vaccine_type",    vaccine_type_col)
  add_char_col(cleaned_data, "school_type",     school_type_col)
  add_char_col(cleaned_data, "school_level",    school_level_col)
  add_char_col(cleaned_data, "level_code",      level_code_col)
  add_char_col(cleaned_data, "excluded_note",   excluded_note_col)
  add_char_col(cleaned_data, "addr_clean",      addr_clean_col)
  add_char_col(cleaned_data, "city",            city_col)
  add_char_col(cleaned_data, "zip",             zip_col)
  add_char_col(cleaned_data, "state",           state_col)
  add_char_col(cleaned_data, "business_status", business_status_col)
  
  if (!is.null(lat_col) && lat_col %in% names(df)) {
    cleaned_data[, lat := suppressWarnings(as.numeric(df[[lat_col]]))]
  }
  if (!is.null(lon_col) && lon_col %in% names(df)) {
    cleaned_data[, lon := suppressWarnings(as.numeric(df[[lon_col]]))]
  }
  
  base_cols  <- c("school_id", "year", "enrollment", "current",
                  "med_exempt", "rel_exempt")
  extra_cols <- intersect(
    c("school_name", "county_name", "cnty_id",
      "vaccine_type", "school_type", "school_level", "level_code",
      "excluded_note", "addr_clean", "city", "zip", "state",
      "business_status", "lat", "lon"),
    names(cleaned_data)
  )
  data.table::setcolorder(cleaned_data, c(base_cols, extra_cols))
  
  cleaned_data
}


# ------------------------------------------------------------------------------
# DQA: AUTOMATIC CORRECTIONS
# ------------------------------------------------------------------------------

#' Remove Records with Zero or Missing Enrollment
#'
#' @description
#' Identifies and removes rows where enrollment is \code{NA} or \eqn{\le 0},
#' exports the removed rows to a CSV in \code{temp_data_dir}, and returns the
#' cleaned data together with an updated DQA report.
#'
#' @param df A data frame with an \code{enrollment} column.
#' @param dqa_report A list used to accumulate DQA results (optional).
#' @param temp_data_dir Directory path for exporting flagged rows.
#' @param state State abbreviation used in the output file name.
#'
#' @return A list with components \code{data} (cleaned data frame) and
#'   \code{report} (updated DQA report list).
#'
#' @importFrom dplyr filter
#' @importFrom readr write_csv
#' @export
dqa_remove_zero_enrollment <- function(df,
                                       dqa_report = NULL,
                                       temp_data_dir,
                                       state = "state") {
  
  flag <- df %>%
    dplyr::filter(is.na(enrollment) | enrollment <= 0)
  
  if (nrow(flag) > 0) {
    readr::write_csv(
      flag,
      file.path(temp_data_dir, paste0(state, "_zero_or_missing_enrollment_removed.csv"))
    )
  }
  
  before_n <- nrow(df)
  df <- df %>%
    dplyr::filter(!is.na(enrollment) & enrollment > 0)
  after_n <- nrow(df)
  
  if (!is.null(dqa_report)) {
    dqa_report$enrollment_zero_or_missing <- list(
      removed = before_n - after_n,
      remaining = after_n,
      description = "Rows with zero or missing enrollment"
    )
  }
  
  message("DQA Check: Removed ", before_n - after_n,
          " rows with zero or missing enrollment.")
  
  list(data = df, report = dqa_report)
}


#' Remove Records with Zero Current Vaccinations
#'
#' @description
#' Removes rows where \code{current} is \code{NA} or zero and returns the
#' cleaned data together with an updated DQA report.
#'
#' @param df A data frame with a \code{current} column.
#' @param dqa_report A list used to accumulate DQA results (optional).
#'
#' @return A list with components \code{data} and \code{report}.
#'
#' @importFrom dplyr filter
#' @export
dqa_remove_zero_current <- function(df, dqa_report = NULL) {
  before_n <- nrow(df)
  df <- df %>%
    dplyr::filter(!is.na(current) & current != 0)
  after_n <- nrow(df)
  removed_n <- before_n - after_n
  
  if (!is.null(dqa_report)) {
    dqa_report$current_vacc_zero <- list(
      removed = removed_n,
      remaining = after_n,
      description = "Rows with zero or NA current vaccinations"
    )
  }
  
  message("DQA Check: Removed ", removed_n,
          " rows with zero current vaccinations. Remaining: ", after_n)
  
  list(data = df, report = dqa_report)
}


#' Fix Invalid Count Values
#'
#' @description
#' Applies automatic corrections for impossible count values:
#' \itemize{
#'   \item \code{current < 0} — set \code{current} to \code{NA}
#'   \item \code{current > enrollment} — set \code{current} to \code{NA}
#'   \item any single exemption count or the sum of exemptions exceeds
#'         \code{enrollment} — set both exemption columns to \code{NA}
#' }
#' Before modifying, affected rows are exported as a CSV to \code{temp_data_dir}
#' for audit purposes.
#'
#' @param df A data frame containing count columns.
#' @param dqa_report A list used to accumulate DQA results (optional).
#' @param enrollment_col Name of the enrollment column (default \code{"enrollment"}).
#' @param current_col Name of the current-vaccination column (default
#'   \code{"current"}).
#' @param med_exempt_col Name of the medical-exemption column (default
#'   \code{"med_exempt"}).
#' @param rel_exempt_col Name of the religious-exemption column (default
#'   \code{"rel_exempt"}).
#' @param delayed_col Name of the delayed-vaccination column (default
#'   \code{"delayed"}).  Used only for NA-count reporting.
#' @param note_col Name of the notes column (created if absent; default
#'   \code{"excluded_note"}).
#' @param temp_data_dir Directory path for exporting changed rows.
#' @param state State abbreviation used in the output file name.
#' @param export_prefix Prefix for the export file name (default
#'   \code{"invalid_values_fixed"}).
#'
#' @return A list with components \code{data} and \code{report}.
#'
#' @importFrom readr write_csv
#' @export
dqa_fix_invalid_values <- function(
    df,
    dqa_report = NULL,
    enrollment_col = "enrollment",
    current_col = "current",
    med_exempt_col = "med_exempt",
    rel_exempt_col = "rel_exempt",
    delayed_col = "delayed",
    note_col = "excluded_note",
    temp_data_dir,
    state = "state",
    export_prefix = "invalid_values_fixed"
) {
  req <- c(enrollment_col, current_col, med_exempt_col, rel_exempt_col)
  missing <- setdiff(req, names(df))
  if (length(missing) > 0) {
    stop("Missing required columns: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  if (missing(temp_data_dir) || is.null(temp_data_dir) || !nzchar(temp_data_dir)) {
    stop("temp_data_dir must be provided (non-empty) to export changed rows.", call. = FALSE)
  }
  if (!dir.exists(temp_data_dir)) dir.create(temp_data_dir, recursive = TRUE)
  
  if (!note_col %in% names(df)) df[[note_col]] <- NA_character_
  
  na_before <- list(
    enrollment = sum(is.na(df[[enrollment_col]])),
    current    = sum(is.na(df[[current_col]])),
    med_exempt = sum(is.na(df[[med_exempt_col]])),
    rel_exempt = sum(is.na(df[[rel_exempt_col]]))
  )
  if (delayed_col %in% names(df)) {
    na_before$delayed <- sum(is.na(df[[delayed_col]]))
  }
  
  enroll <- df[[enrollment_col]]
  curr   <- df[[current_col]]
  med    <- df[[med_exempt_col]]
  rel    <- df[[rel_exempt_col]]
  
  neg_current    <- !is.na(curr) & curr < 0
  current_gt_enr <- !is.na(enroll) & !is.na(curr) & curr > enroll
  
  ex_med_gt_enr  <- !is.na(enroll) & !is.na(med) & med > enroll
  ex_rel_gt_enr  <- !is.na(enroll) & !is.na(rel) & rel > enroll
  ex_sum_gt_enr  <- !is.na(enroll) & !is.na(med) & !is.na(rel) & (med + rel) > enroll
  
  invalid_exemptions <- ex_med_gt_enr | ex_rel_gt_enr | ex_sum_gt_enr
  
  rows_changed <- neg_current | current_gt_enr | invalid_exemptions
  
  if (any(rows_changed, na.rm = TRUE)) {
    changed_rows <- df[rows_changed, , drop = FALSE]
    
    out_path <- file.path(
      temp_data_dir,
      paste0(state, "_", export_prefix, ".csv")
    )
    
    readr::write_csv(changed_rows, out_path)
    message("DQA export: wrote ", sum(rows_changed, na.rm = TRUE),
            " changed rows to ", out_path)
  } else {
    message("DQA export: no invalid-value corrections needed (no rows changed).")
  }
  
  n_neg_current        <- sum(neg_current, na.rm = TRUE)
  n_current_gt_enr     <- sum(current_gt_enr, na.rm = TRUE)
  n_invalid_exemptions <- sum(invalid_exemptions, na.rm = TRUE)
  
  if (!is.null(dqa_report)) {
    dqa_report$negative_current <- list(
      count = n_neg_current,
      description = "Rows with current < 0 (set to NA)"
    )
    dqa_report$current_gt_enrollment <- list(
      count = n_current_gt_enr,
      description = "Rows with current > enrollment (set to NA)"
    )
    dqa_report$invalid_exemptions_counts <- list(
      count = n_invalid_exemptions,
      description = "Rows where exemption counts exceed enrollment (set exemptions to NA)"
    )
  }
  
  append_note <- function(old, add) {
    ifelse(is.na(old) | old == "", add, paste0(old, "; ", add))
  }
  
  df[[note_col]] <- ifelse(neg_current,
                           append_note(df[[note_col]], "current <0"),
                           df[[note_col]])
  df[[note_col]] <- ifelse(current_gt_enr,
                           append_note(df[[note_col]], "current > enrollment"),
                           df[[note_col]])
  df[[note_col]] <- ifelse(invalid_exemptions,
                           append_note(df[[note_col]], "exemptions > enrollment"),
                           df[[note_col]])
  
  df[[current_col]]    <- ifelse(neg_current | current_gt_enr, NA, df[[current_col]])
  df[[med_exempt_col]] <- ifelse(invalid_exemptions, NA, df[[med_exempt_col]])
  df[[rel_exempt_col]] <- ifelse(invalid_exemptions, NA, df[[rel_exempt_col]])
  
  na_after <- list(
    enrollment = sum(is.na(df[[enrollment_col]])),
    current    = sum(is.na(df[[current_col]])),
    med_exempt = sum(is.na(df[[med_exempt_col]])),
    rel_exempt = sum(is.na(df[[rel_exempt_col]]))
  )
  if (delayed_col %in% names(df)) {
    na_after$delayed <- sum(is.na(df[[delayed_col]]))
  }
  
  if (!is.null(dqa_report)) {
    dqa_report$na_counts <- list(
      before = na_before,
      after  = na_after,
      description = "NA counts before and after invalid-value corrections (count columns)"
    )
  }
  
  list(data = df, report = dqa_report)
}


# ------------------------------------------------------------------------------
# DQA: FLAGGING CHECKS (no automatic removals)
# ------------------------------------------------------------------------------

#' Check for Duplicate School-Year Combinations
#'
#' @description
#' Identifies duplicate \code{school_id}-\code{year}-\code{school_name}
#' combinations, distinguishes identical from conflicting duplicates, and
#' optionally exports conflicting cases to a CSV.
#'
#' @param df A data frame with \code{school_id}, \code{year}, and
#'   \code{school_name} columns.
#' @param state_dir State-specific directory (used to determine output location).
#' @param state State abbreviation.
#'
#' @return A list with components \code{data} (unchanged) and \code{n_flagged}.
#'
#' @importFrom dplyr group_by summarise filter n semi_join n_distinct pick all_of across distinct ungroup
#' @importFrom readr write_csv
#' @export
dqa_check_duplicates <- function(df, state_dir = NULL, state = "state") {
  core_cols <- colnames(df)[1:min(10, ncol(df))]
  core_data_cols <- setdiff(core_cols, c("school_id", "year"))
  
  dup_counts <- df %>%
    dplyr::group_by(school_id, year) %>%
    dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
    dplyr::filter(n > 1)
  
  if (nrow(dup_counts) > 0) {
    message("Found ", nrow(dup_counts), " duplicated school_id-year combinations.")
    
    dupes <- df %>%
      dplyr::semi_join(dup_counts, by = c("school_id", "year")) %>%
      dplyr::group_by(school_id, year) %>%
      dplyr::summarise(
        n_rows = dplyr::n(),
        all_identical = dplyr::n_distinct(dplyr::pick(dplyr::all_of(core_data_cols))) == 1,
        .groups = "drop"
      )
    
    true_dupes <- dupes %>% dplyr::filter(all_identical)
    if (nrow(true_dupes) > 0) {
      message("Merging ", nrow(true_dupes), " identical duplicates.")
      df <- df %>%
        dplyr::group_by(school_id, year) %>%
        dplyr::distinct(dplyr::across(dplyr::all_of(core_cols)), .keep_all = TRUE) %>%
        dplyr::ungroup()
    }
    
    diff_dupes <- dupes %>% dplyr::filter(!all_identical)
    if (nrow(diff_dupes) > 0) {
      message("Found ", nrow(diff_dupes), " duplicates with conflicting core data.")
      conflict_rows <- df %>%
        dplyr::semi_join(diff_dupes, by = c("school_id", "year"))
      print(head(conflict_rows, 10))
      if (!is.null(state_dir)) {
        readr::write_csv(conflict_rows, file.path(state_dir, "duplicate_conflicts.csv"))
      }
    }
  } else {
    message("No duplicate school_id-year combinations found in core columns.")
  }
  
  list(data = df, n_flagged = nrow(dup_counts))
}


#' Check for Negative Values in Key Columns
#'
#' @description
#' Flags rows containing negative values in \code{current}, \code{med_exempt},
#' \code{rel_exempt}, or \code{enrollment} and optionally exports them to a CSV.
#'
#' @param df A data frame with numeric vaccination count columns.
#' @param state_dir State-specific directory for exporting flagged rows.
#' @param state State abbreviation.
#'
#' @return A list with components \code{data} (unchanged) and \code{n_flagged}.
#'
#' @importFrom dplyr filter
#' @importFrom readr write_csv
#' @export
dqa_check_negatives <- function(df, state_dir = NULL, state = "state") {
  negatives <- df %>%
    dplyr::filter(current < 0 | med_exempt < 0 | rel_exempt < 0 | enrollment < 0)
  
  if (nrow(negatives) > 0) {
    warning("Found negative values in ", nrow(negatives), " rows.")
    print(head(negatives, 5))
    if (!is.null(state_dir)) {
      readr::write_csv(negatives, file.path(state_dir, "negatives_check.csv"))
    }
  }
  
  list(data = df, n_flagged = nrow(negatives))
}


#' Check for Values Exceeding Enrollment
#'
#' @description
#' Flags rows where \code{current}, \code{med_exempt}, or \code{rel_exempt}
#' individually exceed \code{enrollment} and optionally exports them to a CSV.
#'
#' @param df A data frame with numeric vaccination count columns.
#' @param state_dir State-specific directory for exporting flagged rows.
#' @param state State abbreviation.
#'
#' @return A list with components \code{data} (unchanged) and \code{n_flagged}.
#'
#' @importFrom dplyr filter
#' @importFrom readr write_csv
#' @export
dqa_check_too_high <- function(df, state_dir = NULL, state = "state") {
  too_high <- df %>%
    dplyr::filter(current > enrollment | med_exempt > enrollment | rel_exempt > enrollment)
  
  if (nrow(too_high) > 0) {
    warning("Found higher-than-enrollment values for current/exempt in ", nrow(too_high), " rows.")
    print(head(too_high, 5))
    if (!is.null(state_dir)) {
      readr::write_csv(too_high, file.path(state_dir, "too_high_check.csv"))
    }
  }
  
  list(data = df, n_flagged = nrow(too_high))
}


#' Check for Coverage Outliers
#'
#' @description
#' Adds an \code{mmr_coverage} column (\code{current / enrollment}) and flags
#' rows with unrealistic values (less than 0 or greater than 105%).
#'
#' @param df A data frame with \code{current} and \code{enrollment} columns.
#' @param state_dir State-specific directory for exporting flagged rows.
#' @param state State abbreviation.
#'
#' @return A list with components \code{data} (with \code{mmr_coverage} added)
#'   and \code{n_flagged}.
#'
#' @importFrom dplyr mutate filter
#' @importFrom readr write_csv
#' @export
dqa_check_coverage_outliers <- function(df, state_dir = NULL, state = "state") {
  df <- df %>%
    dplyr::mutate(mmr_coverage = current / enrollment)
  
  coverage_outliers <- df %>%
    dplyr::filter(!is.na(mmr_coverage) & (mmr_coverage < 0 | mmr_coverage > 1.05))
  
  if (nrow(coverage_outliers) > 0) {
    warning("Found ", nrow(coverage_outliers), " rows with unrealistic coverage (<0 or >105%).")
    print(head(coverage_outliers, 5))
    if (!is.null(state_dir)) {
      readr::write_csv(coverage_outliers, file.path(state_dir, "coverage_outliers.csv"))
    }
  }
  
  list(data = df, n_flagged = nrow(coverage_outliers))
}


#' Check for Over-Coverage
#'
#' @description
#' Flags rows where \code{current + med_exempt + rel_exempt > enrollment}.
#'
#' @param df A data frame with vaccination count columns.
#' @param state_dir State-specific directory for exporting flagged rows.
#' @param state State abbreviation.
#'
#' @return A list with components \code{data} (unchanged) and \code{n_flagged}.
#'
#' @importFrom dplyr filter
#' @importFrom readr write_csv
#' @export
dqa_check_over_coverage <- function(df, state_dir = NULL, state = "state") {
  over_coverage <- df %>%
    dplyr::filter(!is.na(enrollment) & (current + med_exempt + rel_exempt) > enrollment)
  
  if (nrow(over_coverage) > 0) {
    warning("Found ", nrow(over_coverage), " rows where current + exempt exceed enrollment.")
    print(head(over_coverage, 5))
    if (!is.null(state_dir)) {
      readr::write_csv(over_coverage, file.path(state_dir, "over_coverage_check.csv"))
    }
  }
  
  list(data = df, n_flagged = nrow(over_coverage))
}


#' Check for Enrollment Deviations from School Historical Average
#'
#' @description
#' Flags rows where a school's enrollment deviates more than \code{threshold}
#' (as a fraction) from that school's mean enrollment across all years.
#' Flagged rows and the full year-history for flagged schools are exported to
#' \code{temp_data_dir}.
#'
#' @param df A data frame with \code{school_id}, \code{year}, and
#'   \code{enrollment} columns.
#' @param state_dir State-specific directory (currently unused; kept for API
#'   consistency).
#' @param state State abbreviation.
#' @param threshold Fractional deviation threshold (default \code{0.5} = 50\%).
#'
#' @return A list with components \code{data} (unchanged) and \code{n_flagged}.
#'
#' @importFrom dplyr group_by mutate if_else ungroup filter arrange desc select all_of any_of left_join distinct
#' @importFrom readr write_csv
#' @export
dqa_check_enrollment_deviation <- function(df,
                                           state_dir = NULL,
                                           state = "state",
                                           threshold = 0.5) {
  
  req <- c("school_id", "year", "enrollment")
  missing <- setdiff(req, names(df))
  if (length(missing) > 0) {
    warning("Skipping enrollment deviation check; missing columns: ",
            paste(missing, collapse = ", "))
    return(list(data = df, n_flagged = NA_integer_))
  }
  
  info_cols <- intersect(c("school_name", "county_name"), names(df))
  
  enrollment_deviation <- df %>%
    dplyr::group_by(school_id) %>%
    dplyr::mutate(
      mean_enrollment = mean(enrollment, na.rm = TRUE),
      pct_diff_from_mean = dplyr::if_else(
        is.na(mean_enrollment) | mean_enrollment == 0,
        NA_real_,
        (enrollment - mean_enrollment) / mean_enrollment
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(!is.na(pct_diff_from_mean) & abs(pct_diff_from_mean) > threshold)
  
  if (nrow(enrollment_deviation) > 0) {
    warning(
      "Found ", nrow(enrollment_deviation),
      " rows with enrollment differing >", threshold * 100,
      "% from that school's mean across years."
    )
    
    preview_cols <- c("school_id", "cnty_id", info_cols, "year",
                      "enrollment", "mean_enrollment", "pct_diff_from_mean")
    preview_cols <- intersect(preview_cols, names(enrollment_deviation))
    
    print(
      enrollment_deviation %>%
        dplyr::arrange(dplyr::desc(abs(pct_diff_from_mean))) %>%
        dplyr::select(dplyr::all_of(preview_cols)) %>%
        head(5)
    )
    
    if (!is.null(state_dir)) {
      if (!dir.exists(state_dir)) dir.create(state_dir, recursive = TRUE)
      
      readr::write_csv(
        enrollment_deviation,
        file.path(state_dir, "enrollment_deviation_check.csv")
      )
      
      flagged_school_ids <- unique(enrollment_deviation$school_id)
      school_history <- df %>%
        dplyr::filter(school_id %in% flagged_school_ids) %>%
        dplyr::left_join(
          enrollment_deviation %>%
            dplyr::select(school_id, dplyr::any_of("mean_enrollment")) %>%
            dplyr::distinct(),
          by = "school_id"
        )
      readr::write_csv(
        school_history,
        file.path(state_dir, "enrollment_deviation_school_history.csv")
      )
    }
  } else {
    message("No enrollment deviations >", threshold * 100, "% from school mean found.")
  }
  
  list(data = df, n_flagged = nrow(enrollment_deviation))
}


#' Check for Current Vaccination Deviations from School Historical Average
#'
#' @description
#' Flags rows where a school's current vaccination count deviates more than
#' \code{threshold} from that school's mean across years.  Flagged rows and
#' school histories are exported to \code{temp_data_dir}.
#'
#' @param df A data frame with \code{school_id}, \code{year}, and \code{current}
#'   columns.
#' @param temp_data_dir Directory path for exporting flagged rows.
#' @param state State abbreviation.
#' @param threshold Fractional deviation threshold (default \code{0.5} = 50\%).
#'
#' @return A list with components \code{data} (unchanged) and \code{n_flagged}.
#'
#' @importFrom dplyr group_by mutate if_else ungroup filter arrange desc select all_of any_of left_join distinct
#' @importFrom readr write_csv
#' @export
dqa_check_current_deviation <- function(df,
                                        temp_data_dir,
                                        state = "state",
                                        threshold = 0.5) {
  
  req <- c("school_id", "year", "current")
  missing <- setdiff(req, names(df))
  if (length(missing) > 0) {
    warning("Skipping current deviation check; missing columns: ",
            paste(missing, collapse = ", "))
    return(list(data = df, n_flagged = NA_integer_))
  }
  
  if (missing(temp_data_dir) || is.null(temp_data_dir) || !nzchar(temp_data_dir)) {
    stop("temp_data_dir must be provided (non-empty) to save deviation CSV.", call. = FALSE)
  }
  if (!dir.exists(temp_data_dir)) dir.create(temp_data_dir, recursive = TRUE)
  
  info_cols <- intersect(c("school_name", "county_name"), names(df))
  
  current_deviation <- df %>%
    dplyr::group_by(school_id) %>%
    dplyr::mutate(
      mean_current = mean(current, na.rm = TRUE),
      pct_diff_from_mean = dplyr::if_else(
        is.na(mean_current) | mean_current == 0,
        NA_real_,
        (current - mean_current) / mean_current
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(!is.na(pct_diff_from_mean) & abs(pct_diff_from_mean) > threshold)
  
  if (nrow(current_deviation) > 0) {
    warning("Found ", nrow(current_deviation),
            " rows with 'current' differing >", threshold * 100,
            "% from that school's mean across years.")
    
    preview_cols <- c("school_id", "cnty_id", info_cols, "year",
                      "current", "mean_current", "pct_diff_from_mean")
    preview_cols <- intersect(preview_cols, names(current_deviation))
    
    print(
      current_deviation %>%
        dplyr::arrange(dplyr::desc(abs(pct_diff_from_mean))) %>%
        dplyr::select(dplyr::all_of(preview_cols)) %>%
        head(5)
    )
    
    out_path <- file.path(temp_data_dir, paste0(state, "_current_deviation_check.csv"))
    readr::write_csv(current_deviation, out_path)
    message("Saved current deviation check CSV to: ", out_path)
    
    flagged_school_ids <- unique(current_deviation$school_id)
    school_history <- df %>%
      dplyr::filter(school_id %in% flagged_school_ids) %>%
      dplyr::left_join(
        current_deviation %>%
          dplyr::select(school_id, dplyr::any_of("mean_current")) %>%
          dplyr::distinct(),
        by = "school_id"
      )
    history_path <- file.path(temp_data_dir, paste0(state, "_current_deviation_school_history.csv"))
    readr::write_csv(school_history, history_path)
    message("Saved current deviation school history CSV to: ", history_path)
    
  } else {
    message("No current deviations >", threshold * 100, "% from school mean found.")
  }
  
  list(data = df, n_flagged = nrow(current_deviation))
}


#' Check for Extreme Enrollment Outliers
#'
#' @description
#' Flags rows where a school's enrollment in a given year is more than
#' \code{threshold} times, or less than \code{1 / threshold} times, that
#' school's median enrollment across all years.  Flagged rows are exported to
#' \code{temp_data_dir}.
#'
#' @param df A data frame with \code{school_id} and \code{enrollment} columns.
#' @param temp_data_dir Directory path for exporting flagged rows.
#' @param state State abbreviation.
#' @param threshold Multiplier threshold (default \code{10}).
#'
#' @return A list with components \code{data} (unchanged) and \code{n_flagged}.
#'
#' @importFrom dplyr group_by summarise left_join filter mutate case_when arrange desc select all_of ungroup
#' @importFrom stats median
#' @importFrom readr write_csv
#' @export
dqa_check_extreme_outliers <- function(df,
                                       temp_data_dir,
                                       state = "state",
                                       threshold = 10) {
  
  req <- c("school_id", "enrollment")
  missing <- setdiff(req, names(df))
  if (length(missing) > 0) {
    warning("Skipping extreme enrollment outlier check; missing columns: ",
            paste(missing, collapse = ", "))
    return(list(data = df, n_flagged = NA_integer_))
  }
  
  if (missing(temp_data_dir) || is.null(temp_data_dir) || !nzchar(temp_data_dir)) {
    stop("temp_data_dir must be provided (non-empty) to save outlier CSV.", call. = FALSE)
  }
  if (!dir.exists(temp_data_dir)) dir.create(temp_data_dir, recursive = TRUE)
  
  median_enrollment <- df %>%
    dplyr::group_by(school_id) %>%
    dplyr::summarise(
      median_enroll = stats::median(enrollment, na.rm = TRUE),
      .groups = "drop"
    )
  
  outliers <- df %>%
    dplyr::left_join(median_enrollment, by = "school_id") %>%
    dplyr::filter(
      !is.na(median_enroll),
      median_enroll > 0,
      enrollment >= threshold * median_enroll |
        enrollment <= median_enroll / threshold
    ) %>%
    dplyr::mutate(
      ratio_to_median = enrollment / median_enroll,
      outlier_type = dplyr::case_when(
        enrollment >= threshold * median_enroll ~ "high",
        enrollment <= median_enroll / threshold ~ "low"
      )
    )
  
  if (nrow(outliers) > 0) {
    warning(
      "Found ", nrow(outliers),
      " rows with extreme enrollment values (\u2265",
      threshold, "x or \u22641/", threshold,
      "x that school's median across years)."
    )
    
    info_cols <- intersect(c("school_name", "county_name"), names(outliers))
    
    preview_cols <- c("school_id", "cnty_id", info_cols, "year",
                      "enrollment", "median_enroll", "ratio_to_median", "outlier_type")
    preview_cols <- intersect(preview_cols, names(outliers))
    
    print(
      outliers %>%
        dplyr::mutate(extremeness = pmax(ratio_to_median, 1 / ratio_to_median)) %>%
        dplyr::arrange(dplyr::desc(extremeness)) %>%
        dplyr::select(dplyr::all_of(preview_cols)) %>%
        head(5)
    )
    
    out_path <- file.path(
      temp_data_dir,
      paste0(state, "_extreme_enrollment_outliers.csv")
    )
    readr::write_csv(outliers, out_path)
    message("Saved extreme enrollment outliers CSV to: ", out_path)
    
  } else {
    message(
      "No extreme enrollment outliers found (\u2265",
      threshold, "x or \u22641/", threshold,
      "x median)."
    )
  }
  
  list(data = df, n_flagged = nrow(outliers))
}


# ------------------------------------------------------------------------------
# FINAL FORMATTING HELPERS
# ------------------------------------------------------------------------------

#' Select and Validate Required Columns
#'
#' @description
#' Keeps only the required columns and any optional columns that are present.
#' Issues a warning for any required columns that are missing.
#'
#' @param df A data frame to format.
#' @param required_cols Character vector of required column names.
#' @param optional_cols Character vector of optional column names.
#'
#' @return A data frame containing only the required and present optional columns.
#'
#' @importFrom dplyr select all_of
#' @export
format_select_columns <- function(df,
                                  required_cols = c("year", "school_id", "school_name",
                                                    "cnty_id", "school_county",
                                                    "enrollment", "current", "delayed",
                                                    "med_exempt", "rel_exempt"),
                                  optional_cols = c("school_type")) {
  missing_cols <- setdiff(required_cols, colnames(df))
  if (length(missing_cols) > 0) {
    warning("Missing required columns: ", paste(missing_cols, collapse = ", "))
  } else {
    message("All required columns are present.")
  }
  
  keep_cols <- c(required_cols, intersect(optional_cols, colnames(df)))
  df <- df %>% dplyr::select(dplyr::all_of(keep_cols))
  
  message("Columns in dataset: ", paste(colnames(df), collapse = ", "))
  
  return(df)
}


#' Validate and Fix Column Classes
#'
#' @description
#' Coerces each recognised column in \code{df} to its expected class
#' (\code{integer}, \code{numeric}, or \code{character}).  Warns when a
#' coercion is required or when the result does not match the expected class.
#'
#' @param df A data frame to validate.
#' @param warn_on_change Logical; warn when a column is coerced (default
#'   \code{TRUE}).
#' @param warn_on_missing Logical; warn when an expected column is absent
#'   (default \code{FALSE}).
#'
#' @return A data frame with corrected column classes.
#'
#' @export
format_fix_column_classes <- function(df, warn_on_change = TRUE, warn_on_missing = FALSE) {
  
  expected_classes <- list(
    year             = "integer",
    school_id        = "integer",
    enrollment       = "integer",
    current          = "integer",
    med_exempt       = "integer",
    rel_exempt       = "integer",
    school_name      = "character",
    county_name      = "character",
    school_type      = "character",
    school_level     = "character",
    excluded_note    = "character",
    addr_clean       = "character",
    city             = "character",
    zip              = "character",
    state            = "character",
    business_status  = "character",
    lat              = "numeric",
    lon              = "numeric"
  )
  
  cols_present <- intersect(names(expected_classes), names(df))
  
  if (warn_on_missing) {
    cols_missing <- setdiff(names(expected_classes), names(df))
    if (length(cols_missing) > 0) {
      warning("Missing columns (skipped): ", paste(cols_missing, collapse = ", "))
    }
  }
  
  to_int <- function(x) {
    x <- as.character(x)
    x <- gsub(",", "", x)
    x <- gsub("%", "", x)
    x <- trimws(x)
    x[x == ""] <- NA_character_
    suppressWarnings(as.integer(as.numeric(x)))
  }
  
  for (col in cols_present) {
    expected <- expected_classes[[col]]
    before_class <- class(df[[col]])[1]
    
    if (expected == "integer") {
      df[[col]] <- to_int(df[[col]])
    } else if (expected == "numeric") {
      df[[col]] <- suppressWarnings(as.numeric(df[[col]]))
    } else if (expected == "character") {
      df[[col]] <- as.character(df[[col]])
    }
    
    after_class <- class(df[[col]])[1]
    
    if (warn_on_change && before_class != expected) {
      warning("Column '", col, "' coerced from '", before_class, "' to expected '", expected, "'.")
    }
    
    if (after_class != expected) {
      warning("Column '", col, "' is class '", after_class,
              "' after coercion, but expected '", expected, "'.")
    }
  }
  
  return(df)
}


#' Print a DQA Summary Report to the Console
#'
#' @description
#' Prints a human-readable summary of issues found and corrected, drawn from
#' the \code{dqa_report} list populated by the DQA check functions.
#'
#' @param dqa_report A named list of DQA results as returned/updated by the
#'   \code{dqa_*} family of functions.
#'
#' @return \code{NULL}, invisibly.  Called for its side-effect of printing.
#'
#' @export
print_dqa_summary <- function(dqa_report) {
  get_in <- function(x, path, default = NA) {
    cur <- x
    for (p in path) {
      if (is.null(cur) || !is.list(cur) || is.null(cur[[p]])) return(default)
      cur <- cur[[p]]
    }
    cur
  }
  
  fmt <- function(x) {
    if (is.na(x)) "(not recorded)" else as.character(x)
  }
  
  message("\n========================================")
  message("DATA QUALITY ASSESSMENT SUMMARY REPORT")
  message("========================================")
  
  message("Initial records: ", fmt(get_in(dqa_report, c("total_records_start"), NA)))
  message("Final records:   ", fmt(get_in(dqa_report, c("total_records_end"), NA)))
  message("Total removed:   ", fmt(get_in(dqa_report, c("total_records_removed"), NA)))
  
  message("\nISSUES FOUND AND CORRECTED:")
  
  message("  - Zero/missing enrollment: ",
          fmt(get_in(dqa_report, c("enrollment_zero_or_missing", "removed"), NA)), " removed")
  message("  - Zero current vaccinations: ",
          fmt(get_in(dqa_report, c("current_vacc_zero", "removed"), NA)), " removed")
  
  neg_new <- get_in(dqa_report, c("negative_current", "count"), NA)
  ex_new  <- get_in(dqa_report, c("invalid_exemptions_counts", "count"), NA)
  curr_gt <- get_in(dqa_report, c("current_gt_enrollment", "count"), NA)
  
  has_new <- !is.na(neg_new) || !is.na(ex_new) || !is.na(curr_gt)
  
  if (has_new) {
    message("  - Current < 0: ", ifelse(is.na(neg_new), 0, neg_new), " set to NA")
    message("  - Current > enrollment: ", ifelse(is.na(curr_gt), 0, curr_gt), " set to NA")
    message("  - Exemption counts > enrollment: ", ifelse(is.na(ex_new), 0, ex_new), " set to NA")
  } else {
    neg_old <- get_in(dqa_report, c("negative_coverage", "count"), NA)
    ex_old  <- get_in(dqa_report, c("exemptions_over_100", "count"), NA)
    message("  - Negative coverage: ", ifelse(is.na(neg_old), 0, neg_old), " set to NA")
    message("  - Exemptions >100%: ", ifelse(is.na(ex_old), 0, ex_old), " set to NA")
  }
  
  message("\nNA COUNTS AFTER CORRECTIONS:")
  
  na_after_enrollment <- get_in(dqa_report, c("na_counts", "after", "enrollment"), NA)
  na_after_current    <- get_in(dqa_report, c("na_counts", "after", "current"), NA)
  na_after_med_exempt <- get_in(dqa_report, c("na_counts", "after", "med_exempt"), NA)
  na_after_rel_exempt <- get_in(dqa_report, c("na_counts", "after", "rel_exempt"), NA)
  
  if (!all(is.na(c(na_after_current, na_after_med_exempt, na_after_rel_exempt, na_after_enrollment)))) {
    message("  - enrollment: ", fmt(na_after_enrollment))
    message("  - current:    ", fmt(na_after_current))
    message("  - med_exempt: ", fmt(na_after_med_exempt))
    message("  - rel_exempt: ", fmt(na_after_rel_exempt))
  } else {
    message("  - vaccine_coverage: ", fmt(get_in(dqa_report, c("na_counts", "after", "vaccine_coverage"), NA)))
    message("  - percent_medical_exemption: ", fmt(get_in(dqa_report, c("na_counts", "after", "percent_medical_exemption"), NA)))
    message("  - percent_religious_exemption: ", fmt(get_in(dqa_report, c("na_counts", "after", "percent_religious_exemption"), NA)))
  }
  
  message("========================================\n")
}


# ------------------------------------------------------------------------------
# WRAPPER
# ------------------------------------------------------------------------------

#' Run Final Formatting and DQA Checks
#'
#' @description
#' State-agnostic wrapper that:
#' \enumerate{
#'   \item Loads the state-specific cleaned data from step 03.
#'   \item Optionally filters to a single vaccine type.
#'   \item Applies automatic DQA corrections (zero enrollment, invalid counts).
#'   \item Selects and type-coerces columns to the canonical format.
#'   \item Saves \code{cleaned_data.rds} and \code{cleaned_data.csv} to
#'         \code{clean_data_dir}.
#'   \item Runs a suite of flagging DQA checks and saves diagnostic CSVs.
#'   \item Builds the \code{obs}, \code{locations}, and \code{obs_populations}
#'         tables for the modelling pipeline, incorporating CDC VaxView data.
#'   \item Saves all model-input tables to \code{outputs_data_dir}.
#' }
#'
#' @param state Two-letter state abbreviation (e.g., \code{"md"}).
#' @param vaccine_type_to_keep Vaccine type string used to filter the
#'   \code{vaccine_type} column (e.g., \code{"mmr"}).  The comparison is
#'   case-insensitive.  If the column is absent the filter is skipped.
#' @param temp_data_dir Path to the directory containing
#'   \code{kinder_vaccination_clean_03.rds} and used for intermediate outputs.
#' @param clean_data_dir Path to the directory where the final
#'   \code{cleaned_data.rds} / \code{cleaned_data.csv} are written.
#' @param vaxview_dir Path to the directory containing
#'   \code{vax_view.parquet}.
#' @param general_data_dir Path to the shared data directory.
#' @param state_dir State-specific root directory, used for DQA diagnostic
#'   output files.
#' @param outputs_data_dir Path to the directory where model-input tables
#'   (\code{obs}, \code{locations}, \code{obs_populations}) are written.
#'
#' @return A named list with components \code{kinder_dat}, \code{obs},
#'   \code{locations}, and \code{obs_populations}, returned invisibly.
#'
#' @importFrom dplyr filter mutate group_by ungroup if_any if_all across everything reframe slice_max arrange select bind_rows row_number case_when
#' @importFrom data.table as.data.table data.table rbindlist setorder
#' @importFrom readr write_csv
#' @importFrom stringr str_to_sentence str_trim str_detect regex
#' @export
run_final_formatting <- function(state,
                                 vaccine_type_to_keep,
                                 temp_data_dir,
                                 clean_data_dir,
                                 vaxview_dir,
                                 general_data_dir,
                                 state_dir,
                                 outputs_data_dir) {
  
  if (!requireNamespace("arrow", quietly = TRUE)) {
    stop("Package 'arrow' is required for reading Parquet files. ",
         "Install with: install.packages('arrow')")
  }
  
  # ---- Load inputs ------------------------------------------------------------
  kinder_dat <- readRDS(file.path(temp_data_dir, "kinder_vaccination_clean_03.rds"))
  
  # ---- PART 0: Vaccine type filter --------------------------------------------
  if ("vaccine_type" %in% names(kinder_dat)) {
    kinder_dat <- kinder_dat %>%
      dplyr::filter(tolower(vaccine_type) == tolower(vaccine_type_to_keep))
    message("Filtered to vaccine_type == '", vaccine_type_to_keep,
            "' (n = ", nrow(kinder_dat), ")")
  } else {
    message("Column 'vaccine_type' not found — skipping filter")
  }
  
  # ---- PART 1: Automatic DQA corrections -------------------------------------
  message("\n=== PART 1: Running data quality checks ===")
  
  dqa_report <- list()
  dqa_report$total_records_start <- nrow(kinder_dat)
  
  result <- dqa_remove_zero_enrollment(
    kinder_dat,
    dqa_report,
    temp_data_dir = temp_data_dir,
    state = state
  )
  kinder_dat <- result$data
  dqa_report <- result$report
  
  result <- dqa_fix_invalid_values(
    kinder_dat,
    dqa_report,
    temp_data_dir = temp_data_dir,
    state = state
  )
  kinder_dat <- result$data
  dqa_report <- result$report
  
  dqa_report$total_records_end     <- nrow(kinder_dat)
  dqa_report$total_records_removed <- dqa_report$total_records_start - dqa_report$total_records_end
  
  print_dqa_summary(dqa_report)
  
  # ---- Remove rows with NAs in key columns ------------------------------------
  na_rows <- dplyr::filter(kinder_dat,
                           dplyr::if_any(c(current, enrollment, med_exempt, rel_exempt), is.na))
  if (nrow(na_rows) > 0) {
    message("  Rows with NA in key columns (to be removed): ", nrow(na_rows))
  }
  kinder_dat <- dplyr::filter(kinder_dat,
                              dplyr::if_all(c(current, enrollment, med_exempt, rel_exempt),
                                            ~!is.na(.)))
  
  # ---- Remove rows with all-zero key values -----------------------------------
  kinder_dat <- kinder_dat %>%
    dplyr::filter(!dplyr::if_all(c(current, enrollment, med_exempt, rel_exempt), ~ . == 0))
  
  # ---- Remove remaining rows with zero current --------------------------------
  kinder_dat <- kinder_dat %>%
    dplyr::filter(current != 0)
  
  # ---- PART 3: Final formatting -----------------------------------------------
  message("\n=== PART 3: Final formatting ===")
  
  kinder_dat <- format_select_columns(
    kinder_dat,
    required_cols = c("school_id", "year", "school_name", "county_name",
                      "enrollment", "current", "med_exempt", "rel_exempt"),
    optional_cols = c("school_type", "school_level", "excluded_note",
                      "addr_clean", "city", "zip", "state", "business_status",
                      "lat", "lon")
  )
  
  kinder_dat <- format_fix_column_classes(kinder_dat)
  
  saveRDS(kinder_dat, file.path(clean_data_dir, "cleaned_data.rds"))
  readr::write_csv(kinder_dat, file.path(clean_data_dir, "cleaned_data.csv"))
  
  # ---- PART 4: Additional flagging DQA checks ---------------------------------
  message("\n=== PART 4: Additional data quality checks ===")
  
  dqa_check_duplicates(kinder_dat, state_dir, state)
  
  n_before <- nrow(kinder_dat)
  n_dup_occurrences <- kinder_dat %>%
    dplyr::group_by(school_id, year, school_name) %>%
    dplyr::filter(dplyr::n() > 1) %>%
    dplyr::ungroup() %>%
    nrow()
  message("  Duplicate school_id-year-school_name rows (total occurrences): ", n_dup_occurrences)
  
  kinder_dat <- kinder_dat %>%
    dplyr::group_by(school_id, year, school_name) %>%
    dplyr::slice_max(enrollment, n = 1, with_ties = FALSE) %>%
    dplyr::ungroup()
  n_dupes_removed <- n_before - nrow(kinder_dat)
  dqa_report$part4$duplicates <- n_dupes_removed
  message("  Duplicate school_id-year-school_name combos removed (kept highest enrollment): ",
          n_dupes_removed)
  
  result <- dqa_check_negatives(kinder_dat, state_dir, state)
  kinder_dat <- result$data
  dqa_report$part4$negatives <- result$n_flagged
  message("  Negative values: ", result$n_flagged, " rows flagged")
  
  result <- dqa_check_too_high(kinder_dat, state_dir, state)
  kinder_dat <- result$data
  dqa_report$part4$too_high <- result$n_flagged
  message("  Values exceeding enrollment: ", result$n_flagged, " rows flagged")
  
  result <- dqa_check_coverage_outliers(kinder_dat, state_dir, state)
  kinder_dat <- result$data
  dqa_report$part4$coverage_outliers <- result$n_flagged
  message("  Coverage outliers (<0 or >105%): ", result$n_flagged, " rows flagged")
  
  result <- dqa_check_over_coverage(kinder_dat, state_dir, state)
  kinder_dat <- result$data
  dqa_report$part4$over_coverage <- result$n_flagged
  message("  Over-coverage (current + exempt > enrollment): ", result$n_flagged, " rows flagged")
  
  kinder_dat <- kinder_dat %>%
    dplyr::mutate(current = dplyr::if_else(
      current + med_exempt + rel_exempt > enrollment,
      enrollment - med_exempt - rel_exempt,
      current
    ))
  
  result <- dqa_check_enrollment_deviation(
    df        = kinder_dat,
    state_dir = state_dir,
    state     = state,
    threshold = 1.0
  )
  kinder_dat <- result$data
  dqa_report$part4$enrollment_deviation <- result$n_flagged
  message("  Enrollment deviation >100% from school mean: ", result$n_flagged, " rows flagged")
  
  result <- dqa_check_current_deviation(
    df            = kinder_dat,
    temp_data_dir = temp_data_dir,
    state         = state,
    threshold     = 0.75
  )
  kinder_dat <- result$data
  dqa_report$part4$current_deviation <- result$n_flagged
  message("  Current vaccination deviation >75% from school mean: ", result$n_flagged, " rows flagged")
  
  result <- dqa_check_extreme_outliers(
    df            = kinder_dat,
    temp_data_dir = temp_data_dir,
    state         = state,
    threshold     = 10
  )
  kinder_dat <- result$data
  dqa_report$part4$extreme_outliers <- result$n_flagged
  message("  Extreme enrollment outliers (10x median): ", result$n_flagged, " rows flagged")
  
  message("\n========================================")
  message("PART 4 DQA SUMMARY (flagged for review, not removed)")
  message("========================================")
  p4 <- dqa_report$part4
  fmt_n <- function(x) if (is.na(x)) "skipped" else as.character(x)
  message("  Duplicate school_id-year combos:       ", fmt_n(p4$duplicates))
  message("  Negative values:                       ", fmt_n(p4$negatives))
  message("  Values exceeding enrollment:           ", fmt_n(p4$too_high))
  message("  Coverage outliers (<0 or >105%):       ", fmt_n(p4$coverage_outliers))
  message("  Over-coverage (current+exempt>enroll): ", fmt_n(p4$over_coverage))
  message("  Enrollment deviation >100% from mean:  ", fmt_n(p4$enrollment_deviation))
  message("  Vaccination deviation >75% from mean:  ", fmt_n(p4$current_deviation))
  message("  Extreme enrollment outliers (10x):     ", fmt_n(p4$extreme_outliers))
  message("========================================")
  message("Check CSVs in ", temp_data_dir, " for detailed results.")
  
  saveRDS(dqa_report, file.path(temp_data_dir, "dqa_report.rds"))
  
  # ---- Build observation tables for modelling pipeline ------------------------
  
  df_na_id <- dplyr::filter(kinder_dat, is.na(school_id))
  message("Schools with NA school_id: ", nrow(df_na_id))
  kinder_dat <- dplyr::filter(kinder_dat, !is.na(school_id))
  
  obs <- data.table::as.data.table(kinder_dat)[, .(
    row_id      = .I,
    positive    = current,
    sample_n    = enrollment,
    school_name = school_name
  )]
  
  kinder_dat <- data.table::as.data.table(kinder_dat)
  kinder_dat[, county_name := stringr::str_to_sentence(stringr::str_trim(county_name))]
  kinder_dat[, row_id := .I]
  
  state_row <- data.table::data.table(id = 1L, parent_id = NA_integer_)
  
  county_locs <- data.table::data.table(
    county_name = sort(unique(kinder_dat$county_name))
  )
  county_locs[, id        := .I + 1L]
  county_locs[, parent_id := 1L]
  
  school_locs <- unique(kinder_dat[!is.na(school_id), .(school_id, county_name)])
  school_locs <- merge(school_locs,
                       county_locs[, .(county_name, county_loc_id = id)],
                       by = "county_name", all.x = TRUE)
  data.table::setorder(school_locs, county_loc_id, school_id)
  school_locs[, id        := .I + max(county_locs$id)]
  school_locs[, parent_id := county_loc_id]
  
  locations <- data.table::rbindlist(list(
    state_row,
    county_locs[, .(id, parent_id)],
    school_locs[, .(id, parent_id)]
  ))
  
  kinder_dat <- merge(
    kinder_dat,
    county_locs[, .(county_name, county_loc_id = id)],
    by = "county_name", all.x = TRUE
  )
  kinder_dat <- merge(
    kinder_dat,
    school_locs[, .(school_id, school_loc_id = id)],
    by = "school_id", all.x = TRUE
  )
  
  obs_populations <- kinder_dat[, list(
    obs_id   = row_id,
    location = school_loc_id,
    cohort   = year,
    age      = 5L,
    dose     = 2L,
    weight   = 1L
  )]
  
  readr::write_csv(kinder_dat,
                   file.path(temp_data_dir, "kinder_vaccination_clean.csv"))
  saveRDS(kinder_dat, file.path(temp_data_dir, "kinder_vaccination_clean.rds"))
  

  
  # ---- PART 7: Incorporate VaxView observations -------------------------------
  vaxview <- arrow::read_parquet(file.path(vaxview_dir, "vax_view.parquet"))
  
  vaxview <- vaxview %>%
    dplyr::filter(stringr::str_detect(vaccine, stringr::regex("mmr", ignore_case = TRUE))) %>%
    dplyr::filter(!stringr::str_detect(vaccine, stringr::regex("PAC", ignore_case = TRUE))) %>%
    dplyr::filter(!age_range %in% c("13 Months", "19 Months")) %>%
    dplyr::filter(!is.na(n), !is.na(p))
  
  # ---- Expand rows: child = 2 rows per year, teen = 6 rows per year ----
  row_id_start <- if ("row_id" %in% names(obs) &&
                      nrow(obs) > 0 &&
                      any(!is.na(obs$row_id))) {
    max(obs$row_id, na.rm = TRUE)
  } else {
    0L
  }
  
  vaxview_child <- vaxview %>%
    dplyr::filter(pop == "child") %>%
    dplyr::mutate(row_id = row_id_start + dplyr::row_number()) %>%
    dplyr::group_by(dplyr::across(dplyr::everything())) %>%
    dplyr::reframe(row_num = 1:2)
  
  n_child <- nrow(vaxview %>% dplyr::filter(pop == "child"))
  
  vaxview_teen <- vaxview %>%
    dplyr::filter(pop == "teen") %>%
    dplyr::mutate(row_id = row_id_start + n_child + dplyr::row_number()) %>%
    dplyr::group_by(dplyr::across(dplyr::everything())) %>%
    dplyr::reframe(row_num = 1:6)
  
  n_teen <- nrow(vaxview %>% dplyr::filter(pop == "teen"))
  
  vaxview_school <- vaxview %>%
    dplyr::filter(pop == "school") %>%
    dplyr::mutate(
      row_id  = row_id_start + n_child + n_teen + dplyr::row_number(),
      row_num = 1L
    )
  
  vaxview_expanded <- dplyr::bind_rows(vaxview_child, vaxview_teen, vaxview_school)
  
  vaxview_expanded <- vaxview_expanded %>%
    dplyr::mutate(cohort = dplyr::case_when(
      pop == "child" & row_num == 1 ~ year,
      pop == "child" & row_num == 2 ~ year - 1,
      pop == "teen"  & row_num == 1 ~ year - 19,
      pop == "teen"  & row_num == 2 ~ year - 18,
      pop == "teen"  & row_num == 3 ~ year - 17,
      pop == "teen"  & row_num == 4 ~ year - 16,
      pop == "teen"  & row_num == 5 ~ year - 15,
      pop == "teen"  & row_num == 6 ~ year - 14,
      TRUE ~ as.numeric(year)
    ))
  
  vaxview_expanded <- vaxview_expanded %>%
    dplyr::mutate(life_year = dplyr::case_when(
      age_range == "24 Months" ~ 2L,
      age_range == "35 Months" ~ 3L,
      pop == "school"          ~ 5L,
      pop == "teen"            ~ as.integer(year - cohort - 1),
      TRUE                     ~ NA_integer_
    ))
  
  vaxview_expanded <- vaxview_expanded %>%
    dplyr::mutate(
      wts = dplyr::case_when(
        pop == "school"                  ~ 1,
        pop == "child"  & row_num == 1  ~ 0.67,
        pop == "child"  & row_num == 2  ~ 0.33,
        pop == "teen"   & row_num == 1  ~ 0.052941176,
        pop == "teen"   & row_num == 2  ~ 0.196078431,
        pop == "teen"   & row_num == 3  ~ 0.196078431,
        pop == "teen"   & row_num == 4  ~ 0.196078431,
        pop == "teen"   & row_num == 5  ~ 0.196078431,
        pop == "teen"   & row_num == 6  ~ 0.162745098
      ),
      dose = dplyr::case_when(
        pop == "child"               ~ 1L,
        pop %in% c("school", "teen") ~ 2L
      )
    )
  
  # ---- Clean up columns (row_id already assigned above) ----
  vaxview_obs <- vaxview_expanded %>%
    dplyr::select(row_id, pop, year, cohort, life_year, n, p, x, sd, se, wts, dose) %>%
    dplyr::mutate(positive = x, sample_n = n)
  
  # ---- Append to obs (one row per obs_id) ----
  obs <- data.table::rbindlist(list(
    obs,
    data.table::as.data.table(
      vaxview_obs %>%
        dplyr::group_by(row_id) %>%
        dplyr::slice(1) %>%
        dplyr::ungroup() %>%
        dplyr::select(row_id, positive, sample_n)
    )
  ), fill = TRUE) %>%
    dplyr::select(row_id, positive, sample_n)
  
  # ---- Build vaxview obs_populations (multiple rows per obs_id, weights sum to 1) ----
  vaxview_obs_pop <- vaxview_obs %>%
    dplyr::mutate(location = 1L) %>%
    dplyr::select(
      obs_id   = row_id,
      location,
      cohort,
      age      = life_year,
      dose,
      weight   = wts
    )
  
  # ---- Append to obs_populations ----
  obs_populations <- data.table::rbindlist(list(
    obs_populations,
    data.table::as.data.table(vaxview_obs_pop)
  ), fill = TRUE)

  # ---- PART 8: BUILD LINKAGE KEY ---------------------------------------------
  # Links kinder_dat rows to their obs, obs_populations, and location IDs
  
  message("\n=== PART 8: Building linkage key ===")
  
  # ---- School-level key (one row per kinder_dat row) ----
  school_key <- kinder_dat[, .(
    # original identifiers
    school_id,
    school_name,
    county_name,
    year,
    # location ids
    county_loc_id,
    school_loc_id,
    # obs id (row_id = obs$row_id for kinder rows)
    obs_id = row_id
  )] 
  
  # Join obs_populations id back in (obs_id is the link)
  school_key <- merge(
    school_key,
    obs_populations[, .(obs_id, location, cohort, age, dose, weight)],
    by = "obs_id",
    all.x = TRUE
  )
  
  # ---- Vaxview key (one row per expanded vaxview obs_populations row) ----
  vaxview_key <- vaxview_obs_pop %>%
    mutate(
      school_id     = NA_character_,
      school_name   = NA_character_,
      county_name   = NA_character_,
      county_loc_id = NA_integer_,
      school_loc_id = NA_integer_
    ) %>%
    rename(obs_id = obs_id)
  
  # ---- Combine into full linkage key ----
  linkage_key <- data.table::rbindlist(list(
    school_key,
    data.table::as.data.table(vaxview_key)
  ), fill = TRUE)
  
  # ---- Save ----
  saveRDS(linkage_key, file.path(outputs_data_dir, "linkage_key.rds"))
  readr::write_csv(linkage_key, file.path(outputs_data_dir, "linkage_key.csv"))
  
  message("Linkage key saved: ", nrow(linkage_key), " rows")
  message("  School rows: ", sum(!is.na(linkage_key$school_id)))
  message("  VaxView rows: ", sum(is.na(linkage_key$school_id)))
  
  
  # ---- PART 9: Export --------------------------------------------------------  
  # ----- Final Formating to match model input specs ----
  obs <- obs %>%
    rename(obs_id = row_id) 
  obs_populations <- obs_populations %>%
    rename(loc_id = location)
  locations <- locations %>%
    rename(loc_id = id) 
  
  # ----- Export ----
  saveRDS(obs, file.path(outputs_data_dir, "obs.rds"))
  saveRDS(locations, file.path(outputs_data_dir, "locations.rds"))
  saveRDS(obs_populations, file.path(outputs_data_dir, "obs_populations.rds"))
  readr::write_csv(obs, file.path(outputs_data_dir, "obs.csv"))
  readr::write_csv(locations, file.path(outputs_data_dir, "locations.csv"))
  readr::write_csv(obs_populations, file.path(outputs_data_dir, "obs_populations.csv"))
  
  invisible(list(
    kinder_dat       = kinder_dat,
    obs              = obs,
    locations        = locations,
    obs_populations  = obs_populations
  ))
}