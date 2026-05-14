#' Clean State-Specific Data
#'
#' @description
#' Formats raw state-specific data to meet standardized requirements for 
#' further processing. Ensures required columns exist, validates column data 
#' types, and handles missing or optional column cases.
#'
#' @param state_mmr A data frame containing raw state-specific MMR vaccination data
#' @param required_cols Character vector of required column names. 
#'   Default: c("year", "school_id", "school_name", "cnty_id", "school_county",
#'              "enrollment", "current", "delayed", "med_exempt", "rel_exempt")
#' @param optional_cols Character vector of optional column names. 
#'   Default: c("school_type")
#' @param expected_classes Named list specifying expected data types for each column.
#'   Default includes standard types for vaccination data columns.
#'
#' @return A cleaned data frame with standardized columns and data types
#'
#' @details
#' The function performs the following operations:
#' \itemize{
#'   \item Checks for presence of required columns
#'   \item Removes extra columns not in required or optional lists
#'   \item Validates and coerces column data types to expected classes
#'   \item Filters out rows with zero or missing enrollment
#'   \item Filters out rows with zero current vaccination count
#' }
#'
#' @examples
#' \dontrun{
#' cleaned_data <- clean_state_data(raw_state_data)
#' }
#'
#' @export
clean_state_data <- function(state_mmr,
                             required_cols = c("year", "school_id", "school_name", 
                                              "cnty_id", "school_county",
                                              "enrollment", "current", "delayed", 
                                              "med_exempt", "rel_exempt"),
                             optional_cols = c("school_type"),
                             expected_classes = list(
                               year = "integer",
                               enrollment = "integer",
                               current = "integer",
                               med_exempt = "integer",
                               rel_exempt = "integer",
                               delayed = "integer",
                               school_name = "character",
                               school_id = "integer",
                               school_county = "character",
                               cnty_id = "integer",
                               school_type = "character"
                             )) {
  
  # Check which required columns are missing
  missing_cols <- setdiff(required_cols, colnames(state_mmr))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  } else {
    message("All required columns are present.")
  }
  
  # Keep only required + optional columns that exist
  keep_cols <- c(required_cols, intersect(optional_cols, colnames(state_mmr)))
  state_mmr <- state_mmr[, keep_cols, drop = FALSE]
  
  message("Columns in dataset: ", paste(colnames(state_mmr), collapse = ", "))
  
  # Check which expected columns exist
  cols_present <- intersect(names(expected_classes), colnames(state_mmr))
  cols_missing <- setdiff(names(expected_classes), colnames(state_mmr))
  
  if (length(cols_missing) > 0) {
    message("Optional columns not present: ", paste(cols_missing, collapse = ", "))
  }
  
  # Check classes for present columns
  for (col in cols_present) {
    actual_class <- class(state_mmr[[col]])[1]
    expected_class <- expected_classes[[col]]
    if (actual_class != expected_class) {
      message(
        "Converting column '", col, "' from '", actual_class, 
        "' to '", expected_class, "'"
      )
    }
  }
  
  # Fix classes for present columns
  for (col in cols_present) {
    expected_class <- expected_classes[[col]]
    if (expected_class == "integer") {
      state_mmr[[col]] <- as.integer(state_mmr[[col]])
    } else if (expected_class == "numeric") {
      state_mmr[[col]] <- as.numeric(state_mmr[[col]])
    } else if (expected_class == "character") {
      state_mmr[[col]] <- as.character(state_mmr[[col]])
    }
  }
  
  # Verify conversion
  for (col in cols_present) {
    actual_class <- class(state_mmr[[col]])[1]
    expected_class <- expected_classes[[col]]
    if (actual_class != expected_class) {
      warning(
        "Column '", col, "' is class '", actual_class, 
        "' but expected '", expected_class, "'"
      )
    }
  }
  
  # Remove rows with zero or missing enrollment
  before_n <- nrow(state_mmr)
  state_mmr <- state_mmr[!is.na(state_mmr$enrollment) & state_mmr$enrollment > 0, ]
  after_n <- nrow(state_mmr)
  removed_n <- before_n - after_n
  
  if (removed_n > 0) {
    message("Removed ", removed_n, " rows with zero or missing enrollment. Remaining rows: ", after_n)
  }
  
  # Remove rows where current = 0
  before_n <- nrow(state_mmr)
  state_mmr <- state_mmr[state_mmr$current != 0 & !is.na(state_mmr$current), ]
  after_n <- nrow(state_mmr)
  removed_n <- before_n - after_n
  
  if (removed_n > 0) {
    message("Removed ", removed_n, " rows with zero current vaccination. Remaining rows: ", after_n)
  }
  
  return(state_mmr)
}
