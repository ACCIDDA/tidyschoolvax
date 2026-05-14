#' Generate DQA Summary Report
#'
#' @description
#' Creates a comprehensive summary report of data quality assurance checks,
#' consolidating flagged issues from all checks into a standardized format.
#'
#' @param dqa_results A list object returned by run_dqa_checks() containing
#'   summary and results components
#' @param output_path Full file path for the output report. 
#'   Supported formats: CSV (.csv), RDS (.rds), or Excel (.xlsx)
#' @param state Optional state abbreviation for report header (e.g., "ca", "md")
#' @param include_timestamp Logical, whether to include timestamp in report (default: TRUE)
#'
#' @return Invisibly returns the summary data frame that was saved
#'
#' @details
#' The function generates a report containing:
#' \itemize{
#'   \item Check name
#'   \item Number of issues found
#'   \item Timestamp (optional)
#'   \item State identifier (optional)
#' }
#'
#' The report can be saved in multiple formats:
#' \itemize{
#'   \item CSV: Comma-separated values file
#'   \item RDS: R data file (preserves R data types)
#'   \item XLSX: Excel spreadsheet (requires writexl package)
#' }
#'
#' @examples
#' \dontrun{
#' # Run DQA checks
#' dqa_results <- run_dqa_checks(state_data)
#' 
#' # Generate CSV report
#' generate_dqa_summary(dqa_results, "output/dqa_summary.csv", state = "ca")
#' 
#' # Generate RDS report
#' generate_dqa_summary(dqa_results, "output/dqa_summary.rds", state = "md")
#' 
#' # Generate Excel report (requires writexl)
#' generate_dqa_summary(dqa_results, "output/dqa_summary.xlsx", state = "ca")
#' }
#'
#' @importFrom tools file_ext
#' @export
generate_dqa_summary <- function(dqa_results,
                                output_path,
                                state = NULL,
                                include_timestamp = TRUE) {
  
  # Validate input
  if (!is.list(dqa_results) || !"summary" %in% names(dqa_results)) {
    stop("dqa_results must be a list with a 'summary' component (from run_dqa_checks)")
  }
  
  # Extract summary data
  summary_df <- dqa_results$summary
  
  # Add metadata columns
  if (!is.null(state)) {
    summary_df$state <- state
  }
  
  if (include_timestamp) {
    summary_df$timestamp <- Sys.time()
  }
  
  # Reorder columns to put metadata first
  metadata_cols <- c("state", "timestamp")[c(!is.null(state), include_timestamp)]
  data_cols <- setdiff(names(summary_df), metadata_cols)
  summary_df <- summary_df[, c(metadata_cols, data_cols), drop = FALSE]
  
  # Determine output format from file extension
  file_ext <- tolower(tools::file_ext(output_path))
  
  # Create output directory if it doesn't exist
  output_dir <- dirname(output_path)
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
    message("Created output directory: ", output_dir)
  }
  
  # Save based on format
  if (file_ext == "csv") {
    write.csv(summary_df, output_path, row.names = FALSE)
    message("DQA summary report saved to: ", output_path)
  } else if (file_ext == "rds") {
    saveRDS(summary_df, output_path)
    message("DQA summary report saved to: ", output_path)
  } else if (file_ext == "xlsx") {
    if (!requireNamespace("writexl", quietly = TRUE)) {
      stop("Package 'writexl' is required for Excel output. Install with: install.packages('writexl')")
    }
    writexl::write_xlsx(summary_df, output_path)
    message("DQA summary report saved to: ", output_path)
  } else {
    stop("Unsupported file format. Use .csv, .rds, or .xlsx")
  }
  
  # Print summary to console
  message("\n========================================")
  message("DQA Summary Report")
  if (!is.null(state)) {
    message("State: ", toupper(state))
  }
  message("========================================")
  print(summary_df)
  
  # Return invisibly
  invisible(summary_df)
}


#' Generate Detailed DQA Report
#'
#' @description
#' Creates a detailed report with all flagged records from each DQA check,
#' optionally combining them into a single consolidated dataset.
#'
#' @param dqa_results A list object returned by run_dqa_checks() containing
#'   summary and results components
#' @param output_dir Directory path where detailed reports will be saved
#' @param state Optional state abbreviation for file naming (e.g., "ca", "md")
#' @param consolidate Logical, whether to create a single consolidated report
#'   with all issues (default: FALSE)
#' @param format Output format: "csv", "rds", or "xlsx" (default: "csv")
#'
#' @return Invisibly returns a list of data frames containing flagged records
#'
#' @details
#' This function saves detailed results from each DQA check to separate files.
#' If consolidate = TRUE, it also creates a master file containing all flagged
#' records with a column indicating which check flagged each record.
#'
#' @examples
#' \dontrun{
#' # Run DQA checks
#' dqa_results <- run_dqa_checks(state_data)
#' 
#' # Save detailed reports
#' generate_detailed_dqa_report(dqa_results, 
#'                               output_dir = "output/dqa_details",
#'                               state = "ca",
#'                               consolidate = TRUE)
#' }
#'
#' @export
generate_detailed_dqa_report <- function(dqa_results,
                                        output_dir,
                                        state = NULL,
                                        consolidate = FALSE,
                                        format = "csv") {
  
  # Validate input
  if (!is.list(dqa_results) || !"results" %in% names(dqa_results)) {
    stop("dqa_results must be a list with a 'results' component (from run_dqa_checks)")
  }
  
  # Create output directory
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
    message("Created output directory: ", output_dir)
  }
  
  # Helper function to save a data frame
  save_dataframe <- function(df, check_name, subname = NULL) {
    if (!is.data.frame(df) || nrow(df) == 0) {
      return(NULL)
    }
    
    # Construct filename
    file_base <- if (!is.null(state)) {
      paste0(state, "_", check_name)
    } else {
      check_name
    }
    
    if (!is.null(subname)) {
      file_base <- paste0(file_base, "_", subname)
    }
    
    filename <- file.path(output_dir, paste0(file_base, ".", format))
    
    # Save based on format
    if (format == "csv") {
      write.csv(df, filename, row.names = FALSE)
    } else if (format == "rds") {
      saveRDS(df, filename)
    } else if (format == "xlsx") {
      if (!requireNamespace("writexl", quietly = TRUE)) {
        stop("Package 'writexl' is required for Excel output.")
      }
      writexl::write_xlsx(df, filename)
    }
    
    message("Saved: ", filename, " (", nrow(df), " rows)")
    return(df)
  }
  
  # Process each check result
  results_list <- list()
  all_flagged <- list()
  
  # Checks that return {flagged, school_history} lists - school_history is saved
  # separately for context but excluded from the consolidated flagged issues report
  deviation_checks <- c("enrollment_deviation", "vaccination_deviation")
  
  for (check_name in names(dqa_results$results)) {
    check_result <- dqa_results$results[[check_name]]
    
    if (is.list(check_result) && !is.data.frame(check_result)) {
      if (check_name %in% deviation_checks && 
          all(c("flagged", "school_history") %in% names(check_result))) {
        # Deviation checks: save flagged rows and full school history separately
        saved_flagged <- save_dataframe(check_result$flagged, check_name, "flagged")
        save_dataframe(check_result$school_history, check_name, "school_history")
        
        if (!is.null(saved_flagged) && consolidate) {
          saved_flagged$dqa_check <- check_name
          all_flagged[[check_name]] <- saved_flagged
        }
      } else if (check_name %in% deviation_checks) {
        warning("Expected list with 'flagged' and 'school_history' for check '",
                check_name, "', but got unexpected structure. Falling back to nested list handling.")
        for (subname in names(check_result)) {
          df <- check_result[[subname]]
          saved_df <- save_dataframe(df, check_name, subname)
          
          if (!is.null(saved_df) && consolidate) {
            saved_df$dqa_check <- paste0(check_name, "_", subname)
            all_flagged[[paste0(check_name, "_", subname)]] <- saved_df
          }
        }
      } else {
        # Handle other nested lists (e.g., duplicates, coverage_outliers)
        for (subname in names(check_result)) {
          df <- check_result[[subname]]
          saved_df <- save_dataframe(df, check_name, subname)
          
          if (!is.null(saved_df) && consolidate) {
            saved_df$dqa_check <- paste0(check_name, "_", subname)
            all_flagged[[paste0(check_name, "_", subname)]] <- saved_df
          }
        }
      }
    } else if (is.data.frame(check_result)) {
      # Handle data frame results
      saved_df <- save_dataframe(check_result, check_name)
      
      if (!is.null(saved_df) && consolidate) {
        saved_df$dqa_check <- check_name
        all_flagged[[check_name]] <- saved_df
      }
    }
    
    results_list[[check_name]] <- check_result
  }
  
  # Create consolidated report if requested
  if (consolidate && length(all_flagged) > 0) {
    # Find common columns across all data frames
    all_cols <- unique(unlist(lapply(all_flagged, names)))
    
    # Standardize all data frames to have the same columns
    all_flagged_std <- lapply(all_flagged, function(df) {
      missing_cols <- setdiff(all_cols, names(df))
      for (col in missing_cols) {
        df[[col]] <- NA
      }
      df[, all_cols]
    })
    
    # Combine all flagged records
    consolidated <- do.call(rbind, all_flagged_std)
    
    # Save consolidated report
    file_base <- if (!is.null(state)) {
      paste0(state, "_all_dqa_issues")
    } else {
      "all_dqa_issues"
    }
    
    filename <- file.path(output_dir, paste0(file_base, ".", format))
    
    if (format == "csv") {
      write.csv(consolidated, filename, row.names = FALSE)
    } else if (format == "rds") {
      saveRDS(consolidated, filename)
    } else if (format == "xlsx") {
      writexl::write_xlsx(consolidated, filename)
    }
    
    message("\nConsolidated report saved: ", filename, " (", nrow(consolidated), " total issues)")
  }
  
  message("\nDetailed DQA reports generation complete.")
  
  # Return invisibly
  invisible(results_list)
}
