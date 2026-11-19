# Reading and writing the tables the pipeline passes around.
#
# Policy: intermediate tables (anything a later step reads) are Parquet, which
# keeps column types and is small; CSV is for tables a person opens (review
# files, the final cleaned data); RDS is kept only for caches that are R lists.

#' Write a table in one of the pipeline's formats
#'
#' Writes \code{x} to \code{<stem>.<ext>} and returns that path invisibly. One
#' file is written, never a CSV/RDS pair.
#'
#' @param x A data frame.
#' @param stem File path without extension, e.g.
#'   \code{file.path(temp_data_dir, "kinder_vaccination_clean_02")}.
#' @param format \code{"parquet"} (default), \code{"csv"} or \code{"rds"}.
#'
#' @return The path written, invisibly.
#' @seealso \code{\link{read_pipeline_table}}
#' @export
write_pipeline_table <- function(x, stem, format = c("parquet", "csv", "rds")) {
  format <- match.arg(format)
  path <- paste0(stem, ".", format)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  switch(
    format,
    parquet = {
      if (!requireNamespace("arrow", quietly = TRUE)) {
        stop("Package 'arrow' is required to write Parquet files. ",
             "Install with: install.packages('arrow')", call. = FALSE)
      }
      # zstd is about 25% smaller than arrow's default (snappy) on these tables
      codec <- if (isTRUE(arrow::codec_is_available("zstd"))) "zstd" else "snappy"
      arrow::write_parquet(as.data.frame(x), path, compression = codec)
    },
    csv = readr::write_csv(x, path),
    rds = saveRDS(x, path)
  )
  invisible(path)
}

#' Read a table written by the pipeline
#'
#' Looks for \code{<stem>.parquet}, then \code{<stem>.rds} (or \code{.RDS}), and
#' reads the first one found, so runs made before the switch to Parquet still
#' work. A \code{.csv} is only read when \code{allow_csv = TRUE}, because a CSV
#' round trip changes column types (integer ids become doubles, and ZIP codes
#' lose their leading zeros).
#'
#' @param stem File path without extension.
#' @param allow_csv Also accept \code{<stem>.csv} as a last resort.
#'
#' @return A data frame (a tibble when read from CSV).
#' @seealso \code{\link{write_pipeline_table}}, \code{\link{read_cleaned_data}}
#' @export
read_pipeline_table <- function(stem, allow_csv = FALSE) {
  parquet <- paste0(stem, ".parquet")
  if (file.exists(parquet)) {
    if (!requireNamespace("arrow", quietly = TRUE)) {
      stop("Package 'arrow' is required to read Parquet files. ",
           "Install with: install.packages('arrow')", call. = FALSE)
    }
    return(as.data.frame(arrow::read_parquet(parquet)))
  }
  for (ext in c(".rds", ".RDS")) {
    p <- paste0(stem, ext)
    if (file.exists(p)) return(readRDS(p))
  }
  if (isTRUE(allow_csv) && file.exists(paste0(stem, ".csv"))) {
    return(readr::read_csv(paste0(stem, ".csv"), show_col_types = FALSE))
  }
  stop("No .parquet or .rds file found for: ", stem, call. = FALSE)
}

#' Read a cleaned-data CSV with the right column types
#'
#' \code{cleaned_data.csv} is the final, human-readable output. Reading it with
#' default guessing turns \code{zip} into a number (losing leading zeros) and
#' the integer id and count columns into doubles. This reads it with the types
#' \code{run_final_formatting()} writes: identifiers, names and ZIP codes as
#' text or integers, counts as integers, coordinates as doubles.
#'
#' @param path Path to \code{cleaned_data.csv} (or the stacked
#'   \code{cleaned_data_all_types.csv}).
#'
#' @return A tibble.
#' @export
read_cleaned_data <- function(path) {
  known <- list(
    school_id = readr::col_integer(), year = readr::col_integer(),
    enrollment = readr::col_integer(), current = readr::col_integer(),
    med_exempt = readr::col_integer(), rel_exempt = readr::col_integer(),
    total_exempt = readr::col_integer(), excluded_note = readr::col_character(),
    zip = readr::col_character(), lat = readr::col_double(), lon = readr::col_double()
  )
  # only name columns that are in the file, so tables without e.g. `zip` (the
  # excluded-rows and years tables) read without a warning
  header <- names(readr::read_csv(path, n_max = 0, show_col_types = FALSE))
  spec <- do.call(readr::cols, c(known[intersect(names(known), header)],
                                 list(.default = readr::col_guess())))
  readr::read_csv(path, col_types = spec)
}
