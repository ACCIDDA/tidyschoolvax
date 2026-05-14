
#' Build a standardized filename for pipeline output files
#'
#' Constructs a filename using an explicit \code{state} value together with a
#' user-supplied prefix and extension.
#'
#' @param state Character string to use as the leading segment of the filename.
#' @param prefix Character string to use as the middle segment of the filename.
#' @param extension File extension without a leading dot (e.g., \code{"csv"}).
#'
#' @return A character string of the form \code{"<state>_<prefix>.<extension>"}.
#' @keywords internal
make_filename <- function(state, prefix, extension) {
  paste0(state, "_", prefix, ".", extension)
}



#' Check that expected output files exist
#'
#' Stops with an informative error if any of the given file paths do not exist.
#' Used at the end of each pipeline step to confirm expected outputs were created.
#'
#' @param paths Character vector of file paths to check.
#' @param step_name Optional character string naming the pipeline step, used in
#'   the error message.
#'
#' @return Invisibly returns \code{TRUE} when all files are present.
#' @export
check_expected_files <- function(paths, step_name = NULL) {
  missing <- paths[!file.exists(paths)]

  if (length(missing) > 0) {
    msg <- if (is.null(step_name)) {
      paste(
        "Missing expected output files:",
        paste(missing, collapse = "\n")
      )
    } else {
      paste(
        paste0("Missing expected output files for ", step_name, ":"),
        paste(missing, collapse = "\n")
      )
    }

    stop(msg, call. = FALSE)
  }

  message("All expected output files are present.")
  invisible(TRUE)
}



#' Create a directory if it does not already exist
#'
#' Thin wrapper around \code{dir.create} that silently does nothing when the
#' directory already exists.
#'
#' @param path Character string giving the path to create.
#'
#' @return \code{NULL} invisibly.
#' @keywords internal
create_dir <- function(path) {
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE)
  }

  invisible(NULL)
}


#' Download a list of Excel files to a local directory
#'
#' Iterates over a character vector of URLs, derives the local file name from
#' the URL basename, and downloads each file in binary mode.  Failed downloads
#' are reported via \code{message} rather than stopping the loop.
#'
#' @param links Character vector of URLs pointing to Excel (.xlsx) files.
#' @param save_dir Path to the local directory where files should be saved.
#'   The directory must already exist.
#'
#' @return \code{NULL} invisibly (called for its side-effect of saving files).
#'
#' @importFrom purrr walk
#' @export
download_xlsx_files <- function(links, save_dir) {
  
  dir.create(save_dir, recursive = TRUE, showWarnings = FALSE)
  
  purrr::walk(links, function(link) {
    file_name <- basename(link)
    dest_path <- file.path(save_dir, file_name)
    message("Downloading: ", file_name)
    tryCatch(
      download.file(link, destfile = dest_path, mode = "wb"),
      error = function(e) message("Failed to download ", link, ": ", e$message)
    )
  })
}



#' Read the second (or first) sheet of an Excel file
#'
#' Reads the second sheet of an Excel workbook if one is available, otherwise
#' reads the first sheet.  Column names are cleaned with
#' \code{janitor::clean_names} via \code{\link{read_excel_dynamic}}.
#'
#' @param f Path to an Excel file.
#'
#' @return A tibble with clean column names.
#' @export
read_file <- function(f) {
  sheets <- readxl::excel_sheets(f)
  sheet_to_use <- if (length(sheets) >= 2) 2 else 1
  read_excel_dynamic(f, sheet_to_use)
}


#' Coerce specified columns in a data frame to numeric
#'
#' Silently converts the named columns to \code{numeric} via \code{as.numeric}.
#' Columns not present in the data frame are skipped.
#'
#' @param df A data frame.
#' @param cols Character vector of column names to convert.
#'
#' @return The data frame with the specified columns coerced to numeric.
#' @export
coerce_columns_to_numeric <- function(df, cols) {
  for (col in cols) {
    if (col %in% names(df)) {
      df[[col]] <- as.numeric(df[[col]])
    }
  }
  return(df)
}


#' Extract a year-range label from a vaccination data filename
#'
#' Looks for a \code{"YYYY-YYYY"} pattern in the filename and returns a
#' two-digit year-range label prefixed by the state abbreviation (e.g.,
#' \code{"md_15_16"}).  If no year pattern is found, the file base name
#' (without extension) is used as the year token.
#'
#' @param filename Character string; the full path or basename of the file.
#' @param state Two-letter state abbreviation used as a prefix (default
#'   \code{"md"}).
#'
#' @return A character string of the form \code{"<state>_<y1>_<y2>"} or
#'   \code{"<state>_unknown_<stem>"} when no year is found.
#' @export
extract_year <- function(filename, state = "md") {
  years <- stringr::str_extract(basename(filename), "20[0-9]{2}-20[0-9]{2}")
  if (!is.na(years)) {
    y1 <- stringr::str_sub(years, 3, 4)
    y2 <- stringr::str_sub(years, 8, 9)
    paste0(state, "_", y1, "_", y2)
  } else {
    paste0(state, "_unknown_", tools::file_path_sans_ext(basename(filename)))
  }
}


#' Read an Excel sheet with a dynamic header row
#'
#' Attempts to locate a header row by searching the first column for a cell
#' matching \code{header_keyword}.  If found, data are read starting at that
#' row so the matched cell becomes the first column header.  If not found, the
#' sheet is read from the top with the first row used as headers.  Column
#' names are cleaned with \code{janitor::clean_names}.
#'
#' @param file Path to the Excel file.
#' @param sheet Sheet number or name to read (default \code{1}).
#' @param header_keyword Character string to search for in the first column
#'   when locating the header row (default \code{"School Name"}).
#'
#' @return A tibble with clean column names.
#'
#' @importFrom readxl read_excel
#' @importFrom janitor clean_names
#' @export
read_excel_dynamic <- function(file, sheet = 1, header_keyword = "School Name") {
  raw <- readxl::read_excel(file, sheet = sheet, col_names = FALSE)
  header_row <- which(raw[[1]] == header_keyword)[1]

  if (is.na(header_row)) {
    message("No '", header_keyword, "' header found in ", basename(file), " - reading from top.")
    df <- readxl::read_excel(file, sheet = sheet, col_names = TRUE)
  } else {
    df <- readxl::read_excel(file, sheet = sheet, skip = header_row - 1)
  }

  janitor::clean_names(df)
}


#' Replace letter O with zero in numeric-looking columns
#'
#' Corrects OCR-induced substitution of the letter \code{"O"} (upper or lower
#' case) for the digit \code{"0"} in columns whose names start with
#' \code{"total_"} or \code{"percent_"}.
#'
#' @param df A data frame, typically raw vaccination counts read from a scanned
#'   source.
#'
#' @return A data frame with the same structure as \code{df}, with \code{"O"}
#'   and \code{"o"} replaced by \code{"0"} in the target columns.
#'
#' @export
fix_o0_problem <- function(df) {
  df %>%
    dplyr::mutate(
      dplyr::across(
        tidyselect::starts_with(c("total_", "percent_")),
        ~ stringr::str_replace_all(.x, "o|O", "0")
      )
    )
}


#' Handle redacted cells and coerce column types
#'
#' Marks rows containing the redaction placeholder \code{"**"} in any column
#' by recording them in a new \code{excluded_note} column, then replaces all
#' \code{"**"} values with \code{NA}.  Also treats \code{"<10"} in
#' \code{total_k_students} as \code{NA}, and coerces \code{percent_*}
#' columns to \code{numeric} and \code{total_*} columns to \code{integer}.
#'
#' @param df A data frame of raw kindergarten vaccination data, typically after
#'   reading from an Excel or CSV source.
#'
#' @return A data frame with an added \code{excluded_note} column, redacted
#'   values replaced by \code{NA}, and column types enforced.
#'
#' @export
clean_redactions <- function(df) {
  df %>%
    dplyr::mutate(
      excluded_note = ifelse(
        dplyr::if_any(tidyselect::everything(), ~ .x == "**"),
        "redacted, enr <10",
        NA
      )
    ) %>%
    dplyr::mutate(
      dplyr::across(tidyselect::everything(), ~ ifelse(.x == "**", NA, .x))
    ) %>%
    dplyr::mutate(
      total_k_students = ifelse(total_k_students == "<10", NA, total_k_students)
    ) %>%
    dplyr::mutate(
      dplyr::across(tidyselect::starts_with("percent_"), as.numeric),
      dplyr::across(tidyselect::starts_with("total_"), as.integer)
    )
}



#' Remove rows that were flagged as redacted
#'
#' Filters out rows where \code{excluded_note} is non-\code{NA}, i.e., rows
#' previously identified by \code{\link{clean_redactions}} as having a
#' redaction placeholder.
#'
#' @param df A data frame with an \code{excluded_note} column (typically
#'   produced by \code{\link{clean_redactions}}).
#'
#' @return A data frame containing only rows where \code{excluded_note} is
#'   \code{NA}.
#'
#' @export
remove_redacted_rows <- function(df) {
  df %>%
    dplyr::filter(is.na(excluded_note))
}



#' Convert exemption percentage columns to numeric
#'
#' Coerces \code{percent_medical_exemption} and
#' \code{percent_religious_exemption} to \code{numeric}.  Useful after reading
#' from Excel where these columns may have been read as character.
#'
#' @param df A data frame containing \code{percent_medical_exemption} and/or
#'   \code{percent_religious_exemption} columns.
#'
#' @return The data frame with those columns coerced to \code{numeric}.
#'
#' @export
convert_exemptions <- function(df) {
  df %>%
    dplyr::mutate(
      percent_medical_exemption   = as.numeric(percent_medical_exemption),
      percent_religious_exemption = as.numeric(percent_religious_exemption)
    )
}



#' Standardize school type labels in a column
#'
#' Normalizes free-text school type values to one of four canonical labels:
#' \code{"charter"}, \code{"private"}, \code{"public"}, or \code{NA}.
#' Matching is case-insensitive and handles common abbreviations and
#' spellings (e.g., \code{"nonpublic"}, \code{"LEA"}, \code{"independent"}).
#'
#' @param df A data frame.
#' @param school_type_col Character string naming the column to standardize.
#'
#' @return \code{df} with the specified column recoded to canonical values.
#'
#' @export
clean_school_types <- function(df, school_type_col) {

  if (!school_type_col %in% names(df)) {
    stop("Column '", school_type_col, "' not found in data frame.", call. = FALSE)
  }

  x_norm <- df[[school_type_col]]
  x_norm <- tolower(trimws(as.character(x_norm)))
  x_norm[x_norm == ""] <- NA_character_

  df[[school_type_col]] <- dplyr::case_when(
    is.na(x_norm) ~ NA_character_,

    # Charter schools
    grepl("charter", x_norm) ~ "charter",

    # Private schools (explicitly catch non-public first)
    grepl("non[- ]?public|nonpublic|oosle", x_norm) ~ "private",
    grepl("private|independent|religious", x_norm) ~ "private",

    # Public schools (avoid matching non-public)
    grepl("\\bpublic\\b", x_norm) ~ "public",
    grepl("\\blea\\b|special", x_norm) ~ "public",

    TRUE ~ NA_character_
  )

  df
}



#' Harmonize a data frame to a required set of columns
#'
#' Adds any columns listed in \code{all_cols} that are missing from \code{df}
#' (filled with \code{NA}), then reorders and subsets \code{df} to exactly
#' the columns in \code{all_cols}.
#'
#' @param df A data frame.
#' @param all_cols Character vector of required column names in the desired
#'   output order.
#'
#' @return A data frame with exactly the columns in \code{all_cols}, in that
#'   order.  Missing columns are filled with \code{NA}.
#'
#' @export
harmonize_columns <- function(df, all_cols) {
  missing_cols <- setdiff(all_cols, colnames(df))
  if (length(missing_cols) > 0) {
    message("Adding missing columns: ", paste(missing_cols, collapse = ", "))
    df[missing_cols] <- NA
  }
  df[, all_cols]
}


#' Standardize column classes for kindergarten vaccination data
#'
#' Coerces a fixed set of well-known columns to their canonical R types:
#' character columns (school identifiers, addresses, labels), integer count
#' columns, and numeric percentage columns.  Columns not present in \code{df}
#' are silently ignored.
#'
#' @param df A data frame of kindergarten vaccination data.
#'
#' @return \code{df} with columns coerced to their canonical types.
#'
#' @importFrom readr parse_number
#' @export
standardize_kinder_classes <- function(df) {
  stopifnot(is.data.frame(df))

  chr_cols <- c(
    "year_source", "school_type", "county", "school_name", "school_number",
    "address", "city", "zip", "vaccine_type", "excluded_note"
  )

  int_cols <- c(
    "total_enrollment", "count_up_to_date", "count_med_exempt",
    "count_rel_exempt", "count_not_up_to_date"
  )

  num_cols <- c(
    "percent_med_exemp", "percent_rel_exemp",
    "percent_up_to_date", "percent_not_up_to_date"
  )

  df %>%
    dplyr::mutate(dplyr::across(tidyselect::any_of(chr_cols), as.character)) %>%
    dplyr::mutate(dplyr::across(tidyselect::any_of(int_cols), ~ {
      x <- readr::parse_number(as.character(.))
      ifelse(is.na(x), NA_integer_, as.integer(x))
    })) %>%
    dplyr::mutate(dplyr::across(tidyselect::any_of(num_cols), ~ {
      readr::parse_number(as.character(.))
    }))
}


#' Clean and pivot raw vaccination data into long format
#'
#' Cleans column names, corrects a known DTP column-name artefact
#' (\code{"d_ta_p"} to \code{"dtap"}), coerces \code{percent} and
#' \code{total} columns to numeric, and pivots the data from wide to long
#' format with one row per school per vaccine per metric.
#'
#' @param data A data frame of raw, wide-format vaccination data.
#'
#' @return A long-format data frame (or \code{NULL} for empty input) with
#'   columns \code{vaccine_type}, \code{metric_type}, and \code{value},
#'   plus any retained identifier columns such as \code{county}.
#'
#' @importFrom janitor clean_names
#' @importFrom tidyr pivot_longer
#' @export
clean_vaccination_data <- function(data) {
  if (is.null(data) || nrow(data) == 0) return(NULL)

  # Determine county presence on the input before entering the pipeline so
  # the test is not evaluated against the dplyr .data pronoun (which is not a
  # real data frame and does not support names()).
  has_county <- "county" %in% names(data)

  data %>%
    janitor::clean_names() %>%
    dplyr::rename_with(~ stringr::str_replace_all(.x, "d_ta_p", "dtap")) %>%
    dplyr::mutate(
      dplyr::across(tidyselect::matches("percent|total"), ~ suppressWarnings(as.numeric(.x)))
    ) %>%
    tidyr::pivot_longer(
      cols = tidyselect::matches("dtap|polio|mmr|varicella|hepb|hib|hep_b"),
      names_to = "vaccine_metric",
      values_to = "value"
    ) %>%
    dplyr::mutate(
      vaccine_type = dplyr::case_when(
        stringr::str_detect(vaccine_metric, "dtap")        ~ "dtap",
        stringr::str_detect(vaccine_metric, "polio")       ~ "polio",
        stringr::str_detect(vaccine_metric, "mmr")         ~ "mmr",
        stringr::str_detect(vaccine_metric, "varicella")   ~ "varicella",
        stringr::str_detect(vaccine_metric, "hepb|hep_b")  ~ "hepb",
        stringr::str_detect(vaccine_metric, "hib")         ~ "hib",
        TRUE ~ "other"
      ),
      metric_type = dplyr::case_when(
        stringr::str_detect(vaccine_metric, "percent")      ~ "percent",
        stringr::str_detect(vaccine_metric, "count|number") ~ "count",
        TRUE ~ "value"
      ),
      value  = as.numeric(value),
      value  = ifelse(metric_type == "percent" & !is.na(value) & value > 1, value / 100, value),
      county = if (has_county) .data$county else NA_character_
    )
}


#' Standardize grade range columns
#'
#' Cleans beginning-grade and end-grade columns to a canonical set of values
#' (\code{"prek"}, \code{"k"}, integer grade numbers), then combines them
#' into a grade-range string such as \code{"k-5"}.
#'
#' @param df A data frame containing grade columns.
#' @param bgn_col Name of the column holding the beginning grade (default
#'   \code{"bgn_grade"}).
#' @param end_col Name of the column holding the ending grade (default
#'   \code{"end_grade"}).
#' @param out_col Name of the output column for the combined grade range
#'   (default \code{"grade"}).
#'
#' @return \code{df} with \code{bgn_col} and \code{end_col} normalized and
#'   a new \code{out_col} column added.
#'
#' @export
standardize_grades <- function(df,
                               bgn_col = "bgn_grade",
                               end_col = "end_grade",
                               out_col = "grade") {

  clean_grade <- function(x) {
    x <- tolower(trimws(as.character(x)))
    x[x %in% c("", "na", "n/a", "null", "ng", "ug")] <- NA_character_

    out <- x

    out[out %in% c("pk", "pre-k", "pre k", "p0", "p3")] <- "prek"
    out[out %in% c("kg", "k")] <- "k"

    is_num <- !is.na(out) & grepl("^[0-9]+$", out)
    out[is_num] <- as.character(as.integer(out[is_num]))

    out
  }

  df %>%
    dplyr::mutate(
      !!bgn_col := clean_grade(.data[[bgn_col]]),
      !!end_col := clean_grade(.data[[end_col]]),
      !!out_col := dplyr::if_else(
        is.na(.data[[bgn_col]]) | is.na(.data[[end_col]]),
        NA_character_,
        paste0(.data[[bgn_col]], "-", .data[[end_col]])
      )
    )
}


#' Standardize a school name string for fuzzy matching
#'
#' Applies a series of normalization steps to make school names comparable
#' across data sources: squishing whitespace, lowercasing, expanding
#' abbreviations (e.g., \\code{"elm"} to \\code{"elementary"}), removing the
#' word \\code{"school"}, correcting common misspellings, and normalizing
#' grade-span notation.
#'
#' @param name Character vector of school names.
#'
#' @return Character vector of normalized school names, suitable for use as a
#'   matching key.
#'
#' @export
standardized_school_name <- function(name) {

  name %>%
    stringr::str_squish() %>%
    stringr::str_to_lower() %>%
    stringr::str_replace_all("[&]", "and") %>%
    stringr::str_replace_all("(?<=\\b[a-z])\\.(?=[a-z]\\b)", "") %>%
    stringr::str_replace_all("\\b([kp]?\\d?)\\s*[-\u2013]\\s*(\\d+)\\b", "\\1-\\2") %>%
    stringr::str_replace_all("\\be\\.?s\\.?\\b", "elementary") %>%
    stringr::str_replace_all("\\belm\\b", "elementary") %>%
    stringr::str_replace_all("\\belem\\b", "elementary") %>%
    stringr::str_replace_all(
      "\\b(elemntary|elemantary|elemenatry|elementery|elementry|elememtary|elemen)\\b",
      "elementary"
    ) %>%
    stringr::str_replace_all("\\b(sdchool|skool|schoool|schools|scholl|schol)\\b", "school") %>%
    stringr::str_replace_all("\\bdev\\b", "development") %>%
    stringr::str_replace_all("[/.-]", " ") %>%
    stringr::str_replace_all("[[:punct:]]", "") %>%
    stringr::str_replace_all("\\bschool\\b", "") %>%
    stringr::str_replace_all("^the\\s+", "") %>%
    stringr::str_squish() %>%
    stringr::str_replace_all("\\bmount\\b", "mt") %>%
    stringr::str_replace_all("\\b pk-8\\b.*$", "")
}


#' Standardize a county name string for fuzzy matching
#'
#' Normalizes county names for comparison across data sources: squishing
#' whitespace, lowercasing, removing \\code{"county"}, removing punctuation,
#' and replacing \\code{"&"} with \\code{"and"}.
#'
#' @param name Character vector of county names.
#'
#' @return Character vector of normalized county names.
#'
#' @export
standardized_county_name <- function(name) {

  name %>%
    stringr::str_squish() %>%
    stringr::str_to_lower() %>%
    stringr::str_replace_all("(?<=\\b[a-z])\\.(?=[a-z]\\b)", "") %>%
    stringr::str_replace_all(stringr::regex("county", ignore_case = TRUE), "") %>%
    stringr::str_replace_all("[/.-]", " ") %>%
    stringr::str_replace_all("[[:punct:]]", "") %>%
    stringr::str_replace_all("&", "and") %>%
    stringr::str_squish()
}


#' Add a school-level classification column
#'
#' Inspects the school name to classify each school into one of:
#' \code{"PreK/Elementary"}, \code{"Elementary/Middle"}, \code{"Middle/High"},
#' \code{"PreK"}, \code{"Elementary"}, \code{"Middle"}, \code{"High"}, or
#' \code{NA}.  Also removes level-indicating words from the school name so that
#' matching functions work on the root name only.
#'
#' @param df A data frame.
#' @param name_col Name of the column containing school names (default
#'   \code{"school_name"}).
#'
#' @return \code{df} with a new \code{school_level} column and the school name
#'   column cleaned of level phrases.
#'
#' @importFrom rlang sym
#' @export
add_school_level <- function(df, name_col = "school_name") {

  df %>%
    dplyr::mutate(!!rlang::sym(name_col) := !!rlang::sym(name_col) %>% stringr::str_squish()) %>%
    dplyr::mutate(
      school_level = dplyr::case_when(
        # Combined levels first
        stringr::str_detect(!!rlang::sym(name_col), stringr::regex("\\bkindergar\\b|\\bkindergarten\\b", ignore_case = TRUE)) &
          stringr::str_detect(!!rlang::sym(name_col), stringr::regex("\\bpre and lower\\b|\\bpre and elem\\b|\\bnursery\\b", ignore_case = TRUE)) ~ "PreK/Elementary",
        stringr::str_detect(!!rlang::sym(name_col), stringr::regex("elementary/middle|elem/middle|elementary and middle|elementary middle", ignore_case = TRUE)) ~ "Elementary/Middle",
        stringr::str_detect(!!rlang::sym(name_col), stringr::regex("middle/high|middle and high|middle high|middlehigh", ignore_case = TRUE)) ~ "Middle/High",
        (stringr::str_detect(!!rlang::sym(name_col), stringr::regex("\\bmiddle\\b", ignore_case = TRUE)) &
           stringr::str_detect(!!rlang::sym(name_col), stringr::regex("\\bhigh\\b", ignore_case = TRUE))) ~ "Middle/High",

        # Preschool indicators
        stringr::str_detect(!!rlang::sym(name_col), stringr::regex("\\bpre[- ]?k\\b|\\bprek\\b|\\bpreschool\\b|\\bnursery\\b", ignore_case = TRUE)) ~ "PreK",

        # Elementary indicators
        stringr::str_detect(!!rlang::sym(name_col), stringr::regex("\\bkindergar\\b|\\bkindergarten\\b", ignore_case = TRUE)) ~ "Elementary",
        stringr::str_detect(!!rlang::sym(name_col), stringr::regex("\\belementary\\b|\\blower\\b|\\bprimary\\b|\\belementary school\\b", ignore_case = TRUE)) ~ "Elementary",

        # Middle indicators
        stringr::str_detect(!!rlang::sym(name_col), stringr::regex("\\bmiddle( school)?\\b", ignore_case = TRUE)) ~ "Middle",

        # High indicators:
        #  - "high school" or "senior high"
        #  - OR standalone "high" at end of name (e.g., "arundel high")
        #  - BUT NOT "high road ..." (negative lookahead)
        stringr::str_detect(
          !!rlang::sym(name_col),
          stringr::regex("\\b(high school|senior high( school)?)\\b|\\bhigh\\b(?!\\s*road\\b)|\\bhs\\b|\\bhigh\\s*$",
                ignore_case = TRUE)
        ) ~ "High",

        TRUE ~ NA_character_
      ),

      # Clean the school_name carefully
      !!rlang::sym(name_col) := !!rlang::sym(name_col) %>%
        stringr::str_to_lower() %>%
        # Remove combined-level phrases
        stringr::str_replace_all(stringr::regex("middle/high|middle and high|middle high|middlehigh", ignore_case = TRUE), "") %>%
        stringr::str_replace_all(stringr::regex("elementary/middle|elem/middle|elementary and middle|elementary middle", ignore_case = TRUE), "") %>%
        # Remove explicit level phrases
        stringr::str_replace_all(stringr::regex("\\belementary school\\b", ignore_case = TRUE), "") %>%
        stringr::str_replace_all(stringr::regex("\\bmiddle school\\b", ignore_case = TRUE), "") %>%
        stringr::str_replace_all(stringr::regex("\\bhigh school\\b|\\bhs\\b|\\bsenior high( school)?\\b", ignore_case = TRUE), "") %>%
        # Remove standalone "elementary/lower/primary/middle"
        stringr::str_replace_all(stringr::regex("\\belementary\\b|\\blower\\b|\\bprimary\\b|\\bmiddle\\b", ignore_case = TRUE), "") %>%
        # Remove "high" only when it is clearly a level word at end of name
        stringr::str_replace_all(stringr::regex("\\bhigh\\b\\s*$", ignore_case = TRUE), "") %>%
        # Common misspellings
        stringr::str_replace_all(stringr::regex("elemntary|elemantary|elemenatry|elementery|elementry|elememtary|elemen", ignore_case = TRUE), "") %>%
        stringr::str_replace_all(stringr::regex("sdchool|skool|schoool|schools", ignore_case = TRUE), "") %>%
        stringr::str_squish()
    )
}



#' Add a school level code column derived from school type
#'
#' Derives a level code string (e.g., \code{"p,e,m,h"}) from keywords present
#' in the \code{name_col} column, encoding the minimum and maximum levels
#' served (PreK = \code{"p"}, Elementary = \code{"e"}, Middle = \code{"m"},
#' High = \code{"h"}).
#'
#' @param df A data frame.
#' @param name_col Name of the column containing school type or name strings
#'   (default \code{"school_name"}).
#'
#' @return \code{df} with a new \code{level_code} column.
#'
#' @importFrom rlang sym
#' @export
add_school_level_code_fromtype <- function(df, name_col = "school_name") {

  grade_levels <- c("p", "e", "m", "h")

  tmp_col <- dplyr::pull(df, !!rlang::sym(name_col)) %>% tolower() %>% stringr::str_squish()
  tmp_col <- gsub("prek", "pk", tmp_col)

  school_level_min <- dplyr::case_when(
    grepl(" pk$| pk/| pk and ", tmp_col, ignore.case = TRUE) | tmp_col == "pk" ~ 1,
    grepl("elementary/| elementary$| elementary/| elementary and | elem$| elem/| elem and | elm$| elm and | elm/| es$| es/", tmp_col, ignore.case = TRUE) | tmp_col == "elementary" ~ 2,
    grepl(" middle$|/middle$|middle/| middle and ", tmp_col, ignore.case = TRUE) | tmp_col == "middle" ~ 3,
    grepl(" high$|/high$|high/| hs$|/hs$", tmp_col, ignore.case = TRUE) | tmp_col == "high" ~ 4,
    TRUE ~ NA
  )
  school_level_max <- dplyr::case_when(
    grepl(" high$|/high$|high/| hs$|/hs$", tmp_col, ignore.case = TRUE) | tmp_col == "high" ~ 4,
    grepl(" middle$|/middle$|middle/| middle and ", tmp_col, ignore.case = TRUE) | tmp_col == "middle" ~ 3,
    grepl("elementary/| elementary$| elementary/| elementary and | elem$| elem/| elem and | elm$| elm and | elm/| es$| es/", tmp_col, ignore.case = TRUE) | tmp_col == "elementary" ~ 2,
    grepl(" pk$| pk/| pk and ", tmp_col, ignore.case = TRUE) | tmp_col == "pk" ~ 1,
    TRUE ~ NA
  )

  school_level_code <- sapply(seq_len(nrow(df)), function(i) {
    if (is.na(school_level_min[i]) | is.na(school_level_max[i])) {
      return(NA_character_)
    }
    paste(grade_levels[school_level_min[i]:school_level_max[i]], collapse = ",")
  })

  df %>% dplyr::mutate(level_code = school_level_code)
}



#' Add a school level code column derived from a grade-span column
#'
#' Parses a grade-span string (e.g., \code{"PK-5"}, \code{"KG,1,2,3"}) from
#' \code{name_col} and produces a level code string (e.g., \code{"p,e"})
#' encoding the minimum and maximum levels served.
#'
#' @param df A data frame.
#' @param name_col Name of the column containing grade span strings (default
#'   \code{"grades"}).
#'
#' @return \code{df} with a new \code{level_code} column.
#'
#' @importFrom rlang sym
#' @export
add_school_level_code <- function(df, name_col = "grades") {

  tmp_col <- dplyr::pull(df, !!rlang::sym(name_col)) %>% tolower()
  tmp_col <- gsub(" & ungraded", "", tmp_col)
  tmp_col <- gsub("prek", "pk", tmp_col)
  tmp_col <- gsub("grades: ", "", tmp_col)
  tmp_col <- stringr::str_squish(tmp_col)

  min_grade <- stringr::str_split_i(tmp_col, ",| |-", 1)
  max_grade <- stringr::str_split_i(tmp_col, ",| |-", -1)

  grade_levels <- c("p", "e", "m", "h")

  school_level_min <- dplyr::case_when(
    min_grade %in% c("pk", "tk") ~ 1,
    min_grade %in% c("k", 0, 1, 2, 3, 4, 5) ~ 2,
    min_grade %in% c(6, 7, 8) ~ 3,
    min_grade %in% c(9, 10, 11, 12) ~ 4,
    TRUE ~ NA_real_
  )

  school_level_max <- dplyr::case_when(
    max_grade %in% c("pk", "tk") ~ 1,
    max_grade %in% c("k", 0, 1, 2, 3, 4, 5, 6) ~ 2,
    max_grade %in% c(7, 8) ~ 3,
    max_grade %in% c(9, 10, 11, 12) ~ 4,
    TRUE ~ NA_real_
  )

  school_level_code <- sapply(seq_len(nrow(df)), function(i) {
    if (is.na(school_level_min[i]) | is.na(school_level_max[i])) {
      return(NA_character_)
    }
    paste(grade_levels[school_level_min[i]:school_level_max[i]], collapse = ",")
  })

  return(df %>% dplyr::mutate(level_code = school_level_code))
}


#' Override school type to "charter" based on school name
#'
#' Sets \code{school_type} to \code{"charter"} for any row where the school
#' name contains the word \code{"charter"} (case-insensitive).
#'
#' @param df A data frame with a \code{school_type} column.
#' @param name_col Name of the column containing school names (default
#'   \code{"school_name"}).
#'
#' @return \code{df} with \code{school_type} corrected for charter schools.
#'
#' @importFrom rlang sym
#' @export
fix_charter_type <- function(df, name_col = "school_name") {

  df %>%
    dplyr::mutate(
      school_type = ifelse(
        stringr::str_detect(!!rlang::sym(name_col), stringr::regex("charter", ignore_case = TRUE)),
        "charter",
        school_type
      )
    )
}



#' Convert all character columns to lowercase
#'
#' Applies \code{tolower} to every character column in the data frame.
#'
#' @param data A data frame.
#'
#' @return \code{data} with all character columns lowercased.
#'
#' @export
clean_lowercase <- function(data) {
  data %>%
    dplyr::mutate(dplyr::across(tidyselect::where(is.character), tolower))
}



#' Standardize column names for DQA checks
#'
#' Renames and recalculates columns from state-specific naming conventions to
#' the standard set expected by the DQA functions: \code{year},
#' \code{enrollment}, \code{current}, \code{med_exempt}, \code{rel_exempt},
#' and \code{delayed}.  The \code{delayed} column (unvaccinated and not
#' exempt) is calculated as enrollment minus the sum of current, medical, and
#' religious exemptions; negative values are set to zero with a warning.
#'
#' @param df A data frame with columns \code{year_source},
#'   \code{total_k_students}, \code{count_vacc}, \code{med_exempt}, and
#'   \code{rel_exempt}.
#'
#' @return \code{df} with the standardized columns added and integer types
#'   enforced on all count columns.
#'
#' @export
standardize_colnames_for_dqa <- function(df) {

  df <- df %>%
    dplyr::mutate(
      # Convert year_source ("md_15_16") to kindergarten starting year (e.g., 2015)
      year = as.integer(paste0("20", stringr::str_sub(year_source, 4, 5))),

      # Convert enrollment-related counts to integers
      enrollment = as.integer(total_k_students),
      current    = as.integer(count_vacc),
      med_exempt = as.integer(med_exempt),
      rel_exempt = as.integer(rel_exempt),

      # Delayed = enrolled - (current + exemptions)
      delayed = ifelse(
        !is.na(enrollment) & !is.na(current) & !is.na(med_exempt) & !is.na(rel_exempt),
        enrollment - (current + med_exempt + rel_exempt),
        NA_integer_
      )
    )

  neg_delayed <- dplyr::filter(df, delayed < 0)

  if (nrow(neg_delayed) > 0) {
    message("Warning: ", nrow(neg_delayed),
            " rows have 'delayed' < 0. Setting these to 0.")
  } else {
    message("No negative 'delayed' values found.")
  }

  df <- df %>%
    dplyr::mutate(delayed = dplyr::if_else(delayed < 0, 0L, delayed))

  neg_delayed_check <- dplyr::filter(df, delayed < 0)
  if (nrow(neg_delayed_check) > 0) {
    warning("Some 'delayed' values are still negative after correction!")
  } else {
    message("'delayed' column successfully corrected; no negative values remain.")
  }

  int_cols <- c("enrollment", "current", "med_exempt", "rel_exempt", "delayed")
  df[int_cols] <- lapply(df[int_cols], as.integer)
  message("Converted columns to integer: ", paste(int_cols, collapse = ", "))

  return(df)
}


#' Standardize a character vector of street addresses
#'
#' Applies USPS-style normalization to each address: transliterates Unicode to
#' ASCII, normalizes PO Box variants, standardizes unit designators (APT, STE,
#' etc.), expands ordinal words (FIRST to 1ST), standardizes street suffixes
#' (STREET to ST) and directionals (NORTH to N), and optionally strips unit
#' segments, directionals, or house numbers.
#'
#' @param x Character vector of raw address strings.
#' @param keep_units Logical; retain unit designators such as APT or STE
#'   (default \code{TRUE}).
#' @param keep_house_number Logical; retain the leading house number (default
#'   \code{TRUE}).
#' @param keep_directionals Logical; retain directional tokens such as N or SW
#'   (default \code{TRUE}).
#' @param target_case One of \code{"upper"}, \code{"lower"}, or \code{"title"};
#'   controls the case of the output (default \code{"upper"}).
#' @param remove_punctuation Logical; remove most punctuation marks while
#'   preserving characters meaningful in addresses (\code{"#"}, \code{"/"},
#'   \code{"-"}, \code{"&"}) before further processing (default \code{TRUE}).
#' @param normalize_ordinals Logical; convert ordinal words (FIRST, SECOND,
#'   etc.) to numeric ordinals (1ST, 2ND, etc.) (default \code{TRUE}).
#'
#' @return A character vector of normalized addresses, the same length as
#'   \code{x}, with empty or \code{NA} inputs returned as \code{NA}.
#'
#' @importFrom stringi stri_trans_general
#' @export
clean_address <- function(x,
                          keep_units = TRUE,
                          keep_house_number = TRUE,
                          keep_directionals = TRUE,
                          target_case = c("upper", "lower", "title"),
                          remove_punctuation = TRUE,
                          normalize_ordinals = TRUE) {
  target_case <- match.arg(target_case)

  apply_case <- function(s) {
    switch(target_case,
           upper = toupper(s),
           lower = tolower(s),
           title = stringr::str_to_title(s))
  }

  suffix_map <- c(
    "ALLEY"="ALY","ALLY"="ALY","ALY"="ALY",
    "AVENUE"="AVE","AV"="AVE","AVE"="AVE","AVEN"="AVE",
    "BOULEVARD"="BLVD","BLVD"="BLVD","BOULV"="BLVD",
    "CIRCLE"="CIR","CIR"="CIR","CIRC"="CIR","CRCL"="CIR",
    "COURT"="CT","CT"="CT",
    "DRIVE"="DR","DRV"="DR","DR"="DR",
    "EXPRESSWAY"="EXPY","EXPWY"="EXPY","EXPY"="EXPY",
    "FREEWAY"="FWY","FRWY"="FWY","FWY"="FWY",
    "HIGHWAY"="HWY","HIWAY"="HWY","HWY"="HWY","HWAY"="HWY",
    "LANE"="LN","LN"="LN",
    "PARKWAY"="PKWY","PKWY"="PKWY","PARKWY"="PKWY",
    "PLACE"="PL","PL"="PL",
    "PLAZA"="PLZ","PLZ"="PLZ",
    "ROAD"="RD","RD"="RD",
    "SQUARE"="SQ","SQ"="SQ",
    "STREET"="ST","STR"="ST","ST"="ST","STRT"="ST",
    "TERRACE"="TER","TERR"="TER","TER"="TER",
    "TRAIL"="TRL","TRLS"="TRL","TRL"="TRL",
    "WAY"="WAY"
  )

  directional_map <- c(
    "NORTH"="N", "NORTHEAST"="NE", "NORTHWEST"="NW",
    "SOUTH"="S", "SOUTHEAST"="SE", "SOUTHWEST"="SW",
    "EAST"="E", "WEST"="W",
    "N"="N", "NE"="NE", "NW"="NW", "S"="S", "SE"="SE", "SW"="SW", "E"="E", "W"="W"
  )

  geography_map <- c(
    "MOUNT"="MT", "MT"="MT"
  )

  ordinal_word_map <- c(
    "FIRST"="1ST","SECOND"="2ND","THIRD"="3RD","FOURTH"="4TH","FIFTH"="5TH",
    "SIXTH"="6TH","SEVENTH"="7TH","EIGHTH"="8TH","NINTH"="9TH","TENTH"="10TH"
  )

  normalize_ordinals_fn <- function(s) {
    pat <- paste0("\\b(", paste(names(ordinal_word_map), collapse = "|"), ")\\b")
    stringr::str_replace_all(s, pat, function(m) ordinal_word_map[toupper(m)])
  }

  standardize_suffix <- function(tokens) {
    if (length(tokens) == 0) return(tokens)
    last <- tokens[length(tokens)]
    up <- toupper(last)
    if (!is.na(suffix_map[up])) {
      tokens[length(tokens)] <- suffix_map[up]
    }
    tokens
  }

  standardize_directionals <- function(tokens) {
    out <- character(length(tokens))
    for (i in seq_along(tokens)) {
      up <- toupper(tokens[i])
      out[i] <- if (!is.na(directional_map[up])) directional_map[up] else tokens[i]
    }
    out
  }

  standardize_geography <- function(tokens) {
    out <- character(length(tokens))
    for (i in seq_along(tokens)) {
      up <- toupper(tokens[i])
      out[i] <- if (!is.na(geography_map[up])) geography_map[up] else tokens[i]
    }
    out
  }

  standardize_units <- function(s) {
    s <- stringr::str_replace_all(s, "(?i)\\bAPARTMENT\\b", "APT")
    s <- stringr::str_replace_all(s, "(?i)\\bSUITE\\b",     "STE")
    s <- stringr::str_replace_all(s, "(?i)\\bROOM\\b",      "RM")
    s <- stringr::str_replace_all(s, "(?i)\\bFLOOR\\b",     "FL")
    s <- stringr::str_replace_all(s, "(?i)\\bBUILDING\\b",  "BLDG")
    s <- stringr::str_replace_all(s, "(?i)\\bDEPARTMENT\\b","DEPT")
    s <- stringr::str_replace_all(s, "(?i)\\s*#\\s*(\\w+)\\b", " APT \\1")
    s <- stringr::str_replace_all(s, "(?i)\\b(APT|UNIT|STE|RM|FL|BLDG|DEPT|LOT)\\s*([A-Z0-9-]+)\\b", "\\1 \\2")
    s
  }

  normalize_po_box <- function(s) {
    s <- stringr::str_replace_all(s, "(?i)\\bP\\.?\\s*O\\.?\\s*BOX\\b", "PO BOX")
    s <- stringr::str_replace_all(s, "(?i)\\bPOBOX\\b", "PO BOX")
    s
  }

  strip_units_if_needed <- function(s) {
    if (keep_units) return(s)
    stringr::str_replace(s, "\\s+(APT|UNIT|STE|RM|FL|BLDG|DEPT|LOT)\\s+[A-Z0-9-]+\\b", "")
  }

  strip_directionals_if_needed <- function(s) {
    if (keep_directionals) return(s)
    stringr::str_replace_all(s, "(^|\\s+)(S|W|N|E|NW|SW|SE|NE)\\b", "")
  }

  strip_house_if_needed <- function(tokens) {
    if (keep_house_number) return(tokens)
    if (length(tokens) == 0) return(tokens)
    if (stringr::str_detect(tokens[1], "^[0-9]+[A-Z]?$")) {
      return(tokens[-1])
    }
    tokens
  }

  out <- vapply(x, function(addr) {
    if (is.na(addr) || stringr::str_trim(addr) == "") return(NA_character_)

    s <- addr

    # Normalize unicode/diacritics to ASCII
    s <- stringi::stri_trans_general(s, "Latin-ASCII")

    # Normalize PO Box first to avoid punctuation loss impacting detection
    s <- normalize_po_box(s)

    # Remove punctuation except '#', '/', '-', '&' which may be meaningful
    if (remove_punctuation) {
      s <- stringr::str_replace_all(s, "[,.;:!?(){}\\[\\]|\"']", " ")
    }

    # Standardize units (turn # to APT etc.)
    s <- standardize_units(s)

    # Collapse extra slashes/ampersands/dashes spacing
    s <- stringr::str_replace_all(s, "\\s*[/&-]\\s*", " ")

    # Normalize ordinals (FIRST -> 1ST etc.)
    if (normalize_ordinals) {
      s <- normalize_ordinals_fn(toupper(s))
    } else {
      s <- toupper(s)
    }

    # Collapse whitespace
    s <- stringr::str_squish(s)

    # Tokenize
    tokens <- unlist(stringr::str_split(s, "\\s+"))
    if (length(tokens) == 0) return(NA_character_)

    # Directionals, geography & suffixes
    tokens <- standardize_directionals(tokens)
    tokens <- standardize_geography(tokens)
    tokens <- standardize_suffix(tokens)

    # Optionally drop house number
    tokens <- strip_house_if_needed(tokens)

    # Rebuild
    s <- paste(tokens, collapse = " ")

    # Optionally remove unit segments entirely
    s <- strip_units_if_needed(s)

    # Drop directionals if needed
    s <- strip_directionals_if_needed(s)

    # Final squish and case
    s <- stringr::str_squish(s)
    s <- apply_case(s)

    s
  }, FUN.VALUE = character(1))

  # Empty strings to NA for consistency
  out[out == ""] <- NA_character_

  out
}


#' Clean and enrich kindergarten vaccination data for school matching
#'
#' Applies standardization and enrichment steps to a kindergarten vaccination
#' data frame: lowercases all character columns, creates standardized school
#' name and county columns for fuzzy matching, adds school level and level code
#' columns, corrects charter school type, and (if an address column is present)
#' creates cleaned address columns for address-based matching.
#'
#' @param kinder_dat A data frame of kindergarten vaccination data.  Must
#'   contain at least \code{school_name} and \code{county} columns.  An address
#'   column whose name contains \code{"address"} or \code{"street"} is detected
#'   automatically.
#'
#' @return A named list with two elements:
#' \describe{
#'   \item{data}{The enriched data frame.}
#'   \item{kinder_has_addr}{Logical; \code{TRUE} if a non-empty address column
#'     was found and cleaned.}
#' }
#'
#' @export
clean_kinder_data <- function(kinder_dat) {

  kinder_dat <- kinder_dat %>%
    clean_lowercase() %>%
    dplyr::mutate(
      school_name_std = standardized_school_name(school_name),
      county_std      = standardized_county_name(county)
    ) %>%
    add_school_level(name_col = "school_name_std") %>%
    add_school_level_code_fromtype(name_col = "school_level") %>%
    fix_charter_type(name_col = "school_name")

  kinder_street_col <- if (any(grepl("address", colnames(kinder_dat)))) {
    grep("address", colnames(kinder_dat), value = TRUE, ignore.case = TRUE)[1]
  } else if (any(grepl("street", colnames(kinder_dat), ignore.case = TRUE))) {
    grep("street", colnames(kinder_dat), value = TRUE, ignore.case = TRUE)[1]
  } else {
    NA_character_
  }

  kinder_has_addr <- !is.na(kinder_street_col) &&
    any(!is.na(kinder_dat[[kinder_street_col]]))

  if (kinder_has_addr) {
    kinder_dat <- kinder_dat %>%
      dplyr::mutate(
        addr_clean_kinder = clean_address(
          .data[[kinder_street_col]], target_case = "lower"
        ),
        addr_clean_kinder_no_unit = clean_address(
          .data[[kinder_street_col]],
          keep_units = FALSE, keep_directionals = FALSE, target_case = "lower"
        ),
        city_kinder = if ("city" %in% colnames(kinder_dat)) .data$city else NA_character_,
        zip_kinder  = if (any(grepl("zip", colnames(kinder_dat), ignore.case = TRUE))) {
          as.character(.data[[which(grepl("zip", colnames(kinder_dat), ignore.case = TRUE))[1]]])
        } else {
          NA_character_
        }
      )
  }

  return(list(
    data            = kinder_dat,
    kinder_has_addr = kinder_has_addr
  ))
}



#' Clean DOE (Department of Education) school reference data
#'
#' Applies standardization steps to a DOE school data frame: lowercases all
#' character columns, creates standardized school name and county matching
#' keys, adds school level and level code columns, cleans address fields for
#' address-based matching, assigns a row identifier, generalizes the level code
#' for cross-source matching, and corrects charter school type.
#'
#' @param doe_dat A data frame of DOE school data.  Must contain at minimum
#'   \code{school_name}, \code{county}, \code{grades}, and \code{street}
#'   columns.
#'
#' @return The enriched data frame with additional columns:
#'   \code{school_name_std}, \code{county_std}, \code{school_level},
#'   \code{level_code}, \code{addr_clean}, \code{addr_clean_no_unit},
#'   \code{data2_id}, and \code{level_code_match}.
#'
#' @importFrom dplyr mutate row_number
#' @export
clean_doe_data <- function(doe_dat) {

  doe_dat <- doe_dat %>%
    clean_lowercase() %>%
    dplyr::mutate(
      school_name_std = standardized_school_name(school_name),
      county_std      = standardized_county_name(county)
    ) %>%
    add_school_level(name_col = "school_name_std") %>%
    add_school_level_code(name_col = "grades")

  doe_dat <- doe_dat %>%
    dplyr::mutate(
      addr_clean         = clean_address(street, target_case = "lower"),
      addr_clean_no_unit = clean_address(street, keep_units = FALSE, keep_directionals = FALSE, target_case = "lower")
    )

  doe_dat <- doe_dat %>%
    dplyr::mutate(data2_id = dplyr::row_number())

  doe_dat <- doe_dat %>%
    dplyr::mutate(level_code_match = level_code) %>%
    dplyr::mutate(
      level_code_match = ifelse(grepl(",e", level_code_match), gsub("p,", "", level_code_match), level_code_match)
    ) %>%
    fix_charter_type(name_col = "school_name")

  return(doe_dat)
}



#' Clean GreatSchools school reference data
#'
#' Applies standardization steps to a GreatSchools data frame: creates
#' standardized school name and county matching keys, adds school level
#' classification, renames selected columns to the package-standard naming
#' convention, cleans address fields for address-based matching, derives
#' geocoded county and ZIP via \code{\link{get_geo_info}}, assigns a row
#' identifier, generalizes the level code for cross-source matching, and
#' corrects charter school type.
#'
#' @param gs_dat A data frame of GreatSchools school data.  Must contain at
#'   minimum \code{name}, \code{county}, \code{county2}, \code{schoolType},
#'   \code{levelCode}, \code{street1}, \code{lat}, \code{lon}, and \code{zip}
#'   columns.
#' @param state_abbr Two-letter state abbreviation (e.g., \code{"md"}) used to
#'   look up geocoded county information via \code{\link{get_geo_info}}.
#'
#' @return The enriched data frame with additional columns:
#'   \code{school_name_std}, \code{county_std}, \code{county2_std},
#'   \code{school_level}, \code{school_type}, \code{school_name},
#'   \code{level_code}, \code{addr_clean}, \code{addr_clean_no_unit},
#'   \code{county_geo}, \code{zip_geo}, \code{county_geo_std},
#'   \code{county_gs}, \code{county_gs_std}, \code{data1_id}, and
#'   \code{level_code_match}.
#'
#' @importFrom dplyr mutate rename row_number bind_cols select
#' @export
clean_greatschools_data <- function(gs_dat, state_abbr) {

  if (!is.character(state_abbr) || length(state_abbr) != 1L || is.na(state_abbr) || !grepl("^[A-Za-z]{2}$", state_abbr)) {
    stop("'state_abbr' must be a single two-letter state abbreviation (e.g., \"md\").", call. = FALSE)
  }

  gs_dat <- gs_dat %>%
    dplyr::mutate(
      school_name_std = standardized_school_name(name),
      county_std      = standardized_county_name(county),
      county2_std     = standardized_county_name(county2)
    )

  gs_dat <- gs_dat %>%
    add_school_level(name_col = "school_name_std") %>%
    dplyr::rename(
      school_type = schoolType,
      school_name = name,
      level_code  = levelCode
    ) %>%
    dplyr::mutate(zip = as.character(zip))

  gs_dat <- gs_dat %>%
    dplyr::mutate(
      addr_clean         = clean_address(street1, target_case = "lower"),
      addr_clean_no_unit = clean_address(street1, keep_units = FALSE, keep_directionals = FALSE, target_case = "lower")
    )

  # Geocode GreatSchools data to verify county and ZIP
  old_tigris_use_cache <- getOption("tigris_use_cache")
  on.exit(options(tigris_use_cache = old_tigris_use_cache), add = TRUE)
  options(tigris_use_cache = TRUE)
  gs_geos <- get_geo_info(state_abbr = state_abbr, lat = gs_dat$lat, lon = gs_dat$lon)
  gs_dat <- gs_dat %>%
    dplyr::bind_cols(
      gs_geos %>% dplyr::select(county_geo = county, zip_geo = zip_code)
    ) %>%
    dplyr::mutate(county_geo_std = standardized_county_name(county_geo))

  # Geocoded counties are more reliable than GreatSchools counties;
  # GreatSchools ZIP codes are more reliable than geocoded ZIPs (due to
  # misalignment between ZIP and ZCTA boundaries).
  gs_dat <- gs_dat %>%
    dplyr::mutate(
      county_gs     = county,
      county_gs_std = county_std,
      county        = county_geo,
      county_std    = county_geo_std
    )

  gs_dat <- gs_dat %>%
    dplyr::mutate(data1_id = dplyr::row_number())

  gs_dat <- gs_dat %>%
    dplyr::mutate(level_code_match = level_code) %>%
    dplyr::mutate(
      level_code_match = ifelse(grepl(",e", level_code_match), gsub("p,", "", level_code_match), level_code_match)
    ) %>%
    fix_charter_type(name_col = "school_name")

  return(gs_dat)
}



#' Load and prepare an optional third school data source
#'
#' Searches for a \code{other_dat.csv} or \code{other_dat.xlsx} file in
#' \code{state_school_dir}, reads it, validates required columns, and applies
#' the same standardization pipeline as \code{\link{clean_doe_data}}:
#' lowercasing, standardized name/county keys, school level, address cleaning,
#' row identifier, level code generalization, and charter type correction.
#'
#' @param other_dat_filenames Character vector of candidate filenames to look
#'   for in \code{state_school_dir} (default
#'   \code{c("other_dat.csv", "other_dat.xlsx")}).
#' @param state_school_dir Path to the directory that may contain the third
#'   data source file.
#'
#' @return The standardized data frame if a file was found and passed
#'   validation, or \code{NULL} if no matching file exists.
#'
#' @importFrom readxl read_xlsx
#' @importFrom readr read_csv
#' @export
setup_other_sourcedata <- function(other_dat_filenames = c("other_dat.csv", "other_dat.xlsx"),
                                   state_school_dir) {

  other_dat_candidates <- file.path(state_school_dir, other_dat_filenames)
  existing_other_dat   <- other_dat_candidates[file.exists(other_dat_candidates)]

  has_other_dat  <- length(existing_other_dat) > 0
  other_dat_path <- if (has_other_dat) existing_other_dat[[1]] else NA_character_

  other_dat <- if (has_other_dat) {
    ext <- tolower(tools::file_ext(other_dat_path))

    if (ext == "csv") {
      readr::read_csv(other_dat_path, show_col_types = FALSE)
    } else if (ext == "xlsx") {
      readxl::read_xlsx(other_dat_path)
    } else {
      stop("Unsupported other_dat file type: ", ext)
    }
  } else {
    NULL
  }

  if (has_other_dat) {
    other_dat_required_cols <- c("school_name", "street", "city", "county", "zip", "school_type", "grades")
    missing_cols <- setdiff(other_dat_required_cols, colnames(other_dat))
    if (length(missing_cols) > 0) {
      stop("other_dat is missing required columns: ", paste(missing_cols, collapse = ", "))
    }

    other_dat <- other_dat %>%
      clean_lowercase() %>%
      dplyr::mutate(
        school_name_std = standardized_school_name(school_name),
        county_std      = standardized_county_name(county)
      ) %>%
      add_school_level(name_col = "school_name_std") %>%
      add_school_level_code(name_col = "grades")

    other_dat <- other_dat %>%
      dplyr::mutate(
        addr_clean         = clean_address(street, target_case = "lower"),
        addr_clean_no_unit = clean_address(street, keep_units = FALSE, keep_directionals = FALSE, target_case = "lower")
      )

    other_dat <- other_dat %>%
      dplyr::mutate(data3_id = dplyr::row_number())

    other_dat <- other_dat %>%
      dplyr::mutate(level_code_match = level_code) %>%
      dplyr::mutate(
        level_code_match = ifelse(grepl(",e", level_code_match), gsub("p,", "", level_code_match), level_code_match)
      ) %>%
      fix_charter_type(name_col = "school_name")
  }

  return(other_dat)
}



# ---- State School Data Utility Functions ----------------------------------------

#' Find a raw state school file in a directory
#'
#' Searches for the first \code{.csv} or \code{.xlsx} file whose basename
#' contains \code{"raw"} (case-insensitive) in the given directory.
#'
#' @param dir Directory to search.
#'
#' @return Full path to the matching file, or \code{NULL} if none found.
#'
#' @export
find_raw_state_file <- function(dir) {
  all_files <- list.files(dir, full.names = TRUE)
  matches <- all_files[
    grepl("raw", basename(all_files), ignore.case = TRUE) &
      grepl("\\.(csv|xlsx)$", basename(all_files), ignore.case = TRUE)
  ]
  if (length(matches) == 0) {
    message("No raw state school file (.csv or .xlsx with 'raw' in name) found in: ", dir)
    return(NULL)
  }
  if (length(matches) > 1) {
    message("Multiple raw state school files found; using: ", matches[1])
  }
  matches[1]
}


#' Read a raw state school file (.csv or .xlsx)
#'
#' Reads a \code{.csv} or \code{.xlsx} file and returns a data frame with
#' column names cleaned via \code{janitor::clean_names}.
#'
#' @param path Full file path to a \code{.csv} or \code{.xlsx} file.
#'
#' @return A data frame with cleaned column names.
#'
#' @importFrom readxl read_excel
#' @importFrom janitor clean_names
#' @export
read_raw_state_file <- function(path) {
  if (grepl("\\.xlsx$", path, ignore.case = TRUE)) {
    tryCatch(
      readxl::read_excel(path) %>% janitor::clean_names(),
      error = function(e) stop("Failed to read xlsx file '", path, "': ", e$message, call. = FALSE)
    )
  } else {
    tryCatch(
      utils::read.csv(path) %>% janitor::clean_names(),
      error = function(e) stop("Failed to read csv file '", path, "': ", e$message, call. = FALSE)
    )
  }
}


#' Parse colon-separated grade codes to a standardized grade range string
#'
#' Converts EDDIE-style grade strings (e.g., \code{"PK:KG:01:02:03"}) to a
#' standardized range format (e.g., \code{"prek-3"}).  Removes \code{"XG"}
#' codes, normalizes \code{"PK"}/\code{"P3"} to \code{"prek"}, \code{"KG"}
#' to \code{"k"}, and numeric grade codes to integers.
#'
#' @param x Character vector of colon-separated grade strings.
#'
#' @return Character vector of standardized grade range strings (\code{NA} for
#'   empty or unrecognized input).
#'
#' @importFrom purrr map_chr
#' @export
parse_colon_grades <- function(x) {
  grade_order <- c("prek", "k", as.character(1:13))

  purrr::map_chr(x, function(grade_str) {
    if (is.na(grade_str) || !nzchar(grade_str)) return(NA_character_)

    g <- strsplit(grade_str, ":", fixed = TRUE)[[1]]
    g <- g[nzchar(g)]
    g <- g[g != "XG"]
    if (length(g) == 0) return(NA_character_)

    g <- dplyr::case_when(
      g %in% c("P3", "PK") ~ "prek",
      g == "KG"             ~ "k",
      TRUE                  ~ g
    )

    g_num <- suppressWarnings(as.numeric(g))
    g[!is.na(g_num)] <- as.character(as.integer(g_num[!is.na(g_num)]))
    g <- g[!is.na(g) & nzchar(g)]
    if (length(g) == 0) return(NA_character_)

    g <- g[order(match(g, grade_order))]
    if (length(g) == 1) g else paste0(g[1], "-", g[length(g)])
  })
}


#' Standardize a raw state school data frame to a common column structure
#'
#' Renames columns per \code{col_map}, cleans ZIP codes to five digits,
#' recodes school type values, and parses colon-separated grade strings.
#' This function is designed to work across states: supply the appropriate
#' \code{col_map} and \code{school_type_map} for each state raw file.
#'
#' @param df Data frame of raw state school data whose column names have
#'   already been cleaned with \code{janitor::clean_names}.
#' @param col_map Named list mapping desired output column names to the
#'   corresponding input column names in \code{df}.  The names of the list
#'   become the output column names (e.g., \code{list(school_name =
#'   "school_name", street = "address_line1", city = "city", state = "state",
#'   zip = "zip_code_5", school_type = "school_designation_desc", grades =
#'   "grade_level_current")}).
#' @param school_type_map Optional named list mapping output school-type labels
#'   (e.g., \code{"charter"}, \code{"public"}) to character vectors of input
#'   values that should receive that label.  Any value not matched defaults to
#'   \code{"private"}, or to the value of the \code{"default"} key if supplied.
#'
#' @return Data frame containing only the columns specified in \code{col_map},
#'   with ZIP codes trimmed to five digits, school types recoded, and grades
#'   converted to a standardized range string.
#'
#' @export
standardize_state_school_data <- function(df, col_map, school_type_map = NULL) {
  rename_vec <- stats::setNames(as.character(unlist(col_map)), names(col_map))

  df <- df %>%
    dplyr::select(dplyr::all_of(rename_vec))

  if ("zip" %in% names(df)) {
    df <- df %>%
      dplyr::mutate(zip = stringr::str_extract(as.character(zip), "^\\d{5}"))
  }

  if (!is.null(school_type_map) && "school_type" %in% names(df)) {
    default_type <- if ("default" %in% names(school_type_map)) {
      school_type_map[["default"]]
    } else {
      "private"
    }
    type_entries <- school_type_map[names(school_type_map) != "default"]
    st     <- df$school_type
    result <- rep(default_type, length(st))
    for (tp in names(type_entries)) {
      result[st %in% type_entries[[tp]]] <- tp
    }
    df$school_type <- result
  }

  if ("grades" %in% names(df)) {
    df <- df %>%
      dplyr::mutate(grades = parse_colon_grades(grades))
  }

  df
}


#' Add a county name column to a data frame using ZIP code lookup
#'
#' Joins county FIPS codes from a ZIP-to-county reference, then maps FIPS
#' to county names using \code{tigris::counties}.  The intermediate helper
#' columns (\code{zip5} and \code{county_fips}) are removed before returning.
#'
#' @param df Data frame containing a ZIP code column.
#' @param state_abbr Two-letter state abbreviation (e.g., \code{"NC"}).
#' @param zip_county_path Path to a \code{zip_county.csv} reference file
#'   (optional).  Required columns: \code{ZIP}, \code{COUNTY} (FIPS code),
#'   \code{TOT_RATIO}.  When \code{NULL} (the default), the bundled
#'   \code{zip_county} package dataset is used.
#' @param zip_col Name of the ZIP code column in \code{df} (default
#'   \code{"zip"}).
#' @param tigris_year Year of TIGER/Line shapefiles to use (default
#'   \code{2022}).
#'
#' @return \code{df} with an added \code{county} character column of county
#'   names.  Rows without a ZIP-to-county match will have \code{NA} for
#'   \code{county}.
#'
#' @importFrom tigris counties
#' @importFrom sf st_drop_geometry
#' @export
add_county_from_zip <- function(df, state_abbr, zip_county_path = NULL,
                                zip_col = "zip", tigris_year = 2022) {
  if (is.null(zip_county_path)) {
    data("zip_county", package = "tidyschoolvax", envir = environment())
    zip_raw <- zip_county
  } else {
    zip_raw <- utils::read.csv(zip_county_path)
  }
  zip_ref <- zip_raw %>%
    dplyr::mutate(zip5 = stringr::str_pad(as.character(ZIP), 5, pad = "0")) %>%
    dplyr::group_by(zip5) %>%
    dplyr::arrange(dplyr::desc(TOT_RATIO)) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::select(zip5, county_fips = COUNTY)

  old_tigris_use_cache <- getOption("tigris_use_cache")
  on.exit(options(tigris_use_cache = old_tigris_use_cache), add = TRUE)
  options(tigris_use_cache = TRUE)
  state_counties <- tigris::counties(state = state_abbr, year = tigris_year) %>%
    sf::st_drop_geometry() %>%
    dplyr::select(county_fips = GEOID, county = NAME) %>%
    dplyr::mutate(county_fips = as.character(county_fips))

  df %>%
    dplyr::mutate(
      zip5 = stringr::str_pad(as.character(.data[[zip_col]]), 5, pad = "0")
    ) %>%
    dplyr::left_join(zip_ref, by = "zip5") %>%
    dplyr::mutate(county_fips = as.character(county_fips)) %>%
    dplyr::left_join(state_counties, by = "county_fips") %>%
    dplyr::select(-zip5, -county_fips)
}
