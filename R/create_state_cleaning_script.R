#' Create a State-Specific State-Cleaning Script from the Generic Template
#'
#' @description
#' Copies the generic \code{03_state_cleaning.R} template into the conventional
#' location for a new state:
#' \code{<project_root>/00_preprocessing/states/<state>/01_cleaning/03_state_cleaning.R}
#'
#' The template provides a fully commented scaffold that covers all six
#' pipeline sections: data loading, state-specific column adjustments, manual
#' data-entry corrections, address/geocoding fixes, column-name standardisation
#' and school ID assignment, and export.  Every state-specific section is
#' marked with a \code{# CUSTOMIZE:} comment so the analyst knows exactly what
#' needs to be edited before running the script.
#'
#' @param state Two-letter, lower-case state abbreviation (e.g. \code{"nc"},
#'   \code{"md"}).  The function normalizes the value to lower case.
#' @param project_root Path to the repository root.  Defaults to
#'   \code{getwd()}.  The output file is written relative to this path.
#' @param overwrite Logical.  If \code{FALSE} (the default) the function
#'   stops with an informative error if the target file already exists.  Set to
#'   \code{TRUE} to replace an existing file.
#'
#' @return The path to the newly created script (invisibly).
#'
#' @details
#' The template bundled with the package is stored in
#' \code{inst/templates/03_state_cleaning.R} and is located at runtime via
#' \code{system.file()}.  This means the function works both when the package
#' is installed (\code{library(tidyschoolvax)}) and during development
#' with \code{devtools::load_all()}.
#'
#' @examples
#' \dontrun{
#' # Scaffold a state-cleaning script for a new state "tx" (Texas)
#' create_state_cleaning_script("tx")
#'
#' # Specify a project root explicitly and allow overwriting
#' create_state_cleaning_script("tx",
#'                              project_root = "/path/to/tidyschoolvax",
#'                              overwrite    = TRUE)
#' }
#'
#' @export
create_state_cleaning_script <- function(state,
                                         project_root = getwd(),
                                         overwrite    = FALSE) {

  # --- Validate inputs -------------------------------------------------------
  if (!is.character(state) || length(state) != 1L) {
    stop("'state' must be a single character string (e.g. \"nc\").",
         call. = FALSE)
  }
  state <- tolower(trimws(state))
  if (!grepl("^[a-z]{2}$", state)) {
    stop(
      "'state' must be a valid two-letter state abbreviation (e.g. \"nc\").",
      call. = FALSE
    )
  }

  if (!is.character(project_root) || length(project_root) != 1L) {
    stop("'project_root' must be a single character string.", call. = FALSE)
  }
  if (!dir.exists(project_root)) {
    stop("'project_root' directory does not exist: ", project_root, call. = FALSE)
  }

  # --- Locate the template ---------------------------------------------------
  template_path <- system.file("templates", "03_state_cleaning.R",
                               package = "tidyschoolvax",
                               mustWork = FALSE)

  if (!nzchar(template_path)) {
    stop(
      "Could not locate the bundled 03_state_cleaning.R template.\n",
      "Ensure the tidyschoolvax package is properly installed or loaded ",
      "with devtools::load_all().",
      call. = FALSE
    )
  }

  # --- Build destination path ------------------------------------------------
  dest_dir  <- file.path(project_root, "00_preprocessing", "states",
                         state, "01_cleaning")
  dest_file <- file.path(dest_dir, "03_state_cleaning.R")

  if (file.exists(dest_file) && !overwrite) {
    stop(
      "A state-cleaning script already exists at:\n  ", dest_file, "\n",
      "Set overwrite = TRUE to replace it.",
      call. = FALSE
    )
  }

  # --- Create directory and copy template ------------------------------------
  if (!dir.exists(dest_dir)) {
    ok <- dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
    if (!ok) {
      stop("Failed to create directory: ", dest_dir,
           "\nCheck that the path is valid and you have write permission.",
           call. = FALSE)
    }
    message("Created directory: ", dest_dir)
  }

  copied <- file.copy(from = template_path, to = dest_file, overwrite = overwrite)
  if (!copied) {
    stop("Failed to copy template to: ", dest_file,
         "\nCheck that the destination path is valid and you have write permission.",
         call. = FALSE)
  }

  message(
    "State cleaning script created at:\n  ", dest_file, "\n",
    "Next steps:\n",
    "  1. Open the file and search for '# CUSTOMIZE:' comments.\n",
    "  2. Set 'percent_cols' and 'count_cols' to match your state's column names.\n",
    "  3. Add any state-specific scale conversions in Part 2a.\n",
    "  4. Add manual data-entry corrections in Part 3.\n",
    "  5. Choose Option A or B in Part 4 for address/geocoding fixes.\n",
    "  6. Update the 'standardize_kinder_format()' call in Part 5 to match\n",
    "     your state's column names."
  )

  invisible(dest_file)
}
