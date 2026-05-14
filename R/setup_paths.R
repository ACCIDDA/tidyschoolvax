#' Set Up Preprocessing Pipeline Paths and Directories
#'
#' @description
#' Computes all standard directory paths used by the preprocessing pipeline
#' for a given state and project root, creates any missing directories, and
#' returns the paths as a named list.
#'
#' @param project_root Character string giving the absolute path to the project
#'   root directory. Defaults to \code{getwd()}.
#' @param state Character string, two-letter lower-case state abbreviation
#'   (e.g., \code{"md"}, \code{"nc"}).
#'
#' @return A named list with the following components:
#'   \describe{
#'     \item{state_dir}{State-specific directory
#'       (\code{00_preprocessing/states/<state>})}
#'     \item{raw_data_dir}{Raw downloaded data directory
#'       (\code{state_dir/00_raw_data})}
#'     \item{temp_data_dir}{Temporary cleaning outputs directory
#'       (\code{state_dir/01_cleaning/cleaning_temp})}
#'     \item{state_geo_dir}{State-specific geocoding directory
#'       (\code{state_dir/02_geocoding})}
#'     \item{clean_data_dir}{Final cleaned data ready for modelling
#'       (\code{state_dir/03_cleaned_data})}
#'     \item{outputs_data_dir}{Model-ready output directory
#'       (\code{state_dir/01_cleaning/output})}
#'     \item{general_data_dir}{Shared data directory used by all states
#'       (\code{project_root/data})}
#'     \item{kinder_dir}{Kindergarten vaccination data directory}
#'     \item{doe_dir}{Department of Education data directory}
#'     \item{greatschools_dir}{GreatSchools.org data directory}
#'     \item{vaxview_dir}{CDC VaxView data directory}
#'     \item{state_school_dir}{Any additional state school data directory}
#'   }
#'
#' @details
#' All directories that do not yet exist are created with
#' \code{dir.create(..., recursive = TRUE)}.  The returned list can be
#' unpacked into the calling environment with
#' \code{list2env(paths, envir = environment())}.
#'
#' @examples
#' \dontrun{
#' paths <- setup_paths(project_root = here::here(), state = "md")
#' list2env(paths, envir = environment())
#' }
#'
#' @export
setup_paths <- function(project_root = NULL, state) {
  if (is.null(project_root)) project_root <- getwd()

  if (!is.character(state) || !nzchar(state)) {
    stop("'state' must be a non-empty character string (e.g., \"md\").", call. = FALSE)
  }

  state_dir        <- file.path(project_root, "00_preprocessing/states", state)
  raw_data_dir     <- file.path(state_dir, "00_raw_data")
  temp_data_dir    <- file.path(state_dir, "01_cleaning/cleaning_temp")
  state_geo_dir    <- file.path(state_dir, "02_geocoding")
  clean_data_dir   <- file.path(state_dir, "03_cleaned_data")
  outputs_data_dir <- file.path(state_dir, "01_cleaning/output")
  general_data_dir <- file.path(project_root, "data")

  kinder_dir       <- file.path(raw_data_dir, "kindergarten_immunity")
  doe_dir          <- file.path(raw_data_dir, "doe")
  greatschools_dir <- file.path(raw_data_dir, "greatschools")
  vaxview_dir      <- file.path(raw_data_dir, "vaxview")
  state_school_dir <- file.path(raw_data_dir, "state_school_data")

  dirs_to_create <- c(
    state_dir, raw_data_dir, temp_data_dir, state_geo_dir, clean_data_dir, outputs_data_dir,
    general_data_dir, kinder_dir, doe_dir, greatschools_dir, vaxview_dir, state_school_dir
  )
  for (d in dirs_to_create) {
    if (!dir.exists(d)) dir.create(d, recursive = TRUE)
  }

  list(
    state_dir        = state_dir,
    raw_data_dir     = raw_data_dir,
    temp_data_dir    = temp_data_dir,
    state_geo_dir    = state_geo_dir,
    clean_data_dir   = clean_data_dir,
    outputs_data_dir = outputs_data_dir,
    general_data_dir = general_data_dir,
    kinder_dir       = kinder_dir,
    doe_dir          = doe_dir,
    greatschools_dir = greatschools_dir,
    vaxview_dir      = vaxview_dir,
    state_school_dir = state_school_dir
  )
}
