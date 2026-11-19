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
#'     \item{code_dir}{The state's scripts (\code{state_dir/code}); tracked}
#'     \item{raw_data_dir}{Downloads and the tables parsed from them, by source
#'       (\code{state_dir/raw}); ignored by git}
#'     \item{manual_dir}{Hand-exported inputs that a script cannot re-create
#'       (\code{state_dir/raw/manual}); tracked}
#'     \item{temp_data_dir}{Work files that a later step reads, plus step
#'       reports (\code{state_dir/work}); ignored by git}
#'     \item{state_geo_dir}{Geocoding and status caches
#'       (\code{state_dir/geocoding}); tracked}
#'     \item{review_dir}{Small CSVs a person opens: data-quality flags and the
#'       unmatched-school review (\code{state_dir/review}); tracked}
#'     \item{final_dir}{What downstream code reads (\code{state_dir/final}); one
#'       subfolder per series; tracked}
#'     \item{clean_data_dir, outputs_data_dir}{Both are \code{final_dir}: the
#'       cleaned tables and the model inputs of a series sit in the same folder}
#'     \item{general_data_dir}{Data shared by all states
#'       (\code{00_preprocessing/shared})}
#'     \item{kinder_dir}{Kindergarten vaccination data directory (in
#'       \code{raw_data_dir})}
#'     \item{doe_dir}{Department of Education data directory (in
#'       \code{raw_data_dir})}
#'     \item{greatschools_dir}{GreatSchools.org data directory (in
#'       \code{raw_data_dir})}
#'     \item{vaxview_dir}{CDC VaxView data directory; national, so shared by all
#'       states (\code{general_data_dir/vaxview})}
#'     \item{state_school_dir}{Any additional state school data directory (in
#'       \code{raw_data_dir})}
#'   }
#'
#' @details
#' All directories that do not yet exist are created with
#' \code{dir.create(..., recursive = TRUE)}.  The returned list can be
#' unpacked into the calling environment with
#' \code{list2env(paths, envir = environment())}.
#'
#' Layout (T = tracked in git, I = ignored):
#' \preformatted{
#' 00_preprocessing/
#'   shared/vaxview/        national VaxView (vax_view.parquet is read; CSVs are sources)
#'   states/<state>/
#'     code/                T   01_download.R, 03_state_cleaning.R
#'     raw/                 I   downloads and parsed tables
#'       manual/            T   hand-exported inputs
#'     work/                I   step files (.parquet) and step reports
#'     geocoding/           T   caches (.rds)
#'     review/<series>/     T   data-quality flags (.csv)
#'     final/<series>/      T   cleaned_data.csv, obs/locations/... .parquet
#' }
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
  code_dir         <- file.path(state_dir, "code")
  raw_data_dir     <- file.path(state_dir, "raw")
  manual_dir       <- file.path(raw_data_dir, "manual")
  temp_data_dir    <- file.path(state_dir, "work")
  state_geo_dir    <- file.path(state_dir, "geocoding")
  review_dir       <- file.path(state_dir, "review")
  final_dir        <- file.path(state_dir, "final")
  clean_data_dir   <- final_dir
  outputs_data_dir <- final_dir
  general_data_dir <- file.path(project_root, "00_preprocessing/shared")

  kinder_dir       <- file.path(raw_data_dir, "kindergarten_immunity")
  doe_dir          <- file.path(raw_data_dir, "doe")
  greatschools_dir <- file.path(raw_data_dir, "greatschools")
  vaxview_dir      <- file.path(general_data_dir, "vaxview")
  state_school_dir <- file.path(raw_data_dir, "state_school_data")

  dirs_to_create <- c(
    state_dir, code_dir, raw_data_dir, manual_dir, temp_data_dir, state_geo_dir,
    review_dir, final_dir, general_data_dir, kinder_dir, doe_dir, greatschools_dir,
    vaxview_dir, state_school_dir
  )
  for (d in dirs_to_create) {
    if (!dir.exists(d)) dir.create(d, recursive = TRUE)
  }

  list(
    state_dir        = state_dir,
    code_dir         = code_dir,
    raw_data_dir     = raw_data_dir,
    manual_dir       = manual_dir,
    temp_data_dir    = temp_data_dir,
    state_geo_dir    = state_geo_dir,
    review_dir       = review_dir,
    final_dir        = final_dir,
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
