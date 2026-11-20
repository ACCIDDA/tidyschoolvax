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
#'     \item{state_dir}{The state's intermediate data
#'       (\code{data_processing/<state>})}
#'     \item{raw_data_dir}{Files exactly as received, one subfolder per source,
#'       with the files directly in it (\code{state_dir/raw}); ignored by git}
#'     \item{manual_dir}{Hand-obtained inputs that a script cannot re-create
#'       (\code{state_dir/raw/manual}); tracked}
#'     \item{temp_data_dir}{Everything the pipeline produces and a later step
#'       reads: the parsed source tables (\code{kinder_dat}, \code{doe_dat},
#'       \code{greatschools_dat}, ...), the step files, the NCES reference and
#'       step reports (\code{state_dir/processed}); ignored by git}
#'     \item{state_geo_dir}{Geocoding and status caches, which cost API
#'       requests to rebuild (\code{state_dir/geocoding}); tracked}
#'     \item{code_dir}{The state's scripts
#'       (\code{source/tidyschoolvax/<state>}); tracked}
#'     \item{final_dir}{What downstream code reads
#'       (\code{output/tidyschoolvax/<state>}); one subfolder per series;
#'       tracked}
#'     \item{clean_data_dir, outputs_data_dir}{Both are \code{final_dir}: the
#'       cleaned tables and the model inputs of a series sit in the same folder}
#'     \item{review_dir}{Small CSVs a person opens: data-quality flags and the
#'       unmatched-school review (\code{checks/tidyschoolvax/<state>}); tracked}
#'     \item{api_log_dir}{Google API call logs, shared by all states
#'       (\code{checks/tidyschoolvax/api_logs}); tracked}
#'     \item{general_data_dir}{Intermediate data shared by all states
#'       (\code{data_processing/shared})}
#'     \item{kinder_dir}{Where the kindergarten vaccination downloads go (in
#'       \code{raw_data_dir})}
#'     \item{doe_dir}{Where Department of Education downloads go (in
#'       \code{raw_data_dir})}
#'     \item{greatschools_dir}{Kept for compatibility: GreatSchools is scraped,
#'       so there is no download to store (in \code{raw_data_dir})}
#'     \item{vaxview_dir}{CDC VaxView data directory; national, so shared by all
#'       states (\code{general_data_dir/vaxview})}
#'     \item{state_school_dir}{Optional third school source, e.g.
#'       \code{other_dat.csv} (in \code{raw_data_dir})}
#'   }
#'
#' @details
#' The folders of the state and of the pipeline are created with
#' \code{dir.create(..., recursive = TRUE)}. The per-source folders under
#' \code{raw_data_dir} (\code{kinder_dir}, \code{doe_dir},
#' \code{greatschools_dir}, \code{state_school_dir}) are returned but not
#' created: a state only has the ones its download script writes to, so the
#' script creates them. The returned list can be unpacked into the calling
#' environment with \code{list2env(paths, envir = environment())}.
#'
#' Rule of thumb: \code{raw/} is what was received, \code{processed/} is what the
#' pipeline made, and the pipeline reads only from \code{processed/},
#' \code{raw/manual/} and \code{shared/}.
#'
#' Layout (T = tracked in git, I = ignored). Code, outputs and checks live
#' outside \code{data_processing/}, which holds intermediate data only; each of
#' \code{source/}, \code{output/} and \code{checks/} has a \code{tidyschoolvax/}
#' folder (this pipeline) and an \code{imugap/} folder (the model):
#' \preformatted{
#' data_processing/
#'   shared/vaxview/        national VaxView (vax_view.parquet is read; CSVs are sources)
#'   <state>/
#'     raw/                 I   files as received (one subfolder per source)
#'       manual/            T   hand-obtained inputs
#'     processed/           I   parsed tables, step files (.parquet), NCES, step reports
#'     geocoding/           T   geocoded_cache.rds, status_cache.rds
#' source/tidyschoolvax/<state>/   T   01_download.R, 03_state_cleaning.R, runner .Rmd
#' output/tidyschoolvax/<state>/<series>/   T   cleaned_data.csv, obs/locations/... .parquet
#' checks/tidyschoolvax/<state>/<series>/   T   data-quality flags (.csv)
#' checks/tidyschoolvax/api_logs/           T   Google API call logs
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

  # intermediate data only
  state_dir        <- file.path(project_root, "data_processing", state)
  raw_data_dir     <- file.path(state_dir, "raw")
  manual_dir       <- file.path(raw_data_dir, "manual")
  temp_data_dir    <- file.path(state_dir, "processed")
  state_geo_dir    <- file.path(state_dir, "geocoding")
  general_data_dir <- file.path(project_root, "data_processing", "shared")

  # code, outputs and checks of the tidyschoolvax pipeline
  code_dir         <- file.path(project_root, "source", "tidyschoolvax", state)
  final_dir        <- file.path(project_root, "output", "tidyschoolvax", state)
  clean_data_dir   <- final_dir
  outputs_data_dir <- final_dir
  review_dir       <- file.path(project_root, "checks", "tidyschoolvax", state)
  api_log_dir      <- file.path(project_root, "checks", "tidyschoolvax", "api_logs")

  kinder_dir       <- file.path(raw_data_dir, "kindergarten_immunity")
  doe_dir          <- file.path(raw_data_dir, "doe")
  greatschools_dir <- file.path(raw_data_dir, "greatschools")
  vaxview_dir      <- file.path(general_data_dir, "vaxview")
  state_school_dir <- file.path(raw_data_dir, "state_school_data")

  # kinder_dir, doe_dir, greatschools_dir and state_school_dir are not created: the
  # download script makes the ones a state uses
  dirs_to_create <- c(
    state_dir, raw_data_dir, manual_dir, temp_data_dir, state_geo_dir, general_data_dir,
    code_dir, final_dir, review_dir, api_log_dir, vaxview_dir
  )
  for (d in dirs_to_create) {
    if (!dir.exists(d)) dir.create(d, recursive = TRUE)
  }

  list(
    state_dir        = state_dir,
    raw_data_dir     = raw_data_dir,
    manual_dir       = manual_dir,
    temp_data_dir    = temp_data_dir,
    state_geo_dir    = state_geo_dir,
    code_dir         = code_dir,
    final_dir        = final_dir,
    clean_data_dir   = clean_data_dir,
    outputs_data_dir = outputs_data_dir,
    review_dir       = review_dir,
    api_log_dir      = api_log_dir,
    general_data_dir = general_data_dir,
    kinder_dir       = kinder_dir,
    doe_dir          = doe_dir,
    greatschools_dir = greatschools_dir,
    vaxview_dir      = vaxview_dir,
    state_school_dir = state_school_dir
  )
}
