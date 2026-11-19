#' Download State-Agnostic Data Sources
#'
#' @description
#' Downloads state-agnostic data sources for any US state. These downloads use
#' identical logic for all states, parameterized by state:
#' \itemize{
#'   \item GreatSchools.org school listings
#'   \item CDC VaxView vaccination coverage data (child, school, teen)
#'   \item NCES public (CCD) and private (PSS) school directories, when
#'     \code{nces_dir} is given (see \code{\link{download_nces_directories}})
#' }
#'
#' @param state Two-letter state abbreviation (e.g., \code{"nc"}).
#' @param state_name Full state name (e.g., \code{"North Carolina"}).
#' @param greatschools_dir Path to directory where GreatSchools data will be saved.
#' @param vaxview_dir Path to directory where VaxView data will be saved.
#' @param workers Number of parallel workers to use when scraping GreatSchools
#'   (default: 8).
#' @param nces_dir Directory for the NCES school directories
#'   (\code{setup_paths()$nces_dir}).  All years with school coordinates are
#'   downloaded (public schools from 2000-01, private from 2013-14).
#'   \code{NULL} (default) skips the NCES download;
#'   \code{\link{standardize_schools}} then downloads it itself.
#'
#' @return Invisibly returns a list with three elements:
#'   \describe{
#'     \item{greatschools_dat}{Data frame of GreatSchools school listings.}
#'     \item{vaxview_dat}{Data frame of combined CDC VaxView vaccination data.}
#'     \item{nces_dat}{List of the NCES \code{ccd} and \code{pss}
#'       directories, or \code{NULL} when \code{nces_dir} is \code{NULL} or
#'       the download failed.}
#'   }
#'
#' @details
#' The function saves the following files:
#' \itemize{
#'   \item \code{<greatschools_dir>/greatschools_dat.csv}
#'   \item \code{<greatschools_dir>/greatschools_dat.rds}
#'   \item \code{<vaxview_dir>/ChildVaxView.csv}
#'   \item \code{<vaxview_dir>/SchoolVaxView.csv}
#'   \item \code{<vaxview_dir>/TeenVaxView.csv}
#'   \item \code{<vaxview_dir>/vax_view.parquet}
#'   \item \code{<nces_dir>/nces_ccd_directory_<state>.rds} and
#'     \code{nces_pss_directory_<state>.rds}
#' }
#'
#' @examples
#' \dontrun{
#' result <- download_agnostic_data(
#'   state          = "nc",
#'   state_name     = "North Carolina",
#'   greatschools_dir = "path/to/greatschools",
#'   vaxview_dir      = "path/to/vaxview",
#'   nces_dir         = "path/to/nces"
#' )
#' }
#'
#' @importFrom readr write_csv
#' @export
download_agnostic_data <- function(state,
                                   state_name,
                                   greatschools_dir,
                                   vaxview_dir,
                                   workers = 8,
                                   nces_dir = NULL) {

  # ---- PART 1. GreatSchools.org Data ----

  cat("  Downloading GreatSchools.org data for", toupper(state), "...\n")

  greatschools_dat <- scrape_greatschools_schools(state_abbr = state, workers = workers)
  greatschools_dat <- clean_school_types(greatschools_dat, "schoolType")

  readr::write_csv(greatschools_dat, file.path(greatschools_dir, "greatschools_dat.csv"))
  saveRDS(greatschools_dat, file.path(greatschools_dir, "greatschools_dat.rds"))

  message("GreatSchools data saved to: ", greatschools_dir)


  # ---- PART 2. CDC VaxView Data ----

  cat("  Downloading CDC VaxView data for", state_name, "...\n")

  school_vaxview_path <- file.path(vaxview_dir, "SchoolVaxView.csv")
  child_vaxview_path  <- file.path(vaxview_dir, "ChildVaxView.csv")
  teen_vaxview_path   <- file.path(vaxview_dir, "TeenVaxView.csv")
  vax_data_path       <- file.path(vaxview_dir, "vax_view.parquet")

  vaxview_dat <- setup_vaxview_data(state_name,
                                    child_vaxview_path,
                                    school_vaxview_path,
                                    teen_vaxview_path,
                                    vax_data_path,
                                    pull_new = TRUE)

  message("VaxView data saved to: ", vaxview_dir)


  # ---- PART 3. NCES school directories ----

  nces_dat <- NULL
  if (!is.null(nces_dir)) {
    cat("  Downloading NCES school directories for", toupper(state), "...\n")
    nces_dat <- download_nces_directories(state, nces_dir)
  }

  invisible(list(greatschools_dat = greatschools_dat, vaxview_dat = vaxview_dat,
                 nces_dat = nces_dat))
}
