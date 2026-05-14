#' Download State-Agnostic Data Sources
#'
#' @description
#' Downloads state-agnostic data sources for any US state. These downloads use
#' identical logic for all states, parameterized by state:
#' \itemize{
#'   \item GreatSchools.org school listings
#'   \item CDC VaxView vaccination coverage data (child, school, teen)
#' }
#'
#' @param state Two-letter state abbreviation (e.g., \code{"nc"}).
#' @param state_name Full state name (e.g., \code{"North Carolina"}).
#' @param greatschools_dir Path to directory where GreatSchools data will be saved.
#' @param vaxview_dir Path to directory where VaxView data will be saved.
#' @param workers Number of parallel workers to use when scraping GreatSchools
#'   (default: 8).
#'
#' @return Invisibly returns a list with two elements:
#'   \describe{
#'     \item{greatschools_dat}{Data frame of GreatSchools school listings.}
#'     \item{vaxview_dat}{Data frame of combined CDC VaxView vaccination data.}
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
#' }
#'
#' @examples
#' \dontrun{
#' result <- download_agnostic_data(
#'   state          = "nc",
#'   state_name     = "North Carolina",
#'   greatschools_dir = "path/to/greatschools",
#'   vaxview_dir      = "path/to/vaxview"
#' )
#' }
#'
#' @importFrom readr write_csv
#' @export
download_agnostic_data <- function(state,
                                   state_name,
                                   greatschools_dir,
                                   vaxview_dir,
                                   workers = 8) {

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

  invisible(list(greatschools_dat = greatschools_dat, vaxview_dat = vaxview_dat))
}
