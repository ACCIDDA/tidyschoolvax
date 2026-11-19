# ==============================================================================
# nces_utils.R
# Purpose: Use the free NCES school directories as a source of school
#   open/closed status and current names, so schools NCES covers don't need a
#   paid Google Place Details lookup.
#
#   - Public schools: Common Core of Data (CCD) directory, one file per school
#     year, via the Urban Institute Education Data API (no key needed).
#     Gives an explicit status code each year; closed schools drop out of
#     later years.
#   - Private schools: Private School Survey (PSS), biennial, downloaded from
#     NCES. Has no "closed" flag, so it is only used to confirm a school is
#     open (present in the latest edition); anything else falls back to Google.
#
#   Google place_ids are linked to NCES schools by location + name
#   (match_places_to_nces()); run_school_status_with_cache() then fills status
#   and name from NCES for matched place_ids and only sends the rest to Google.
# ==============================================================================


.ccd_api_url <- "https://educationdata.urban.org/api/v1/schools/ccd/directory/%d/?fips=%s"
.pss_zip_url <- "https://nces.ed.gov/surveys/pss/zip/pss%02d%02d_pu_csv.zip"

# CCD school_status codes (Urban Institute coding) -> Google-style status.
# 7 ("future") is left NA so Google decides.
.ccd_open_codes   <- c(1, 3, 4, 5, 8)  # open, new, added, changed agency, reopened
.ccd_closed_codes <- 2

# Directory columns kept from the CCD API: status/matching fields, plus the
# state's own school code, grades, enrollment and address for later steps.
.ccd_keep_cols <- c("year", "ncessch", "seasch", "school_name", "school_status",
                    "charter", "school_level", "lowest_grade_offered",
                    "highest_grade_offered", "enrollment", "street_location",
                    "city_location", "zip_location", "county_code",
                    "latitude", "longitude")
.ccd_inactive_codes <- 6


#' Download the NCES public and private school directories for a state
#'
#' Downloads (or reads from cache) every year of the NCES directories that
#' has school coordinates -- the Common Core of Data public-school directory
#' from 2000-01 and every Private School Survey edition from 2013-14 (the
#' first with a CSV file) -- up to the latest available, the same way the
#' other state-agnostic sources are pulled in full.  They are saved as they
#' are, one row per school per year, in \code{nces_dir}:
#' \itemize{
#'   \item \code{nces_ccd_directory_<state>.rds}: \code{year},
#'     \code{ncessch}, \code{seasch} (the state's own school code),
#'     \code{school_name}, \code{school_status}, \code{charter}, grades
#'     offered, \code{enrollment}, address and county columns, and
#'     \code{latitude}/\code{longitude}.
#'   \item \code{nces_pss_directory_<state>.rds}: \code{year} (the spring year
#'     of the edition), \code{ppin}, \code{school_name}, address (physical
#'     location where reported, else mailing), \code{county_fips}, and
#'     \code{latitude}/\code{longitude}.
#' }
#' This is the one NCES download the pipeline makes:
#' \code{\link{download_agnostic_data}} calls it in step 1, and
#' \code{\link{standardize_schools}} (through \code{\link{load_nces_reference}})
#' reads the same files in step 2.
#'
#' Each file is re-downloaded once it is older than \code{max_age_days} (new
#' CCD years are released annually).  If a download fails, a stale copy is used when there is
#' one.  If only the private-school download fails and there is no copy, the
#' public schools are still saved and returned, and the private schools are
#' tried again next time.
#'
#' @param state_abbr Two-letter state abbreviation (e.g. \code{"md"}).
#' @param nces_dir Directory for the saved directories
#'   (\code{setup_paths()$nces_dir}).
#' @param max_age_days Re-download when a saved file is older than this.
#' @param refresh Logical; force a re-download.
#'
#' @return Invisibly, a list with \code{ccd} and \code{pss} (\code{NULL} when
#'   unavailable); \code{NULL} if no public-school directory could be loaded.
#' @export
download_nces_directories <- function(state_abbr, nces_dir,
                                      max_age_days = 180, refresh = FALSE) {
  .download_nces_directories(state_abbr, nces_dir, first_year = .nces_first_coord_year,
                             max_age_days = max_age_days, refresh = refresh)
}

# First CCD year with school coordinates; earlier years can't be matched to
# places, so a full download starts here.
.nces_first_coord_year <- 2000L
# First PSS edition (fall year) published as a CSV zip; earlier ones are 404s.
.pss_first_csv_edition <- 2013L

# Internal: download_nces_directories() for an explicit first year. Files that
# already reach back to first_year are reused; only a state whose vaccination
# data starts before 2001 asks for earlier years than the full download has.
.download_nces_directories <- function(state_abbr, nces_dir, first_year,
                                       max_age_days = 180, refresh = FALSE) {
  state_abbr <- toupper(state_abbr)
  first_year <- min(first_year, .nces_first_coord_year)
  paths <- .nces_directory_paths(nces_dir, state_abbr)

  usable <- function(x) {
    if (is.null(x) || refresh) return(FALSE)
    age <- as.numeric(difftime(Sys.time(), attr(x, "downloaded_at"), units = "days"))
    covers <- !is.null(attr(x, "first_year")) && attr(x, "first_year") <= first_year
    length(age) == 1 && age <= max_age_days && covers
  }
  read_cached <- function(path) if (file.exists(path)) readRDS(path) else NULL
  save_dir <- function(x, path) {
    attr(x, "downloaded_at") <- Sys.time()
    attr(x, "first_year") <- first_year
    if (!dir.exists(nces_dir)) dir.create(nces_dir, recursive = TRUE)
    saveRDS(x, path)
    x
  }

  # Public schools (CCD)
  ccd <- read_cached(paths[["ccd"]])
  if (!usable(ccd)) {
    fresh <- tryCatch(.download_ccd_directory(state_abbr, first_year = first_year),
                      error = function(e) {
      warning("NCES directory download failed (", conditionMessage(e), ")",
              if (!is.null(ccd)) "; using cached copy." else
                "; school status will come from Google only.", call. = FALSE)
      NULL
    })
    if (!is.null(fresh)) ccd <- save_dir(fresh, paths[["ccd"]])
  }
  if (is.null(ccd)) return(invisible(NULL))

  # Private schools (PSS)
  pss <- read_cached(paths[["pss"]])
  if (!usable(pss)) {
    fresh <- tryCatch(.download_pss_directory(state_abbr, first_year = first_year),
                      error = function(e) {
      if (!is.null(pss)) {
        warning("NCES private school survey download failed (", conditionMessage(e),
                "); using cached copy.", call. = FALSE)
      } else {
        warning("NCES private school survey download failed (", conditionMessage(e),
                "); continuing with public schools only.", call. = FALSE)
      }
      NULL
    })
    if (!is.null(fresh)) pss <- save_dir(fresh, paths[["pss"]])
  }

  message(sprintf("NCES directories for %s: %d public, %d private school-years (in %s).",
                  state_abbr, nrow(ccd), if (is.null(pss)) 0L else nrow(pss), nces_dir))
  invisible(list(ccd = ccd, pss = pss))
}

.nces_directory_paths <- function(nces_dir, state_abbr) {
  st <- tolower(state_abbr)
  c(ccd = file.path(nces_dir, sprintf("nces_ccd_directory_%s.rds", st)),
    pss = file.path(nces_dir, sprintf("nces_pss_directory_%s.rds", st)))
}


#' Load NCES public and private school directories for a state
#'
#' Reads the NCES directories saved by \code{\link{download_nces_directories}}
#' (downloading them first if they are missing or stale) and reduces the years from \code{first_year} on to one
#' row per NCES school with a Google-style \code{business_status}:
#' \itemize{
#'   \item Public (CCD): \code{OPERATIONAL} if listed as open in the latest
#'     year; \code{CLOSED_PERMANENTLY} if marked closed, or missing from the
#'     latest year; \code{CLOSED_TEMPORARILY} if inactive; \code{NA}
#'     otherwise (e.g. "future" schools).
#'   \item Private (PSS): \code{OPERATIONAL} if in the latest edition,
#'     otherwise \code{NA} (PSS can't distinguish closure from non-response).
#' }
#' Rows with \code{NA} status still match place_ids but leave the status
#' lookup to Google.  Only years from \code{first_year} on are used.
#'
#' If nothing can be loaded, \code{NULL} is returned with a warning, so
#' callers fall back to Google.
#'
#' @param state_abbr Two-letter state abbreviation (e.g. \code{"md"}).
#' @param cache_dir Directory holding the saved NCES directories
#'   (\code{setup_paths()$nces_dir}).
#' @param first_year First school year to use (fall year, e.g.
#'   \code{2015} for 2015-16). Older years add closed schools and former names.
#'   \code{\link{standardize_schools}} sets this to the year before the first
#'   year of vaccination data.
#' @param max_age_days,refresh Passed to
#'   \code{\link{download_nces_directories}}.
#'
#' @return A tibble with one row per NCES school: \code{nces_id},
#'   \code{nces_source} (\code{"nces_ccd"} or \code{"nces_pss"}),
#'   \code{nces_name}, \code{nces_names} (all names seen, \code{" | "}
#'   separated), \code{lat}, \code{lon}, \code{last_seen}, and
#'   \code{business_status}; or \code{NULL} if nothing could be loaded.
#' @export
load_nces_reference <- function(state_abbr, cache_dir, first_year = 2015,
                                max_age_days = 180, refresh = FALSE) {
  dirs <- .download_nces_directories(state_abbr, cache_dir, first_year = first_year,
                                     max_age_days = max_age_days, refresh = refresh)
  if (is.null(dirs)) return(NULL)
  from_first <- function(x) if (is.null(x)) NULL else x[x$year >= first_year, , drop = FALSE]
  dplyr::bind_rows(.summarise_ccd(from_first(dirs$ccd)), .summarise_pss(from_first(dirs$pss)))
}


# Internal: first NCES school year to pull for a state, the year before its
# first year of vaccination data. year_source holds the school year in any of
# the forms the state files use ("2015-2016", "ca_2013-2014", "md_15_16"), and
# NCES years are labelled by their fall year, so "2015-2016" gives 2014.
.nces_first_year <- function(year_source, fallback = 2015L) {
  x <- as.character(year_source)
  four_digit <- stringr::str_extract(x, "(?<![0-9])(19|20)[0-9]{2}(?![0-9])")
  two_digit  <- stringr::str_match(x, "(?<![0-9])([0-9]{2})[-_/][0-9]{2}(?![0-9])")[, 2]
  start <- dplyr::coalesce(as.integer(four_digit), 2000L + as.integer(two_digit))
  if (all(is.na(start))) {
    warning("Could not read a year from year_source; pulling NCES data from ",
            fallback, ".", call. = FALSE)
    return(fallback)
  }
  min(start, na.rm = TRUE) - 1L
}


# Internal: all CCD directory years from first_year to the latest one the
# Urban Institute API has, for one state.
.download_ccd_directory <- function(state_abbr, first_year) {
  fips <- tigris::fips_codes$state_code[match(state_abbr, tigris::fips_codes$state)]
  if (is.na(fips)) stop("unknown state: ", state_abbr)

  this_year <- as.integer(format(Sys.Date(), "%Y"))
  years <- list()
  for (y in this_year:first_year) {
    d <- .fetch_ccd_year(y, fips)
    # Skip not-yet-released years at the top; stop at the first gap after.
    if (nrow(d) == 0) {
      if (length(years) == 0) next else break
    }
    years[[as.character(y)]] <- d
  }
  if (length(years) == 0) stop("no CCD directory data returned")
  dplyr::bind_rows(years)
}

.fetch_ccd_year <- function(year, fips) {
  url <- sprintf(.ccd_api_url, year, as.integer(fips))
  pages <- list()
  while (!is.null(url)) {
    j <- jsonlite::fromJSON(url)
    if (length(j$results) > 0) {
      pages[[length(pages) + 1]] <- j$results %>%
        dplyr::select(dplyr::any_of(.ccd_keep_cols))
    }
    url <- j[["next"]]
  }
  if (length(pages) == 0) {
    return(tibble::tibble(year = integer(), ncessch = character(), school_name = character(),
                          school_status = integer(), latitude = numeric(), longitude = numeric()))
  }
  dplyr::bind_rows(pages)
}

# Internal: the PSS editions available from NCES that cover first_year onward.
.download_pss_directory <- function(state_abbr, first_year) {
  this_year <- as.integer(format(Sys.Date(), "%Y"))
  tmp <- tempfile("pss_")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  editions <- list()
  # PSS editions start in odd fall years (2021-22, 2023-24, ...), so going back
  # to first_year - 1 includes the edition in effect during first_year.
  last_start <- max(first_year - 1, .pss_first_csv_edition)
  for (start in seq(this_year - (this_year %% 2 == 0), last_start, by = -2)) {
    url <- sprintf(.pss_zip_url, start %% 100, (start + 1) %% 100)
    zip <- file.path(tmp, basename(url))
    if (!.download_pss_zip(url, zip)) next
    csv <- utils::unzip(zip, exdir = tmp)
    csv <- csv[grepl("\\.csv$", csv, ignore.case = TRUE)][1]
    x <- readr::read_csv(csv, show_col_types = FALSE, guess_max = 1e5,
                         col_types = readr::cols(.default = readr::col_character()))
    editions[[length(editions) + 1]] <- .parse_pss_edition(x, state_abbr, start)
  }
  if (length(editions) == 0) stop("no PSS editions found")
  dplyr::bind_rows(editions)
}

# Internal: TRUE once the zip is on disk, FALSE if NCES doesn't have this edition
# (404). Any other failure is an error: skipping a published edition would make
# an older one look like the latest, and schools only in that older edition
# would be reported as open.
.download_pss_zip <- function(url, dest) {
  res <- httr::GET(url, httr::write_disk(dest, overwrite = TRUE), httr::timeout(300))
  code <- httr::status_code(res)
  if (code == 404) return(FALSE)
  if (code != 200) stop("HTTP ", code, " for ", url)
  TRUE
}

# Internal: one PSS file reduced to a state's schools. Column names vary by
# edition: 2015-16 is lowercase, and the coordinates carry the year (LATITUDE24).
.parse_pss_edition <- function(x, state_abbr, start) {
  names(x) <- toupper(names(x))
  lat_col <- grep("^LATITUDE", names(x), value = TRUE)[1]
  lon_col <- grep("^LONGITUDE", names(x), value = TRUE)[1]
  x <- x[x$PSTABB %in% state_abbr, , drop = FALSE]
  # Physical location where the edition reports it (PL_*), else mailing address.
  col <- function(...) {
    out <- rep(NA_character_, nrow(x))
    for (nm in c(...)) if (nm %in% names(x)) out <- dplyr::coalesce(out, dplyr::na_if(x[[nm]], ""))
    out
  }
  tibble::tibble(
    ppin = x$PPIN, school_name = x$PINST,
    address = col("PL_ADD", "PADDRS"), city = col("PL_CIT", "PCITY"),
    zip = col("PL_ZIP", "PZIP"), county_fips = col("PCNTY"),
    latitude = as.numeric(x[[lat_col]]), longitude = as.numeric(x[[lon_col]]),
    year = start + 1L
  )
}

.summarise_ccd <- function(ccd) {
  latest <- max(ccd$year)
  # Status and last_seen use every year; only the coordinates skip years that
  # lack them, so a gap in the latest year can't make an open school look closed.
  ccd %>%
    dplyr::arrange(ncessch, dplyr::desc(year)) %>%
    dplyr::group_by(ncessch) %>%
    dplyr::summarise(
      nces_name   = dplyr::first(school_name),
      nces_names  = paste(unique(school_name), collapse = " | "),
      lat         = dplyr::first(latitude[!is.na(latitude) & !is.na(longitude)]),
      lon         = dplyr::first(longitude[!is.na(latitude) & !is.na(longitude)]),
      last_seen   = max(year),
      last_status = dplyr::first(school_status),
      .groups = "drop"
    ) %>%
    dplyr::filter(!is.na(lat), !is.na(lon)) %>%
    dplyr::mutate(
      business_status = dplyr::case_when(
        last_seen < latest                  ~ "CLOSED_PERMANENTLY",
        last_status %in% .ccd_closed_codes  ~ "CLOSED_PERMANENTLY",
        last_status %in% .ccd_inactive_codes ~ "CLOSED_TEMPORARILY",
        last_status %in% .ccd_open_codes    ~ "OPERATIONAL",
        TRUE                                ~ NA_character_
      )
    ) %>%
    dplyr::transmute(nces_id = as.character(ncessch), nces_source = "nces_ccd",
                     nces_name, nces_names, lat, lon,
                     last_seen = as.integer(last_seen), business_status)
}

.summarise_pss <- function(pss) {
  if (is.null(pss) || nrow(pss) == 0) return(NULL)
  latest <- max(pss$year)
  pss %>%
    dplyr::arrange(ppin, dplyr::desc(year)) %>%
    dplyr::group_by(ppin) %>%
    dplyr::summarise(
      nces_name  = dplyr::first(school_name),
      nces_names = paste(unique(school_name), collapse = " | "),
      lat        = dplyr::first(latitude[!is.na(latitude) & !is.na(longitude)]),
      lon        = dplyr::first(longitude[!is.na(latitude) & !is.na(longitude)]),
      last_seen  = max(year),
      .groups = "drop"
    ) %>%
    dplyr::filter(!is.na(lat), !is.na(lon)) %>%
    dplyr::transmute(nces_id = ppin, nces_source = "nces_pss", nces_name, nces_names,
                     lat, lon, last_seen = as.integer(last_seen),
                     business_status = dplyr::if_else(last_seen == latest, "OPERATIONAL",
                                                      NA_character_))
}


#' Link Google place_ids to NCES schools by location and name
#'
#' A place matches an NCES school when the two are within \code{max_dist_m}
#' metres and their standardized names (\code{\link{standardized_school_name}})
#' have Jaro-Winkler similarity of at least \code{min_name_sim}. Every name
#' NCES has listed for the school counts, so a renamed school still matches on
#' its former name. A place may carry several candidate names (one row each,
#' e.g. the source-data name and Google's name); the best-scoring pair of names
#' is used. More than \code{min_name_share} of a place's names must match the
#' school: a county-level result or a district office address can carry many
#' different schools, and a match for one of them says nothing about the rest.
#' When several NCES schools
#' qualify, one with a known open status is preferred (a new school on the
#' site of a closed one), then the most similar name, then the nearest.
#'
#' @param places Data frame with \code{place_id}, \code{lat}, \code{lon},
#'   and \code{name}; may contain several rows per \code{place_id}.
#' @param nces_ref Output of \code{\link{load_nces_reference}}.
#' @param max_dist_m Maximum distance in metres.
#' @param min_name_sim Minimum Jaro-Winkler similarity (0-1).
#' @param min_name_share A place's names must match the school in more than this
#'   share of cases (0-1).
#'
#' @return One row per matched \code{place_id} with \code{nces_id},
#'   \code{nces_source}, \code{nces_name} (the school's current name, even
#'   when a former name matched), \code{business_status},
#'   \code{dist_m}, and \code{name_sim}.
#' @export
match_places_to_nces <- function(places, nces_ref, max_dist_m = 300, min_name_sim = 0.85,
                                 min_name_share = 0.5) {
  empty <- tibble::tibble(place_id = character(), nces_id = character(),
                          nces_source = character(), nces_name = character(),
                          business_status = character(), dist_m = numeric(),
                          name_sim = numeric())
  if (is.null(nces_ref) || nrow(nces_ref) == 0) return(empty)

  p <- places %>%
    dplyr::filter(!is.na(place_id), !is.na(lat), !is.na(lon), !is.na(name), name != "") %>%
    dplyr::mutate(name_std = standardized_school_name(name)) %>%
    dplyr::distinct(place_id, lat, lon, name_std)
  if (nrow(p) == 0) return(empty)

  # Bounding-box pre-filter (non-equi join on latitude), then exact distance.
  dlat <- max_dist_m / 111320
  # One row per name the school has had; nces_name stays the current name.
  school_names <- strsplit(dplyr::coalesce(nces_ref$nces_names, nces_ref$nces_name), " | ",
                           fixed = TRUE)
  n <- data.table::as.data.table(nces_ref)[rep(seq_len(nrow(nces_ref)), lengths(school_names))]
  n[, `:=`(lat_lo = lat - dlat, lat_hi = lat + dlat,
           nces_name_std = standardized_school_name(unlist(school_names)))]
  data.table::setnames(n, c("lat", "lon"), c("n_lat", "n_lon"))
  pdt <- data.table::as.data.table(p)
  cand <- n[pdt, on = .(lat_lo <= lat, lat_hi >= lat), nomatch = 0L, allow.cartesian = TRUE,
            .(place_id, lat = i.lat, lon = i.lon, name_std, nces_id, nces_source, nces_name,
              nces_name_std, business_status, n_lat, n_lon)]
  if (nrow(cand) == 0) return(empty)

  place_names <- dplyr::summarise(dplyr::group_by(p, place_id), n_names = dplyr::n_distinct(name_std))

  tibble::as_tibble(cand) %>%
    dplyr::mutate(
      dist_m   = .haversine_m(lat, lon, n_lat, n_lon),
      name_sim = stringdist::stringsim(name_std, nces_name_std, method = "jw", p = 0.1)
    ) %>%
    dplyr::filter(dist_m <= max_dist_m, name_sim >= min_name_sim) %>%
    dplyr::left_join(place_names, by = "place_id") %>%
    dplyr::group_by(place_id, nces_id) %>%
    dplyr::filter(dplyr::n_distinct(name_std) / n_names > min_name_share) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(place_id, dplyr::desc(business_status %in% "OPERATIONAL"),
                   dplyr::desc(name_sim), dist_m) %>%
    dplyr::distinct(place_id, .keep_all = TRUE) %>%
    dplyr::select(dplyr::all_of(names(empty)))
}

.haversine_m <- function(lat1, lon1, lat2, lon2) {
  rad <- pi / 180
  a <- sin((lat2 - lat1) * rad / 2)^2 +
    cos(lat1 * rad) * cos(lat2 * rad) * sin((lon2 - lon1) * rad / 2)^2
  2 * 6371000 * asin(sqrt(pmin(1, a)))
}


# Internal: NCES status for the given place_ids. Coordinates and candidate
# names come from `df` (when it has them) and from the geocoding cache in
# geo_dir, which holds every place_id the pipeline has geocoded -- so this
# works even when the caller only has a place_id column.
.nces_status_for_place_ids <- function(place_ids, df, geo_dir, nces_ref) {
  geo_path <- .geocoding_cache_paths(geo_dir)[["geocoded"]]
  name_cols <- c("school_name", "school_name_std", "google_place_name")

  as_places <- function(x) {
    if (is.null(x) || !all(c("place_id", "lat", "lon") %in% names(x))) return(NULL)
    cols <- intersect(name_cols, names(x))
    if (length(cols) == 0) return(NULL)
    x %>%
      dplyr::filter(place_id %in% place_ids) %>%
      dplyr::select(place_id, lat, lon, dplyr::all_of(cols)) %>%
      dplyr::mutate(lat = as.numeric(lat), lon = as.numeric(lon),
                    dplyr::across(dplyr::all_of(cols), as.character)) %>%
      tidyr::pivot_longer(dplyr::all_of(cols), names_to = "name_source", values_to = "name") %>%
      dplyr::select(place_id, lat, lon, name)
  }

  places <- dplyr::bind_rows(
    as_places(df),
    as_places(if (file.exists(geo_path)) readRDS(geo_path) else NULL)
  )
  if (nrow(places) == 0) return(match_places_to_nces(places, NULL))

  match_places_to_nces(places, nces_ref) %>%
    dplyr::filter(!is.na(business_status))
}
