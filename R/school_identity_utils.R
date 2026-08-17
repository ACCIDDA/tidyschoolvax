# ==============================================================================
# school_identity_utils.R
# Purpose: Resolve a school's stable Google identity (place_id + Google's
#   current name for that place) EARLY in the pipeline — right after each
#   source is cleaned — so it can be used as a matching signal, not just as
#   end-of-pipeline enrichment. This is what lets the matching cascade in
#   standardize_schools.R bridge a school rename: two records at the same
#   physical place, under different name strings in different years/sources,
#   resolve to the same place_id, and Google Places can report what that
#   place is called *now*.
#
#   Deliberately thin: all the actual geocoding/caching/rate-limiting work is
#   done by the existing run_full_geocoding() / run_school_status_with_cache()
#   machinery in googleapi_utils.R. Nothing here re-implements an HTTP call.
# ==============================================================================


#' Resolve Google \code{place_id} for a table of schools
#'
#' Thin wrapper around \code{\link{run_full_geocoding}} that geocodes just
#' enough to attach a \code{place_id} to each row with a usable address,
#' reusing the same on-disk cache (\code{geocoded_cache.rds}) that the
#' pipeline's final geocoding pass reads from later — so calling this early
#' does not meaningfully increase Google API usage, it just moves the calls
#' earlier so the identity is available in time to be used for matching.
#'
#' @param df A data frame of (near-)unique schools.
#' @param geo_dir Path to the directory used for the geocoding cache.
#' @param google_api_key Google Geocoding API key.
#' @param state_abbr Optional two-letter state abbreviation used to fill in
#'   a \code{state} column when \code{df} doesn't already have one (e.g.
#'   kindergarten data, whose \code{state} is otherwise only assigned much
#'   later in the pipeline). Ignored when \code{state_col} is already present.
#' @param name_col,addr_col,city_col,state_col,zip_col,county_col Column
#'   names in \code{df} to treat as \code{school_name}, \code{addr_clean},
#'   \code{city}, \code{state}, \code{zip}, and \code{county_std} for the
#'   duration of the geocoding call (the columns \code{run_full_geocoding()}
#'   expects); \code{df} is returned with its original column names intact.
#' @param parallel_cache,api_qps See \code{\link{run_full_geocoding}}.
#'
#' @return \code{df} with a \code{place_id} column added (\code{NA} for rows
#'   that couldn't be geocoded, or when no address/state information was
#'   available at all).
#' @export
resolve_school_place_ids <- function(df,
                                     geo_dir,
                                     google_api_key,
                                     state_abbr    = NULL,
                                     name_col      = "school_name",
                                     addr_col      = "addr_clean",
                                     city_col      = "city",
                                     state_col     = "state",
                                     zip_col       = "zip",
                                     county_col    = "county_std",
                                     parallel_cache = TRUE,
                                     api_qps        = 50) {

  if (nrow(df) == 0) {
    df$place_id <- character(0)
    return(df)
  }

  rename_map <- stats::setNames(
    c(name_col, addr_col, city_col, state_col, zip_col, county_col),
    c("school_name", "addr_clean", "city", "state", "zip", "county_std")
  )
  rename_map <- rename_map[rename_map %in% names(df) & rename_map != names(rename_map)]

  std <- df
  if (length(rename_map) > 0) {
    std <- std %>% dplyr::rename(dplyr::any_of(rename_map))
  }

  if (!"state" %in% names(std)) {
    if (is.null(state_abbr)) {
      warning("resolve_school_place_ids(): no state column found and no ",
              "state_abbr supplied; skipping place_id resolution.", call. = FALSE)
      df$place_id <- NA_character_
      return(df)
    }
    std$state <- state_abbr
  }

  if (!"school_name" %in% names(std) || all(is.na(std$school_name))) {
    df$place_id <- NA_character_
    return(df)
  }

  if (!dir.exists(geo_dir)) dir.create(geo_dir, recursive = TRUE)

  # Row-id join back to df rather than positional assignment: run_full_geocoding()
  # chunks internally and does not guarantee within-chunk row order is preserved
  # (already-cached rows and freshly-geocoded rows are recombined separately).
  std$.resolve_row_id <- seq_len(nrow(std))

  geocoded <- run_full_geocoding(
    unique_schools = std,
    geo_dir        = geo_dir,
    google_api_key = google_api_key,
    parallel_cache = parallel_cache,
    api_qps        = api_qps
  )

  place_id_lookup <- geocoded %>%
    dplyr::select(.resolve_row_id, place_id) %>%
    dplyr::distinct(.resolve_row_id, .keep_all = TRUE)

  df$.resolve_row_id <- seq_len(nrow(df))
  df <- df %>%
    dplyr::left_join(place_id_lookup, by = ".resolve_row_id") %>%
    dplyr::select(-.resolve_row_id)

  df
}


#' Add Google's current place name as a matching key
#'
#' For every row with a resolved \code{place_id}, looks up (via the same
#' cached \code{\link{run_school_status_with_cache}} call the pipeline's
#' final enrichment step already makes) the name Google currently has on
#' file for that physical location, and adds it — standardized the same way
#' as every other school-name matching key — as \code{school_name_std_google}.
#' A kinder-year record using an old name can then be matched against a
#' reference row exposing Google's updated name (or vice versa) without
#' either side needing to have been re-geocoded.
#'
#' @param df A data frame with a \code{place_id} column (e.g. the output of
#'   \code{\link{resolve_school_place_ids}}).
#' @param geo_dir Path to the directory used for the status/name cache.
#' @param google_api_key Google Places API key.
#'
#' @return \code{df} with a \code{school_name_std_google} column added
#'   (\code{NA} where no place name could be resolved).
#' @export
add_google_name_variants <- function(df, geo_dir, google_api_key) {

  if (!"place_id" %in% names(df) || all(is.na(df$place_id))) {
    df$school_name_std_google <- NA_character_
    return(df)
  }

  if (!dir.exists(geo_dir)) dir.create(geo_dir, recursive = TRUE)

  place_ids <- df %>%
    dplyr::filter(!is.na(place_id), place_id != "") %>%
    dplyr::distinct(place_id)

  if (nrow(place_ids) == 0) {
    df$school_name_std_google <- NA_character_
    return(df)
  }

  status <- run_school_status_with_cache(
    place_ids, geo_dir = geo_dir, google_api_key = google_api_key
  )

  lookup <- status %>%
    dplyr::filter(!is.na(place_id), !is.na(google_place_name)) %>%
    dplyr::distinct(place_id, .keep_all = TRUE) %>%
    dplyr::transmute(place_id, school_name_std_google = standardized_school_name(google_place_name))

  df %>% dplyr::left_join(lookup, by = "place_id")
}


#' Add a Google-name synonym row for schools whose Google name diverges
#'
#' Duplicates each row of \code{ref_df} whose \code{school_name_std_google}
#' differs from \code{school_name_std}, with \code{school_name_std}
#' overwritten by the Google name, and row-binds the duplicate back onto
#' \code{ref_df}. Both the original and duplicate row carry the same
#' \code{data2_id}, so whichever name variant wins a fuzzy match resolves to
#' the same reference school downstream. Rows where Google's name is missing
#' or identical to what's already on file are left as a single row (keeping
#' the augmented table close to the same size as the original in the common,
#' non-renamed case).
#'
#' @param ref_df A reference-school data frame with \code{school_name_std}
#'   and \code{school_name_std_google} columns.
#' @param name_col Name of the standardized-name column to synonym-augment
#'   (default \code{"school_name_std"}).
#' @param google_col Name of the Google-name column (default
#'   \code{"school_name_std_google"}).
#'
#' @return \code{ref_df} with extra synonym rows appended.
#' @export
augment_with_google_name_variant <- function(ref_df,
                                             name_col   = "school_name_std",
                                             google_col = "school_name_std_google") {

  if (!google_col %in% names(ref_df)) return(ref_df)

  variant_rows <- ref_df %>%
    dplyr::filter(
      !is.na(.data[[google_col]]),
      .data[[google_col]] != .data[[name_col]]
    ) %>%
    dplyr::mutate(!!name_col := .data[[google_col]])

  dplyr::bind_rows(ref_df, variant_rows)
}


#' Collapse rows within a single cleaned source that share a Google place_id
#'
#' Two rows in the \emph{same} source (e.g. two GreatSchools listings, or two
#' DOE listings) that resolved to the same Google \code{place_id} are the
#' same physical school, regardless of how differently their names are
#' spelled — no fuzzy-matching judgment call needed. This collapses each such
#' group down to one canonical row (the first occurrence, by \code{id_col})
#' \strong{before} that source is cross-matched against another one, so
#' cross-source matching (\code{\link{build_reference_key}}) never has to
#' silently pick between two internal duplicates.
#'
#' Rows without a resolved \code{place_id} (or when the column is absent
#' entirely) pass through unchanged, each treated as its own group of one.
#'
#' @param df A cleaned source data frame (e.g. \code{greatschools_dat} or
#'   \code{doe_dat}) with a \code{place_id} column and a stable per-row id
#'   column.
#' @param id_col Name of the row-identity column (e.g. \code{"data1_id"} or
#'   \code{"data2_id"}).
#' @param name_col Name of the raw school-name column.
#' @param name_std_col Name of the standardized school-name column.
#' @param source_label Character scalar recorded in the \code{source} column
#'   of the returned crosswalk (e.g. \code{"greatschools"}, \code{"doe"}).
#'
#' @return A named list:
#' \describe{
#'   \item{data}{\code{df} with collapsed-duplicate rows dropped, keeping only
#'     the canonical row per \code{place_id} group. All original columns are
#'     preserved.}
#'   \item{crosswalk}{A tibble with one row per \emph{original} input row —
#'     \code{source}, \code{orig_id}, \code{orig_name}, \code{orig_name_std},
#'     \code{place_id}, \code{new_id}, \code{new_name}, \code{new_name_std},
#'     \code{n_collapsed} (group size, including the survivor), and
#'     \code{collapsed} (\code{TRUE} when this row was dropped in favor of a
#'     different canonical row). Rows that were never part of a multi-row
#'     group have \code{new_id == orig_id} and \code{collapsed == FALSE}.}
#' }
#' @export
collapse_reference_by_place_id <- function(df, id_col, name_col, name_std_col, source_label) {

  empty_crosswalk <- tibble::tibble(
    source = character(0), orig_id = character(0), orig_name = character(0),
    orig_name_std = character(0), place_id = character(0), new_id = character(0),
    new_name = character(0), new_name_std = character(0),
    n_collapsed = integer(0), collapsed = logical(0)
  )

  if (nrow(df) == 0) return(list(data = df, crosswalk = empty_crosswalk))

  if (!"place_id" %in% names(df)) df$place_id <- NA_character_

  df2 <- df %>%
    dplyr::mutate(
      .orig_id       = as.character(.data[[id_col]]),
      .orig_name     = .data[[name_col]],
      .orig_name_std = .data[[name_std_col]],
      # Rows without a resolved place_id are each their own group of one —
      # a synthetic key unique to that row, so they never collapse together.
      .group_key     = dplyr::if_else(
        !is.na(place_id) & place_id != "",
        place_id,
        paste0("__row__", .orig_id)
      )
    )

  canonical <- df2 %>%
    dplyr::group_by(.group_key) %>%
    dplyr::slice(1L) %>%
    dplyr::ungroup() %>%
    dplyr::transmute(.group_key, new_id = .orig_id, new_name = .orig_name,
                     new_name_std = .orig_name_std)

  group_sizes <- df2 %>% dplyr::count(.group_key, name = "n_collapsed")

  crosswalk <- df2 %>%
    dplyr::select(.group_key, .row_place_id = place_id,
                  orig_id = .orig_id, orig_name = .orig_name,
                  orig_name_std = .orig_name_std) %>%
    dplyr::left_join(canonical,    by = ".group_key") %>%
    dplyr::left_join(group_sizes,  by = ".group_key") %>%
    dplyr::transmute(
      source = source_label,
      orig_id, orig_name, orig_name_std,
      place_id = .row_place_id,
      new_id, new_name, new_name_std,
      n_collapsed,
      collapsed = orig_id != new_id
    )

  kept_ids  <- unique(crosswalk$new_id)
  kept_data <- df[as.character(df[[id_col]]) %in% kept_ids, , drop = FALSE]

  list(data = kept_data, crosswalk = crosswalk)
}


#' Write a QC report of school-name pairs reconciled during matching
#'
#' Filters a matched-schools data frame (the \code{matched} element returned
#' by \code{\link{match_schools_names}} or \code{\link{run_matching_cascade}})
#' to rows where the two sides' standardized names differ, and writes them to
#' \code{school_renames_detected.csv} for human review — most usefully sorted
#' so identity-based matches (\code{match_method == "place_id"}, the
#' strongest rename signal: same physical place, different name string) sort
#' to the top.
#'
#' @param matched_df A matched-schools data frame with
#'   \code{school_name_std_data1}, \code{school_name_std_data2}, and
#'   (optionally) \code{match_method}, \code{match_score}, \code{county_std}
#'   columns.
#' @param out_dir Directory to write \code{school_renames_detected.csv} to.
#'
#' @return The filtered data frame of detected renames (invisibly usable;
#'   also written to disk). Returns an empty tibble, without writing a file,
#'   when \code{matched_df} lacks the required columns or no renames are
#'   found.
#' @export
log_school_renames <- function(matched_df, out_dir) {

  required_cols <- c("school_name_std_data1", "school_name_std_data2")
  if (is.null(matched_df) || nrow(matched_df) == 0 ||
      !all(required_cols %in% names(matched_df))) {
    return(invisible(tibble::tibble()))
  }

  renames <- matched_df %>%
    dplyr::filter(
      !is.na(school_name_std_data1), !is.na(school_name_std_data2),
      school_name_std_data1 != school_name_std_data2
    )

  if ("match_method" %in% names(renames)) {
    renames <- renames %>%
      dplyr::arrange(dplyr::desc(match_method == "place_id"),
                     dplyr::across(dplyr::any_of(c("county_std", "school_name_std_data1"))))
  }

  if (nrow(renames) == 0) {
    message("No school renames detected.")
    return(invisible(renames))
  }

  message(nrow(renames), " school name pair(s) reconciled during matching — ",
          "see school_renames_detected.csv for review.")
  write_report(renames, report_name = "school_renames_detected", out_dir = out_dir)

  renames
}
