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
    # A target name (e.g. "school_name") can already exist in df as its own,
    # unrelated column — e.g. geocode_original_then_std()'s retry attempt
    # renames school_name_std -> school_name while df still carries its own
    # literal school_name column. Drop any such pre-existing target column
    # first so the rename can't produce duplicate column names.
    clobbered <- intersect(names(rename_map), names(std))
    if (length(clobbered) > 0) {
      std <- std %>% dplyr::select(-dplyr::all_of(clobbered))
    }
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


# Internal: union level_code across a (possibly "; "-packed) orig_id string,
# using a precomputed id -> level_code lookup vector. orig_id can already be
# packed at this point because build_location_key_table() may have merged
# several raw source rows into one location-key row before geocoding ever
# ran. Ids not present in the lookup (or with no level_code at all) simply
# contribute nothing to the union.
#' @keywords internal
.union_level_code <- function(orig_id_packed, level_code_lookup) {
  vapply(strsplit(orig_id_packed, ";\\s*"), function(ids) {
    codes <- unlist(strsplit(unname(level_code_lookup[ids]), ",\\s*"))
    codes <- unique(trimws(codes[!is.na(codes) & codes != ""]))
    if (length(codes) == 0) NA_character_ else paste(sort(codes), collapse = ",")
  }, character(1))
}


# Internal: pairwise-union two already-resolved level_code vectors (not ids —
# the level_code strings themselves), elementwise. Used when a source row
# place_id-matches an EXISTING master row rather than being appended as a
# new one: the source row is then discarded (master already represents the
# school), but its own level_code would otherwise be lost — e.g.
# GreatSchools lists only a preschool Head Start program at an address,
# while DOE independently confirms the same address also serves grades 1-5;
# without this, master's level_code stays preschool-only forever and the
# school silently fails the downstream elementary filter even though a
# geocoded, place_id-matchable source row proved otherwise. Found via a
# real MD run ("Patuxent Elementary Head Start" vs DOE's "Patuxent
# Elementary", same address, different names so exact-match never merges
# them itself).
#' @keywords internal
.union_level_code_pairs <- function(a, b) {
  mapply(function(x, y) {
    codes <- unlist(strsplit(c(x, y), ",\\s*"))
    codes <- unique(trimws(codes[!is.na(codes) & codes != ""]))
    if (length(codes) == 0) NA_character_ else paste(sort(codes), collapse = ",")
  }, a, b, USE.NAMES = FALSE)
}


# Internal: partition a group of rows that share a raw group key (i.e. the
# same place_id) into sub-groups by level_code compatibility — connected
# components under "level_code sets overlap" (share at least one grade-level
# code). A row with no determinable level_code is compatible with anything
# (empty set intersects trivially), so it joins whatever it's adjacent to
# rather than forcing its own sub-group. Two rows go in the SAME sub-group
# only if there's a compatibility path between them, directly or through
# other rows in the group — so {p}/{p,e,m}/{e} would all end up together
# (p<->p,e,m<->e), while {p}/{e} alone (no bridge) would split into two.
# Pure set/graph logic, no I/O — unit-testable without any pipeline
# fixtures.
#' @keywords internal
.level_code_subgroups <- function(level_codes) {
  n <- length(level_codes)
  if (n <= 1) return(rep(1L, n))

  codes <- lapply(strsplit(level_codes, ",", fixed = TRUE), function(x) {
    trimws(x[!is.na(x) & x != ""])
  })

  compatible <- function(a, b) {
    length(a) == 0 || length(b) == 0 || length(intersect(a, b)) > 0
  }

  parent <- seq_len(n)
  find <- function(x) {
    while (parent[x] != x) x <- parent[x]
    x
  }

  for (i in seq_len(n - 1)) {
    for (j in (i + 1):n) {
      if (compatible(codes[[i]], codes[[j]])) {
        ri <- find(i); rj <- find(j)
        if (ri != rj) parent[max(ri, rj)] <- min(ri, rj)
      }
    }
  }

  vapply(seq_len(n), find, integer(1))
}


#' Collapse rows within a single cleaned source that share a Google place_id
#'
#' Two rows in the \emph{same} source (e.g. two GreatSchools listings, or two
#' DOE listings) that resolved to the same Google \code{place_id} are usually
#' the same physical school, regardless of how differently their names are
#' spelled — no fuzzy-matching judgment call needed. This collapses each such
#' group down to one canonical row (the first occurrence, by \code{id_col})
#' \strong{before} that source is cross-matched against another one, so
#' cross-source matching (\code{\link{build_reference_key}}) never has to
#' silently pick between two internal duplicates.
#'
#' "Usually" the same school, not always: many campuses host a genuinely
#' separate ancillary program in the same building — a YMCA/preschool
#' childcare program sharing the elementary school's address is common, and
#' Google frequently resolves both to the identical place_id. Collapsing
#' those together would wrongly treat, say, a PK-only childcare listing and
#' the K-5 school itself as one school (found via a real CA run: "Antioch
#' YMCA Child Care-Laurel", level_code "p", vs "Laurel Elementary School",
#' level_code "e", sharing one building/place_id). When \code{source_df} and
#' \code{source_id_col} are supplied, two same-place_id rows are only
#' collapsed together if their \code{level_code} sets actually overlap (share
#' at least one grade-level code) — a row with no determinable level_code is
#' treated as compatible with anything, since there's no positive evidence
#' it's a different institution. Omit both (the default) to skip this check
#' entirely and collapse purely on place_id, matching the original behavior.
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
#' @param source_df,source_id_col Optional: the full cleaned source data
#'   frame and its id column name, used \emph{only} to look up
#'   \code{level_code} for the grade-level-compatibility check described
#'   above (not stored in the output). \code{id_col} in \code{df} may already
#'   be "; "-packed from \code{\link{build_location_key_table}}'s own
#'   within-source deduplication; level_code is unioned across every packed
#'   id before comparing. Both must be supplied together, and
#'   \code{source_df} must have a \code{level_code} column, for the check to
#'   run.
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
collapse_reference_by_place_id <- function(df, id_col, name_col, name_std_col, source_label,
                                           source_df = NULL, source_id_col = NULL) {

  empty_crosswalk <- tibble::tibble(
    source = character(0), orig_id = character(0), orig_name = character(0),
    orig_name_std = character(0), place_id = character(0), new_id = character(0),
    new_name = character(0), new_name_std = character(0),
    n_collapsed = integer(0), collapsed = logical(0)
  )

  if (nrow(df) == 0) return(list(data = df, crosswalk = empty_crosswalk))

  if (!"place_id" %in% names(df)) df$place_id <- NA_character_

  level_code_lookup <- if (!is.null(source_df) && !is.null(source_id_col) &&
                           "level_code" %in% names(source_df)) {
    stats::setNames(as.character(source_df$level_code), as.character(source_df[[source_id_col]]))
  } else {
    NULL
  }

  df2 <- df %>%
    dplyr::mutate(
      .orig_id       = as.character(.data[[id_col]]),
      .orig_name     = .data[[name_col]],
      .orig_name_std = .data[[name_std_col]],
      # Rows without a resolved place_id are each their own group of one —
      # a synthetic key unique to that row, so they never collapse together.
      .raw_group_key = dplyr::if_else(
        !is.na(place_id) & place_id != "",
        place_id,
        paste0("__row__", .orig_id)
      ),
      .level_code = if (!is.null(level_code_lookup)) {
        .union_level_code(.orig_id, level_code_lookup)
      } else {
        NA_character_
      }
    ) %>%
    # Within each raw (place_id-based) group, split further by grade-level
    # compatibility -- see docs above. A group with no level_code lookup
    # supplied, or where every member's level_code is unknown, gets exactly
    # one sub-group per raw group (identical to skipping this check).
    dplyr::group_by(.raw_group_key) %>%
    dplyr::mutate(.group_key = paste0(.raw_group_key, "__lvl", .level_code_subgroups(.level_code))) %>%
    dplyr::ungroup()

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


#' Build a deduplicated location-key table for one source
#'
#' Every source that feeds into matching (GreatSchools, DOE, kindergarten,
#' the optional third/state-specific source) names its columns slightly
#' differently — \code{street1} vs \code{street} vs an auto-detected
#' address column; \code{addr_clean} vs \code{addr_clean_kinder}; \code{city}
#' vs \code{city_kinder}; and so on. This function normalizes any one of
#' them into one common schema, and reduces it to the distinct combinations
#' of location-defining fields — the same exact-key deduplication idea
#' \code{build_kinder_unique_schools()} already applied to kinder alone,
#' generalized to every source so Google is geocoded once per distinct
#' location rather than once per raw row.
#'
#' Every original row is still reachable: \code{orig_id} packs the
#' contributing rows' \code{id_col} values ("; "-joined, matching the
#' \code{vacc_data_ids} packing convention already used elsewhere in this
#' package), so a caller can always expand a location-key row back out to
#' its original rows.
#'
#' @param df A cleaned source data frame (e.g. \code{greatschools_dat},
#'   \code{doe_dat}, or an already-aggregated kinder table).
#' @param source_label Character scalar recorded in the \code{source} column
#'   (e.g. \code{"greatschools"}, \code{"doe"}, \code{"kinder"}, \code{"other"}).
#' @param id_col Name of \code{df}'s row-identity column (e.g.
#'   \code{"data1_id"}, \code{"data2_id"}, \code{"vacc_school_id"}).
#' @param name_col,name_std_col Raw and standardized school-name columns.
#' @param addr_col,city_col,zip_col,state_col,county_col,district_col Names
#'   of the location columns to normalize, or \code{NULL} to skip a concept
#'   this source doesn't have (e.g. DOE may have no \code{state} column) —
#'   the corresponding output column is filled with \code{NA} either way, so
#'   every location-key table has the same shape regardless of source.
#'
#' @return A tibble with one row per distinct location: \code{source},
#'   \code{orig_id} (packed), \code{n_records} (how many original rows
#'   collapsed into this one), \code{school_name}, \code{school_name_std},
#'   \code{addr_clean}, \code{city}, \code{zip}, \code{state},
#'   \code{county_std}, \code{district_std}.
#' @export
build_location_key_table <- function(df,
                                     source_label,
                                     id_col,
                                     name_col     = "school_name",
                                     name_std_col = "school_name_std",
                                     addr_col     = "addr_clean",
                                     city_col     = "city",
                                     zip_col      = "zip",
                                     state_col    = "state",
                                     county_col   = "county_std",
                                     district_col = "district_std") {

  loc_cols <- c("school_name", "school_name_std", "addr_clean", "city",
                "zip", "state", "county_std", "district_std")
  empty <- tibble::tibble(
    source = character(0), orig_id = character(0), n_records = integer(0),
    school_name = character(0), school_name_std = character(0),
    addr_clean = character(0), city = character(0), zip = character(0),
    state = character(0), county_std = character(0), district_std = character(0)
  )

  if (nrow(df) == 0 || !id_col %in% names(df)) return(empty)

  rename_map <- c(
    school_name     = name_col,
    school_name_std = name_std_col,
    addr_clean      = addr_col,
    city            = city_col,
    zip             = zip_col,
    state           = state_col,
    county_std      = county_col,
    district_std    = district_col
  )
  rename_map <- rename_map[rename_map %in% names(df) & rename_map != names(rename_map)]

  std <- df %>%
    dplyr::mutate(.orig_id = as.character(.data[[id_col]])) %>%
    dplyr::rename(dplyr::any_of(rename_map))

  for (col in loc_cols) {
    if (!col %in% names(std)) std[[col]] <- NA_character_
  }
  std <- std %>%
    dplyr::mutate(
      dplyr::across(dplyr::all_of(loc_cols), as.character),
      # Blank strings and true NA should dedup together, not create two groups.
      dplyr::across(dplyr::all_of(loc_cols), ~ dplyr::na_if(.x, ""))
    )

  # Group on standardized name + every location field EXCEPT raw school_name
  # (raw name is display/geocoding-query material, not identity — that's
  # exactly what school_name_std is for; grouping on the raw string would
  # defeat its purpose).
  key_cols <- c("school_name_std", "addr_clean", "city", "zip",
               "county_std", "district_std")

  std %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(key_cols))) %>%
    dplyr::summarise(
      school_name = school_name[1L],
      state       = state[1L],
      orig_id     = paste(sort(unique(.orig_id)), collapse = "; "),
      n_records   = dplyr::n(),
      .groups     = "drop"
    ) %>%
    dplyr::mutate(source = source_label) %>%
    dplyr::select(source, orig_id, n_records, dplyr::all_of(loc_cols))
}


# Internal: which rows of a just-attempted geocode still lack a place_id and
# so need a second attempt. Factored out of geocode_original_then_std() so
# the retry-selection logic is unit-testable without a live API call.
#' @keywords internal
.rows_needing_retry <- function(place_id) {
  which(is.na(place_id) | place_id == "")
}


# Internal: fold a second-attempt geocoding result (run on the
# .rows_needing_retry() subset of attempt1) back into attempt1, and derive
# geocode_name_used ("original"/"standardized"/"failed"). Pure data
# manipulation — no API calls — so this is unit-testable on its own.
#' @keywords internal
.merge_geocode_retry <- function(attempt1, attempt2, retry_idx) {
  geocode_name_used <- rep("original", nrow(attempt1))
  geocode_name_used[retry_idx] <- "standardized"

  # attempt2 should have exactly one row per retry_idx entry with a place_id
  # column — but a malformed live API response for a pathological query
  # (e.g. a row with no usable name/address at all, producing an empty
  # geocoding query string) can occasionally come back missing that column,
  # or with a different row count, instead of a clean all-NA place_id.
  # Treat that as a failed retry rather than crashing the whole run: these
  # rows are exactly the ones meant to fall through to fuzzy matching as a
  # last resort, so silently failing them is the correct, intended outcome.
  attempt2_place_id <- if (nrow(attempt2) == length(retry_idx) && "place_id" %in% names(attempt2)) {
    attempt2$place_id
  } else {
    rep(NA_character_, length(retry_idx))
  }
  attempt1$place_id[retry_idx] <- attempt2_place_id

  still_failed <- is.na(attempt1$place_id) | attempt1$place_id == ""
  geocode_name_used[still_failed] <- "failed"

  attempt1$geocode_name_used <- geocode_name_used
  attempt1
}


#' Geocode a table, retrying on the standardized name where the original fails
#'
#' Calls \code{\link{resolve_school_place_ids}} once using \code{name_col}
#' (the source's original school name) for the query; any row that still
#' lacks a \code{place_id} afterward gets a second, independent attempt using
#' \code{name_std_col} (the standardized name) instead — retried on only
#' that subset, not the whole table. Adds \code{geocode_name_used}
#' (\code{"original"}, \code{"standardized"}, or \code{"failed"}) so it's
#' visible which attempt (if either) resolved each row — \code{"failed"}
#' rows are exactly the ones that should fall through to fuzzy matching as a
#' last resort, rather than being treated as an exact-match/place_id miss.
#'
#' Both attempts query Google with the same address/city/state/zip/county
#' context \code{\link{resolve_school_place_ids}} already appends — only the
#' name component of the query differs between them.
#'
#' @inheritParams resolve_school_place_ids
#' @param name_std_col Column to use for the query on the second (retry)
#'   attempt. Defaults to \code{"school_name_std"}.
#'
#' @return \code{df} with \code{place_id} and \code{geocode_name_used}
#'   columns added.
#' @export
geocode_original_then_std <- function(df,
                                      geo_dir,
                                      google_api_key,
                                      state_abbr    = NULL,
                                      name_col      = "school_name",
                                      name_std_col  = "school_name_std",
                                      addr_col      = "addr_clean",
                                      city_col      = "city",
                                      state_col     = "state",
                                      zip_col       = "zip",
                                      county_col    = "county_std",
                                      parallel_cache = TRUE,
                                      api_qps        = 50) {

  if (nrow(df) == 0) {
    df$place_id           <- character(0)
    df$geocode_name_used   <- character(0)
    return(df)
  }

  attempt1 <- resolve_school_place_ids(
    df, geo_dir = geo_dir, google_api_key = google_api_key,
    state_abbr = state_abbr, name_col = name_col, addr_col = addr_col,
    city_col = city_col, state_col = state_col, zip_col = zip_col,
    county_col = county_col, parallel_cache = parallel_cache, api_qps = api_qps
  )

  retry_idx <- .rows_needing_retry(attempt1$place_id)

  if (length(retry_idx) == 0) {
    attempt1$geocode_name_used <- "original"
    return(attempt1)
  }

  attempt2 <- resolve_school_place_ids(
    attempt1[retry_idx, , drop = FALSE],
    geo_dir = geo_dir, google_api_key = google_api_key,
    state_abbr = state_abbr, name_col = name_std_col, addr_col = addr_col,
    city_col = city_col, state_col = state_col, zip_col = zip_col,
    county_col = county_col, parallel_cache = parallel_cache, api_qps = api_qps
  )

  .merge_geocode_retry(attempt1, attempt2, retry_idx)
}
