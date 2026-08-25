# standardize_schools.R
# Purpose: Package-level functions for school standardization pipeline.
#   standardize_schools() is the top-level entry point, called from the
#   preprocessing-orchestration vignette.
#
# Speed-ups over the original script:
#   - data.table used for all grouped aggregations with paste/sort/unique
#   - data.table used to expand packed ID columns (replaces tidyr::separate_rows)
#   - Redundant group_by+summarise passes consolidated where safe
#   - Useless complete/incomplete school split eliminated
#   - Duplicate addr_source_pref == "kinder" block removed


# ==============================================================================
# EXPORTED FUNCTION
# ==============================================================================

#' Standardize school names and assign unique IDs
#'
#' Orchestrates all steps of school name standardization for a given state:
#' loads and cleans data sources, matches across sources to build a reference
#' key, matches kindergarten vaccination records to the reference, geocodes
#' schools, produces QC reports, and saves the final dataset.
#'
#' @param state_id Character.  Two-letter state abbreviation (e.g., \code{"md"}).
#' @param kinder_dir Path to directory containing \code{kinder_dat.rds}.
#' @param greatschools_dir Path to directory containing \code{greatschools_dat.RDS}.
#' @param doe_dir Path to directory containing \code{doe_dat.rds}.
#' @param state_school_dir Path to directory for the optional third data source.
#' @param temp_data_dir Path for temporary output files.
#' @param state_geo_dir Path for geocoding cache files.
#' @param state_dir Path to the state-level root directory (used for QC
#'   reports written to \code{<state_dir>/01_cleaning/cleaning_temp}).
#' @param addr_source_pref Character.  Address source to prefer as the primary
#'   address in the output.  One of \code{"greatschools"} (default),
#'   \code{"doe"}, \code{"third"}, or \code{"kinder"}.
#' @param parallel Logical. If \code{TRUE}, the per-row R post-processing loop
#'   inside every \code{\link{match_schools_names}} call is dispatched to worker
#'   processes via \code{\link[furrr:future_map]{furrr::future_map()}}. The
#'   caller must configure a \code{future} plan (e.g.
#'   \code{future::plan(future::multisession)}) before setting this to
#'   \code{TRUE}; if no non-sequential plan is active a warning is issued and
#'   execution falls back to sequential. Defaults to \code{FALSE}.
#' @param parallel_cache Logical. Passed to \code{\link{run_full_geocoding}}.
#'   When \code{TRUE} (default), chunks whose schools are all already geocoded
#'   (no API call required) are processed in parallel using the active
#'   \pkg{furrr} back-end.  Set to \code{FALSE} for sequential processing
#'   (useful for reproducibility or debugging).
#' @param api_qps Positive numeric. Passed to \code{\link{run_full_geocoding}}.
#'   Maximum Google Geocoding API queries per second used to throttle
#'   sequential API chunks.  Defaults to \code{50}.
#'
#' @return The final cleaned \code{data.frame} (invisibly).  The same object is
#'   also written to \code{temp_data_dir} as
#'   \code{kinder_vaccination_clean_02.csv} and
#'   \code{kinder_vaccination_clean_02.rds}.
#'
#' @details
#' All preprocessing utility functions (\code{clean_kinder_data},
#' \code{match_schools_names}, etc.) are exported by the
#' \code{tidyschoolvax} package and do not need to be sourced separately.
#' The typical entry point for the full pipeline is the
#' \code{vignettes/preprocessing-orchestration.R} script (or its
#' \code{.Rmd} counterpart), which calls \code{setup_paths()}, sources the
#' state-specific \code{01_download.R}, and then calls
#' \code{download_agnostic_data()}, \code{standardize_schools()}, and
#' \code{run_final_formatting()} in order.
#'
#' @importFrom dplyr mutate row_number group_by slice ungroup filter select arrange left_join bind_rows rename distinct any_of semi_join inner_join full_join coalesce
#' @importFrom data.table as.data.table data.table rbindlist setkey
#' @importFrom stringr str_sub
#' @importFrom utils write.csv
#' @export
standardize_schools <- function(state_id,
                                kinder_dir,
                                greatschools_dir,
                                doe_dir,
                                state_school_dir,
                                temp_data_dir,
                                state_geo_dir,
                                state_dir,
                                addr_source_pref = "greatschools",
                                parallel = FALSE,
                                parallel_cache = TRUE,
                                api_qps = 50) {
  
  # Reviewer-facing QC directory — computed early since Google identity
  # resolution (Parts 1.5/3.5 below) writes school_renames_detected.csv here
  # as soon as renames are found, rather than only at the very end.
  review_dir <- file.path(state_dir, "01_cleaning", "cleaning_temp")
  google_api_key <- Sys.getenv("GOOGLEGEO_API_KEY")

  # ---- PART 1: Load and clean all data sources --------------------------------
  kinder_result   <- clean_kinder_data(readRDS(file.path(kinder_dir, "kinder_dat.rds")))
  kinder_dat      <- kinder_result$data
  kinder_has_addr <- kinder_result$kinder_has_addr

  greatschools_dat <- clean_greatschools_data(
    gs_dat = readRDS(file.path(greatschools_dir, "greatschools_dat.RDS")),
    state_abbr = state_id
  )

  doe_dat <- clean_doe_data(readRDS(file.path(doe_dir, "doe_dat.rds")))

  other_dat <- setup_other_sourcedata(
    other_dat_filenames = c("other_dat.csv", "other_dat.xlsx"),
    state_school_dir    = state_school_dir
  )

  .run_school_matching_pipeline(
    greatschools_dat = greatschools_dat, doe_dat = doe_dat, kinder_dat = kinder_dat,
    kinder_has_addr  = kinder_has_addr, other_dat = other_dat,
    state_id         = state_id, state_geo_dir = state_geo_dir,
    temp_data_dir    = temp_data_dir, state_dir = state_dir, review_dir = review_dir,
    google_api_key   = google_api_key, addr_source_pref = addr_source_pref,
    parallel         = parallel, parallel_cache = parallel_cache, api_qps = api_qps
  )
}


#' Run the geocode-first source-matching and assembly pipeline
#'
#' Everything \code{\link{standardize_schools}} does \emph{after} loading and
#' cleaning the raw sources (its Part 1) — Parts 2–7 of the pipeline
#' described there. Factored into its own internal function, taking already
#' cleaned source data frames directly, so it can be exercised in tests and
#' smoke-checks without needing real files on disk or a live Google API key
#' for the cleaning step.
#'
#' @param greatschools_dat,doe_dat,kinder_dat,other_dat Already-cleaned
#'   source data frames (\code{other_dat} may be \code{NULL}).
#' @param kinder_has_addr Logical; see \code{\link{clean_kinder_data}}.
#' @param state_id,state_geo_dir,temp_data_dir,state_dir,addr_source_pref,parallel,parallel_cache,api_qps
#'   See \code{\link{standardize_schools}}.
#' @param review_dir Directory QC reports are written to (normally
#'   \code{file.path(state_dir, "01_cleaning", "cleaning_temp")}).
#' @param google_api_key Google Geocoding/Places API key.
#'
#' @return The final long-form data frame (invisibly), also written to
#'   \code{temp_data_dir} as \code{kinder_vaccination_clean_02.csv/.rds}.
#' @keywords internal
.run_school_matching_pipeline <- function(greatschools_dat, doe_dat, kinder_dat,
                                          kinder_has_addr, other_dat,
                                          state_id, state_geo_dir, temp_data_dir,
                                          state_dir, review_dir, google_api_key,
                                          addr_source_pref = "greatschools",
                                          parallel = FALSE, parallel_cache = TRUE,
                                          api_qps = 50) {

  # ==============================================================================
  # PART 2: Build a location-key table per source
  # ==============================================================================
  # Every source that feeds matching gets reduced, up front, to its distinct
  # locations (build_location_key_table()) — every original row still
  # reachable via a packed orig_id — so Google gets geocoded once per
  # distinct location, not once per raw row, and every source is treated
  # uniformly regardless of its own column-naming quirks.
  gs_keys <- build_location_key_table(
    greatschools_dat, source_label = "greatschools", id_col = "data1_id"
  )
  doe_keys <- build_location_key_table(
    doe_dat, source_label = "doe", id_col = "data2_id"
  )
  other_keys <- if (!is.null(other_dat)) {
    build_location_key_table(other_dat, source_label = "other", id_col = "data3_id")
  } else {
    NULL
  }

  gs_extra_cols  <- c("school_type", "school_level", "level_code", "district", "lat", "lon")
  doe_extra_cols <- c("school_type", "school_level", "level_code", "district")

  place_id_crosswalks <- list()
  exact_match_log      <- list()
  place_id_match_log    <- list()

  # ==============================================================================
  # PART 3: GreatSchools — geocode, collapse within-source duplicates
  # ==============================================================================
  gs_keys <- gs_keys %>%
    geocode_original_then_std(geo_dir = state_geo_dir, google_api_key = google_api_key,
                              state_abbr = state_id)
  gs_collapsed <- collapse_reference_by_place_id(
    gs_keys, id_col = "orig_id", name_col = "school_name",
    name_std_col = "school_name_std", source_label = "greatschools",
    source_df = greatschools_dat, source_id_col = "data1_id"
  )
  gs_keys <- .repack_collapsed_orig_ids(gs_collapsed) %>%
    .attach_source_metadata(greatschools_dat, id_col = "data1_id", extra_cols = gs_extra_cols)
  place_id_crosswalks$greatschools <- gs_collapsed$crosswalk

  master_keys <- gs_keys

  # ==============================================================================
  # PART 4: DOE — exact match against GreatSchools, geocode leftovers,
  #         place_id-match, append still-unmatched into the master
  # ==============================================================================
  doe_result <- .match_source_against_master(
    source_keys    = doe_keys, master_keys = master_keys, source_label = "doe",
    source_df      = doe_dat, source_id_col = "data2_id", extra_cols = doe_extra_cols,
    state_id       = state_id, state_geo_dir = state_geo_dir, google_api_key = google_api_key,
    exact_cols     = c("city", "addr_clean"), exact_require = "any"
  )
  master_keys <- doe_result$master_keys
  place_id_crosswalks$doe   <- doe_result$place_id_crosswalk
  exact_match_log$doe        <- doe_result$exact_matched
  place_id_match_log$doe     <- doe_result$place_id_matched

  # ==============================================================================
  # PART 5: Optional third/state-specific source — identical pattern, right
  #         after DOE, so kinder matches against the fullest master available
  # ==============================================================================
  if (!is.null(other_keys)) {
    other_result <- .match_source_against_master(
      source_keys    = other_keys, master_keys = master_keys, source_label = "other",
      source_df      = other_dat, source_id_col = "data3_id", extra_cols = doe_extra_cols,
      state_id       = state_id, state_geo_dir = state_geo_dir, google_api_key = google_api_key,
      exact_cols     = c("city", "addr_clean"), exact_require = "any"
    )
    master_keys <- other_result$master_keys
    place_id_crosswalks$other <- other_result$place_id_crosswalk
    exact_match_log$other      <- other_result$exact_matched
    place_id_match_log$other   <- other_result$place_id_matched
  }

  master_keys <- master_keys %>%
    dplyr::mutate(school_id = dplyr::row_number()) %>%
    # Resolve Google's current name for every school that has a place_id —
    # feeds augment_with_google_name_variant() in the fuzzy-fallback step
    # below, so even the last-resort fuzzy pass can match a kinder record
    # against whichever name Google currently has on file, not just
    # whatever GS/DOE happened to record.
    add_google_name_variants(geo_dir = state_geo_dir, google_api_key = google_api_key)

  # ==============================================================================
  # PART 6: Kindergarten — exact match (requiring every geographic variable
  #         actually available for this state) against the master, geocode
  #         leftovers, place_id-match, fuzzy match only as a last resort for
  #         records that never geocoded, report whatever's still unmatched
  # ==============================================================================
  kinder_dat <- kinder_dat %>%
    dplyr::mutate(
      school_type  = gsub(" (non-public)", "", school_type, fixed = TRUE),
      vacc_data_id = dplyr::row_number()
    )

  year_source_levels <- sort(unique(kinder_dat$year_source))
  n_years_data       <- length(year_source_levels)

  # build_kinder_unique_schools() still does the real data-quality work here
  # (exact-key aggregation, NA-fixing, majority-vote school_type correction)
  # — that's independent of *how* matching happens downstream, so it's kept
  # exactly as before, not superseded by build_location_key_table().
  #
  # .merge_kinder_type_duplicates_if_unlisted(): a second, more targeted
  # school_type remediation on top of build_kinder_unique_schools()'s own
  # majority-vote correction (which only fires when a group's total record
  # count is small relative to n_years_data — too conservative to catch a
  # school whose type flip-flopped across enough years to trip that guard).
  # When a school is reported under BOTH "public" and "private" in
  # different years and is absent from GreatSchools and DOE entirely, there
  # is no authoritative source to resolve the conflict against — but DOE's
  # own roster IS the definition of "public" for a state, so a school
  # missing from it cannot be public. Merge the duplicate groups and assume
  # "private" (found via a real MD run: "Chesterton Academy of Annapolis",
  # public in 2021's data, private in 2023's, absent from both md doe_dat
  # and greatschools_dat entirely — a small private school GreatSchools
  # simply hasn't indexed, not two different schools).
  kinder_unique <- build_kinder_unique_schools(kinder_dat, n_years_data) %>%
    .merge_kinder_type_duplicates_if_unlisted(greatschools_dat, doe_dat) %>%
    dplyr::filter(grepl("e", level_code, fixed = FALSE) | is.na(level_code))

  # .attach_kinder_addresses() needs kinder_unique's native vacc_school_id/
  # vacc_data_ids naming intact — do this BEFORE any renaming.
  if (isTRUE(kinder_has_addr)) {
    kinder_unique <- kinder_unique %>% .attach_kinder_addresses(kinder_dat)
  }

  # orig_id is this pipeline's identity column for matching (exact_match_locations()/
  # .place_id_match()); vacc_data_ids (packed raw record ids) is left untouched —
  # .assemble_kinder_output() needs it later to expand back to one row per record.
  #
  # school_name_geocode: a single representative pre-standardization name
  # (the first of school_names_orig's packed set) to geocode on FIRST, per
  # geocode_original_then_std()'s original-name-first design -- using the
  # already level-word-stripped school_name_std for the first attempt (as
  # this pipeline did until this fix) makes for a needlessly vague query
  # ("laurel" + county, instead of "laurel elementary" + county) that
  # Google can resolve to the wrong, generic place instead of the actual
  # school. Falls back to school_name_std itself if school_names_orig is
  # ever missing/blank.
  kinder_elem <- kinder_unique %>%
    dplyr::rename(orig_id = vacc_school_id) %>%
    dplyr::mutate(
      orig_id = as.character(orig_id),
      school_name_geocode = .kinder_geocode_name(school_names_orig, school_name_std)
    )

  # master_full (no level_code restriction) feeds exact-match and
  # place_id-match; master_elem (elementary-eligible only) is reserved for
  # the fuzzy fallback below. Both used to share one elementary-filtered
  # pool, but exact and place_id matches are deterministic — narrowing
  # their candidate pool doesn't add safety, it just produces false
  # negatives for a real school GreatSchools/DOE happens to classify as
  # non-elementary (e.g. a combined middle/alternative program). Found via
  # a real MD run: GreatSchools lists "Mary Moss at Adams Academy"
  # (levelCode "m", DOE confirms grades 6-9) — but kinder, GreatSchools,
  # and DOE all independently geocode to the IDENTICAL place_id, so the
  # match is certain regardless of grade-level classification. Fuzzy
  # matching is the one method that genuinely gets less safe with a wider
  # candidate pool (loose string similarity to the wrong, unrelated
  # non-elementary school is a real false-positive risk — the whole reason
  # this redesign demoted fuzzy to a last resort in the first place), so
  # it alone stays restricted to master_elem.
  master_full <- master_keys %>%
    dplyr::mutate(orig_id = as.character(school_id))

  master_elem <- master_full %>%
    dplyr::filter(is.na(level_code) | grepl("e", level_code, fixed = FALSE))

  exact_cols_kinder <- .usable_geo_cols(
    kinder_elem, cols = c("county_std", "district_std", "city", "addr_clean")
  )

  kinder_exact <- if (length(exact_cols_kinder) > 0) {
    exact_match_locations(kinder_elem, master_full, cols = exact_cols_kinder, require = "all")
  } else {
    list(matched = tibble::tibble(orig_id_1 = character(0), orig_id_2 = character(0),
                                  match_cols = character(0)),
        unmatched_dat1 = kinder_elem, unmatched_dat2 = master_full)
  }

  kinder_geocoded <- kinder_exact$unmatched_dat1 %>%
    geocode_original_then_std(
      geo_dir = state_geo_dir, google_api_key = google_api_key, state_abbr = state_id,
      name_col = "school_name_geocode", name_std_col = "school_name_std",
      addr_col = if (isTRUE(kinder_has_addr)) "addr_clean_kinder" else "addr_clean",
      city_col = if (isTRUE(kinder_has_addr)) "city_kinder" else "city",
      zip_col  = if (isTRUE(kinder_has_addr)) "zip_kinder"  else "zip"
    )

  # kinder first, master second: downstream (all_kinder_matches, below)
  # combines this with kinder_exact$matched and fuzzy_matched under the
  # universal convention orig_id_1 = kinder's orig_id, orig_id_2 = master's
  # school_id — matching that order here (rather than master-first, as
  # .match_source_against_master() uses) keeps the three match sources
  # consistent instead of reversed relative to each other.
  kinder_pid <- .place_id_match(kinder_geocoded, master_full,
                                data_1_source = "kinder", data_2_source = "master")

  kinder_still_unmatched <- kinder_geocoded %>%
    dplyr::filter(!(orig_id %in% kinder_pid$matched$orig_id_1))

  # ---- County-typo correction: last-resort fix before falling to fuzzy -----
  # See .detect_kinder_county_outliers() for the detection logic (a school
  # reported under a "wrong" county for exactly the year(s) its real
  # county's own records are missing). Applied narrowly, only to rows that
  # have ALREADY failed both exact-match and place_id-match -- i.e.
  # "geocoding did not succeed" is read as "did not lead anywhere the
  # normal pipeline could confirm," not literally "place_id is NA": a bare
  # name+wrong-county query with no address to verify against frequently
  # resolves to SOME real-looking but unrelated place instead of failing
  # outright (found via the real MD case this targets -- "Woodmore
  # Elementary School" under Anne Arundel for 2022 geocoded "successfully"
  # to an address in Bowie that isn't the school at all, rather than
  # returning nothing). A row that's still here has already had every
  # chance to independently verify a DIFFERENT real identity and failed to,
  # so there's nothing legitimate left to accidentally override.
  county_fix_candidates <- .detect_kinder_county_outliers(kinder_elem)
  county_corrected_matched <- tibble::tibble(orig_id_1 = character(0), orig_id_2 = character(0),
                                             match_cols = character(0))
  if (nrow(county_fix_candidates) > 0) {
    county_typo_rows <- kinder_still_unmatched %>%
      dplyr::filter(orig_id %in% county_fix_candidates$orig_id) %>%
      dplyr::left_join(county_fix_candidates, by = c("orig_id", "county_std")) %>%
      dplyr::mutate(county_std = corrected_county_std) %>%
      dplyr::select(-corrected_county_std)

    if (nrow(county_typo_rows) > 0) {
      retry_cols <- .usable_geo_cols(
        county_typo_rows, cols = c("county_std", "district_std", "city", "addr_clean")
      )
      county_retry <- if (length(retry_cols) > 0) {
        exact_match_locations(county_typo_rows, master_full, cols = retry_cols, require = "all")
      } else {
        list(matched = tibble::tibble(orig_id_1 = character(0), orig_id_2 = character(0),
                                      match_cols = character(0)))
      }
      if (nrow(county_retry$matched) > 0) {
        county_corrected_matched <- county_retry$matched %>%
          dplyr::mutate(match_cols = paste0("county_corrected_", match_cols))
        # Carry the correction into kinder_elem itself, not just the match
        # -- otherwise the final output would show the right school_id
        # alongside the original, wrong county_std for these rows.
        kinder_elem <- kinder_elem %>%
          dplyr::rows_update(
            county_typo_rows %>%
              dplyr::filter(orig_id %in% county_corrected_matched$orig_id_1) %>%
              dplyr::select(orig_id, county_std),
            by = "orig_id"
          )
      }
    }
  }

  kinder_still_unmatched <- kinder_still_unmatched %>%
    dplyr::filter(!(orig_id %in% county_corrected_matched$orig_id_1))

  # ---- Fuzzy fallback: for records place_id-matching couldn't resolve -------
  # Originally scoped to ONLY rows that never geocoded at all
  # (geocode_name_used %in% c(NA,"failed")) — but for a state whose kinder
  # data has no address column at all (e.g. CA), a bare name+county geocode
  # query for a common school name ("Jefferson", "Chabot", ...) frequently
  # resolves to *some* place_id that just isn't the same place GS/DOE's
  # precise, address-based query resolved to for that same real school —
  # a technical geocoding "success" that place_id-matching correctly
  # rejects as a non-match, but is not evidence the school is unidentifiable.
  # Found via a real CA run: narrowing to only-never-geocoded rows silently
  # dropped ~100k previously-good matches for exactly this reason. Anything
  # still unmatched after BOTH exact and place_id matching is a fair fuzzy
  # candidate, regardless of whether a (evidently wrong) place_id happened
  # to attach along the way.
  kinder_fuzzy_candidates <- kinder_still_unmatched

  fuzzy_matched <- tibble::tibble(orig_id_1 = character(0), orig_id_2 = character(0),
                                  match_cols = character(0), match_score = numeric(0))
  if (nrow(kinder_fuzzy_candidates) > 0) {
    ref_for_fuzzy <- master_elem %>%
      dplyr::rename(data2_id = orig_id) %>%
      dplyr::mutate(data2_id = as.integer(data2_id)) %>%
      augment_with_google_name_variant()

    fuzzy_input <- kinder_fuzzy_candidates %>%
      dplyr::rename(data1_id = orig_id) %>%
      dplyr::mutate(data1_id = dplyr::row_number())
    id_lookup <- tibble::tibble(data1_id = fuzzy_input$data1_id,
                                orig_id_1 = kinder_fuzzy_candidates$orig_id)

    has_district_fuzzy <- function(d1, d2) {
      "district_std" %in% names(d1) && any(!is.na(d1$district_std) & d1$district_std != "") &&
        "district_std" %in% names(d2) && any(!is.na(d2$district_std) & d2$district_std != "")
    }
    fuzzy_passes <- list(
      list(label = "district_type",
          match_cols1 = c("county_std", "district_std", "school_type"),
          match_cols2 = c("county_std", "district_std", "school_type"),
          threshold_jw = 0.169, threshold_jw_min = 0.3, exact_jw = 0.10,
          condition = has_district_fuzzy),
      list(label = "county_type",
          match_cols1 = c("county_std", "school_type"),
          match_cols2 = c("county_std", "school_type"),
          threshold_jw = 0.169, threshold_jw_min = 0.3, exact_jw = 0.10),
      list(label = "county",
          match_cols1 = c("county_std"), match_cols2 = c("county_std"),
          threshold_jw = 0.15, threshold_jw_min = 0.3, exact_jw = 0.10)
    )
    fuzzy_cascade <- run_matching_cascade(
      data1 = fuzzy_input, data2 = ref_for_fuzzy, passes = fuzzy_passes,
      data_1_source = "kinder", data_2_source = "master", parallel = parallel
    )
    if (!is.null(review_dir)) log_school_renames(fuzzy_cascade$matched, out_dir = review_dir)

    if (nrow(fuzzy_cascade$matched) > 0) {
      fuzzy_matched <- fuzzy_cascade$matched %>%
        dplyr::left_join(id_lookup, by = "data1_id") %>%
        dplyr::transmute(orig_id_1, orig_id_2 = as.character(data2_id),
                         match_cols = paste0("fuzzy_", match_method),
                         match_score)
    }
  }

  # ---- Combine every matching stage into one vacc_school_id -> school_id map -
  # Exact and place_id matches are boolean-certain (no native distance
  # score), so they get a flat match_score of 1; the fuzzy fallback keeps
  # its own genuine, computed match_score from run_matching_cascade().
  all_kinder_matches <- dplyr::bind_rows(
    kinder_exact$matched %>% dplyr::mutate(match_score = 1),
    kinder_pid$matched   %>% dplyr::mutate(match_score = 1),
    county_corrected_matched %>% dplyr::mutate(match_score = 1),
    fuzzy_matched
  )

  kinder_still_unmatched_final <- kinder_still_unmatched %>%
    dplyr::filter(!(orig_id %in% fuzzy_matched$orig_id_1))
  if (nrow(kinder_still_unmatched_final) > 0) {
    report_unmatched_schools(kinder_still_unmatched_final, out_dir = review_dir)
  }

  vacc_elem_all <- kinder_elem %>%
    dplyr::inner_join(
      all_kinder_matches %>%
        dplyr::transmute(orig_id = orig_id_1, match_score,
                         school_id = as.integer(orig_id_2)),
      by = "orig_id"
    ) %>%
    dplyr::rename(vacc_school_id = orig_id) %>%
    dplyr::mutate(vacc_school_id = as.integer(vacc_school_id))

  place_id_crosswalks$kinder <- tibble::tibble()  # kinder never re-geocoded per-source like GS/DOE; nothing to collapse here

  # ---- Write the combined place_id crosswalk (all sources) -------------------
  write_report(
    dplyr::bind_rows(place_id_crosswalks), report_name = "place_id_crosswalk", out_dir = review_dir
  )

  # ---- Assemble the final long-form kinder output ----------------------------
  # .assemble_kinder_output() (extracted from the old match_kinder_to_reference())
  # expects kinder_elem keyed by data1_id, matching vacc_elem_all$vacc_school_id's
  # value space — both trace back to the same original per-group orig_id here.
  vacc_data_final <- .assemble_kinder_output(
    vacc_elem_all    = vacc_elem_all,
    kinder_elem      = kinder_elem %>% dplyr::rename(data1_id = orig_id) %>%
      dplyr::mutate(data1_id = as.integer(data1_id)),
    matched_df       = master_keys %>%
      dplyr::rename(data_1_source = source) %>%
      dplyr::mutate(school_name = dplyr::coalesce(school_name, school_name_std)),
    kinder_dat       = kinder_dat,
    temp_data_dir    = temp_data_dir,
    addr_source_pref = addr_source_pref,
    kinder_has_addr  = kinder_has_addr
  )

  # ---- PART 7: Geocode and school-status lookup ------------------------------
  vacc_data_final[["state"]] <- state_id
  vacc_data_final <- normalize_missing_strings(vacc_data_final)
  
  unique_schools <- vacc_data_final %>%
    dplyr::group_by(school_id, school_name_std_vacc, county_std) %>%
    dplyr::slice(1L) %>%
    dplyr::ungroup()
  
  geocoded_schools <- run_full_geocoding(
    unique_schools = unique_schools,
    geo_dir        = state_geo_dir,
    google_api_key = google_api_key,
    parallel_cache = parallel_cache,
    api_qps        = api_qps
  )

  # Keep columns up to and including geo_source
  geocoded_schools_clean <- geocoded_schools %>%
    dplyr::select(1:which(colnames(geocoded_schools) == "geo_source"))

  schools_status <- run_school_status_with_cache(
    df             = geocoded_schools_clean,
    geo_dir        = state_geo_dir,
    google_api_key = google_api_key
  )

  # Normalise business_status to lowercase for consistency
  schools_status <- schools_status %>%
    dplyr::mutate(business_status = tolower(business_status))

  school_vax_joined <- merge_geocoding_results(vacc_data_final, schools_status) %>%
    dplyr::mutate(year2 = as.numeric(stringr::str_sub(year_source, -2L, -1L)))

  # ---- QC reports ------------------------------------------------------------
  report_missing_geo_status(school_vax_joined, out_dir = review_dir)
  report_potential_duplicate_schools(school_vax_joined, out_dir = review_dir,
                                     distance_meters = 50)
  report_closed_schools(school_vax_joined, out_dir = review_dir,
                        year_col = "year2")
  report_unique_missing_addresses(school_vax_joined, out_dir = review_dir)
  
  # ---- Save outputs ----------------------------------------------------------
  final_csv <- file.path(temp_data_dir, "kinder_vaccination_clean_02.csv")
  final_rds <- file.path(temp_data_dir, "kinder_vaccination_clean_02.rds")
  utils::write.csv(school_vax_joined, final_csv, row.names = FALSE)
  saveRDS(school_vax_joined, final_rds)
  
  message("Master processing complete for state: ", state_id)
  message("Final CSV: ", final_csv)
  message("Final RDS: ", final_rds)
  
  invisible(school_vax_joined)
}


# ==============================================================================
# Helpers for the geocode-first pipeline above
# ==============================================================================

# Internal: collapse_reference_by_place_id()'s `data` keeps only the ONE
# canonical row's (already-packed) orig_id per place_id group — the other
# collapsed rows' orig_id tokens would otherwise be silently dropped from
# link-back coverage. This re-derives the full, unioned orig_id for every
# surviving row from the crosswalk collapse_reference_by_place_id() also
# returns (which does track every original row), so a location-key row
# collapsed twice over (once by build_location_key_table()'s own exact-key
# grouping, again here by place_id) still traces back to every original
# source row.
#' @keywords internal
.repack_collapsed_orig_ids <- function(collapsed) {
  if (nrow(collapsed$crosswalk) == 0 || nrow(collapsed$data) == 0) return(collapsed$data)

  full_ids <- collapsed$crosswalk %>%
    tidyr::separate_longer_delim(orig_id, delim = "; ") %>%
    dplyr::group_by(new_id) %>%
    dplyr::summarise(orig_id_full = paste(sort(unique(orig_id)), collapse = "; "), .groups = "drop")

  collapsed$data %>%
    dplyr::left_join(full_ids, by = c("orig_id" = "new_id")) %>%
    dplyr::mutate(orig_id = dplyr::coalesce(orig_id_full, orig_id)) %>%
    dplyr::select(-orig_id_full)
}


# Internal: reattach representative attribute columns build_location_key_table()
# deliberately leaves out (it only keeps location-defining fields — see its
# docs) but the final output still needs, e.g. school_type/school_level/
# level_code/lat/lon. Picks the first packed orig_id's row as canonical,
# matching the "first wins" convention already used by
# collapse_reference_by_place_id() elsewhere in this file.
#
# level_code is the one exception: it's unioned across EVERY packed orig_id
# in the group, not just the first. Reason (found via a real CA run):
# collapse_reference_by_place_id() can merge two source listings that
# resolve to the same physical place_id but specialize in different grade
# bands -- e.g. GreatSchools sometimes lists a campus's preschool program
# ("Our Savior Luth Ministries", level_code "p") and its TK-8 program
# ("Our Savior Lutheran", level_code "p,e,m") as two separate rows that
# both geocode to the identical place. "First wins" happened to keep the
# preschool-only row's level_code, which then failed downstream's
# elementary filter (grepl("e", level_code)) and made a real, geocoded,
# place_id-matchable elementary school unmatchable. Unioning keeps whatever
# grade coverage ANY of the merged listings actually reported.
#' @keywords internal
.attach_source_metadata <- function(lkt, source_df, id_col, extra_cols) {
  extra_cols <- intersect(extra_cols, names(source_df))
  if (nrow(lkt) == 0 || length(extra_cols) == 0) return(lkt)

  first_id <- trimws(sub(";.*$", "", lkt$orig_id))
  lookup <- source_df %>%
    dplyr::mutate(.join_id = as.character(.data[[id_col]])) %>%
    dplyr::select(.join_id, dplyr::all_of(extra_cols)) %>%
    dplyr::distinct(.join_id, .keep_all = TRUE)

  out <- lkt %>%
    dplyr::mutate(.join_id = first_id) %>%
    dplyr::left_join(lookup, by = ".join_id") %>%
    dplyr::select(-.join_id)

  if ("level_code" %in% names(out)) {
    lc_map <- source_df %>%
      dplyr::mutate(.join_id = as.character(.data[[id_col]])) %>%
      dplyr::select(.join_id, level_code) %>%
      dplyr::distinct(.join_id, .keep_all = TRUE)
    lc_vec <- stats::setNames(lc_map$level_code, lc_map$.join_id)

    all_ids <- strsplit(lkt$orig_id, ";\\s*")
    out$level_code <- vapply(all_ids, function(ids) {
      codes <- unlist(strsplit(unname(lc_vec[ids]), ",\\s*"))
      codes <- unique(trimws(codes[!is.na(codes) & codes != ""]))
      if (length(codes) == 0) NA_character_ else paste(sort(codes), collapse = ",")
    }, character(1))
  }

  out
}


# Internal: which of the candidate geographic columns are actually usable
# (have at least one non-NA, non-empty value) in df — the adaptive "match on
# every geographic variable actually available" requirement from the
# pipeline redesign (county for most states' kinder data, district for
# states like CA that group schools by district rather than county).
#' @keywords internal
.usable_geo_cols <- function(df, cols = c("county_std", "district_std", "city", "addr_clean")) {
  cols[vapply(cols, function(col) {
    col %in% names(df) && any(!is.na(df[[col]]) & df[[col]] != "")
  }, logical(1))]
}


# Internal: run one source (DOE, or the optional third/state-specific
# source) through the shared exact-match -> geocode leftovers (original name,
# then standardized) -> place_id-match -> append-still-unmatched pattern
# against the accumulated master reference. DOE and other_dat use this
# pattern identically — only the source_label/exact-match columns differ —
# so it's factored out once rather than duplicated per source.
#' @keywords internal
.match_source_against_master <- function(source_keys, master_keys, source_label,
                                         source_df, source_id_col, extra_cols,
                                         state_id, state_geo_dir, google_api_key,
                                         exact_cols, exact_require = "any") {

  exact <- exact_match_locations(source_keys, master_keys, cols = exact_cols,
                                 require = exact_require)

  geocoded <- exact$unmatched_dat1 %>%
    geocode_original_then_std(geo_dir = state_geo_dir, google_api_key = google_api_key,
                              state_abbr = state_id)

  collapsed <- collapse_reference_by_place_id(
    geocoded, id_col = "orig_id", name_col = "school_name",
    name_std_col = "school_name_std", source_label = source_label,
    source_df = source_df, source_id_col = source_id_col
  )
  geocoded <- .repack_collapsed_orig_ids(collapsed)

  pid <- .place_id_match(master_keys, geocoded,
                         data_1_source = "master", data_2_source = source_label)

  # A source row that place_id-matches an existing master row is about to be
  # discarded (master already represents the school) -- but its own
  # level_code shouldn't be lost just because it lost the naming lottery.
  # See .union_level_code_pairs() docs for the motivating real MD case.
  if (nrow(pid$matched) > 0 && "level_code" %in% names(master_keys) &&
      "level_code" %in% names(source_df)) {
    source_level_lookup <- stats::setNames(
      as.character(source_df$level_code), as.character(source_df[[source_id_col]])
    )
    absorbed_level <- .union_level_code(pid$matched$orig_id_2, source_level_lookup)
    master_idx <- match(pid$matched$orig_id_1, master_keys$orig_id)
    hit <- !is.na(master_idx)
    master_keys$level_code[master_idx[hit]] <- .union_level_code_pairs(
      master_keys$level_code[master_idx[hit]], absorbed_level[hit]
    )
  }

  still_unmatched <- geocoded %>%
    dplyr::filter(!(orig_id %in% pid$matched$orig_id_2)) %>%
    .attach_source_metadata(source_df, id_col = source_id_col, extra_cols = extra_cols)

  list(
    master_keys        = dplyr::bind_rows(master_keys, still_unmatched),
    exact_matched       = exact$matched,
    place_id_matched     = pid$matched,
    place_id_crosswalk   = collapsed$crosswalk
  )
}


# ==============================================================================
# PART 2: BUILD REFERENCE KEY
# ==============================================================================

#' Build the GS + DOE (+ optional third) school reference key
#'
#' \strong{Deprecated:} superseded by the geocode-first sequence in
#' \code{\link{standardize_schools}} (build a location-key table per source
#' via \code{\link{build_location_key_table}}, geocode with
#' \code{\link{geocode_original_then_std}}, exact-match with
#' \code{\link{exact_match_locations}}, place_id-match with
#' \code{.run_exact_join_pass()}, fuzzy match only as a last resort) — the
#' fuzzy cascade this function ran primarily on turned out to be too
#' permissive in practice (see \code{exact_match_locations()}'s docs for the
#' motivating false-positive example). Kept for backward compatibility and
#' as regression test coverage; not called by \code{standardize_schools()}.
#'
#' Matches GreatSchools data to DOE data using a cascade of matching passes —
#' an exact \code{place_id} join first (when both sources have resolved a
#' Google identity; see \code{\link{resolve_school_place_ids}}), then fuzzy
#' string matching on address, ZIP, city, district (conditional), and county —
#' optionally integrates a third dataset, applies the chosen address source
#' preference, and returns a single reference data frame. The \code{place_id}
#' pass is what lets two GS/DOE records at the same physical school be
#' reconciled even when one source's name is stale after a rename; the
#' district pass is only executed when \code{district_std} is available
#' (non-\code{NA}) in both data sources and differs from \code{county_std}.
#'
#' @param greatschools_dat Cleaned GreatSchools data frame.
#' @param doe_dat Cleaned DOE data frame.
#' @param other_dat Optional cleaned third-source data frame, or \code{NULL}.
#' @param addr_source_pref See \code{\link{standardize_schools}}.
#' @param parallel See \code{\link{standardize_schools}}.
#' @param out_dir Optional directory to write \code{school_renames_detected.csv}
#'   to via \code{\link{log_school_renames}}. When \code{NULL} (default), no
#'   rename report is written.
#'
#' @return A data frame: the merged reference key.
#' @keywords internal
build_reference_key <- function(greatschools_dat,
                                doe_dat,
                                other_dat        = NULL,
                                addr_source_pref = "greatschools",
                                parallel         = FALSE,
                                out_dir          = NULL) {

  gs <- greatschools_dat %>% dplyr::select(-county2, -county2_std)
  gs_city  <- gs      %>% dplyr::mutate(city_cln = tolower(city))
  doe_city <- doe_dat %>% dplyr::mutate(city_cln = tolower(city))

  has_place_id <- function(d1, d2) {
    "place_id" %in% names(d1) && "place_id" %in% names(d2) &&
      any(!is.na(d1$place_id)) && any(!is.na(d2$place_id))
  }
  has_district <- function(d1, d2) {
    "district_std" %in% names(d1) && any(!is.na(d1$district_std) & d1$district_std != "") &&
      "district_std" %in% names(d2) && any(!is.na(d2$district_std) & d2$district_std != "")
  }
  non_county_district <- function(df) {
    df %>% dplyr::filter(!is.na(district_std) & district_std != "", district_std != county_std)
  }

  # ---- Matching cascade: place_id, address, zip, city, (district,) county ----
  passes <- list(
    list(label = "place_id", type = "exact_join", join_col = "place_id",
        condition = has_place_id),
    list(label = "address",
        match_cols1 = c("addr_clean_no_unit", "school_type"),
        match_cols2 = c("addr_clean_no_unit", "school_type"),
        threshold_jw = 0.3, threshold_jw_min = 0.6, exact_jw = 0.15),
    list(label = "zip",
        match_cols1 = c("zip", "school_type", "level_code_match"),
        match_cols2 = c("zip", "school_type", "level_code_match"),
        threshold_jw = 0.25, threshold_jw_min = 0.5, exact_jw = 0.15),
    list(label = "city",
        match_cols1 = c("city_cln", "school_type", "level_code_match"),
        match_cols2 = c("city_cln", "school_type", "level_code_match"),
        threshold_jw = 0.2, threshold_jw_min = 0.5, exact_jw = 0.15),
    list(label = "district",
        match_cols1 = c("district_std", "school_type", "level_code_match"),
        match_cols2 = c("district_std", "school_type", "level_code_match"),
        threshold_jw = 0.2, threshold_jw_min = 0.5, exact_jw = 0.15,
        condition = has_district,
        row_filter1 = non_county_district, row_filter2 = non_county_district),
    list(label = "county",
        match_cols1 = c("county_std", "school_type", "level_code_match"),
        match_cols2 = c("county_std", "school_type", "level_code_match"),
        threshold_jw = 0.2, threshold_jw_min = 0.5, exact_jw = 0.15)
  )

  cascade <- run_matching_cascade(
    data1 = gs_city, data2 = doe_city, passes = passes,
    data_1_source = "GreatSchools", data_2_source = "DOE", parallel = parallel
  )
  matched_scores <- cascade$matched %>%
    dplyr::arrange(county_std, school_name_data1, match_score)

  if (!is.null(out_dir)) log_school_renames(matched_scores, out_dir = out_dir)

  # ---- Assemble matched + unmatched GS and DOE records ----------------------
  greatschools_dat$zip <- as.character(greatschools_dat$zip)

  matched_df <- matched_scores %>%
    dplyr::select(match_score, data1_id, data2_id, data_1_source, data_2_source) %>%
    dplyr::full_join(
      greatschools_dat %>%
        dplyr::mutate(source = "Great Schools") %>%
        dplyr::select(data1_id, source, county_std, county, school_name,
                      school_name_std, level_code, level_code_match, school_type,
                      city, state, zip, lat, lon, street = street1, addr_clean,
                      dplyr::any_of(c("district", "district_std",
                                      "place_id", "school_name_std_google"))),
      by = "data1_id"
    ) %>%
    dplyr::left_join(
      doe_dat %>%
        dplyr::rename(dplyr::any_of(c(
          district_doe     = "district",
          district_std_doe = "district_std"
        ))) %>%
        dplyr::select(data2_id,
                      school_name_doe     = school_name,
                      school_name_std_doe = school_name_std,
                      county_doe          = county,
                      zip_doe             = zip,
                      addr_clean_doe      = addr_clean,
                      street_doe          = street,
                      city_doe            = city,
                      dplyr::any_of(c("district_doe", "district_std_doe"))),
      by = "data2_id"
    ) %>%
    dplyr::bind_rows(
      doe_dat %>%
        dplyr::filter(!(data2_id %in% matched_scores$data2_id)) %>%
        dplyr::mutate(source = "DOE") %>%
        dplyr::select(data2_id, source, county_std, county, school_name,
                      school_name_std, level_code, level_code_match, school_type,
                      city, state, zip, lat, lon, street, addr_clean,
                      dplyr::any_of(c("district", "district_std",
                                      "place_id", "school_name_std_google")))
    ) %>%
    dplyr::arrange(county_std, school_name_std, match_score) %>%
    fix_charter_type(name_col = "school_name") %>%
    dplyr::select(data1_id, data2_id, school_name_std, school_name_std_doe,
                  county_std, source, dplyr::everything())

  # ---- Optionally integrate third dataset ------------------------------------
  if (!is.null(other_dat)) {
    matched_df <- .integrate_third_dataset(matched_df, other_dat,
                                           parallel = parallel)
  }

  # ---- Apply address source preference ---------------------------------------
  matched_df <- .apply_addr_source_pref(matched_df, other_dat, addr_source_pref)

  matched_df
}


# Internal: integrate third dataset into the reference key
# Deprecated along with build_reference_key() (see its roxygen note) —
# superseded by the same geocode-first pattern applied to other_dat right
# after DOE in the new standardize_schools() sequence.
.integrate_third_dataset <- function(matched_df, other_dat, parallel = FALSE) {
  matched_df <- matched_df %>% dplyr::mutate(temp_id = dplyr::row_number())
  
  matched_df_for_third <- matched_df %>%
    dplyr::rename(data1_id_prev = data1_id, data2_id_prev = data2_id) %>%
    dplyr::mutate(data2_id = temp_id)
  
  third_match <- other_dat %>%
    dplyr::mutate(
      data3_id = dplyr::row_number(),
      zip      = as.character(zip)
    ) %>%
    dplyr::rename(data1_id = data3_id)

  # Matching cascade for the third dataset (address, zip, district
  # (conditional), county) — same pattern as build_reference_key(), extracted
  # to run_matching_cascade() so both call sites share one implementation.
  non_county_district <- function(df) {
    df %>% dplyr::filter(!is.na(district_std) & district_std != "", district_std != county_std)
  }
  passes <- list(
    list(label = "address",
        match_cols1 = c("addr_clean_no_unit", "school_type"),
        match_cols2 = c("addr_clean_no_unit", "school_type"),
        threshold_jw = 0.3, threshold_jw_min = 0.6, exact_jw = 0.15),
    list(label = "zip",
        match_cols1 = c("zip", "school_type", "level_code_match"),
        match_cols2 = c("zip", "school_type", "level_code_match"),
        threshold_jw = 0.25, threshold_jw_min = 0.5, exact_jw = 0.15),
    list(label = "district",
        match_cols1 = c("district_std", "school_type", "level_code_match"),
        match_cols2 = c("district_std", "school_type", "level_code_match"),
        threshold_jw = 0.2, threshold_jw_min = 0.5, exact_jw = 0.15,
        condition = function(d1, d2) {
          "district_std" %in% names(d1) && any(!is.na(d1$district_std) & d1$district_std != "") &&
            "district_std" %in% names(d2) && any(!is.na(d2$district_std) & d2$district_std != "")
        },
        row_filter1 = non_county_district, row_filter2 = non_county_district),
    list(label = "county",
        match_cols1 = c("county_std", "school_type", "level_code_match"),
        match_cols2 = c("county_std", "school_type", "level_code_match"),
        threshold_jw = 0.2, threshold_jw_min = 0.5, exact_jw = 0.15)
  )

  cascade <- run_matching_cascade(
    data1 = third_match, data2 = matched_df_for_third, passes = passes,
    parallel = parallel
  )
  matched_third <- cascade$matched %>%
    dplyr::arrange(county_std, school_name_data1, match_score)
  
  # Add third-dataset address columns to matched reference rows
  matched_df <- matched_df %>%
    dplyr::left_join(
      matched_third %>%
        dplyr::select(data3_id = data1_id, temp_id = data2_id) %>%
        dplyr::inner_join(
          other_dat %>%
            dplyr::rename(dplyr::any_of(c(
              district_std_third = "district_std"
            ))) %>%
            dplyr::select(data3_id,
                          school_name_third     = school_name,
                          school_name_std_third = school_name_std,
                          addr_clean_third      = addr_clean,
                          street_third          = street,
                          city_third            = city,
                          zip_third             = zip,
                          county_std_third      = county_std,
                          dplyr::any_of("district_std_third")),
          by = "data3_id"
        ),
      by = "temp_id"
    ) %>%
    dplyr::select(-temp_id)
  
  # Append unmatched third records to the key
  matched_df$zip <- as.character(matched_df$zip)
  other_dat$zip  <- as.character(other_dat$zip)
  
  matched_df <- dplyr::bind_rows(
    matched_df,
    third_match %>%
      dplyr::filter(!(data1_id %in% matched_third$data1_id)) %>%
      dplyr::rename(data3_id = data1_id) %>%
      dplyr::mutate(source = "Third") %>%
      dplyr::select(data3_id, source, county_std, county, school_name,
                    school_name_std, level_code, level_code_match, school_type,
                    city, dplyr::any_of("state"), zip,
                    dplyr::any_of(c("lat", "lon")),
                    street, addr_clean,
                    dplyr::any_of(c("district", "district_std")))
  ) %>%
    dplyr::arrange(county_std, school_name_std, match_score)
  
  matched_df
}


# Internal: apply the chosen address source preference to matched_df
.apply_addr_source_pref <- function(matched_df, other_dat, addr_source_pref) {
  if (addr_source_pref == "doe" &&
      "addr_clean_doe" %in% colnames(matched_df)) {
    matched_df <- matched_df %>%
      dplyr::mutate(
        addr_clean = dplyr::coalesce(addr_clean_doe, addr_clean),
        street     = dplyr::coalesce(street_doe, street),
        city       = dplyr::coalesce(city_doe, city),
        zip        = dplyr::coalesce(as.character(zip_doe), as.character(zip))
      )
  } else if (addr_source_pref == "third" &&
             !is.null(other_dat) &&
             "addr_clean_third" %in% colnames(matched_df)) {
    matched_df <- matched_df %>%
      dplyr::mutate(
        addr_clean = dplyr::coalesce(addr_clean_third, addr_clean),
        street     = dplyr::coalesce(street_third, street),
        city       = dplyr::coalesce(city_third, city),
        zip        = dplyr::coalesce(as.character(zip_third), as.character(zip))
      )
  }
  matched_df
}


# ==============================================================================
# PART 3: BUILD KINDER UNIQUE SCHOOLS  (data.table-accelerated)
# ==============================================================================

#' Deduplicate and clean kindergarten vaccination school records
#'
#' Creates one row per unique (school, county, type, level) combination,
#' fixes missing school_level / school_type values, and applies a
#' majority-vote correction for school_type inconsistencies across years.
#' Uses \pkg{data.table} for all grouped aggregations to maximise speed.
#'
#' @param kinder_dat Data frame of cleaned kindergarten records. Must contain
#'   columns \code{school_name_std}, \code{county_std}, \code{school_type},
#'   \code{school_level}, \code{level_code}, \code{year_source}, and
#'   \code{vacc_data_id} (integer).  When a \code{district_std} column is
#'   present, the modal (most common non-\code{NA}) district per unique school
#'   is preserved in the output.
#' @param n_years_data Integer. Number of distinct year sources; used as the
#'   threshold in \code{fix_school_level_na} / \code{fix_school_type_na} and
#'   for the majority-vote mistype fix.
#'
#' @return A data frame with columns \code{school_name_std}, \code{county_std},
#'   \code{school_type}, \code{school_level}, \code{level_code},
#'   \code{n_records}, \code{year_sources}, \code{vacc_data_ids},
#'   \code{school_names_orig}, \code{vacc_school_id}, and (when available in
#'   input) \code{district_std}. \code{school_names_orig} is the "; "-packed
#'   set of distinct \code{school_name} values (kinder's own name BEFORE
#'   \code{\link{add_school_level}} strips level words like "elementary" out
#'   of \code{school_name_std}) that collapsed into this group — used
#'   downstream to geocode on the fuller original name first, per
#'   \code{\link{geocode_original_then_std}}'s original-name-first design.
#' @keywords internal
build_kinder_unique_schools <- function(kinder_dat, n_years_data) {

  key_cols <- c("school_name_std", "county_std", "school_type",
                "school_level", "level_code")

  has_district <- "district_std" %in% colnames(kinder_dat)

  if (has_district){
    key_cols <- c(key_cols, "district_std")
  }

  # ---- Pass 1: initial aggregation from raw records --------------------------
  dt <- data.table::as.data.table(kinder_dat)
  dt[, vacc_data_id := as.character(vacc_data_id)]

  dt_unique <- dt[, .(
    n_records     = .N,
    year_sources  = paste(sort(unique(year_source)), collapse = "; "),
    vacc_data_ids = paste(sort(unique(vacc_data_id)), collapse = "; "),
    # Kinder's own name BEFORE add_school_level() strips level words out of
    # school_name_std (e.g. "laurel elementary" -> std "laurel") -- the
    # fuller name geocodes far more reliably than the bare, stripped one
    # (a bare "laurel" + county query is ambiguous enough that Google can
    # resolve it to the wrong, generic place instead of the actual school;
    # found via a real CA run on "Laurel Elementary").
    school_names_orig = paste(sort(unique(school_name)), collapse = "; ")
  ), by = key_cols][
    order(school_name_std, county_std)
  ][, vacc_school_id := .I][]

  # ---- Fix NAs in school_level and school_type (existing package functions) --
  dt_unique <- fix_school_level_na(data = dt_unique, n_years_data = n_years_data,
                                   id_col = "vacc_school_id")
  dt_unique <- fix_school_type_na(data  = dt_unique, n_years_data = n_years_data,
                                  id_col = "vacc_school_id")

  # ---- Pass 2: re-aggregate after level/type fixes ---------------------------
  dt_unique <- .reaggregate_kinder_by(
    dt_unique[, c(key_cols, "n_records", "year_sources", "vacc_data_ids", "school_names_orig"),
              with = FALSE],
    key_cols = key_cols
  )

  # ---- Majority-vote school_type correction ----------------------------------
  mistype_grp <- c("school_name_std", "county_std", "school_level", "level_code")
  if (has_district) mistype_grp <- c(mistype_grp, "district_std")

  dt_unique[, total_recs := sum(n_records), by = mistype_grp]
  dt_unique[, type_wt    := n_records / total_recs,  by = mistype_grp]

  # Identify rows belonging to fixable groups
  ids_to_fix <- dt_unique[
    total_recs <= n_years_data & type_wt != 1,
    if (any(type_wt > 0.5)) vacc_school_id,
    by = mistype_grp
  ]$vacc_school_id

  if (length(ids_to_fix) > 0) {
    # Set school_type to the majority value within each group
    dt_unique[
      vacc_school_id %in% ids_to_fix,
      school_type := school_type[which.max(type_wt)],
      by = mistype_grp
    ]
    # Remove helper columns before Pass 3
    dt_unique[, c("total_recs", "type_wt") := NULL]
    # Pass 3: re-aggregate after mistype fix
    dt_unique <- .reaggregate_kinder_by(
      dt_unique[, c(key_cols, "n_records", "year_sources", "vacc_data_ids", "school_names_orig"),
                with = FALSE],
      key_cols = key_cols
    )
  } else {
    dt_unique[, c("total_recs", "type_wt") := NULL]
  }

  as.data.frame(dt_unique)
}


# Internal: merge kinder school-groups that differ ONLY in school_type
# (public/private, most commonly — build_kinder_unique_schools() keys on
# school_type, so an inconsistently-reported type across years splits one
# real school into two permanently-separate groups) when the school is
# absent from BOTH greatschools_dat and doe_dat entirely. With no
# authoritative source to arbitrate, default to "private" -- a state's DOE
# roster is definitionally its public-school list, so a school missing
# from it cannot be public. This is deliberately narrower than
# build_kinder_unique_schools()'s own majority-vote school_type fix (which
# only fires when the group's total record count is small relative to
# n_years_data): it's gated on source absence, not record-count shape, so
# it still catches a school whose type flip-flopped across MOST of its
# reporting history.
#
# name_col/county_col/district_col let a caller point at whichever columns
# hold the standardized name/county/district in kinder_unique and the two
# source frames — all three are assumed to already share the same
# standardization convention (as they do throughout this pipeline).
#' @keywords internal
.merge_kinder_type_duplicates_if_unlisted <- function(kinder_unique, greatschools_dat, doe_dat,
                                                       name_col = "school_name_std") {
  if (nrow(kinder_unique) == 0) return(kinder_unique)

  known_names <- unique(c(
    if (name_col %in% names(greatschools_dat)) greatschools_dat[[name_col]] else NULL,
    if (name_col %in% names(doe_dat)) doe_dat[[name_col]] else NULL
  ))
  known_names <- known_names[!is.na(known_names) & known_names != ""]

  group_cols <- intersect(c(name_col, "county_std", "district_std"), names(kinder_unique))

  dt <- data.table::as.data.table(kinder_unique)
  dt[, .n_types    := data.table::uniqueN(school_type), by = group_cols]
  dt[, .in_sources := .SD[[1]] %in% known_names, .SDcols = name_col]
  dt[, .fix_me     := .n_types > 1L & !.in_sources]

  fixable <- dt[.fix_me == TRUE]
  if (nrow(fixable) == 0) {
    dt[, c(".n_types", ".in_sources", ".fix_me") := NULL]
    return(as.data.frame(dt))
  }

  pack <- function(x) paste(sort(unique(unlist(strsplit(x, "; ", fixed = TRUE)))), collapse = "; ")

  merged <- fixable[, .(
    n_records         = sum(n_records, na.rm = TRUE),
    year_sources      = pack(year_sources),
    vacc_data_ids     = pack(vacc_data_ids),
    school_names_orig = if ("school_names_orig" %in% names(fixable)) pack(school_names_orig) else NA_character_,
    school_type       = "private",
    school_level      = school_level[1],
    level_code        = level_code[1]
  ), by = group_cols]

  rest <- dt[.fix_me == FALSE | is.na(.fix_me)]
  rest[, c(".n_types", ".in_sources", ".fix_me") := NULL]

  out <- data.table::rbindlist(list(rest, merged), fill = TRUE)
  data.table::setorderv(out, c(name_col, "county_std"))
  out[, vacc_school_id := .I]

  as.data.frame(out)
}


# Internal: detect kinder rows whose county_std looks like a one-off
# data-entry typo rather than a real second school. Triggering pattern: the
# same school_name_std is reported under two (or more) counties across
# years, one of them ("dominant") accounting for most of the records, and a
# minority county's year(s) DON'T overlap with the dominant county's own
# year coverage at all -- i.e. the dominant county's records are missing
# exactly the year(s) the minority county has, rather than the minority
# county's year(s) being duplicately available there too. That specific
# complementary-gap shape is what a typo looks like; two genuinely
# different, coincidentally same-named schools in different counties
# wouldn't have their years partition so neatly.
#
# Found via a real MD case: "Woodmore Elementary School" reported under
# Prince George's County for every year except 2022, and under Anne
# Arundel County for exactly 2022 -- Prince George's own records are
# missing 2022, not duplicated there.
#
# Returns a lookup (orig_id, county_std, corrected_county_std) for every
# kinder row belonging to a flagged minority-county group; the caller
# decides whether/when to actually apply the correction (see the
# "geocoding never succeeded" gate at the call site in
# .run_school_matching_pipeline() -- this function only does the pure,
# no-I/O pattern detection, so it's unit-testable on its own).
#' @keywords internal
.detect_kinder_county_outliers <- function(kinder_elem, name_col = "school_name_std") {
  empty <- tibble::tibble(orig_id = character(0), county_std = character(0),
                          corrected_county_std = character(0))
  required_cols <- c(name_col, "county_std", "year_sources", "n_records", "orig_id")
  if (nrow(kinder_elem) == 0 || !all(required_cols %in% names(kinder_elem))) return(empty)

  by_name_county <- kinder_elem %>%
    dplyr::filter(!is.na(.data[[name_col]]), !is.na(county_std), county_std != "") %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(c(name_col, "county_std")))) %>%
    dplyr::summarise(
      n_total = sum(n_records, na.rm = TRUE),
      years   = list(unique(unlist(strsplit(paste(year_sources, collapse = "; "), "; ", fixed = TRUE)))),
      .groups = "drop"
    ) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(name_col))) %>%
    dplyr::filter(dplyr::n() > 1) %>%
    dplyr::ungroup()

  # Check for emptiness BEFORE computing .is_dominant: max() on an empty
  # group (every name has exactly one county, so nothing survived the
  # filter above) warns "no non-missing arguments to max" even though the
  # mutate below would apply to zero rows either way.
  if (nrow(by_name_county) == 0) return(empty)

  by_name_county <- by_name_county %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(name_col))) %>%
    dplyr::mutate(.is_dominant = n_total == max(n_total)) %>%
    dplyr::ungroup()

  dominant <- by_name_county %>%
    dplyr::filter(.is_dominant) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(name_col))) %>%
    dplyr::slice(1) %>%  # deterministic tie-break
    dplyr::ungroup() %>%
    dplyr::select(dplyr::all_of(name_col), dom_county = county_std, dom_years = years)

  outliers <- by_name_county %>%
    dplyr::filter(!.is_dominant) %>%
    dplyr::inner_join(dominant, by = name_col) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(.overlaps = length(intersect(years, dom_years)) > 0) %>%
    dplyr::ungroup() %>%
    dplyr::filter(!.overlaps)

  if (nrow(outliers) == 0) return(empty)

  kinder_elem %>%
    dplyr::inner_join(
      outliers %>% dplyr::select(dplyr::all_of(name_col), county_std, corrected_county_std = dom_county),
      by = c(name_col, "county_std")
    ) %>%
    dplyr::transmute(orig_id = as.character(orig_id), county_std, corrected_county_std)
}


# Internal: re-aggregate a data.table that already has packed n_records/
# year_sources/vacc_data_ids columns, grouping by key_cols and re-deriving a
# fresh sequential vacc_school_id. Shared by build_kinder_unique_schools()'s
# repeated re-aggregation passes (grouped by name/county/type/level/district)
# and .collapse_kinder_by_place_id() (which groups by place_id instead, after
# picking a canonical row's attribute columns for the group — see that
# function; it does not call this one directly for that reason, but reuses
# the same merge expressions).
.reaggregate_kinder_by <- function(dt, key_cols) {
  dt[, .(
    n_records     = sum(n_records, na.rm = TRUE),
    year_sources  = paste(sort(unique(
      unlist(strsplit(year_sources, "; ", fixed = TRUE)))), collapse = "; "),
    vacc_data_ids = paste(sort(unique(
      unlist(strsplit(vacc_data_ids, "; ", fixed = TRUE)))), collapse = "; "),
    school_names_orig = paste(sort(unique(
      unlist(strsplit(school_names_orig, "; ", fixed = TRUE)))), collapse = "; ")
  ), by = key_cols][
    order(school_name_std, county_std)
  ][, vacc_school_id := .I][]
}



# Internal: pick a single, geocoding-friendly name per kinder school-group —
# the first (alphabetically, for determinism) of build_kinder_unique_schools()'s
# "; "-packed school_names_orig, which predates add_school_level() stripping
# level words (e.g. "elementary") out of school_name_std. Geocoding on the
# bare, stripped name is needlessly vague ("laurel" + county vs "laurel
# elementary" + county) and can resolve to the wrong, generic place instead
# of the actual school — found via a real CA run on "Laurel Elementary".
# Pure string logic, factored out so it's unit-testable without a live
# geocoding call.
#' @keywords internal
.kinder_geocode_name <- function(school_names_orig, school_name_std) {
  first_orig <- trimws(sub(";.*$", "", school_names_orig))
  dplyr::coalesce(dplyr::na_if(first_orig, ""), school_name_std)
}


# Internal: expand a "; "-packed column to individual rows using data.table
# Returns a data.table with vacc_data_id (character) plus by_cols.
.dt_expand_ids <- function(df, packed_col, by_cols) {
  dt <- data.table::as.data.table(df)
  dt[, .(vacc_data_id = as.character(
    unlist(strsplit(.SD[[packed_col]], "; ", fixed = TRUE))
  )), by = by_cols]
}


# Internal: attach a representative (modal) address to each unique kinder
# school-group. build_kinder_unique_schools() aggregates away per-record
# address columns (its output is keyed on name/county/type/level, not
# address), so callers that want to geocode the unique-school table — namely
# Part 3.5 of standardize_schools(), to resolve a place_id per kinder school
# ahead of matching — need this attached back on first.
.attach_kinder_addresses <- function(kinder_dat_for_matching, kinder_dat) {
  addr_cols <- intersect(
    c("addr_clean_kinder", "city_kinder", "zip_kinder"),
    colnames(kinder_dat)
  )
  if (length(addr_cols) == 0) return(kinder_dat_for_matching)

  addr_map <- kinder_dat %>%
    dplyr::mutate(vacc_data_id = as.character(vacc_data_id)) %>%
    dplyr::select(vacc_data_id, dplyr::all_of(addr_cols))

  exp <- as.data.frame(.dt_expand_ids(
    kinder_dat_for_matching[, c("vacc_data_ids", "vacc_school_id")],
    packed_col = "vacc_data_ids", by_cols = "vacc_school_id"
  )) %>%
    dplyr::left_join(addr_map, by = "vacc_data_id")

  addr_modal <- exp %>%
    dplyr::group_by(vacc_school_id) %>%
    dplyr::summarise(
      dplyr::across(dplyr::all_of(addr_cols), ~ .get_modal_value(as.character(.x))),
      .groups = "drop"
    )

  kinder_dat_for_matching %>%
    dplyr::left_join(addr_modal, by = "vacc_school_id")
}


# Internal: collapse kinder unique-school rows that share a resolved
# place_id — the kinder-side counterpart of collapse_reference_by_place_id()
# (school_identity_utils.R), used for the GreatSchools/DOE sources. Unlike
# .reaggregate_kinder_by() (which groups by identical name/county/type/level
# and can just re-derive every column from the grouping key), this groups by
# place_id across rows that may carry DIFFERENT names/types/levels recorded
# in different years — so it additionally has to decide which row's
# attributes to keep as canonical. It keeps the row whose own individual
# year is most recent (year_sources is pre-sorted ascending, so a row's last
# token is its own latest year), on the theory that the most recently
# reported name/type/level is the one most likely to still be current.
#
# Rows without a resolved place_id pass through untouched. Returns
# list(data=, crosswalk=) in the same shared crosswalk schema as
# collapse_reference_by_place_id().
#' @keywords internal
.collapse_kinder_by_place_id <- function(dt_unique) {

  empty_crosswalk <- tibble::tibble(
    source = character(0), orig_id = character(0), orig_name = character(0),
    orig_name_std = character(0), place_id = character(0), new_id = character(0),
    new_name = character(0), new_name_std = character(0),
    n_collapsed = integer(0), collapsed = logical(0)
  )

  if (nrow(dt_unique) == 0 || !"place_id" %in% names(dt_unique)) {
    return(list(data = dt_unique, crosswalk = empty_crosswalk))
  }

  dt <- data.table::as.data.table(dt_unique)
  dt[, orig_id    := as.character(vacc_school_id)]
  dt[, latest_year := sub(".*; ", "", year_sources)]
  dt[, group_key   := data.table::fifelse(
    !is.na(place_id) & place_id != "", place_id, paste0("__row__", orig_id)
  )]
  # Sort so the most-recent-year row of each group lands first; the
  # aggregation below takes its attribute columns as the group's canonical
  # values via .SD[1L].
  data.table::setorder(dt, group_key, -latest_year)

  attr_cols <- intersect(
    c("school_name_std", "county_std", "school_type", "school_level",
      "level_code", "district_std", "addr_clean_kinder", "city_kinder",
      "zip_kinder", "place_id"),
    names(dt)
  )

  collapsed <- dt[, c(
    lapply(.SD, `[`, 1L),
    list(
      n_records     = sum(n_records, na.rm = TRUE),
      year_sources  = paste(sort(unique(
        unlist(strsplit(year_sources, "; ", fixed = TRUE)))), collapse = "; "),
      vacc_data_ids = paste(sort(unique(
        unlist(strsplit(vacc_data_ids, "; ", fixed = TRUE)))), collapse = "; "),
      orig_id       = orig_id[1L]
    )
  ), by = group_key, .SDcols = attr_cols]

  collapsed <- collapsed[order(school_name_std, county_std)]
  collapsed[, vacc_school_id := .I]

  id_map <- collapsed[, .(group_key, new_id = as.character(vacc_school_id),
                          new_name = school_name_std)]
  group_sizes <- dt[, .N, by = group_key]

  crosswalk_dt <- dt[, .(group_key, orig_id, orig_name = school_name_std,
                         orig_name_std = school_name_std, place_id)]
  crosswalk_dt <- merge(crosswalk_dt, id_map, by = "group_key", all.x = TRUE)
  crosswalk_dt <- merge(crosswalk_dt, group_sizes, by = "group_key", all.x = TRUE)
  data.table::setnames(crosswalk_dt, "N", "n_collapsed")
  crosswalk_dt[, `:=`(
    source       = "kinder",
    new_name_std = new_name,
    collapsed    = orig_id != new_id
  )]
  crosswalk_dt <- crosswalk_dt[, .(source, orig_id, orig_name, orig_name_std,
                                   place_id, new_id, new_name, new_name_std,
                                   n_collapsed, collapsed)]

  collapsed[, c("group_key", "orig_id") := NULL]

  list(
    data      = as.data.frame(collapsed),
    crosswalk = tibble::as_tibble(crosswalk_dt)
  )
}


# ==============================================================================
# PARTS 4–5: MATCH KINDER TO REFERENCE  (data.table-accelerated joins)
# ==============================================================================

#' Match kindergarten vaccination records to the school reference key
#'
#' \strong{Deprecated:} superseded by the geocode-first sequence in
#' \code{\link{standardize_schools}} — exact-match kinder against the master
#' reference (\code{\link{exact_match_locations}}, requiring every
#' geographic column actually available for that state), then place_id
#' match, and only fall back to fuzzy matching (\code{\link{run_matching_cascade}})
#' for kinder rows that failed to geocode on both the original and
#' standardized name. Kept for backward compatibility and as regression test
#' coverage; not called by \code{standardize_schools()}. Its output-assembly
#' logic (id expansion, joining vaccination counts, \code{addr_source_pref})
#' was extracted to \code{\link{.assemble_kinder_output}}, which the new
#' sequence calls directly.
#'
#' Runs a matching cascade (place_id, district+county+type, county+type,
#' county-only) to link kinder unique schools to the GS+DOE reference,
#' self-matches remaining unmatched records within the same county, expands
#' packed ID columns using \pkg{data.table}, joins back vaccination counts,
#' and optionally applies the kinder address preference. This is where a
#' renamed school gets bridged across years: the place_id pass links records
#' that share a Google identity regardless of name string, and the reference
#' table is expanded with a Google-current-name synonym for every school
#' (see \code{\link{augment_with_google_name_variant}}) so the remaining
#' fuzzy passes can also match a kinder record against whichever name — old
#' or new — Google currently has on file. The district pass is only executed
#' when \code{district_std} is non-\code{NA} in both \code{kinder_dat_for_matching}
#' and \code{matched_df}, and differs from \code{county_std}.
#'
#' @param kinder_dat_for_matching Output of \code{build_kinder_unique_schools},
#'   optionally with a \code{place_id} column attached by
#'   \code{\link{resolve_school_place_ids}}.
#' @param matched_df Reference key from \code{build_reference_key}.
#' @param kinder_dat Full kinder data frame (with \code{vacc_data_id}).
#' @param temp_data_dir Directory where \code{schools_unmatched_pregeocode.csv}
#'   is written.
#' @param addr_source_pref See \code{\link{standardize_schools}}.
#' @param kinder_has_addr Logical. Whether \code{kinder_dat} contains address
#'   columns (\code{addr_clean_kinder}, \code{city_kinder}, \code{zip_kinder}).
#' @param parallel See \code{\link{standardize_schools}}.
#' @param out_dir Optional directory to write \code{school_renames_detected.csv}
#'   to via \code{\link{log_school_renames}}. When \code{NULL} (default), no
#'   rename report is written.
#'
#' @return A long data frame with one row per vaccination record, merged with
#'   school metadata and (optionally) kinder addresses.
#' @keywords internal
match_kinder_to_reference <- function(kinder_dat_for_matching,
                                      matched_df,
                                      kinder_dat,
                                      temp_data_dir,
                                      addr_source_pref = "greatschools",
                                      kinder_has_addr  = FALSE,
                                      parallel         = FALSE,
                                      out_dir          = NULL) {

  matched_df <- matched_df %>%
    dplyr::distinct() %>%
    dplyr::mutate(school_id = dplyr::row_number())

  # Filter to elementary (or unknown) level
  kinder_elem <- kinder_dat_for_matching %>%
    dplyr::filter(grepl("e", level_code, fixed = FALSE) | is.na(level_code)) %>%
    dplyr::select(-dplyr::any_of("data1_id")) %>%
    dplyr::rename(data1_id = vacc_school_id)

  ref_elem <- matched_df %>%
    dplyr::filter(grepl("e", level_code, fixed = FALSE) | is.na(level_code)) %>%
    dplyr::select(-dplyr::any_of("data2_id")) %>%
    dplyr::rename(data2_id = school_id)

  # Expand the reference table with a Google-current-name synonym row for any
  # school where that differs from what's on file — lets the fuzzy passes
  # below match a kinder record against either name without requiring the
  # kinder side to have been geocoded itself.
  ref_elem_matching <- augment_with_google_name_variant(ref_elem)

  has_district <- function(d1, d2) {
    "district_std" %in% names(d1) && any(!is.na(d1$district_std) & d1$district_std != "") &&
      "district_std" %in% names(d2) && any(!is.na(d2$district_std) & d2$district_std != "")
  }
  has_place_id <- function(d1, d2) {
    "place_id" %in% names(d1) && "place_id" %in% names(d2) &&
      any(!is.na(d1$place_id)) && any(!is.na(d2$place_id))
  }

  passes <- list(
    list(label = "place_id", type = "exact_join", join_col = "place_id",
        condition = has_place_id),
    list(label = "district_type",
        match_cols1 = c("county_std", "district_std", "school_type"),
        match_cols2 = c("county_std", "district_std", "school_type"),
        threshold_jw = 0.169, threshold_jw_min = 0.3, exact_jw = 0.10,
        condition = has_district),
    list(label = "county_type",
        match_cols1 = c("county_std", "school_type"),
        match_cols2 = c("county_std", "school_type"),
        threshold_jw = 0.169, threshold_jw_min = 0.3, exact_jw = 0.10),
    list(label = "county",
        match_cols1 = c("county_std"),
        match_cols2 = c("county_std"),
        threshold_jw = 0.15, threshold_jw_min = 0.3, exact_jw = 0.10)
  )

  cascade <- run_matching_cascade(
    data1 = kinder_elem, data2 = ref_elem_matching, passes = passes,
    data_1_source = "Kinder", data_2_source = "Reference", parallel = parallel
  )
  matched_elem <- cascade$matched %>%
    dplyr::arrange(dplyr::across(dplyr::any_of(c("county_std", "district_std", "school_name_std_data1", "match_score"))))

  if (!is.null(out_dir)) log_school_renames(matched_elem, out_dir = out_dir)
  
  # ---- Build vacc_data_matched (kinder schools matched to reference) ---------
  vacc_matched <- kinder_elem %>%
    dplyr::rename(vacc_school_id = data1_id) %>%
    dplyr::inner_join(
      matched_elem %>%
        dplyr::select(match_score,
                      vacc_school_id = data1_id,
                      school_id      = data2_id),
      by = "vacc_school_id"
    ) %>%
    dplyr::filter(grepl("e", level_code, fixed = FALSE) | is.na(level_code)) %>%
    dplyr::arrange(across(any_of(c("county_std", "district_std", "school_name_std", "match_score"))))
  
  
  
  # ---- Self-match: match remaining unmatched kinder records to each other ----
  
  vacc_to_match   <- kinder_elem %>%
    dplyr::filter(!(data1_id %in% vacc_matched$vacc_school_id))
  
  if (nrow(vacc_to_match)>0){
    
    vacc_matchto <- vacc_matched %>%
      dplyr::select(-dplyr::any_of("data2_id")) %>%
      dplyr::rename(data2_id = vacc_school_id)
    
    
    if ("district_std" %in% colnames(vacc_to_match)){
      self_match <- match_schools_names(
        data1 = vacc_to_match, 
        data2 = vacc_matchto,
        match_cols1 = c("county_std", "district_std"), 
        match_cols2 = c("county_std", "district_std"),
        threshold_jw = 0.20, threshold_jw_min = 0.35, exact_jw = 0.10,
        parallel = parallel
      )
    } else {
      self_match <- match_schools_names(
        data1 = vacc_to_match, 
        data2 = vacc_matchto,
        match_cols1 = c("county_std"), 
        match_cols2 = c("county_std"),
        threshold_jw = 0.20, threshold_jw_min = 0.35, exact_jw = 0.10,
        parallel = parallel
      )
    }
    
    vacc_matched_self <- kinder_elem %>%
      dplyr::rename(vacc_school_id = data1_id) %>%
      dplyr::inner_join(
        self_match$matched %>%
          dplyr::select(match_score,
                        vacc_school_id  = data1_id,
                        vacc_school_id2 = data2_id),
        by = "vacc_school_id"
      ) %>%
      dplyr::arrange(county_std, school_name_std, match_score) %>%
      dplyr::rename(school_type_orig  = school_type,
                    school_level_orig = school_level) %>%
      dplyr::left_join(
        kinder_elem %>%
          dplyr::select(vacc_school_id2       = data1_id,
                        school_name_std_match = school_name_std,
                        school_type, school_level),
        by = "vacc_school_id2"
      ) %>%
      dplyr::rename(school_name_std_orig = school_name_std,
                    school_name_std      = school_name_std_match,
                    vacc_school_id_orig  = vacc_school_id,
                    vacc_school_id       = vacc_school_id2)
    
    # Resolve to the school_id of the matched target.
    # We only need school_id from vacc_matched; all other data (vacc_data_ids,
    # n_records, year_sources, school_name_std, etc.) already comes from the
    # original unmatched kinder_elem row stored in vacc_matched_self.
    vacc_self_cln <- vacc_matched_self %>%
      dplyr::rename(vacc_school_id_orig2 = vacc_school_id_orig) %>%
      dplyr::left_join(
        vacc_matched %>% dplyr::select(vacc_school_id, school_id),
        by = "vacc_school_id"
      )
  } else {
    vacc_self_cln <- NULL
  }
  
  # Combine all matched elementary records
  vacc_elem_all <- dplyr::bind_rows(vacc_matched, vacc_self_cln) %>%
    dplyr::arrange(county_std, school_name_std, match_score)

  .assemble_kinder_output(
    vacc_elem_all    = vacc_elem_all,
    kinder_elem      = kinder_elem,
    matched_df       = matched_df,
    kinder_dat       = kinder_dat,
    temp_data_dir    = temp_data_dir,
    addr_source_pref = addr_source_pref,
    kinder_has_addr  = kinder_has_addr
  )
}


#' Assemble the final long-form kinder output from a school_id mapping
#'
#' The tail half of what used to be \code{match_kinder_to_reference()}:
#' given a table already mapping kinder unique-school rows to a
#' \code{school_id} (however that mapping was produced — the fuzzy cascade
#' \code{match_kinder_to_reference()} still uses, or the geocode-first
#' exact/place_id/fuzzy-fallback sequence in \code{standardize_schools()}),
#' expands the packed \code{vacc_data_ids} back to one row per vaccination
#' record, joins reference-key school metadata and the raw vaccination
#' counts/years back on, reorders columns, writes the pre-geocode
#' unmatched-schools report, and applies the \code{"kinder"}
#' \code{addr_source_pref} when requested. Extracted so both matching
#' strategies produce output through the exact same, already-tested
#' assembly logic.
#'
#' @param vacc_elem_all A data frame of kinder unique-school rows already
#'   resolved to a school_id — must have \code{vacc_data_ids} (packed),
#'   \code{vacc_school_id}, \code{match_score}, and \code{school_id}.
#'   Optionally \code{vacc_school_id_orig2}, for callers (like the fuzzy
#'   cascade's self-match step) that resolve a row's identity via another
#'   row rather than directly.
#' @param kinder_elem Kinder unique-school rows (all of them, matched or
#'   not) — must have \code{vacc_data_ids}, \code{data1_id},
#'   \code{school_name_std}, \code{county_std}, \code{school_type},
#'   \code{school_level}.
#' @param matched_df The GS+DOE(+other) reference key, with \code{school_id}.
#' @param kinder_dat Full kinder data frame (with \code{vacc_data_id}).
#' @param temp_data_dir Directory where \code{schools_unmatched_pregeocode.csv}
#'   is written.
#' @param addr_source_pref See \code{\link{standardize_schools}}.
#' @param kinder_has_addr Logical. Whether \code{kinder_dat} contains address
#'   columns (\code{addr_clean_kinder}, \code{city_kinder}, \code{zip_kinder}).
#'
#' @return A long data frame with one row per vaccination record, merged with
#'   school metadata and (optionally) kinder addresses.
#' @keywords internal
.assemble_kinder_output <- function(vacc_elem_all,
                                    kinder_elem,
                                    matched_df,
                                    kinder_dat,
                                    temp_data_dir,
                                    addr_source_pref = "greatschools",
                                    kinder_has_addr  = FALSE) {

  # ---- Expand packed IDs and join (data.table replaces separate_rows) --------

  # 1. Expand matched records
  dt_exp <- .dt_expand_ids(
    vacc_elem_all[, c("vacc_data_ids", "match_score", "school_id")],
    packed_col = "vacc_data_ids",
    by_cols    = c("match_score", "school_id")
  )

  # 2. Expand kinder_elem for name lookup; rename for clarity
  dt_kinder_names <- .dt_expand_ids(
    kinder_elem[, c("vacc_data_ids", "data1_id", "school_name_std")],
    packed_col = "vacc_data_ids",
    by_cols    = c("data1_id", "school_name_std")
  )
  dt_kinder_names[, school_name_std_vacc := school_name_std]
  dt_kinder_names <- dt_kinder_names[, .(school_name_std_vacc, vacc_data_id)]

  # 3. Expand unmatched kinder records.
  # all_matched_ids collects both the directly-matched school IDs (vacc_school_id)
  # and the original IDs of self-matched schools (vacc_school_id_orig2, always
  # present as a column in vacc_elem_all because vacc_self_cln introduces it).
  orig2_col <- "vacc_school_id_orig2"
  orig2_ids <- if (orig2_col %in% names(vacc_elem_all)){
    na.omit(vacc_elem_all[[orig2_col]])
  } else {
    integer(0)
  }
  all_matched_ids <- union(vacc_elem_all$vacc_school_id, orig2_ids)
  dt_unmatched_exp <- .dt_expand_ids(
    kinder_elem %>%
      dplyr::filter(!(data1_id %in% all_matched_ids)) %>%
      dplyr::select(vacc_data_ids, data1_id, school_name_std,
                    county_std, school_type, school_level),
    packed_col = "vacc_data_ids",
    by_cols    = c("data1_id", "school_name_std",
                   "county_std", "school_type", "school_level")
  )
  dt_unmatched_exp[, school_name_std_vacc := school_name_std]
  dt_unmatched_exp <- dt_unmatched_exp[
    , .(school_name_std_vacc, vacc_data_id, county_std, school_type, school_level)
  ]

  # 4. Merge names onto expanded matched IDs
  data.table::setkey(dt_exp,          vacc_data_id)
  data.table::setkey(dt_kinder_names, vacc_data_id)
  dt_exp <- dt_kinder_names[dt_exp]   # left join: keep all dt_exp rows

  # 5. Add reference-key school metadata
  ref_cols <- c("school_id", "school_name_std", "county_std", "county",
                "school_type", "school_level", "level_code",
                "district", "district_std",
                "addr_clean", "city", "state", "zip", "lat", "lon",
                "data_1_source")
  dt_ref <- data.table::as.data.table(
    matched_df %>%
      dplyr::rename(school_name_orig = school_name,
                    match_source     = data_1_source) %>%
      dplyr::select(dplyr::any_of(c(ref_cols, "school_name_orig",
                                    "match_source")))
  )
  data.table::setkey(dt_ref, school_id)
  data.table::setkey(dt_exp, school_id)
  dt_matched_full <- dt_ref[dt_exp]   # left join on school_id

  # 6. Stack matched + unmatched
  dt_all <- data.table::rbindlist(
    list(dt_matched_full, dt_unmatched_exp), fill = TRUE
  )

  # 7. Join vaccination counts / year data back
  dt_kinder_main <- data.table::as.data.table(
    kinder_dat %>%
      dplyr::mutate(vacc_data_id = as.character(vacc_data_id)) %>%
      dplyr::select(-dplyr::any_of(c("school_name_std", "county_std",
                                     "county", "school_type", "school_level",
                                     "city", "zip", "district_std", "district")))
  )
  data.table::setkey(dt_kinder_main, vacc_data_id)
  data.table::setkey(dt_all,         vacc_data_id)
  vacc_data_cln <- dt_kinder_main[dt_all]

  # Reorder columns
  priority <- c("year_source", "vacc_data_id", "school_id", "match_score",
                "match_source", "school_name_std", "school_name_std_vacc",
                "county_std", "county", "district", "district_std",
                "school_type", "school_level",
                "level_code", "addr_clean", "city", "state", "zip",
                "lat", "lon")
  col_order <- c(intersect(priority, names(vacc_data_cln)),
                 setdiff(names(vacc_data_cln), priority))
  vacc_data_cln <- vacc_data_cln[, col_order, with = FALSE]

  vacc_data_final <- as.data.frame(vacc_data_cln)

  # ---- Write unmatched-schools pre-geocode report ----------------------------
  schools_unmatched <- vacc_data_final %>%
    dplyr::filter(is.na(school_id)) %>%
    dplyr::select(year_source, vacc_data_id,
                  dplyr::any_of(c("school_name_std_vacc", "school_name",
                                  "school_name_orig")),
                  county_std, school_level, school_type) %>%
    dplyr::distinct() %>%
    dplyr::left_join(
      as.data.frame(.dt_expand_ids(
        kinder_elem[, c("vacc_data_ids", "data1_id")],
        packed_col = "vacc_data_ids",
        by_cols    = c("data1_id")
      )) %>% dplyr::rename(vacc_school_id = data1_id),
      by = "vacc_data_id"
    )

  if (!dir.exists(temp_data_dir)) dir.create(temp_data_dir, recursive = TRUE)
  utils::write.csv(
    schools_unmatched,
    file.path(temp_data_dir, "schools_unmatched_pregeocode.csv"),
    row.names = FALSE
  )

  # ---- Apply kinder address preference (if set) ------------------------------
  if (addr_source_pref == "kinder" && isTRUE(kinder_has_addr)) {
    dt_f <- data.table::as.data.table(vacc_data_final)

    # Step 1: use kinder address where available (data.table native fifelse)
    if ("addr_clean_kinder" %in% names(dt_f)) {
      dt_f[, addr_clean := data.table::fifelse(
        is.na(addr_clean_kinder), addr_clean, addr_clean_kinder)]
      dt_f[, city := data.table::fifelse(
        is.na(city_kinder), city, city_kinder)]
      dt_f[, zip  := data.table::fifelse(
        is.na(zip_kinder), as.character(zip), as.character(zip_kinder))]
    }

    # Step 2: compute modal address/city/zip once per school_id group,
    # then fill remaining NAs
    dt_f[!is.na(school_id), `:=`(
      .addr_modal = .get_modal_value(addr_clean),
      .city_modal = .get_modal_value(city),
      .zip_modal  = .get_modal_value(as.character(zip))
    ), by = school_id]
    dt_f[, `:=`(
      addr_clean = data.table::fifelse(is.na(addr_clean), .addr_modal, addr_clean),
      city       = data.table::fifelse(is.na(city),       .city_modal, city),
      zip        = data.table::fifelse(is.na(zip),        .zip_modal,  as.character(zip))
    )]
    dt_f[, c(".addr_modal", ".city_modal", ".zip_modal") := NULL]

    vacc_data_final <- as.data.frame(dt_f)
  }

  vacc_data_final
}



# Internal: return the most common non-NA, non-empty value (modal)
.get_modal_value <- function(x) {
  x <- x[!is.na(x) & x != ""]
  if (length(x) == 0L) return(NA_character_)
  names(sort(table(x), decreasing = TRUE))[1L]
}
