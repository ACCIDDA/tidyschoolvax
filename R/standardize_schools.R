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

  # ---- PART 1.5: Resolve Google identity (place_id + current name) early -----
  # Moved here (instead of only at the very end, as previously) so that
  # matching can use a school's stable Google place_id and Google's current
  # name for that place as signals — the fix for renamed schools getting
  # matched incorrectly (or not at all) by name-string distance alone. This
  # reuses the same on-disk geocoding cache the final enrichment pass (Part 6)
  # reads from, so it does not meaningfully increase Google API usage.
  greatschools_dat <- greatschools_dat %>%
    resolve_school_place_ids(geo_dir = state_geo_dir, google_api_key = google_api_key,
                             state_abbr = state_id) %>%
    add_google_name_variants(geo_dir = state_geo_dir, google_api_key = google_api_key)

  doe_dat <- doe_dat %>%
    resolve_school_place_ids(geo_dir = state_geo_dir, google_api_key = google_api_key,
                             state_abbr = state_id) %>%
    add_google_name_variants(geo_dir = state_geo_dir, google_api_key = google_api_key)

  # ---- PART 2: Build GS+DOE+(optional third) reference key -------------------
  matched_df <- build_reference_key(
    greatschools_dat = greatschools_dat,
    doe_dat          = doe_dat,
    other_dat        = other_dat,
    addr_source_pref = addr_source_pref,
    parallel         = parallel,
    out_dir          = review_dir
  )

  # ---- PART 3: Build unique kinder school records ----------------------------
  kinder_dat <- kinder_dat %>%
    dplyr::mutate(
      school_type  = gsub(" (non-public)", "", school_type, fixed = TRUE),
      vacc_data_id = dplyr::row_number()
    )

  year_source_levels <- sort(unique(kinder_dat$year_source))
  n_years_data       <- length(year_source_levels)

  kinder_dat_for_matching <- build_kinder_unique_schools(kinder_dat, n_years_data)

  # ---- PART 3.5: Resolve Google identity for kinder unique-school groups -----
  # Only possible when the raw kinder data actually carries a street address
  # (kinder_has_addr); many states' kinder files don't, in which case kinder
  # schools still benefit from Part 1.5's Google-name augmentation on the
  # reference side (see match_kinder_to_reference()) even without their own
  # place_id.
  if (isTRUE(kinder_has_addr)) {
    kinder_dat_for_matching <- kinder_dat_for_matching %>%
      .attach_kinder_addresses(kinder_dat) %>%
      resolve_school_place_ids(
        geo_dir = state_geo_dir, google_api_key = google_api_key,
        state_abbr = state_id, name_col = "school_name_std",
        addr_col = "addr_clean_kinder", city_col = "city_kinder", zip_col = "zip_kinder"
      )
  }

  # ---- PARTS 4–5: Match kinder to reference; assemble final dataset ----------
  vacc_data_final <- match_kinder_to_reference(
    kinder_dat_for_matching = kinder_dat_for_matching,
    matched_df              = matched_df,
    kinder_dat              = kinder_dat,
    temp_data_dir           = temp_data_dir,
    addr_source_pref        = addr_source_pref,
    kinder_has_addr         = kinder_has_addr,
    parallel                = parallel,
    out_dir                 = review_dir
  )

  # ---- PART 6: Geocode and school-status lookup ------------------------------
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
# PART 2: BUILD REFERENCE KEY
# ==============================================================================

#' Build the GS + DOE (+ optional third) school reference key
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
#'   \code{vacc_school_id}, and (when available in input) \code{district_std}.
#' @keywords internal
build_kinder_unique_schools <- function(kinder_dat, n_years_data) {
  
  key_cols <- c("school_name_std", "county_std", "school_type",
                "school_level", "level_code")
  
  has_district <- "district_std" %in% colnames(kinder_dat)
  
  if (has_district){
    key_cols <- c(key_cols, "district_std")
  }
  
  # Helper: re-aggregate a data.table that already has packed id/year columns
  .reaggregate_kinder <- function(dt) {
    dt[, .(
      n_records     = sum(n_records, na.rm = TRUE),
      year_sources  = paste(sort(unique(
        unlist(strsplit(year_sources, "; ", fixed = TRUE)))), collapse = "; "),
      vacc_data_ids = paste(sort(unique(
        unlist(strsplit(vacc_data_ids, "; ", fixed = TRUE)))), collapse = "; ")
    ), by = key_cols][
      order(school_name_std, county_std)
    ][, vacc_school_id := .I][]
  }
  
  # ---- Pass 1: initial aggregation from raw records --------------------------
  dt <- data.table::as.data.table(kinder_dat)
  dt[, vacc_data_id := as.character(vacc_data_id)]
  
  dt_unique <- dt[, .(
    n_records     = .N,
    year_sources  = paste(sort(unique(year_source)), collapse = "; "),
    vacc_data_ids = paste(sort(unique(vacc_data_id)), collapse = "; ")
  ), by = key_cols][
    order(school_name_std, county_std)
  ][, vacc_school_id := .I][]
  
  # ---- Fix NAs in school_level and school_type (existing package functions) --
  dt_unique <- fix_school_level_na(data = dt_unique, n_years_data = n_years_data,
                                   id_col = "vacc_school_id")
  dt_unique <- fix_school_type_na(data  = dt_unique, n_years_data = n_years_data,
                                  id_col = "vacc_school_id")
  
  # ---- Pass 2: re-aggregate after level/type fixes ---------------------------
  dt_unique <- .reaggregate_kinder(
    dt_unique[, c(key_cols, "n_records", "year_sources", "vacc_data_ids"),
              with = FALSE]
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
    dt_unique <- .reaggregate_kinder(
      dt_unique[, c(key_cols, "n_records", "year_sources", "vacc_data_ids"),
                with = FALSE]
    )
  } else {
    dt_unique[, c("total_recs", "type_wt") := NULL]
  }
  
  as.data.frame(dt_unique)
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


# ==============================================================================
# PARTS 4–5: MATCH KINDER TO REFERENCE  (data.table-accelerated joins)
# ==============================================================================

#' Match kindergarten vaccination records to the school reference key
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
