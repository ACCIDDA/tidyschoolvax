# ==============================================================================
# review_unmatched_schools.R
# Purpose: Help a reviewer decide why a kindergarten-data school was not linked
#   to any reference school, by listing the closest candidate from each
#   reference source.
# ==============================================================================


#' Review kindergarten schools that matched no reference school
#'
#' \code{\link{standardize_schools}} links each kindergarten-data school to a
#' reference school (GreatSchools, the state's DOE directory, an optional third
#' source) and takes its \code{school_id}, name and status from there. Schools
#' it cannot link keep \code{school_id = NA}. For each such school this lists the
#' closest candidate from every reference source, and NCES, with a likely
#' explanation, so the unmatched schools can be reviewed and wrong ones fixed by
#' hand. In practice most are closed or renamed schools, or have a name that is
#' formatted differently in the reference source.
#'
#' Candidates are the best name match by Jaro-Winkler similarity of the
#' standardized names. Reference sources are searched within the same county;
#' NCES (which has no county) is searched within \code{nces_max_dist_m} metres.
#' NCES keeps former names, so a renamed school can match on its old name; the
#' name that matched is reported alongside the current one. The distances use
#' each school's median coordinates, which for unmatched schools come from
#' geocoding the name alone and are therefore approximate.
#'
#' \code{standardize_schools()} calls this at the end of a run and writes the
#' result to \code{unmatched_school_review.csv} in the review directory. It passes
#' the reference tables the matching used, which by default are limited to
#' elementary schools, so a school that was left out of matching (middle, high or
#' pre-K-only) cannot appear as a candidate. Call the function directly with
#' unfiltered tables (for example \code{clean_doe_data(..., elementary_only = FALSE)})
#' to search them as well.
#'
#' @param school_vax_joined The data frame returned by
#'   \code{\link{standardize_schools}}. Needs \code{school_id},
#'   \code{school_name}, \code{school_name_std_vacc}, \code{county_std} and
#'   \code{year_source}; \code{lat} and \code{lon} are used when present.
#' @param reference_sources A named list of cleaned reference data frames, for
#'   example \code{list(doe = doe_dat, greatschools = greatschools_dat)}. Each
#'   needs \code{school_name}, \code{school_name_std} and \code{county_std};
#'   \code{city}, \code{lat}, \code{lon} and a status column
#'   (\code{current_status} or \code{business_status}) are used when present.
#'   The names become column prefixes.
#' @param nces_ref Optional NCES reference from \code{\link{load_nces_reference}}.
#' @param nces_max_dist_m Search radius for NCES candidates, in metres.
#' @param strong_sim,possible_sim Similarity thresholds for a confident and a
#'   possible name match.
#' @param strong_dist_m An NCES match is only called confident when it is also
#'   within this many metres.
#'
#' @return A tibble with one row per unmatched school (name and county):
#'   \code{school_name}, \code{county_std}, \code{reporting}, \code{likely_explanation},
#'   \code{best_sim}, first and last year and number of years of data, the
#'   approximate \code{lat}/\code{lon}, then for each source \code{<source>_name},
#'   \code{_matched_name} (NCES former name), \code{_city}, \code{_status},
#'   \code{_detail}, \code{_sim} and \code{_dist_m}, and blank
#'   \code{reviewer_decision}, \code{matched_reference_name} and
#'   \code{reviewer_notes} columns for recording manual decisions. Zero rows if
#'   every school matched.
#'
#' @export
review_unmatched_schools <- function(school_vax_joined,
                                     reference_sources = list(),
                                     nces_ref          = NULL,
                                     nces_max_dist_m   = 25000,
                                     strong_sim        = 0.9,
                                     possible_sim      = 0.8,
                                     strong_dist_m     = 5000) {

  needed <- c("school_id", "school_name", "school_name_std_vacc", "county_std", "year_source")
  missing_cols <- setdiff(needed, names(school_vax_joined))
  if (length(missing_cols) > 0L) {
    stop("school_vax_joined is missing column(s): ", paste(missing_cols, collapse = ", "),
         call. = FALSE)
  }
  if (length(reference_sources) > 0L &&
      (is.null(names(reference_sources)) || any(!nzchar(names(reference_sources))))) {
    stop("reference_sources must be a named list.", call. = FALSE)
  }

  df <- as.data.frame(school_vax_joined)
  if (!"lat" %in% names(df)) df$lat <- NA_real_
  if (!"lon" %in% names(df)) df$lon <- NA_real_
  df$.year <- suppressWarnings(as.integer(substr(df$year_source, 1, 4)))
  latest_year <- suppressWarnings(max(df$.year, na.rm = TRUE))

  unmatched <- df[is.na(df$school_id), , drop = FALSE] %>%
    dplyr::group_by(school_name, county_std) %>%
    dplyr::summarise(
      school_name_std = dplyr::first(school_name_std_vacc),
      first_year      = suppressWarnings(min(.year, na.rm = TRUE)),
      last_year       = suppressWarnings(max(.year, na.rm = TRUE)),
      n_years         = dplyr::n_distinct(.year),
      # these schools were geocoded by name alone: the location is approximate
      lat             = .median_or_na(lat),
      lon             = .median_or_na(lon),
      .groups = "drop"
    )
  if (nrow(unmatched) == 0L) {
    message("review_unmatched_schools(): every school matched a reference school.")
    return(tibble::as_tibble(unmatched))
  }

  # ---- candidates from each source ---------------------------------------------
  review <- unmatched
  used_sources <- character()
  for (src in names(reference_sources)) {
    ref <- .prepare_review_reference(reference_sources[[src]], src)
    if (is.null(ref)) next
    review <- dplyr::bind_cols(review, .review_candidates(unmatched, ref, src, same_county = TRUE))
    used_sources <- c(used_sources, src)
  }
  if (!is.null(nces_ref) && nrow(nces_ref) > 0L) {
    ref <- .prepare_nces_review_reference(nces_ref)
    review <- dplyr::bind_cols(
      review,
      .review_candidates(unmatched, ref, "nces", same_county = FALSE, max_dist_m = nces_max_dist_m)
    )
  }

  sim_cols  <- grep("_sim$", names(review), value = TRUE)
  best_sim  <- if (length(sim_cols) > 0L) {
    do.call(pmax, c(unname(as.list(review[sim_cols])), na.rm = TRUE))
  } else {
    rep(NA_real_, nrow(review))
  }
  best_sim[!is.finite(best_sim)] <- NA_real_

  strong_ref <- rep(FALSE, nrow(review))
  for (src in used_sources) {
    s <- review[[paste0(src, "_sim")]]
    strong_ref <- strong_ref | (!is.na(s) & s >= strong_sim)
  }
  nces_strong <- nces_closed <- nces_renamed <- rep(FALSE, nrow(review))
  if ("nces_sim" %in% names(review)) {
    nces_strong  <- !is.na(review$nces_sim) & review$nces_sim >= strong_sim &
      !is.na(review$nces_dist_m) & review$nces_dist_m <= strong_dist_m
    nces_closed  <- nces_strong & review$nces_status %in% "CLOSED_PERMANENTLY"
    nces_renamed <- nces_strong & !is.na(review$nces_matched_name)
  }

  review$best_sim <- best_sim
  review$likely_explanation <- dplyr::case_when(
    nces_closed ~
      "closed school: matches a closed NCES school nearby",
    nces_renamed ~
      "renamed school: matches an open NCES school by its former name",
    nces_strong | strong_ref ~
      "open school in a reference source under a similar name (name mismatch)",
    !is.na(best_sim) & best_sim >= possible_sim ~
      "possible name match (lower similarity): check",
    is.finite(latest_year) & review$last_year < latest_year - 1L ~
      "no similar reference school; stopped reporting before the latest years, probably closed",
    TRUE ~
      "no similar reference school; still reporting: check"
  )
  review$reporting <- dplyr::if_else(
    is.finite(latest_year) & review$last_year >= latest_year,
    "still reporting in the latest year",
    paste0("last reported ", review$last_year)
  )
  review$reviewer_decision      <- NA_character_
  review$matched_reference_name <- NA_character_
  review$reviewer_notes         <- NA_character_

  review %>%
    dplyr::select(-school_name_std) %>%
    dplyr::relocate(school_name, county_std, reporting, likely_explanation, best_sim) %>%
    dplyr::arrange(likely_explanation, county_std, school_name) %>%
    tibble::as_tibble()
}


# Median of the finite values, NA if there are none (no warning on empty input).
.median_or_na <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) == 0L) NA_real_ else stats::median(x)
}


# Great-circle distance in metres.
.haversine_metres <- function(lat1, lon1, lat2, lon2) {
  rad <- pi / 180
  a <- sin((lat2 - lat1) * rad / 2)^2 +
    cos(lat1 * rad) * cos(lat2 * rad) * sin((lon2 - lon1) * rad / 2)^2
  6371000 * 2 * asin(pmin(1, sqrt(a)))
}


# Reduce a cleaned reference table to the columns the review uses.
.prepare_review_reference <- function(ref, source_name) {
  ref <- as.data.frame(ref)
  req <- c("school_name", "school_name_std", "county_std")
  miss <- setdiff(req, names(ref))
  if (length(miss) > 0L) {
    warning("review_unmatched_schools(): skipping source '", source_name,
            "', missing column(s): ", paste(miss, collapse = ", "), call. = FALSE)
    return(NULL)
  }
  status_col <- intersect(c("current_status", "business_status"), names(ref))[1]
  data.frame(
    ref_name        = ref$school_name,
    ref_variant     = ref$school_name,
    ref_city        = if ("city" %in% names(ref)) ref$city else NA_character_,
    ref_status      = if (is.na(status_col)) NA_character_ else toupper(as.character(ref[[status_col]])),
    ref_detail      = if ("school_type" %in% names(ref)) as.character(ref$school_type) else NA_character_,
    school_name_std = ref$school_name_std,
    county_std      = ref$county_std,
    lat             = if ("lat" %in% names(ref)) suppressWarnings(as.numeric(ref$lat)) else NA_real_,
    lon             = if ("lon" %in% names(ref)) suppressWarnings(as.numeric(ref$lon)) else NA_real_,
    stringsAsFactors = FALSE
  )
}


# NCES: one row per (school, name variant), names standardized like every other source.
.prepare_nces_review_reference <- function(nces_ref) {
  nces <- as.data.frame(nces_ref)
  variants <- strsplit(dplyr::coalesce(nces$nces_names, nces$nces_name), " | ", fixed = TRUE)
  idx <- rep(seq_len(nrow(nces)), lengths(variants))
  out <- data.frame(
    school_name = unlist(variants),
    county      = NA_character_,
    school_type = NA_character_,
    ref_name    = nces$nces_name[idx],
    ref_city    = NA_character_,
    ref_status  = toupper(as.character(nces$business_status[idx])),
    ref_detail  = if ("last_seen" %in% names(nces)) paste0("last seen ", nces$last_seen[idx]) else NA_character_,
    lat         = as.numeric(nces$lat[idx]),
    lon         = as.numeric(nces$lon[idx]),
    stringsAsFactors = FALSE
  )
  std <- purrr::pluck(clean_kinder_data(out), "data")
  data.frame(
    ref_name        = out$ref_name,
    ref_variant     = out$school_name,
    ref_city        = out$ref_city,
    ref_status      = out$ref_status,
    ref_detail      = out$ref_detail,
    school_name_std = std$school_name_std,
    county_std      = NA_character_,
    lat             = out$lat,
    lon             = out$lon,
    stringsAsFactors = FALSE
  )
}


# Best candidate for every unmatched school from one source; columns prefixed.
.review_candidates <- function(unmatched, ref, prefix, same_county, max_dist_m = Inf) {
  none <- tibble::tibble(name = NA_character_, matched_name = NA_character_, city = NA_character_,
                         status = NA_character_, detail = NA_character_, sim = NA_real_,
                         dist_m = NA_real_)
  rows <- lapply(seq_len(nrow(unmatched)), function(i) {
    school <- unmatched[i, ]
    cand <- ref
    if (same_county) cand <- cand[cand$county_std %in% school$county_std, , drop = FALSE]
    cand$dist_m <- .haversine_metres(school$lat, school$lon, cand$lat, cand$lon)
    if (is.finite(max_dist_m)) cand <- cand[!is.na(cand$dist_m) & cand$dist_m <= max_dist_m, , drop = FALSE]
    cand <- cand[!is.na(cand$school_name_std), , drop = FALSE]
    if (nrow(cand) == 0L || is.na(school$school_name_std)) return(none)
    cand$sim <- stringdist::stringsim(school$school_name_std, cand$school_name_std,
                                      method = "jw", p = 0.1)
    best <- cand[order(-cand$sim, cand$dist_m), , drop = FALSE][1, ]
    tibble::tibble(
      name         = best$ref_name,
      matched_name = if (!identical(best$ref_variant, best$ref_name)) best$ref_variant else NA_character_,
      city         = best$ref_city,
      status       = best$ref_status,
      detail       = best$ref_detail,
      sim          = round(best$sim, 3),
      dist_m       = round(best$dist_m)
    )
  })
  dplyr::bind_rows(rows) %>% dplyr::rename_with(~ paste0(prefix, "_", .x))
}
