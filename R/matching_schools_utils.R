



#' Deduplicate school_info based on key columns
#'
#' Removes duplicate rows based on standardized school name and location fields.
#'
#' @param school_info A data frame containing school metadata.
#' @return A cleaned data frame with unique school entries and assigned IDs.
deduplicate_school_info <- function(school_info) {
  school_info_clean <- school_info %>%
    dplyr::select(-objectid) %>%
    distinct(school_name_std, school_name, county, school_type, street, .keep_all = TRUE) %>%
    dplyr::mutate(school_info_id = dplyr::row_number(),
                  school_id_matched = NA)
  return(school_info_clean)
}




#' Assign unique IDs to vaccination data rows
#'
#' @param vax_data A data frame of school-level vaccination data.
#' @return The same data frame with a new column `vacc_school_id`.
assign_vaccination_ids <- function(vax_data) {
  vax_data %>%
    dplyr::mutate(vacc_school_id = dplyr::row_number())
}











#' @name standardize_string
#' @title standardize_string
#' @description Standardize indivual levels of a location of grouped names, separated by "|", for example "DC|Maryland".
#' This is used by standardize_location_strings.
#' @param string string location name to standardize.
#' @return standardized string of the location names.
standardize_string <- function(string){
  
  ranked_encodings <- c('UTF-8', 'LATIN1') # Encodings to try
  string <- as.character(string)
  string <- strsplit(string, '|', fixed = TRUE)
  string <- lapply(string, function(x){
    return(
      stringi::stri_trans_general(
        stringi::stri_trans_general(
          stringi::stri_trans_general(x,"Any-Latn"),
          "Latin-ASCII"
        ),
        "Any-Lower"
      )
    )
  })
  string <- lapply(string, function(x){
    gsub(' ', '', x)
  })
  string <- lapply(string, function(x){
    gsub('[[:punct:]]', '', x)
  })
  string[sapply(string, length) == 0] <- ''
  return(string)
}




#' @name standardize_location_strings
#' @title standardize_location_strings
#' @description Standardize each level of a string name. This is used by others functions that standardize and match names.
#' @param location_name location name to match
#' @return standardized string of the location name.
standardize_location_strings <- function(location_name){
  if(length(location_name) == 0){
    return(location_name)
  }
  location_tmp <- location_name
  location_tmp <- gsub('|', 'vertcharacter', location_tmp, fixed = TRUE)
  location_tmp <- gsub('-', 'dashcharacter', location_tmp, fixed = TRUE)
  location_tmp <- gsub('::', 'doublecoloncharacter', location_tmp, fixed = TRUE)
  location_tmp <- standardize_string(location_tmp)
  location_tmp <- gsub('doublecoloncharacter', '::', location_tmp, fixed = TRUE)
  location_tmp <- gsub('dashcharacter', '-', location_tmp, fixed = TRUE)
  location_tmp <- gsub('vertcharacter', '|', location_tmp, fixed = TRUE)
  while(any(
    grepl(pattern = '::$', location_tmp) |
    grepl(pattern = '::NA$', location_tmp) |
    grepl(pattern = '::::', location_tmp)
  )){
    location_tmp <- gsub(':::', ':', location_tmp, fixed = TRUE)
    location_tmp <- gsub('::NA$', '', location_tmp)
    location_tmp <- gsub('::$', '', location_tmp)
  }
  return(location_tmp)
}



#' @name match_locs_level2
#' @title match_locs_level2
#' @description use string distance metrics to get best match for a location name
#' @param a location name to match
#' @param names location names to match against
#' @param return_Code TRUE/FALSE
#' @param return_name TRUE/FALSE return standardized name
#' @param return_score TRUE/FALSE
#' @param return_score_matrix TRUE/FALSE
#' @param pre_standardized Logical. If \code{TRUE}, skip the internal call to
#'   \code{standardize_location_strings()} on both \code{a} and \code{names}
#'   because the caller has already standardized them. Defaults to \code{FALSE},
#'   which preserves the original behaviour and is safe for all external callers.
#' @return ISOs, country names, matching scores, full matching distance matrix
#' @export
match_locations <- function(
    a,
    names,
    return_name=TRUE,
    return_score=FALSE,
    return_score_matrix=FALSE,
    pre_standardized=FALSE
){

  if (is.null(names)){
    stop("Need to supply vector to 'names_standard' to match against.")
  }

  if(length(a) > 1 | length(a) == 0){
    stop("ERROR: 'a' can only be of length 1")
  }
  if(is.na(a)){
    return(NA)
  }

  if (isTRUE(pre_standardized)) {
    a_cln <- as.character(a)
    b_cln <- as.character(names)
  } else {
    a_cln <- standardize_location_strings(a)
    b_cln <- standardize_location_strings(names)
  }

  # All six distance metrics computed in a single vectorised C++ pass,
  # replacing the previous for-loop over stringdist::stringdist().
  dists <- score_candidates_cpp(a_cln, b_cln)
  dists$name <- names   # restore original (unstandardised) names

  best_ <- NULL
  # get best from results
  if (any(dists$osa <= 1)){
    best_ <- which.min(dists$score_sums)
  } else if (any(dists$jw <= .1)){
    best_ <- which.min(dists$score_sums)
  } else if (any(dists$osa <= 3 & dists$jw <= 0.31 & dists$soundex == 0)){
    best_ <- which(dists$osa <= 3 & dists$jw <= 0.31 & dists$soundex == 0)
  }

  if (length(best_) == 0 & !return_score_matrix){
    return(NA)
  } else if (length(best_) == 0 & return_score_matrix){
    return(dists)
  }
  if (length(best_) > 1){
    name <- paste(names[best_], collapse = ", ")
    score_sum <- paste(dists$score_sums[best_], collapse = ", ")
  } else {
    name <- names[best_]
    score_sum <- dists$score_sums[best_]
  }

  res <- data.frame(name = name, score_sum = score_sum)
  return(res[, c(return_name, return_score), drop = FALSE])
}









# Vaccination Data Cleaning and Matching ----------------------------------


#' Fix missing school_level when only one non-NA level exists per school
#'
#' This function identifies groups (by school_name_std, county_std, school_type)
#' where school_level has NAs but all non-NA values are identical. In these cases,
#' the function fills in missing school_level values for all rows in the group.
#'
#' @param data A data.frame, tibble, or data.table containing school-level records.
#' @param n_years_data Integer. Threshold for checking unusually large group sizes.
#' @param id_col String. Column name of unique row IDs (unused; kept for backward
#'   compatibility with the previous dplyr-based implementation).
#'
#' @return A data.table when \code{data} is a data.table; a data.frame otherwise.
#'   school_level NA values are fixed where appropriate.
#' @importFrom data.table as.data.table is.data.table
#' @importFrom stats na.omit
#' @export
#'
#' @examples
#' dat <- data.frame(
#'   school_name_std = c("oak elementary", "oak elementary"),
#'   county_std      = c("riverside", "riverside"),
#'   school_type     = c("public", "public"),
#'   school_level    = c("elementary", NA_character_)
#' )
#' cleaned <- fix_school_level_na(dat, n_years_data = 10, id_col = "ids_tmp")
fix_school_level_na <- function(data, n_years_data, id_col = "ids_tmp") {

  grp_cols <- c("school_name_std", "county_std", "school_type")

  dt <- if (data.table::is.data.table(data)) data.table::copy(data)
        else data.table::as.data.table(data)
  dt[, school_level_orig := school_level]

  # For each group with >1 row and exactly one distinct non-NA school_level,
  # fill every row in that group with that value (NAs are updated; non-NA rows
  # are a no-op since they already hold the only non-NA value).
  dt[, c(".n_grp", ".n_unique") := .(
    .N,
    length(stats::na.omit(unique(school_level)))
  ), by = grp_cols]

  dt[.n_grp > 1L & .n_unique == 1L, `:=`(
    school_level = stats::na.omit(unique(school_level))[1L],
    n_group      = .N
  ), by = grp_cols]

  dt[, c(".n_grp", ".n_unique") := NULL]

  # QA: flag groups where the filled size exceeds n_years_data
  qa_dt    <- dt[!is.na(n_group), .N, by = c(grp_cols, "school_level")][N > n_years_data]
  qa_check <- as.data.frame(qa_dt)

  if (nrow(qa_check) > 0L) {
    message("QA warning: Some fixed groups have more rows than n_years_data. Inspect returned 'qa_check' attribute.")
  }

  out <- if (data.table::is.data.table(data)) dt else as.data.frame(dt)
  attr(out, "qa_check") <- qa_check
  return(out)
}




#' Fix missing school_type when only one non-NA type exists per school
#'
#' This function identifies groups (by school_name_std, county_std, school_level)
#' where school_type has NAs but all non-NA values are identical. In these cases,
#' the function fills in missing school_type values for all rows in the group.
#'
#' @param data A data.frame, tibble, or data.table containing school-level records.
#' @param n_years_data Integer. Threshold for checking unusually large group sizes.
#' @param id_col String. Column name of unique row IDs (unused; kept for backward
#'   compatibility with the previous dplyr-based implementation).
#'
#' @return A data.table when \code{data} is a data.table; a data.frame otherwise.
#'   school_type NA values are fixed where appropriate.
#' @importFrom data.table as.data.table is.data.table
#' @importFrom stats na.omit
#' @export
#'
#' @examples
#' dat <- data.frame(
#'   school_name_std = c("oak elementary", "oak elementary"),
#'   county_std      = c("riverside", "riverside"),
#'   school_level    = c("elementary", "elementary"),
#'   school_type     = c("public", NA_character_)
#' )
#' cleaned <- fix_school_type_na(dat, n_years_data = 10, id_col = "ids_tmp")
fix_school_type_na <- function(data, n_years_data, id_col = "ids_tmp") {

  grp_cols <- c("school_name_std", "county_std", "school_level")

  dt <- if (data.table::is.data.table(data)) data.table::copy(data)
        else data.table::as.data.table(data)
  dt[, school_type_orig := school_type]

  dt[, c(".n_grp", ".n_unique") := .(
    .N,
    length(stats::na.omit(unique(school_type)))
  ), by = grp_cols]

  dt[.n_grp > 1L & .n_unique == 1L, `:=`(
    school_type = stats::na.omit(unique(school_type))[1L],
    n_group     = .N
  ), by = grp_cols]

  dt[, c(".n_grp", ".n_unique") := NULL]

  qa_dt    <- dt[!is.na(n_group), .N, by = c(grp_cols, "school_type")][N > n_years_data]
  qa_check <- as.data.frame(qa_dt)

  if (nrow(qa_check) > 0L) {
    message("QA warning: Some fixed groups have more rows than n_years_data. Inspect returned 'qa_check' attribute.")
  }

  out <- if (data.table::is.data.table(data)) dt else as.data.frame(dt)
  attr(out, "qa_check") <- qa_check
  return(out)
}




#' Match school records across two datasets by name and location
#'
#' Deduplicates the selected columns from each input dataset and performs
#' school-name matching within the provided grouping columns using the supplied
#' string-distance thresholds.
#'
#' The per-group distance computations (Jaro-Winkler, Soundex, Cosine, OSA,
#' Q-gram, Jaccard) are handled entirely by compiled C++ routines
#' (\code{match_schools_batch_cpp} and \code{score_candidates_cpp}), eliminating
#' R interpreter overhead for the inner loops.  The R layer is responsible only
#' for tie-breaking, the optional detailed scoring for ambiguous cases, and
#' assembling the final result tables.  When \code{parallel = TRUE} this R-level
#' work is dispatched across worker processes via \code{furrr::future_map()};
#' the sequential path uses \code{lapply()} over the same per-row closure.
#'
#' @param data1 A data frame containing the first set of school records.
#' @param data2 A data frame containing the second set of school records.
#' @param match_cols1 Character vector of column names used to group records in
#'   `data1` before matching. Defaults to `"county_std"`.
#' @param match_cols2 Character vector of column names used to group records in
#'   `data2` before matching. Defaults to `"county_std"`.
#' @param data_1_source A label describing the source of `data1`.
#' @param data_2_source A label describing the source of `data2`.
#' @param threshold_jw Numeric Jaro-Winkler distance threshold used for
#'   candidate matching.
#' @param threshold_jw_min Numeric minimum Jaro-Winkler threshold used in the
#'   matching workflow.
#' @param exact_jw Numeric threshold used to identify near-exact
#'   Jaro-Winkler matches.
#' @param parallel Logical. If `TRUE`, the per-row R post-processing is
#'   dispatched to worker processes via [furrr::future_map()].  The caller must
#'   configure a `future` plan (e.g.
#'   `future::plan(future::multisession, workers = N)`) **before** setting this
#'   to `TRUE`; if no non-sequential plan is active a warning is issued and
#'   execution falls back to sequential.  Defaults to `FALSE`.
#'
#' @return A named list with elements \code{matched}, \code{unmatched_dat1},
#'   \code{unmatched_dat2}, \code{match_options}, and \code{match_summary}.
#'   The \code{matched} data frame includes distance-metric columns
#'   \code{osa}, \code{qgram}, \code{cosine}, \code{jaccard}, \code{jw},
#'   \code{soundex}, and \code{score_sums} for ambiguous matches
#'   (all \code{NA} for exact and unambiguous matches).
#'   The \code{matched} data frame always includes \code{district_std},
#'   \code{district} (from \code{data1}) and \code{district_std_data2},
#'   \code{district_data2} (from \code{data2}); when the relevant input columns
#'   are absent they are filled with \code{NA_character_}.  \code{unmatched_dat1}
#'   and \code{unmatched_dat2} carry through whichever district columns exist in
#'   \code{data1} and \code{data2} respectively.
#'
#' @importFrom furrr future_map furrr_options
#' @importFrom future plan
#' @export
match_schools_names <- function(data1, data2,
                                match_cols1 = "county_std",
                                match_cols2 = "county_std",
                                data_1_source = "GreatSchools",
                                data_2_source = "DOE",
                                threshold_jw = 0.6,
                                threshold_jw_min = 0.6,
                                exact_jw = 0.05,
                                parallel = FALSE) {

  # If parallel requested, verify that a non-sequential future plan is active.
  # Fall back to sequential with a warning when none is configured.
  if (isTRUE(parallel)) {
    plan_cls <- class(future::plan())
    if ("sequential" %in% plan_cls) {
      warning(
        "`parallel = TRUE` was requested but no non-sequential `future` plan is ",
        "active.  Falling back to sequential execution.  Call ",
        "`future::plan(future::multisession, workers = N)` before ",
        "`match_schools_names()` to enable parallel processing.",
        call. = FALSE
      )
      parallel <- FALSE
    }
  }

  data1 <- data1 %>%
    dplyr::select(tidyselect::any_of(unique(c(
      "data1_id", "state", "county", "county_std", "city",
      "district", "district_std", match_cols1,
      "school_name", "school_name_std", "school_level", "school_type"
    )))) %>%
    distinct()

  data2 <- data2 %>%
    dplyr::select(tidyselect::any_of(unique(c(
      "data2_id", "state", "county", "county_std", "city",
      "district", "district_std", match_cols2,
      "school_name", "school_name_std", "school_level", "school_type"
    )))) %>%
    distinct()

  # Build composite group keys (lowercase; "|||" separator is unlikely in county/school names)
  make_group_key <- function(data, cols) {
    if (length(cols) == 1L) {
      tolower(as.character(data[[cols]]))
    } else {
      apply(data[, cols, drop = FALSE], 1L, function(row) {
        paste(tolower(as.character(row)), collapse = "|||")
      })
    }
  }

  groups1 <- make_group_key(data1, match_cols1)
  groups2 <- make_group_key(data2, match_cols2)

  # Pre-compute lowercased match-column values for every data1 row so we don't
  # reconstruct them inside the loop for each ambiguous (status 3) match.
  filter_vals_all <- data1[, match_cols1, drop = FALSE] %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), tolower))

  # C++ handles: per-group filtering, JW/Soundex/Cosine computation, candidate
  # selection (exact vs non-exact), and top-10 trimming for ambiguous cases.
  cpp_res <- match_schools_batch_cpp(
    names1           = data1$school_name_std,
    groups1          = groups1,
    names2           = data2$school_name_std,
    groups2          = groups2,
    threshold_jw_min = threshold_jw_min,
    threshold_jw     = threshold_jw,
    exact_jw         = exact_jw
  )

  # ---------------------------------------------------------------------------
  # Per-row closure: used by the parallel dispatch path (parallel = TRUE).
  # Returns a list with three slots:
  #   matched    – a one-row tibble (match record), or NULL if no match
  #   unmatched  – the data1 row if unmatched, or NULL
  #   match_opts – a named list (length 0 or 1) of the match-options data frame
  #                produced for ambiguous (status 3) rows
  #
  # All objects referenced from the enclosing scope (data1, data2, cpp_res,
  # filter_vals_all, match_cols1, data_1_source, data_2_source) are plain R
  # data structures and are safely serialisable for furrr workers.
  .process_one_row <- function(i) {
    data1_row         <- data1[i, ]
    best_match_scores <- NULL
    status_i          <- cpp_res$status[i]

    # ---- No group match (0) or unmatched (1) --------------------------------
    if (status_i == 0L || status_i == 1L) {
      return(list(matched = NULL, unmatched = data1_row, match_opts = list()))
    }

    # Shared setup for exact (2) and candidate (3) paths
    cand_idx  <- cpp_res$candidates_idx[[i]]   # 1-based indices into data2
    jw_dists  <- cpp_res$candidates_jw[[i]]
    data2_sub <- data2[cand_idx, ]

    # ---- Exact match path (status 2) ----------------------------------------
    if (status_i == 2L) {

      if (length(cand_idx) == 1L) {
        best_idx <- 1L
      } else {
        # Tie-break: narrowest JW first, then school_level
        min_jw      <- min(jw_dists)
        min_jw_idxs <- which(jw_dists == min_jw)

        if (length(min_jw_idxs) > 1L &&
            "school_level" %in% names(data1_row) &&
            !is.na(data1_row$school_level)) {
          level_match <- data2_sub$school_level[min_jw_idxs] == data1_row$school_level
          if (any(level_match, na.rm = TRUE)) {
            best_idx <- min_jw_idxs[which(level_match)[1L]]
          } else {
            best_idx <- min_jw_idxs[1L]
          }
        } else {
          best_idx <- min_jw_idxs[1L]
        }
      }

    # ---- Candidate (non-exact) path (status 3) ------------------------------
    } else {

      # data2_sub is already filtered and trimmed to top-10 by the C++ function.
      # Use the pre-computed scores from cpp_res directly instead of recomputing
      # all 6 metrics via match_locations() / score_candidates_cpp().
      osa_v    <- cpp_res$candidates_osa[[i]]
      qgram_v  <- cpp_res$candidates_qgram[[i]]
      cosine_v <- cpp_res$candidates_cosine[[i]]
      jac_v    <- cpp_res$candidates_jaccard[[i]]
      jw_v2    <- cpp_res$candidates_jw[[i]]
      sd_v     <- cpp_res$candidates_soundex[[i]]
      dists_gtbl <- data.frame(
        name       = data2_sub$school_name_std,
        osa        = osa_v,
        qgram      = qgram_v,
        cosine     = cosine_v,
        jaccard    = jac_v,
        jw         = jw_v2,
        soundex    = sd_v,
        score_sums = osa_v + qgram_v + cosine_v + jac_v + jw_v2 + sd_v, # same formula as score_candidates_cpp()
        stringsAsFactors = FALSE
      )

      # Use pre-computed lowercased filter values for this row
      filter_vals_i <- filter_vals_all[i, , drop = FALSE]
      filter_vals_rep <- filter_vals_i[rep(1L, nrow(dists_gtbl)), , drop = FALSE]
      # Guard empty candidate names so prob_osa remains finite when dividing by
      # the standardized candidate-name length.
      candidate_name_length <- pmax(nchar(data2_sub$school_name_std), 1L)

      dists_gtbl <- dists_gtbl %>%
        dplyr::mutate(prob_osa = osa / candidate_name_length)

      mo <- dists_gtbl %>%
        dplyr::as_tibble() %>%
        dplyr::mutate(
          name         = data1_row$school_name_std,
          name_options = data2_sub$school_name_std
        ) %>%
        dplyr::bind_cols(filter_vals_rep) %>%
        dplyr::select(name, name_options, dplyr::any_of(match_cols1),
                      dplyr::everything()) %>%
        dplyr::filter(jw < .5, jaccard < .5) %>%
        dplyr::mutate(match_level = dplyr::case_when(
          (jaccard <= 0.05 & cosine <= 0.05)                           ~ 1L,
          (jw <= 0.15)                                                  ~ 1L,
          (soundex == 0 & jw <= 0.3)                                    ~ 1L,
          (soundex == 0 & cosine <= 0.25)                               ~ 2L,
          (jaccard <= 0.15 & cosine <= 0.15 & prob_osa <= 0.4)          ~ 2L,
          (jw <= 0.21)                                                   ~ 2L,
          TRUE                                                           ~ 1000L
        ))

      if (any(mo$match_level <= 3L)) {
        best_local        <- which.min(mo$match_level)
        best_match_scores <- mo[best_local, ]
        best_idx          <- match(mo$name_options[best_local],
                                   data2_sub$school_name_std)
      } else {
        return(list(matched    = NULL,
                    unmatched  = data1_row,
                    match_opts = setNames(list(mo), data1_row$school_name_std)))
      }
    }

    # ---- Build match record -------------------------------------------------
    best_distance <- jw_dists[best_idx]

    match_record <- tibble::tibble(
      match_score = 1 - best_distance,
      match_category = dplyr::case_when(
        best_distance <= exact_jw           ~ "Exact Match",
        best_distance <= exact_jw * 2       ~ "High",
        best_distance <= exact_jw * 5       ~ "Moderate",
        TRUE                                ~ "Low"
      ),
      county_std            = data1_row$county_std,
      county                = data1_row$county,
      district_std          = if ("district_std" %in% names(data1_row)) data1_row$district_std else NA_character_,
      district              = if ("district"     %in% names(data1_row)) data1_row$district     else NA_character_,
      district_std_data2    = if ("district_std" %in% names(data2_sub)) data2_sub$district_std[best_idx] else NA_character_,
      district_data2        = if ("district"     %in% names(data2_sub)) data2_sub$district[best_idx]     else NA_character_,
      data_1_source         = data_1_source,
      data_2_source         = data_2_source,
      school_name_data1     = data1_row$school_name,
      school_name_data2     = data2_sub$school_name[best_idx],
      school_name_std_data1 = data1_row$school_name_std,
      school_name_std_data2 = data2_sub$school_name_std[best_idx],
      school_level_data1    = if ("school_level" %in% names(data1_row)) data1_row$school_level else NA_character_,
      school_level_data2    = if ("school_level" %in% names(data2_sub)) data2_sub$school_level[best_idx] else NA_character_,
      school_type           = if ("school_type"  %in% names(data2_sub)) data2_sub$school_type[best_idx]  else NA_character_,
      city                  = if ("city"         %in% names(data2_sub)) data2_sub$city[best_idx]         else NA_character_,
      state                 = if ("state"        %in% names(data2_sub)) data2_sub$state[best_idx]        else NA_character_,
      zip                   = if ("zip"          %in% names(data2_sub)) as.character(data2_sub$zip[best_idx]) else NA_character_,
      lat                   = if ("lat"          %in% names(data2_sub)) data2_sub$lat[best_idx]          else NA_real_,
      lon                   = if ("lon"          %in% names(data2_sub)) data2_sub$lon[best_idx]          else NA_real_,
      street1               = if ("street1"      %in% names(data2_sub)) data2_sub$street1[best_idx]      else NA_character_,
      data1_id              = data1_row$data1_id,
      data2_id              = data2_sub$data2_id[best_idx],
      {if (!is.null(best_match_scores)) {
        best_match_scores %>%
          dplyr::select(-c(name, name_options,
                           dplyr::any_of(match_cols1),
                           dplyr::any_of(c("prob_osa", "match_level"))))
      } else {
        tibble::tibble(osa = NA_integer_, qgram = NA_integer_,
                       cosine = NA_real_, jaccard = NA_real_,
                       jw = NA_real_, soundex = NA_real_,
                       score_sums = NA_real_)
      }}
    )

    match_record <- match_record %>%
      dplyr::bind_cols(
        data1_row %>%
          dplyr::select(match_cols1[!(match_cols1 %in% colnames(match_record))])
      )

    # Carry forward match_options entry for status-3 matches.
    # `mo` is the full candidate-scoring data frame already assembled above;
    # reuse it directly rather than re-transforming best_match_scores.
    row_opts <- if (!is.null(best_match_scores)) {
      setNames(list(mo), data1_row$school_name_std)
    } else {
      list()
    }

    list(matched = match_record, unmatched = NULL, match_opts = row_opts)
  }

  # ---------------------------------------------------------------------------
  # Dispatch: both paths use the same .process_one_row() closure.
  # The parallel path distributes rows across workers via furrr::future_map();
  # the sequential path uses lapply().  The expensive distance computations are
  # already done by the C++ routine above regardless of this flag.
  row_indices <- seq_len(nrow(data1))

  if (isTRUE(parallel)) {
    row_results <- furrr::future_map(row_indices, .process_one_row,
                                     .options = furrr::furrr_options(globals = TRUE))
  } else {
    row_results <- lapply(row_indices, .process_one_row)
  }

  matched_rows   <- lapply(row_results, `[[`, "matched")
  unmatched_rows <- lapply(row_results, `[[`, "unmatched")
  match_options  <- unlist(lapply(row_results, `[[`, "match_opts"), recursive = FALSE)
  matched_df     <- dplyr::bind_rows(matched_rows)
  unmatched_dat1 <- dplyr::bind_rows(unmatched_rows)

  matched_names <- rlang::`%||%`(matched_df[["school_name_std_data2"]], character(0))
  unmatched_dat2 <- data2 %>%
    dplyr::filter(!(school_name_std %in% matched_names))

  match_options <- if (length(match_options) > 0L) {
    dplyr::bind_rows(match_options)
  } else {
    tibble::tibble()
  }

  match_summary <- table(c(
    if ("match_category" %in% names(matched_df)) matched_df$match_category else character(0),
    rep("Unmatched_data1", nrow(unmatched_dat1)),
    rep("Unmatched_data2", nrow(unmatched_dat2))
  ))

  return(list(
    matched        = matched_df,
    unmatched_dat1 = unmatched_dat1,
    unmatched_dat2 = unmatched_dat2,
    match_options  = match_options,
    match_summary  = match_summary
  ))
}




# Matching cascade helper ---------------------------------------------------
#
# build_reference_key(), .integrate_third_dataset(), and
# match_kinder_to_reference() (standardize_schools.R) each hand-roll the same
# pattern: run match_schools_names() on the still-unmatched rows of data1
# against data2 with one set of match_cols/thresholds, shrink the "still
# unmatched" set, then repeat with a different set of match_cols/thresholds
# (address -> zip -> city -> district -> county, etc.), tagging each pass's
# output with a match_method label. run_matching_cascade() below is that
# pattern extracted once, driven by a list of pass-specs, so those call sites
# can be a pass list + one function call instead of a hand-written chain.


#' Exact-join matching pass on a shared identity key (e.g. \code{place_id})
#'
#' Unlike \code{\link{match_schools_names}}, this performs a plain equi-join
#' on \code{join_col} rather than fuzzy string-distance scoring. Used as a
#' high-confidence "Pass 0" ahead of the fuzzy passes: two records that share
#' a Google \code{place_id} are the same physical school regardless of what
#' name string each one uses that year, which is exactly the signal needed to
#' bridge a school rename that fuzzy name-matching cannot.
#'
#' @param data1,data2 Data frames with \code{data1_id}/\code{data2_id} and
#'   \code{join_col} columns (plus \code{school_name}/\code{school_name_std}
#'   and, optionally, \code{county_std}/\code{district_std} for reporting).
#' @param join_col Character scalar naming the identity column to join on
#'   (default \code{"place_id"}).
#' @param data_1_source,data_2_source Labels carried into the output, as in
#'   \code{\link{match_schools_names}}.
#'
#' @return A named list with \code{matched}, \code{unmatched_dat1}, and
#'   \code{unmatched_dat2}, matching the shape \code{\link{match_schools_names}}
#'   returns (only the columns actually consumed downstream are populated;
#'   string-distance columns are absent since no fuzzy scoring occurs).
#' @keywords internal
.run_exact_join_pass <- function(data1, data2, join_col = "place_id",
                                 data_1_source = "data1", data_2_source = "data2") {

  empty <- list(
    matched = tibble::tibble(
      match_score = numeric(0), match_category = character(0),
      data1_id = integer(0), data2_id = integer(0),
      data_1_source = character(0), data_2_source = character(0),
      county_std = character(0), district_std = character(0),
      school_name_data1 = character(0), school_name_data2 = character(0),
      school_name_std_data1 = character(0), school_name_std_data2 = character(0)
    ),
    unmatched_dat1 = data1,
    unmatched_dat2 = data2
  )

  if (!join_col %in% names(data1) || !join_col %in% names(data2)) return(empty)

  d1 <- data1 %>% dplyr::filter(!is.na(.data[[join_col]]), .data[[join_col]] != "")
  d2 <- data2 %>%
    dplyr::filter(!is.na(.data[[join_col]]), .data[[join_col]] != "") %>%
    dplyr::distinct(.data[[join_col]], .keep_all = TRUE)

  if (nrow(d1) == 0 || nrow(d2) == 0) return(empty)

  hit_rows <- which(d1[[join_col]] %in% d2[[join_col]])
  if (length(hit_rows) == 0) return(empty)
  hits <- d1[hit_rows, ]
  idx2 <- match(hits[[join_col]], d2[[join_col]])

  get_or_na <- function(df, nm, i) if (nm %in% names(df)) df[[nm]][i] else NA_character_

  matched <- tibble::tibble(
    match_score           = 1,
    match_category        = "Exact Match (place_id)",
    data1_id               = hits$data1_id,
    data2_id               = d2$data2_id[idx2],
    data_1_source           = data_1_source,
    data_2_source           = data_2_source,
    county_std             = get_or_na(hits, "county_std", seq_len(nrow(hits))),
    district_std           = get_or_na(hits, "district_std", seq_len(nrow(hits))),
    school_name_data1       = hits$school_name,
    school_name_data2       = get_or_na(d2, "school_name", idx2),
    school_name_std_data1   = hits$school_name_std,
    school_name_std_data2   = get_or_na(d2, "school_name_std", idx2)
  )

  list(
    matched        = matched,
    unmatched_dat1 = data1 %>% dplyr::filter(!(data1_id %in% hits$data1_id)),
    unmatched_dat2 = data2 %>% dplyr::filter(!(.data[[join_col]] %in% hits[[join_col]]))
  )
}


#' Run a multi-pass school-matching cascade
#'
#' Generalizes the "try pass 1, keep whatever's still unmatched, try pass 2,
#' ..." chains that \code{build_reference_key()}, \code{.integrate_third_dataset()},
#' and \code{match_kinder_to_reference()} previously hand-wrote as repeated,
#' near-identical blocks. Each pass either calls \code{\link{match_schools_names}}
#' (fuzzy string matching within \code{match_cols1}/\code{match_cols2} groups)
#' or \code{\link{.run_exact_join_pass}} (exact-key join, e.g. on
#' \code{place_id}), and only ever operates on \code{data1} rows left
#' unmatched by prior passes.
#'
#' @param data1,data2 Data frames of school records, as in
#'   \code{\link{match_schools_names}}.
#' @param passes A list of pass-specs. Each element is itself a list with:
#'   \describe{
#'     \item{label}{Character. Recorded as \code{match_method} in the output.}
#'     \item{type}{\code{"fuzzy"} (default) to call
#'       \code{\link{match_schools_names}}, or \code{"exact_join"} to call
#'       \code{\link{.run_exact_join_pass}}.}
#'     \item{join_col}{Required when \code{type = "exact_join"}.}
#'     \item{match_cols1, match_cols2}{Required when \code{type = "fuzzy"}.}
#'     \item{threshold_jw, threshold_jw_min, exact_jw}{Fuzzy thresholds,
#'       passed through to \code{\link{match_schools_names}}.}
#'     \item{condition}{Optional \code{function(data1, data2)} evaluated once
#'       against the cascade's original (full) \code{data1}/\code{data2}; the
#'       pass is skipped entirely when it returns other than \code{TRUE}
#'       (e.g. "only run the district pass when both sources have usable
#'       district_std").}
#'     \item{row_filter1, row_filter2}{Optional \code{function(df)} applied
#'       to this pass's data1 (the current unmatched subset) / data2 just
#'       before matching — e.g. adding a derived column, or restricting to
#'       rows with a non-trivial \code{district_std}.}
#'   }
#' @param data_1_source,data_2_source Labels passed through to each pass.
#' @param parallel See \code{\link{match_schools_names}}; passed through to
#'   every fuzzy pass.
#'
#' @return A named list with:
#'   \itemize{
#'     \item \code{matched}: all passes' matches, row-bound and tagged with \code{match_method}.
#'     \item \code{unmatched_dat1}: data1 rows left unmatched after every pass.
#'     \item \code{unmatched_dat2}: data2 rows left unmatched after every pass.
#'   }
#' @export
run_matching_cascade <- function(data1, data2, passes,
                                 data_1_source = "data1",
                                 data_2_source = "data2",
                                 parallel = FALSE) {

  remaining1   <- data1
  all_matched  <- list()

  for (p in passes) {
    if (nrow(remaining1) == 0) break

    if (!is.null(p$condition) && !isTRUE(p$condition(data1, data2))) next

    d1 <- remaining1
    d2 <- data2
    if (!is.null(p$row_filter1)) d1 <- p$row_filter1(d1)
    if (!is.null(p$row_filter2)) d2 <- p$row_filter2(d2)
    if (nrow(d1) == 0 || nrow(d2) == 0) next

    pass_result <- if (identical(p$type, "exact_join")) {
      .run_exact_join_pass(
        d1, d2, join_col = p$join_col,
        data_1_source = data_1_source, data_2_source = data_2_source
      )
    } else {
      match_schools_names(
        data1 = d1, data2 = d2,
        match_cols1 = p$match_cols1, match_cols2 = p$match_cols2,
        data_1_source = data_1_source, data_2_source = data_2_source,
        threshold_jw = p$threshold_jw, threshold_jw_min = p$threshold_jw_min,
        exact_jw = p$exact_jw, parallel = parallel
      )
    }

    if (nrow(pass_result$matched) > 0) {
      all_matched[[p$label]] <- dplyr::mutate(pass_result$matched, match_method = p$label)
    }

    # Track by data1_id, not school_name: match_schools_names()'s own
    # match_record carries data1_id unconditionally, but school_name is not
    # guaranteed to exist on every data1 shape run_matching_cascade() is used
    # with (build_kinder_unique_schools() output, for one, only ever has
    # school_name_std). data1_id is also strictly safer than a name-based key
    # when duplicate names exist within data1.
    matched_ids <- rlang::`%||%`(pass_result$matched$data1_id, integer(0))
    remaining1 <- remaining1 %>% dplyr::filter(!(data1_id %in% matched_ids))
  }

  matched_df <- dplyr::bind_rows(all_matched)

  unmatched2_ids <- rlang::`%||%`(matched_df$data2_id, integer(0))
  unmatched_dat2 <- if ("data2_id" %in% names(data2)) {
    data2 %>% dplyr::filter(!(data2_id %in% unmatched2_ids))
  } else {
    data2
  }

  list(
    matched        = matched_df,
    unmatched_dat1 = remaining1,
    unmatched_dat2 = unmatched_dat2
  )
}


#' Exact-equality match between two location-key tables
#'
#' A plain equi-join matcher, deliberately independent of
#' \code{\link{match_schools_names}}/\code{\link{run_matching_cascade}}:
#' boolean equality on a defined key is a different operation from fuzzy
#' string-distance scoring, and routing it through the fuzzy engine's
#' hardcoded column allowlist would only add risk for no benefit. Always
#' requires \code{name_col} (standardized school name) to match exactly, plus
#' \code{cols} matched according to \code{require}:
#' \describe{
#'   \item{\code{"any"}}{At least one of \code{cols} must also match — tried
#'     one column at a time, in the order given, each pass only attempted on
#'     rows the previous pass left unmatched (same "each pass only sees
#'     what's still unmatched" idea as \code{\link{run_matching_cascade}}).}
#'   \item{\code{"all"}}{Every column in \code{cols} that is actually
#'     populated \strong{for that row} must match simultaneously — row-
#'     adaptive, not a single fixed set for the whole call. A row missing
#'     \code{district_std} isn't excluded from matching just because some
#'     \emph{other} row in \code{data1} happens to have it; it's only
#'     required to match on whichever of \code{cols} it itself has. Rows
#'     with none of \code{cols} populated are skipped (name-only would be
#'     too permissive/ambiguous) rather than treated as trivially
#'     satisfying "match every available column."}
#' }
#' A row missing (\code{NA}) any column required for a given attempt is
#' excluded from that attempt rather than joined — an equi-join would
#' otherwise treat two \code{NA}s as matching, which would silently assert a
#' match this function has no actual evidence for. This matters most for
#' \code{"all"}: a fixed, dataset-wide column requirement would wrongly
#' exclude every row missing just one column that happens to be populated
#' elsewhere (e.g. many private/religious schools have no district but do
#' have a county — found via a real CA run where this cost ~100k
#' previously-good matches before being made row-adaptive).
#'
#' @param data1,data2 Location-key tables (e.g. from
#'   \code{\link{build_location_key_table}}) — must each have an
#'   \code{orig_id} column and the columns named in \code{name_col}/\code{cols}.
#' @param cols Character vector of secondary column names to match on.
#' @param require \code{"any"} (default) or \code{"all"} — see above.
#' @param name_col Column required to match exactly on every attempt
#'   (default \code{"school_name_std"}).
#'
#' @return A named list:
#' \describe{
#'   \item{matched}{Tibble with \code{orig_id_1}, \code{orig_id_2}, and
#'     \code{match_cols} (which column(s) the pair matched on), one row per
#'     matched pair.}
#'   \item{unmatched_dat1, unmatched_dat2}{The input tables filtered to rows
#'     with no match.}
#' }
#' @export
exact_match_locations <- function(data1, data2, cols, require = c("any", "all"),
                                  name_col = "school_name_std") {
  require <- match.arg(require)

  empty_matched <- tibble::tibble(
    orig_id_1 = character(0), orig_id_2 = character(0), match_cols = character(0)
  )
  empty <- list(matched = empty_matched, unmatched_dat1 = data1, unmatched_dat2 = data2)

  if (nrow(data1) == 0 || nrow(data2) == 0) return(empty)

  cols <- intersect(cols, intersect(names(data1), names(data2)))
  if (length(cols) == 0 || !name_col %in% names(data1) || !name_col %in% names(data2)) {
    return(empty)
  }

  d2_small <- data2 %>% dplyr::select(dplyr::all_of(c(name_col, cols)), orig_id_2 = orig_id)

  run_pass <- function(join_cols, remaining1) {
    keep <- stats::complete.cases(remaining1[, c(name_col, join_cols), drop = FALSE])
    complete1 <- remaining1[keep, , drop = FALSE]
    if (nrow(complete1) == 0) return(tibble::tibble())

    complete1 %>%
      dplyr::select(dplyr::all_of(c(name_col, join_cols)), orig_id_1 = orig_id) %>%
      dplyr::inner_join(
        d2_small %>%
          dplyr::distinct(dplyr::across(dplyr::all_of(c(name_col, join_cols))), .keep_all = TRUE),
        by = c(name_col, join_cols)
      ) %>%
      dplyr::transmute(orig_id_1, orig_id_2, match_cols = paste(join_cols, collapse = "+"))
  }

  if (require == "all") {
    # Row-adaptive: group data1 rows by their OWN non-NA pattern among
    # `cols` and run one pass per group, using only that group's populated
    # columns as the join key — rather than a single global set (see docs
    # above for why a fixed set silently drops rows that are otherwise
    # perfectly identifiable on name + whatever they do have).
    na_flags <- vapply(cols, function(col) is.na(data1[[col]]) | data1[[col]] == "",
                       logical(nrow(data1)))
    na_flags <- matrix(na_flags, nrow = nrow(data1), ncol = length(cols))
    row_sig <- apply(!na_flags, 1, function(r) paste(cols[r], collapse = "|"))

    matched_list <- list()
    for (key in unique(row_sig)) {
      row_cols <- if (nchar(key) == 0) character(0) else strsplit(key, "|", fixed = TRUE)[[1]]
      if (length(row_cols) == 0) next
      group <- data1[row_sig == key, , drop = FALSE]
      pass_res <- run_pass(row_cols, group)
      if (nrow(pass_res) > 0) matched_list[[key]] <- pass_res
    }
    # bind_rows(list()) on an empty list returns a 0-row, 0-COLUMN tibble
    # (no orig_id_1/orig_id_2/match_cols at all), which breaks the
    # distinct()-by-those-names guard below when nothing matched in any
    # group — fall back to the properly-shaped empty tibble instead.
    matched <- if (length(matched_list) == 0) empty_matched else dplyr::bind_rows(matched_list)
  } else {
    remaining1  <- data1
    matched_list <- list()
    for (col in cols) {
      if (nrow(remaining1) == 0) break
      pass_res <- run_pass(col, remaining1)
      if (nrow(pass_res) > 0) {
        matched_list[[col]] <- pass_res
        remaining1 <- remaining1[!(remaining1$orig_id %in% pass_res$orig_id_1), , drop = FALSE]
      }
    }
    matched <- dplyr::bind_rows(matched_list)
  }

  # dplyr::bind_rows(list()) drops all columns when nothing matched (unlike
  # an empty-but-typed tibble), which breaks the distinct() calls below —
  # fall back to the properly-shaped empty tibble defined up top.
  if (nrow(matched) == 0) matched <- empty_matched

  # Guard against ambiguous exact matches (e.g. two candidates identical on
  # the matched columns) by keeping one pairing per side, deterministically.
  matched <- matched %>%
    dplyr::distinct(orig_id_2, .keep_all = TRUE) %>%
    dplyr::distinct(orig_id_1, .keep_all = TRUE)

  list(
    matched        = matched,
    unmatched_dat1 = data1 %>% dplyr::filter(!(orig_id %in% matched$orig_id_1)),
    unmatched_dat2 = data2 %>% dplyr::filter(!(orig_id %in% matched$orig_id_2))
  )
}


# Internal: place_id-match two orig_id-keyed tables (e.g. two location-key
# tables from build_location_key_table()), wrapping .run_exact_join_pass()
# — which expects data1_id/data2_id — and translating its result into the
# same orig_id_1/orig_id_2/match_cols shape exact_match_locations() returns,
# so pipeline orchestration code can treat every matching stage uniformly
# regardless of whether it was an exact-field match or a place_id match.
#' @keywords internal
.place_id_match <- function(data1, data2, id1_col = "orig_id", id2_col = "orig_id",
                            data_1_source = "data1", data_2_source = "data2") {
  d1 <- data1 %>% dplyr::rename(data1_id = dplyr::all_of(id1_col))
  d2 <- data2 %>% dplyr::rename(data2_id = dplyr::all_of(id2_col))

  res <- .run_exact_join_pass(d1, d2, join_col = "place_id",
                              data_1_source = data_1_source, data_2_source = data_2_source)

  matched <- if (nrow(res$matched) > 0) {
    res$matched %>%
      dplyr::transmute(orig_id_1 = as.character(data1_id),
                       orig_id_2 = as.character(data2_id),
                       match_cols = "place_id")
  } else {
    tibble::tibble(orig_id_1 = character(0), orig_id_2 = character(0), match_cols = character(0))
  }

  list(
    matched        = matched,
    unmatched_dat1 = data1 %>% dplyr::filter(!(.data[[id1_col]] %in% matched$orig_id_1)),
    unmatched_dat2 = data2 %>% dplyr::filter(!(.data[[id2_col]] %in% matched$orig_id_2))
  )
}












