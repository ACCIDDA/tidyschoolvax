



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
  return(res[, c(return_name, return_score)])
}









# Vaccination Data Cleaning and Matching ----------------------------------


#' Fix missing school_level when only one non-NA level exists per school
#'
#' This function identifies groups (by school_name_std, county_std, school_type)
#' where school_level has NAs but all non-NA values are identical. In these cases,
#' the function fills in missing school_level values for all rows in the group.
#'
#' @param data A data.frame or tibble containing school-level records.
#' @param n_years_data Integer. Threshold for checking unusually large group sizes.
#' @param id_col String. Column name of unique row IDs (unused; kept for backward
#'   compatibility with the previous dplyr-based implementation).
#'
#' @return A data.frame with school_level NA values fixed where appropriate.
#' @importFrom data.table as.data.table
#' @importFrom stats na.omit
#' @export
#'
#' @examples
#' cleaned <- fix_school_level_na(kinder_dat_unique, n_years_data = 10, id_col = "ids_tmp")
fix_school_level_na <- function(data, n_years_data, id_col = "ids_tmp") {

  grp_cols <- c("school_name_std", "county_std", "school_type")

  dt <- data.table::copy(data.table::as.data.table(data))
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

  out <- as.data.frame(dt)
  attr(out, "qa_check") <- qa_check
  return(out)
}




#' Fix missing school_type when only one non-NA type exists per school
#'
#' This function identifies groups (by school_name_std, county_std, school_level)
#' where school_type has NAs but all non-NA values are identical. In these cases,
#' the function fills in missing school_type values for all rows in the group.
#'
#' @param data A data.frame or tibble containing school-level records.
#' @param n_years_data Integer. Threshold for checking unusually large group sizes.
#' @param id_col String. Column name of unique row IDs (unused; kept for backward
#'   compatibility with the previous dplyr-based implementation).
#'
#' @return A data.frame with school_type NA values fixed where appropriate.
#' @importFrom data.table as.data.table
#' @importFrom stats na.omit
#' @export
#'
#' @examples
#' cleaned <- fix_school_type_na(kinder_dat_unique, n_years_data = 10, id_col = "ids_tmp")
fix_school_type_na <- function(data, n_years_data, id_col = "ids_tmp") {

  grp_cols <- c("school_name_std", "county_std", "school_level")

  dt <- data.table::as.data.table(data)
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

  out <- as.data.frame(dt)
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
      "data1_id", "state", "county", "county_std", "city", match_cols1,
      "school_name", "school_name_std", "school_level", "school_type"
    )))) %>%
    distinct()

  data2 <- data2 %>%
    dplyr::select(tidyselect::any_of(unique(c(
      "data2_id", "state", "county", "county_std", "city", match_cols2,
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

      # When there is only one candidate, select it directly.
      if (nrow(dists_gtbl) == 1L) {
        best_idx <- match(dists_gtbl$name, data2_sub$school_name_std)

      } else {
        dists_gtbl <- dists_gtbl %>%
          dplyr::mutate(prob_osa = osa / nchar(data2_sub$school_name_std))

        # Use pre-computed lowercased filter values for this row
        filter_vals_i <- filter_vals_all[i, , drop = FALSE]

        mo <- dists_gtbl %>%
          dplyr::as_tibble() %>%
          dplyr::mutate(
            name         = data1_row$school_name_std,
            name_options = data2_sub$school_name_std
          ) %>%
          dplyr::bind_cols(filter_vals_i[rep(1L, nrow(.)), ]) %>%
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
    }

    # ---- Build match record -------------------------------------------------
    best_distance <- jw_dists[best_idx]

    match_record <- tibble::tibble(
      match_score = 1 - best_distance,
      match_category = dplyr::case_when(
        best_distance <= 0.05 ~ "Exact Match",
        best_distance <= 0.10 ~ "High",
        best_distance <= 0.25 ~ "Moderate",
        TRUE                  ~ "Low"
      ),
      county_std            = data1_row$county_std,
      county                = data1_row$county,
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

  unmatched_dat2 <- data2 %>%
    dplyr::filter(!(school_name_std %in% matched_df$school_name_std_data2))

  match_options <- dplyr::bind_rows(match_options)

  match_summary <- table(c(
    matched_df$match_category,
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
















