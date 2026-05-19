



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



# vax_data <- md_dat %>% mutate(state = current_state)
# school_info <- md_greatschools 
# match_threshold = 0.25







#' Match vaccination data to school metadata using fuzzy string matching
#'
#' This function deduplicates school metadata, assigns IDs, and matches vaccination records
#' to school info using Jaro-Winkler string distance within counties.
#'
#' @param vax_data A data frame of school-level vaccination data.
#' @param school_info A data frame of school metadata.
#' @param match_threshold Numeric threshold for Jaro-Winkler distance (default = 0.25).
#'
#' @return A list with matched schools, unmatched schools, updated school_info, and match summary.
# match_vaccination_schools <- function(vax_data, school_info, match_threshold = 0.25) {
# 
#   vax_data <- vax_data %>%
#     dplyr::select(tidyselect::any_of(c("state", "county_std", "city",
#                                        "school_name", "school_name_std", 
#                                        "school_level", "school_type"))) %>%
#     distinct() %>%
#     mutate(vacc_school_id = row_number())  # Assign IDs to vaccination data
# 
#   # Deduplicate school_info
#   school_info <- school_info %>%
#     dplyr::select(tidyselect::any_of(c("state", "county_std", "city",
#                                        "school_name", "school_name_std", 
#                                        "school_level", "gradeLevels", "school_type","zip"))) %>%
#     distinct() %>%
#     mutate(school_info_id = row_number(),
#            school_id_matched = NA)
#   
# 
#   matched_schools <- list()
#   unmatched_schools <- list()
#   
#   for (i in seq_len(nrow(vax_data))) {
#     vax_school <- vax_data[i, ]
#     county_schools <- school_info %>% filter(county_std == vax_school$county_std)
#     
#     if (nrow(county_schools) == 0) {
#       unmatched_schools[[length(unmatched_schools) + 1]] <- vax_school
#       next
#     }
#     
#     distances <- stringdist(vax_school$school_name_std, county_schools$school_name_std, method = "jw")
#     if (all(distances > match_threshold)) {
#       unmatched_schools[[length(unmatched_schools) + 1]] <- vax_school
#       next
#     }
#     
#     best_idx <- which.min(distances)
#     best_distance <- distances[best_idx]
#     if (best_distance > match_threshold) {
#       unmatched_schools[[length(unmatched_schools) + 1]] <- vax_school
#       next
#     }
#     
#     match_record <- tibble(
#       match_score = 1 - best_distance,
#       match_category = case_when(
#         best_distance == 0 ~ "Exact Match",
#         best_distance <= 0.05 ~ "High",
#         best_distance <= 0.15 ~ "Moderate",
#         TRUE ~ "Low"
#       ),
#       county_std = vax_school$county_std,
#       county = vax_school$county,
#       school_name_vax = vax_school$school_name,
#       school_name_info = county_schools$school_name[best_idx],
#       school_name_std_vax = vax_school$school_name_std,
#       school_name_std_info = county_schools$school_name_std[best_idx],
#       school_level_vax = vax_school$school_level,
#       school_level_info = county_schools$school_level[best_idx],
#       school_type_info = county_schools$school_type[best_idx],
#       street = county_schools$street[best_idx],
#       city = county_schools$city[best_idx],
#       state = county_schools$state[best_idx],
#       zip = county_schools$zip[best_idx],
#       school_id_vax = vax_school$vacc_school_id,
#       school_id_info = county_schools$school_info_id[best_idx]
#     )
#     
#     matched_schools[[length(matched_schools) + 1]] <- match_record
#     school_info$school_id_matched[school_info$school_info_id == county_schools$school_info_id[best_idx]] <- vax_school$vacc_school_id
#   }
#   
#   matched_df <- bind_rows(matched_schools)
#   unmatched_df <- bind_rows(unmatched_schools)
#   
#   match_summary <- table(c(matched_df$match_category, rep("Unmatched", nrow(unmatched_df))))
#   
#   return(list(
#     matched = matched_df,
#     unmatched = unmatched_df,
#     updated_school_info = school_info,
#     match_summary = match_summary
#   ))
# }


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
#' @description use stringdist to get best match for country name, if not official country
#' @param a location name to match
#' @param names location names to match against
#' @param return_Code TRUE/FALSE
#' @param return_name TRUE/FALSE return standardized name
#' @param return_score TRUE/FALSE
#' @param return_score_matrix TRUE/FALSE
#' @return ISOs, country names, matching scores, full matching distance matrix
#' @importFrom stringdist stringdist
#' @export
match_locations <- function(
    a,
    names,
    return_name=TRUE,
    return_score=FALSE,
    return_score_matrix=FALSE
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
  
  a_cln <- standardize_location_strings(a)
  b_cln <- standardize_location_strings(names)
  
  methods <- c(
    "osa",
    "qgram",
    "cosine",
    "jaccard",
    "jw",
    "soundex"
  )
  dists <- as.data.frame(matrix(NA, nrow = length(b_cln), ncol = length(methods)+1,
                                dimnames = list(b_cln, c("name", methods))))
  for (j in 1:length(methods)){
    dists[, j+1]  <-
      suppressWarnings(stringdist::stringdist(a_cln, b_cln, method = methods[j]))
  }
  dists$score_sums <- rowSums(dists, na.rm = TRUE)
  dists$osa <- as.integer(dists$osa)
  dists$name <- names
  
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
  if (length(best_ > 1)){
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
#' The per-group distance computations (Jaro-Winkler, Soundex, Cosine) and
#' candidate selection are handled by a compiled C++ routine
#' (\code{match_schools_batch_cpp}), eliminating R interpreter overhead for
#' the inner loop.  The R layer is responsible only for tie-breaking, the
#' optional detailed \code{match_locations()} scoring for ambiguous cases, and
#' assembling the final result tables.
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
#' @param parallel Logical. Accepted for API compatibility with prior versions of
#'   this function; currently ignored. The per-group distance computations run
#'   entirely in the compiled C++ batch routine (`match_schools_batch_cpp`) and
#'   do not require external parallelism. Passing `TRUE` will emit a warning.
#'   Defaults to `FALSE`.
#'
#' @return A named list with elements \code{matched}, \code{unmatched_dat1},
#'   \code{unmatched_dat2}, \code{match_options}, and \code{match_summary}.
#'   
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

  if (isTRUE(parallel)) {
    warning("`parallel = TRUE` is ignored and can be removed: matching now runs in compiled C++ without requiring furrr/future.",
            call. = FALSE)
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
  # The `parallel` parameter is accepted for API compatibility; the C++ batch
  # call already handles the hot path without requiring furrr workers.
  cpp_res <- match_schools_batch_cpp(
    names1           = data1$school_name_std,
    groups1          = groups1,
    names2           = data2$school_name_std,
    groups2          = groups2,
    threshold_jw_min = threshold_jw_min,
    threshold_jw     = threshold_jw,
    exact_jw         = exact_jw
  )

  # ---- Pre-allocate output vectors (avoid per-row tibble() overhead) --------
  # Check once which optional columns exist in data1 / data2
  has_school_level_d1 <- "school_level" %in% names(data1)
  has_school_level_d2 <- "school_level" %in% names(data2)
  has_school_type_d2  <- "school_type"  %in% names(data2)
  has_city_d2         <- "city"         %in% names(data2)
  has_state_d2        <- "state"        %in% names(data2)
  has_zip_d2          <- "zip"          %in% names(data2)
  has_lat_d2          <- "lat"          %in% names(data2)
  has_lon_d2          <- "lon"          %in% names(data2)
  has_street1_d2      <- "street1"      %in% names(data2)

  # Columns in match_cols1 not already covered by the fixed output schema
  core_out_cols <- c("match_score", "match_category", "county_std", "county",
                     "data_1_source", "data_2_source",
                     "school_name_data1", "school_name_data2",
                     "school_name_std_data1", "school_name_std_data2",
                     "school_level_data1", "school_level_data2",
                     "school_type", "city", "state", "zip", "lat", "lon",
                     "street1", "data1_id", "data2_id",
                     "osa", "lv", "dl", "lcs", "qgram", "cosine",
                     "jaccard", "jw", "soundex", "score_sums")
  extra_match_cols <- match_cols1[!(match_cols1 %in% core_out_cols)]

  n1             <- nrow(data1)
  out_score      <- rep(NA_real_,      n1)
  out_cat        <- rep(NA_character_, n1)
  out_county_std <- rep(NA_character_, n1)
  out_county     <- rep(NA_character_, n1)
  out_sn_d1      <- rep(NA_character_, n1)
  out_sn_d2      <- rep(NA_character_, n1)
  out_sn_std_d1  <- rep(NA_character_, n1)
  out_sn_std_d2  <- rep(NA_character_, n1)
  out_sl_d1      <- rep(NA_character_, n1)
  out_sl_d2      <- rep(NA_character_, n1)
  out_stype      <- rep(NA_character_, n1)
  out_city       <- rep(NA_character_, n1)
  out_state      <- rep(NA_character_, n1)
  out_zip        <- rep(NA_character_, n1)
  out_lat        <- rep(NA_real_,      n1)
  out_lon        <- rep(NA_real_,      n1)
  out_street1    <- rep(NA_character_, n1)
  out_data1_id   <- { v <- vector(typeof(data1$data1_id), n1); v[] <- NA; v }
  out_data2_id   <- { v <- vector(typeof(data2$data2_id), n1); v[] <- NA; v }
  out_osa        <- rep(NA_real_, n1)
  out_lv         <- rep(NA_real_, n1)
  out_dl         <- rep(NA_real_, n1)
  out_lcs        <- rep(NA_real_, n1)
  out_qgram      <- rep(NA_real_, n1)
  out_cosine     <- rep(NA_real_, n1)
  out_jaccard    <- rep(NA_real_, n1)
  out_jw_sc      <- rep(NA_real_, n1)
  out_soundex    <- rep(NA_real_, n1)
  out_score_sums <- rep(NA_real_, n1)

  # Pre-allocate vectors for any extra match_cols1 columns
  extra_out <- setNames(
    lapply(extra_match_cols, function(.col) {
      v <- vector(typeof(data1[[.col]]), n1); v[] <- NA; v
    }),
    extra_match_cols
  )

  matched_count   <- 0L
  unmatched_idxs  <- integer(n1)
  unmatched_count <- 0L
  match_options   <- list()

  for (i in seq_len(n1)) {

    best_match_scores <- NULL
    status_i          <- cpp_res$status[i]

    # ---- No group match (0) or unmatched (1) --------------------------------
    if (status_i == 0L || status_i == 1L) {
      unmatched_count <- unmatched_count + 1L
      unmatched_idxs[unmatched_count] <- i
      next
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
            has_school_level_d1 &&
            !is.na(data1$school_level[i])) {
          level_match <- data2_sub$school_level[min_jw_idxs] == data1$school_level[i]
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
      # Call match_locations() for detailed multi-metric scoring.
      dists_gtbl <- match_locations(
        a                   = data1$school_name_std[i],
        names               = data2_sub$school_name_std,
        return_score        = TRUE,
        return_score_matrix = TRUE
      )

      # match_locations() returns a single row when the match is unambiguous
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
            name         = data1$school_name_std[i],
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

        match_options[[length(match_options) + 1L]] <- mo
        names(match_options)[length(match_options)]  <- data1$school_name_std[i]

        if (any(mo$match_level <= 3L)) {
          best_local        <- which.min(mo$match_level)
          best_match_scores <- mo[best_local, ]
          best_idx          <- match(mo$name_options[best_local],
                                     data2_sub$school_name_std)
        } else {
          unmatched_count <- unmatched_count + 1L
          unmatched_idxs[unmatched_count] <- i
          next
        }
      }
    }

    # ---- Fill pre-allocated output vectors ----------------------------------
    best_distance    <- jw_dists[best_idx]
    matched_count    <- matched_count + 1L
    k                <- matched_count

    out_score[k]     <- 1 - best_distance
    out_cat[k]       <- if      (best_distance <= 0.05) "Exact Match"
                        else if (best_distance <= 0.10) "High"
                        else if (best_distance <= 0.25) "Moderate"
                        else                            "Low"
    out_county_std[k] <- data1$county_std[i]
    out_county[k]     <- data1$county[i]
    out_sn_d1[k]      <- data1$school_name[i]
    out_sn_d2[k]      <- data2_sub$school_name[best_idx]
    out_sn_std_d1[k]  <- data1$school_name_std[i]
    out_sn_std_d2[k]  <- data2_sub$school_name_std[best_idx]
    out_sl_d1[k]      <- if (has_school_level_d1) data1$school_level[i]              else NA_character_
    out_sl_d2[k]      <- if (has_school_level_d2) data2_sub$school_level[best_idx]   else NA_character_
    out_stype[k]      <- if (has_school_type_d2)  data2_sub$school_type[best_idx]    else NA_character_
    out_city[k]       <- if (has_city_d2)          data2_sub$city[best_idx]           else NA_character_
    out_state[k]      <- if (has_state_d2)         data2_sub$state[best_idx]          else NA_character_
    out_zip[k]        <- if (has_zip_d2)           as.character(data2_sub$zip[best_idx]) else NA_character_
    out_lat[k]        <- if (has_lat_d2)           data2_sub$lat[best_idx]            else NA_real_
    out_lon[k]        <- if (has_lon_d2)           data2_sub$lon[best_idx]            else NA_real_
    out_street1[k]    <- if (has_street1_d2)       data2_sub$street1[best_idx]        else NA_character_
    out_data1_id[k]   <- data1$data1_id[i]
    out_data2_id[k]   <- data2_sub$data2_id[best_idx]

    if (!is.null(best_match_scores)) {
      sc <- best_match_scores %>%
        dplyr::select(-c(name, name_options, dplyr::any_of(match_cols1)))
      out_osa[k]        <- sc$osa
      out_lv[k]         <- sc$lv
      out_dl[k]         <- sc$dl
      out_lcs[k]        <- sc$lcs
      out_qgram[k]      <- sc$qgram
      out_cosine[k]     <- sc$cosine
      out_jaccard[k]    <- sc$jaccard
      out_jw_sc[k]      <- sc$jw
      out_soundex[k]    <- sc$soundex
      out_score_sums[k] <- sc$score_sums
    }
    # score columns for exact / unambiguous paths remain NA (pre-initialised above)

    for (.col in extra_match_cols) {
      extra_out[[.col]][k] <- data1[[.col]][i]
    }
  }

  # ---- Combine matched + unmatched into result tables -----------------------
  idx        <- seq_len(matched_count)
  matched_df <- data.frame(
    match_score           = out_score[idx],
    match_category        = out_cat[idx],
    county_std            = out_county_std[idx],
    county                = out_county[idx],
    data_1_source         = rep_len(data_1_source, matched_count),
    data_2_source         = rep_len(data_2_source, matched_count),
    school_name_data1     = out_sn_d1[idx],
    school_name_data2     = out_sn_d2[idx],
    school_name_std_data1 = out_sn_std_d1[idx],
    school_name_std_data2 = out_sn_std_d2[idx],
    school_level_data1    = out_sl_d1[idx],
    school_level_data2    = out_sl_d2[idx],
    school_type           = out_stype[idx],
    city                  = out_city[idx],
    state                 = out_state[idx],
    zip                   = out_zip[idx],
    lat                   = out_lat[idx],
    lon                   = out_lon[idx],
    street1               = out_street1[idx],
    data1_id              = out_data1_id[idx],
    data2_id              = out_data2_id[idx],
    osa                   = out_osa[idx],
    lv                    = out_lv[idx],
    dl                    = out_dl[idx],
    lcs                   = out_lcs[idx],
    qgram                 = out_qgram[idx],
    cosine                = out_cosine[idx],
    jaccard               = out_jaccard[idx],
    jw                    = out_jw_sc[idx],
    soundex               = out_soundex[idx],
    score_sums            = out_score_sums[idx],
    stringsAsFactors      = FALSE
  )
  if (length(extra_match_cols) > 0L) {
    matched_df <- cbind(matched_df,
                        as.data.frame(lapply(extra_out, `[`, idx),
                                      stringsAsFactors = FALSE))
  }

  unmatched_dat1 <- data1[unmatched_idxs[seq_len(unmatched_count)], , drop = FALSE]
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




















#' Fuzzy match unmatched kindergarten records within same county across years
#'
#' This function takes records that were unmatched in the initial matching process
#' and performs fuzzy matching within the same county to identify schools that may
#' have slight misspellings across different years. It groups similar school names
#' together and creates a standardized name for each group.
#'
#' @param unmatched_data A data frame containing unmatched records. Required columns:
#'   - school_name_data1: Original school name from first dataset
#'   - school_name_std_data1: Standardized school name from first dataset
#'   - county_std: Standardized county name
#'   - match_category: Match category (should be "Unmatched_data1")
#'   All other columns in the data frame will be preserved in the output.
#' @param match_threshold Numeric threshold for Jaro-Winkler distance (default = 0.15).
#'   Lower values require closer matches. Typical range: 0.10 (strict) to 0.20 (lenient).
#' @param use_globaltoolbox Logical, whether to use globaltoolboxlite for detailed matching (default = TRUE).
#' @param jw_preliminary_factor Numeric factor applied to match_threshold for preliminary filtering (default = 2).
#'   Only used when use_globaltoolbox = TRUE. Higher values allow more candidates for detailed matching.
#'
#' @return A data frame with original data plus new columns:
#'   - fuzzy_match_group: Integer ID for each group of matched schools
#'   - fuzzy_match_canonical_name: Standardized name chosen for each group (most common or first)
#'   - fuzzy_match_count: Number of schools in each group
#'   - fuzzy_match_names: List of all school names in the group
#'
#' @examples
#' # Extract unmatched data1 records
#' unmatched_kinder <- kinder_match$all_rows %>%
#'   filter(match_category == "Unmatched_data1")
#' 
#' # Perform fuzzy matching
#' fuzzy_matched <- fuzzy_match_unmatched_schools(unmatched_kinder)
fuzzy_match_unmatched_schools <- function(unmatched_data, 
                                          match_threshold = 0.15,
                                          use_globaltoolbox = TRUE,
                                          jw_preliminary_factor = 2) {
  
  # Return early if no unmatched data
  if (nrow(unmatched_data) == 0) {
    return(unmatched_data %>%
             mutate(
               fuzzy_match_group = NA_integer_,
               fuzzy_match_canonical_name = NA_character_,
               fuzzy_match_count = NA_integer_,
               fuzzy_match_names = NA_character_
             ))
  }
  
  # Initialize result columns
  unmatched_data <- unmatched_data %>%
    mutate(
      fuzzy_match_group = NA_integer_,
      fuzzy_match_canonical_name = NA_character_,
      fuzzy_match_count = NA_integer_,
      fuzzy_match_names = NA_character_
    )
  
  # Get unique counties
  counties <- unique(unmatched_data$county_std)
  
  # Track which group we're on
  current_group <- 1
  
  # Process each county separately
  for (county in counties) {
    if (is.na(county)) next
    
    # Get all schools in this county that haven't been assigned to a group yet
    county_schools_idx <- which(unmatched_data$county_std == county & 
                                  is.na(unmatched_data$fuzzy_match_group))
    
    if (length(county_schools_idx) == 0) next
    
    county_schools <- unmatched_data[county_schools_idx, ]
    
    # Build a distance matrix for all pairs in this county
    n_schools <- nrow(county_schools)
    
    if (n_schools == 1) {
      # Single school - assign it to its own group
      unmatched_data$fuzzy_match_group[county_schools_idx[1]] <- current_group
      unmatched_data$fuzzy_match_canonical_name[county_schools_idx[1]] <- 
        county_schools$school_name_std_data1[1]
      unmatched_data$fuzzy_match_count[county_schools_idx[1]] <- 1
      unmatched_data$fuzzy_match_names[county_schools_idx[1]] <- 
        county_schools$school_name_std_data1[1]
      current_group <- current_group + 1
      next
    }
    
    # Calculate distance matrix
    # Note: This is O(n²) but acceptable since:
    # 1. Processing is done per county (typically small n)
    # 2. Only unmatched schools are processed (subset of total data)
    # 3. Readability and correctness prioritized over micro-optimization
    # Alternative: Could use combn() or vectorized distance matrix calculation
    # but current approach is clear and sufficient for typical use cases
    # Note: n_schools > 1 at this point (single school case handled above)
    dist_matrix <- matrix(1, nrow = n_schools, ncol = n_schools)
    
    for (i in 1:(n_schools - 1)) {
      for (j in (i + 1):n_schools) {
        name_i <- county_schools$school_name_std_data1[i]
        name_j <- county_schools$school_name_std_data1[j]
        
        # Skip if either name is NA
        if (is.na(name_i) || is.na(name_j)) {
          dist_matrix[i, j] <- dist_matrix[j, i] <- 1
          next
        }
        
        # Calculate Jaro-Winkler distance
        jw_dist <- stringdist(name_i, name_j, method = "jw")
        
        # If using globaltoolbox and distance is promising, get detailed score
        jw_preliminary_threshold <- match_threshold * jw_preliminary_factor
        if (use_globaltoolbox && jw_dist <= jw_preliminary_threshold) {
          tryCatch({
            detailed <- globaltoolboxlite::match_locations(
              a = name_i,
              names = name_j,
              return_score = TRUE,
              return_score_matrix = TRUE
            )
            if (nrow(detailed) > 0 && !is.na(detailed$jw[1])) {
              # Use the more detailed distance if available
              jw_dist <- detailed$jw[1]
            }
          }, error = function(e) {
            # Log warning if detailed matching fails, but continue with simple JW distance
            warning("globaltoolboxlite matching failed for '", name_i, "' vs '", name_j, 
                    "': ", e$message, ". Using simple Jaro-Winkler distance.")
          })
        }
        
        dist_matrix[i, j] <- dist_matrix[j, i] <- jw_dist
      }
    }
    
    # Create groups using connected components where distance <= threshold
    # Build an adjacency matrix
    adj_matrix <- dist_matrix <= match_threshold
    diag(adj_matrix) <- TRUE  # Each school is connected to itself
    
    # Use igraph to find connected components
    g <- igraph::graph_from_adjacency_matrix(adj_matrix, mode = "undirected")
    components <- igraph::components(g)
    
    # Assign group IDs
    if (components$no > 0) {
      for (comp_id in seq_len(components$no)) {
        # Get indices in this component
        comp_idx <- which(components$membership == comp_id)
        
        # Validate indices are in bounds (should be 1:n_schools)
        if (length(comp_idx) == 0 || any(comp_idx < 1) || any(comp_idx > n_schools)) {
          warning("Invalid component indices detected (outside 1:", n_schools, 
                  "), skipping component ", comp_id)
          next
        }
        
        global_idx <- county_schools_idx[comp_idx]
        
        # Get all school names in this group
        group_names <- county_schools$school_name_std_data1[comp_idx]
        
        # Choose canonical name (most frequent, or first if tie)
        # Filter out NA values when selecting canonical name
        non_na_names <- group_names[!is.na(group_names)]
        if (length(non_na_names) > 0) {
          name_counts <- table(non_na_names)
          if (length(name_counts) > 0) {
            canonical_name <- names(name_counts)[which.max(name_counts)]
          } else {
            # Safety fallback (should not reach here)
            canonical_name <- NA_character_
          }
        } else {
          # If all names are NA, use NA as canonical
          canonical_name <- NA_character_
        }
        
        # Assign to all members of this group
        unmatched_data$fuzzy_match_group[global_idx] <- current_group
        unmatched_data$fuzzy_match_canonical_name[global_idx] <- canonical_name
        unmatched_data$fuzzy_match_count[global_idx] <- length(comp_idx)
        # Filter out NA values from fuzzy_match_names for cleaner output
        non_na_unique_names <- unique(group_names[!is.na(group_names)])
        if (length(non_na_unique_names) > 0) {
          unmatched_data$fuzzy_match_names[global_idx] <- paste(sort(non_na_unique_names), collapse = " | ")
        } else {
          unmatched_data$fuzzy_match_names[global_idx] <- NA_character_
        }
        
        current_group <- current_group + 1
      }
    }
  }
  
  return(unmatched_data)
}