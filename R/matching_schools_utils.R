



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
#'
#' @return A data frame containing the school-name matching results between the
#'   two input datasets.
#' @export
match_schools_names <- function(data1, data2,
                                match_cols1 = "county_std",
                                match_cols2 = "county_std",
                                data_1_source = "GreatSchools",
                                data_2_source = "DOE",
                                threshold_jw = 0.6,
                                threshold_jw_min = 0.6,
                                exact_jw = 0.05,
                                parallel = FALSE){
  
  data1 <- data1 %>%
    dplyr::select(tidyselect::any_of(unique(c("data1_id", "state","county", "county_std", "city", match_cols1,
                                       "school_name", "school_name_std", 
                                       "school_level", "school_type")))) %>%
    distinct()
  
  # Deduplicate data2
  data2_cols <- unique(c("data2_id","state","county", "county_std", "city", match_cols2,
                  "school_name", "school_name_std", 
                  "school_level", "school_type"))
  
  data2 <- data2 %>%
    dplyr::select(tidyselect::any_of(data2_cols)) %>%
    distinct()

  # Pre-lowercase the match columns of data2 ONCE before the loop (avoids
  # repeating mutate(across(tolower)) on every iteration).  Build a named list
  # keyed by the group values so each per-row lookup is O(1) instead of an
  # inner_join over the full data2.
  data2_lowered <- data2 %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(match_cols2), tolower))

  # Compound key: paste all match_cols2 values together with a rare separator
  data2_lowered$.grp_key_ <- do.call(
    paste,
    c(lapply(match_cols2, function(col) data2_lowered[[col]]), sep = "\x01")
  )
  data2_by_group <- split(data2_lowered, data2_lowered$.grp_key_, drop = TRUE)
  data2_lowered$.grp_key_ <- NULL

  # ---------------------------------------------------------------------------
  # Inner helper: match a single data1 row against data2_by_group.
  # Returns list(matched = <tibble or NULL>, unmatched = <row or NULL>,
  #              match_opts = <named list of 0 or 1 data frames>).
  # Defined as a closure so it captures the pre-processed data2_by_group and
  # all threshold parameters without extra argument passing.  When parallel=TRUE
  # it is dispatched to furrr workers; all captured state is plain R objects and
  # therefore safely serialisable.
  .process_one_row <- function(data1_row) {
    best_match_scores <- NULL
    row_match_opts <- list()

    filter_vals <- data1_row %>%
      dplyr::select(dplyr::all_of(match_cols1)) %>%
      dplyr::mutate(dplyr::across(dplyr::everything(), tolower))

    group_key <- paste(unlist(filter_vals[match_cols1], use.names = FALSE),
                       collapse = "\x01")

    data2_sub <- data2_by_group[[group_key]]

    if (is.null(data2_sub) || nrow(data2_sub) == 0L) {
      return(list(matched = NULL, unmatched = data1_row, match_opts = list()))
    }

    data2_sub$.grp_key_ <- NULL

    dists_jw      <- stringdist::stringdist(data1_row$school_name_std, data2_sub$school_name_std, method = "jw")
    dists_soundex <- stringdist::stringdist(data1_row$school_name_std, data2_sub$school_name_std, method = "soundex")
    dists_cosine  <- stringdist::stringdist(data1_row$school_name_std, data2_sub$school_name_std, method = "cosine")

    if (all(dists_jw > threshold_jw_min)) {
      return(list(matched = NULL, unmatched = data1_row, match_opts = list()))
    }

    best_idx <- NULL

    # ~ GOOD MATCHES ~
    if (any(dists_jw <= exact_jw)) {
      exact_match_idx <- which(dists_jw <= exact_jw)
      if (length(exact_match_idx) > 1) {
        exact_match_idx <- which(dists_jw == min(dists_jw))
      }
      if (length(exact_match_idx) > 1 && !is.na(data1_row$school_level)) {
        level_match <- data2_sub$school_level[exact_match_idx] == data1_row$school_level
        if (any(level_match, na.rm = TRUE)) {
          best_idx <- exact_match_idx[level_match][1]
        } else {
          best_idx <- exact_match_idx[1]
        }
      } else {
        best_idx <- exact_match_idx[1]
      }

    # ~ POTENTIAL OTHER MATCHES ~
    } else {
      keep_idx <- dists_jw <= threshold_jw | (dists_soundex == 0 & dists_cosine <= 0.25)
      data2_sub     <- data2_sub[keep_idx, ]
      dists_jw      <- dists_jw[keep_idx]
      dists_soundex <- dists_soundex[keep_idx]
      dists_cosine  <- dists_cosine[keep_idx]

      if (nrow(data2_sub) == 0) {
        return(list(matched = NULL, unmatched = data1_row, match_opts = list()))
      }

      candidate_indices <- order(dists_jw)[1:min(10, length(dists_jw))]
      data2_sub <- data2_sub[candidate_indices, ]
      dists_jw  <- dists_jw[candidate_indices]

      dists_gtbl <- match_locations(
        a = data1_row$school_name_std,
        names = data2_sub$school_name_std,
        return_score = TRUE,
        return_score_matrix = TRUE
      )

      if (nrow(dists_gtbl) == 1) {
        best_idx <- match(dists_gtbl$name, data2_sub$school_name_std)
      } else {
        dists_gtbl <- dists_gtbl %>%
          dplyr::mutate(prob_osa = osa / nchar(data2_sub$school_name_std))

        opt_df <- dists_gtbl %>%
          dplyr::as_tibble() %>%
          dplyr::mutate(
            name         = data1_row$school_name_std,
            name_options = data2_sub$school_name_std
          ) %>%
          dplyr::bind_cols(filter_vals[rep(1, nrow(.)), ]) %>%
          dplyr::select(name, name_options, dplyr::any_of(match_cols1), dplyr::everything()) %>%
          dplyr::filter(jw < .5, jaccard < .5) %>%
          dplyr::mutate(match_level = dplyr::case_when(
            (jaccard <= 0.05 & cosine <= 0.05)                    ~ 1,
            (jw <= 0.15)                                          ~ 1,
            (soundex == 0 & jw <= 0.3)                           ~ 1,
            (soundex == 0 & cosine <= 0.25)                      ~ 2,
            (jaccard <= 0.15 & cosine <= 0.15 & prob_osa <= 0.4) ~ 2,
            (jw <= 0.21)                                          ~ 2,
            TRUE                                                  ~ 1000
          ))

        row_match_opts <- stats::setNames(list(opt_df), data1_row$school_name_std)

        if (any(opt_df$match_level <= 3)) {
          best_local_idx <- which.min(opt_df$match_level)
          best_match_scores <- opt_df[best_local_idx, ]
          best_idx <- match(opt_df$name_options[best_local_idx], data2_sub$school_name_std)
        } else {
          return(list(matched = NULL, unmatched = data1_row, match_opts = row_match_opts))
        }
      }
    }

    best_distance <- dists_jw[best_idx]

    match_record <- tibble::tibble(
      match_score = 1 - best_distance,
      match_category = dplyr::case_when(
        best_distance <= 0.05 ~ "Exact Match",
        best_distance <= 0.10 ~ "High",
        best_distance <= 0.25 ~ "Moderate",
        TRUE ~ "Low"
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
      city                  = if ("city"          %in% names(data2_sub)) data2_sub$city[best_idx]          else NA_character_,
      state                 = if ("state"         %in% names(data2_sub)) data2_sub$state[best_idx]         else NA_character_,
      zip                   = if ("zip"           %in% names(data2_sub)) as.character(data2_sub$zip[best_idx]) else NA_character_,
      lat                   = if ("lat"           %in% names(data2_sub)) data2_sub$lat[best_idx]           else NA_real_,
      lon                   = if ("lon"           %in% names(data2_sub)) data2_sub$lon[best_idx]           else NA_real_,
      street1               = if ("street1"       %in% names(data2_sub)) data2_sub$street1[best_idx]       else NA_character_,
      data1_id              = data1_row$data1_id,
      data2_id              = data2_sub$data2_id[best_idx],
      {if (!is.null(best_match_scores)) {
        best_match_scores %>% dplyr::select(-c(name, name_options, dplyr::any_of(match_cols1)))
      } else {
        tibble::tibble(osa = NA, lv = NA, dl = NA, lcs = NA, qgram = NA, cosine = NA,
                       jaccard = NA, jw = NA, soundex = NA, score_sums = NA)
      }}
    )

    match_record <- match_record %>%
      dplyr::bind_cols(
        data1_row %>% dplyr::select(match_cols1[!(match_cols1 %in% colnames(match_record))])
      )

    list(matched = match_record, unmatched = NULL, match_opts = row_match_opts)
  }  # end .process_one_row

  # ---------------------------------------------------------------------------
  # Dispatch: sequential (default) or parallel via furrr.
  # When parallel = TRUE the caller must set up a future plan first, e.g.:
  #   future::plan(future::multisession, workers = parallel::detectCores() - 1)
  # If no plan has been set, furrr falls back to sequential execution.
  if (isTRUE(parallel)) {
    if (inherits(future::plan(), "sequential")) {
      warning(
        "parallel = TRUE but no future plan has been set; falling back to sequential execution. ",
        "Call future::plan(future::multisession) before invoking match_schools_names() to enable parallelism.",
        call. = FALSE
      )
    }
    row_results <- furrr::future_map(
      seq_len(nrow(data1)),
      function(i) .process_one_row(data1[i, ]),
      .options = furrr::furrr_options(seed = TRUE)
    )
  } else {
    row_results <- lapply(
      seq_len(nrow(data1)),
      function(i) .process_one_row(data1[i, ])
    )
  }

  matched_rows   <- Filter(Negate(is.null), lapply(row_results, `[[`, "matched"))
  unmatched_rows <- Filter(Negate(is.null), lapply(row_results, `[[`, "unmatched"))
  match_options  <- do.call(c, lapply(row_results, `[[`, "match_opts"))

  # --- Combine matched + unmatched into one table with consistent columns ---
  
  # Ensure these exist even if no matches/unmatches
  matched_df <- dplyr::bind_rows(matched_rows)
  unmatched_dat1 <- dplyr::bind_rows(unmatched_rows)
  unmatched_dat2 <- data2 %>%
    filter(!(school_name_std %in% matched_df$school_name_std_data2))
  
  match_options <- dplyr::bind_rows(match_options)
  
  # Update match summary to include unmatched data2
  match_summary <- table(c(matched_df$match_category,
                           rep("Unmatched_data1", nrow(unmatched_dat1)),
                           rep("Unmatched_data2", nrow(unmatched_dat2))))
  
  return(list(
    matched = matched_df,
    unmatched_dat1 = unmatched_dat1,
    unmatched_dat2 = unmatched_dat2,
    match_options = match_options,
    match_summary = match_summary
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