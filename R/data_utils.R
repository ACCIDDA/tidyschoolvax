





# vaxmodel/
# ├── DESCRIPTION
# ├── NAMESPACE
# ├── R/
# │   ├── load_model_estimates.R
# │   ├── interpolate_enrollment.R
# │   ├── fill_enrollment_gaps.R
# │   ├── build_prediction_results.R
# │   ├── summarize_state_coverage.R
# │   ├── summarize_vaxview.R
# │   ├── summarize_phi.R
# │   ├── summarize_offsets.R
# │   └── plot_functions.R






# VAXVIEW DATA ------------------------------------------------------------


#' Get SD from Confidence Interval
#'
#' @param lower_ci Lower confidence interval
#' @param upper_ci Upper confidence interval
#' @param n Sample size
#' @param conf_level Confidence level (default 0.95)
#'
#' @returns Standard deviation
#' @importFrom stats qt
#' @export
#'
#' @examples
#' get_sd_from_ci(40, 60, 100)
#' get_sd_from_ci(30, 70, 50, conf_level = 0.99)
#' 
get_sd_from_ci <- function(lower_ci, upper_ci, n, conf_level = 0.95) {
  # Calculate the alpha level
  alpha <- 1 - conf_level
  
  # Calculate the critical t-value
  # For a two-sided CI, we use alpha/2 and n-1 degrees of freedom
  t_value <- qt(1 - alpha / 2, df = n - 1)
  
  # Calculate the standard deviation
  sd <- (upper_ci - lower_ci) * sqrt(n) / (2 * t_value)
  
  return(sd)
}






#' Load and process child-level vaccine coverage data
#'
#' @param path Path to the ChildVaxView CSV file
#' @param state_name Full state name (e.g., "North Carolina")
#' @return A data.table with processed child-level vaccine data for all available vaccines
#' @importFrom data.table fread fwrite
#' @importFrom janitor clean_names
#' @importFrom dplyr arrange mutate
#' @importFrom tidyr separate
#' @export
load_child_vaxview <- function(path = "model/data/nis/ChildVaxView.csv", 
                               state_name,
                               pull_new = TRUE) {
  
  # Source: https://data.cdc.gov/api/views/fhky-rtsk/rows.csv?accessType=DOWNLOAD
  
  if (pull_new){
    tmp <- data.table::fread("https://data.cdc.gov/api/views/fhky-rtsk/rows.csv") %>%
      janitor::clean_names() %>% dplyr::as_tibble() 
    tmp <- tmp %>%
      tidyr::separate(x95_percent_ci_percent,
                      into = c("ci_low", "ci_high"),
                      sep = " to ", remove = TRUE, fill = "right")
    data.table::fwrite(tmp, path)
  }
  
  data.table::fread(path)[
    geography == state_name &
      dimension_type == "Age" & 
      !grepl("-", birth_year_birth_cohort),
    .(
      pop = "child",
      vaccine = vaccine,
      year = as.integer(birth_year_birth_cohort),
      year_type = "birth",
      age_range = dimension,
      n = sample_size,
      p = estimate_percent / 100,
      x = round((estimate_percent / 100) * sample_size)
      # sd = get_sd_from_ci(ci_low / 100, ci_high / 100, sample_size, conf_level = 0.95),
    )
  ] %>%
    dplyr::arrange(year) %>%
    dplyr::mutate(
      sd = sqrt(p * (1 - p)),
      se = sqrt((p * (1 - p)) / n)
    )
}





#' Load and process school-level vaccine coverage data
#'
#' @param path Path to the SchoolVaxView CSV file
#' @param state_name Full state name
#' @return A data.table with processed school-level vaccine data for all available vaccines
#' @importFrom data.table fread fwrite
#' @importFrom janitor clean_names
#' @importFrom dplyr filter mutate select rename arrange as_tibble
#' @export
load_school_vaxview <- function(path = "model/data/nis/SchoolVaxView.csv", 
                                state_name, 
                                pull_new = TRUE) {
  
  # Source: https://catalog.data.gov/dataset/vaccination-coverage-and-exemptions-among-kindergartners-fdd61/resource/d6134d10-478b-45dc-ab79-aa9b15b16fec
  
  if (pull_new){
    tmp <- data.table::fread("https://data.cdc.gov/api/views/ijqb-a7ye/rows.csv") %>%
      janitor::clean_names() %>% dplyr::as_tibble() 
    tmp <- tmp %>%
      dplyr::rename(vaccine = vaccine_exemption) %>%
      dplyr::mutate(estimate_percent = as.numeric(estimate_percent),
                    percent_surveyed = as.numeric(percent_surveyed),
                    population_size = as.integer(population_size))
    data.table::fwrite(tmp, path)
  }
  
  data.table::fread(path) %>%
    dplyr::filter(geography == state_name) %>%
    dplyr::mutate(sample_size = round(population_size * (percent_surveyed / 100))) %>%
    dplyr::mutate(survey_year = as.numeric(substr(school_year, 1, 4))) %>%
    dplyr::mutate(
      pop = "school",
      year = survey_year,
      year_type = "survey",
      age_range = "5-6 Years",
      n = sample_size,
      p = estimate_percent / 100, 
      x = round(as.numeric((estimate_percent / 100) * sample_size)),
    ) %>%
    dplyr::select(
      pop,
      vaccine,
      year,
      year_type,
      age_range,
      n, p, x
    ) %>%
    dplyr::arrange(year)  %>%
    dplyr::mutate(
      sd = sqrt(p * (1 - p)),
      se = sqrt((p * (1 - p)) / n)
    )
}



#' Load and process teen-level vaccine coverage data
#'
#' @param path Path to the TeenVaxView CSV file
#' @param state_name Full state name
#' @return A data.table with processed teen-level vaccine data for all available vaccines
#' @importFrom data.table fread fwrite
#' @importFrom janitor clean_names
#' @importFrom dplyr arrange mutate rename as_tibble
#' @importFrom tidyr separate
#' @export
load_teen_vaxview <- function(path = "model/data/nis/TeenVaxView.csv", 
                              state_name,
                              pull_new = TRUE) {
  
  # Source: https://catalog.data.gov/dataset/vaccination-coverage-among-adolescents-13-17-years-acc00/resource/7e332ad5-8e0d-437e-b3b2-029b21c8472e
  
  if (pull_new){
    tmp <- data.table::fread("https://data.cdc.gov/api/views/ee48-w5t6/rows.csv") %>%
      janitor::clean_names() %>% dplyr::as_tibble()
    tmp <- tmp %>%
      tidyr::separate(x95_percent_ci_percent,
                      into = c("ci_low", "ci_high"),
                      sep = " to ", remove = TRUE, fill = "right") %>%
      dplyr::rename(vaccine = vaccine_sample)
    data.table::fwrite(tmp, path)
  }
  
  data.table::fread(path)[
    geography == state_name &
      dimension_type == "Age",
    .(
      pop = "teen",
      vaccine = vaccine,
      year = as.numeric(survey_year),
      year_type = "survey",
      age_range = dimension,
      n = sample_size,
      p = estimate_percent / 100,
      x = round((estimate_percent / 100) * sample_size)
      # sd = get_sd_from_ci(ci_low / 100, ci_high / 100, sample_size, conf_level = 0.95),
    )
  ] %>%
    dplyr::arrange(year)  %>%
    dplyr::mutate(
      sd = sqrt(p * (1 - p)),
      se = sqrt((p * (1 - p)) / n)
    )
}



#' Combine child, school, and teen vaccine coverage data
#'
#' @param child_dt Child-level data.table
#' @param school_dt School-level data.table
#' @param teen_dt Teen-level data.table
#' @return A unified data.table of vaccine coverage
#' @importFrom dplyr bind_rows
#' @export
combine_vaxview_data <- function(child_dt, school_dt, teen_dt) {
  dplyr::bind_rows(child_dt, school_dt, teen_dt)
}


#' Save vaccine coverage data to Parquet format
#'
#' @param vv_all Combined vaccine coverage data.table
#' @param path Path to save the Parquet file
#' @return NULL
#' @importFrom arrow write_parquet
#' @export
save_vaxview_parquet <- function(vv_all, path) {
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  arrow::write_parquet(vv_all, path)
}


#' set up vaxview data
#' @param state_name Full state name
#' @param child_vaxview_path Path to child VaxView CSV
#' @param school_vaxview_path Path to school VaxView CSV
#' @param teen_vaxview_path Path to teen VaxView CSV
#' @param vax_data_path Path to save combined vaccine data Parquet
#' 
#' @return A data.table of combined vaccine coverage data
#' @export
setup_vaxview_data <- function(state_name, 
                               child_vaxview_path,
                               school_vaxview_path,
                               teen_vaxview_path,
                               vax_data_path, 
                               pull_new = TRUE) {
  
  vv_child <- load_child_vaxview(child_vaxview_path, state_name)
  vv_school <- load_school_vaxview(school_vaxview_path, state_name)
  vv_teen <- load_teen_vaxview(teen_vaxview_path, state_name)
  
  vv_all <- combine_vaxview_data(vv_child, vv_school, vv_teen)
  
  save_vaxview_parquet(vv_all = vv_all, path = vax_data_path)
  
  return(vv_all)
}

