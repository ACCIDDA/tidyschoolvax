


#' Get county-zip mapping with FIPS codes
#'
#' @description
#' Get a mapping of zip codes to counties with FIPS codes
#' 
#' @returns A data.frame mapping zip codes to counties with FIPS codes
#' @importFrom dplyr select mutate left_join
#' 
#' @export 
#'
#' @examples 
#' county_zips <- get_county_zips()
#' 
get_county_zips <- function(){
  
  data("fips_codes", package = "tigris")
  data("zip_county", package = "tidyschoolvax")
  
  county_zips <- zip_county %>%
    dplyr::select(zip = ZIP, fips = COUNTY, state = USPS_ZIP_PREF_STATE, ratio = TOT_RATIO)
  
  fips_codes <- fips_codes %>%
    dplyr::mutate(fips = paste0(state_code, county_code)) %>%
    dplyr::select(state, county, fips)
  
  county_zips <- county_zips %>%
    dplyr::left_join(fips_codes, by = c("state", "fips"))
  
  return(county_zips)
}





#' Extract schools data from GreatSchools.org for a single zip code
#'
#' @param zipcode A US zip code (default = 21231) 
#'
#' @returns A data.frame of schools data from GreatSchools.org
#' 
#' @importFrom httr GET content user_agent timeout
#' @importFrom jsonlite fromJSON
#' @importFrom stringr str_match
#' @importFrom dplyr bind_rows
#' 
#' @details
#' This function fetches the schools data for a given zip code from GreatSchools.org
#' by making an HTTP GET request to the search page and extracting the embedded JSON data.
#' The schools data is returned as a data.frame.
#' 
#' @export
#' 
extract_gs_schoolsdata <- function(zipcode = 21231) {
  
  page_valid   <- TRUE
  i            <- 1
  schools_data <- list()
  
  while (page_valid) {
    page_num <- i
    message("Fetching ZIP: ", zipcode, ", Page: ", page_num)
    
    url <- if (page_num == 1) {
      paste0("https://www.greatschools.org/search/search.page?q=", zipcode)
    } else {
      paste0("https://www.greatschools.org/search/search.page?page=", page_num, "&q=", zipcode)
    }
    
    schools <- NULL
    
    tryCatch({
      resp <- httr::GET(
        url,
        httr::user_agent("Mozilla/5.0"),
        httr::timeout(10)
      )
      
      if (resp$status_code == 200) {
        htmltxt <- httr::content(resp, "text", encoding = "UTF-8")
        htmltxt <- paste(htmltxt, collapse = "\n")
        
        # extract JSON after "gon.search=" and before the next semicolon
        json_txt <- stringr::str_match(htmltxt, "gon\\.search=([^;]+);")[, 2]
        
        # if pattern not found, json_txt will be NA -> treat as no schools
        if (!is.na(json_txt)) {
          gs <- jsonlite::fromJSON(json_txt)
          schools <- gs$schools
          if (!is.null(schools) && nrow(schools) > 0) {
            schools_data[[i]] <- schools
          }
        }
      } else {
        message("[", i, "] Non-200 response (", resp$status_code, ") for ", url)
      }
      
    }, error = function(e) {
      # FIX: query_final doesn't exist; log what we *do* have
      message("[", i, "] Error fetching ZIP=", zipcode, " page=", page_num,
              " (", url, "): ", e$message)
    })
    
    if (is.null(schools) || nrow(schools) == 0) {
      page_valid <- FALSE
      break
    }
    
    i <- i + 1
  }
  
  if (length(schools_data) == 0) {
    return(NULL)
  } else {
    return(dplyr::bind_rows(schools_data))
  }
}









#' Scrape schools from GreatSchools.org by state
#'
#' @description
#' Given a CSV of school names and cities, scrape addresses from GreatSchools.org,
#' validate ZIP codes by state, and return a cleaned dataframe.
#'
#' @param state_abbr Two-letter state abbreviation (default = "md").
#' @param workers Number of parallel workers to use (default = 10).
#' @return A list with two dataframes:
#'   - `found`: rows with valid addresses
#'   - `not_found`: rows missing address info
#'   
#' @importFrom dplyr as_tibble filter select bind_rows left_join
#' @importFrom dplyr rename everything
#' @importFrom future plan multisession sequential
#' @importFrom furrr future_map
#' @importFrom purrr compact
#' @details
#' The function reads a CSV file containing school names and cities, constructs search queries,
#'
#' @examples
#' \dontrun{
#' results <- scrape_greatschools("md_schools.csv")
#' saveRDS(results$found, "addressed_greatschools.rds")
#' saveRDS(results$not_found, "input_match.rds")
#' }
#' 
#' @export
#' 
scrape_greatschools_schools <- function(state_abbr = "md", 
                                        workers = 10) {
  
  # Derive state from filename
  message("Running GreatSchools scraper for state: ", toupper(state_abbr))
  
  # Look up ZIP info & Filter for state
  state_zips <- get_county_zips() %>%
    dplyr::as_tibble() %>%
    dplyr::filter(state == toupper(state_abbr)) %>%
    dplyr::select(state, county, zip, ratio)
    
  zips <- state_zips$zip %>% unique()
  
  
  # Parallel scraping
  
  # Define the worker plan
  future::plan(future::multisession, workers = workers)   # adjust workers as needed
  
  results <- furrr::future_map(zips, tidyschoolvax::process_zip, .progress = TRUE)
  # tmp <- process_zip(zips[9])
  future::plan(future::sequential)  # reset to sequential plan
  names(results) <- zips
  # Combine into one data.frame (dropping NULLs)
  df_schools <- dplyr::bind_rows(purrr::compact(results))
  
  # assign county based on zipcode
  df_schools <- df_schools %>%
    dplyr::left_join(state_zips %>% dplyr::filter(ratio >= 0.5) %>% dplyr::select(-ratio), 
                     by = c("zip" = "zip", "state"="state"), relationship = "many-to-many") %>%
    dplyr::left_join(state_zips %>% dplyr::filter(ratio < 0.5) %>% rename(county2 = county) %>% dplyr::select(-ratio), 
                     by = c("zip" = "zip", "state"="state"), relationship = "many-to-many") %>%
    dplyr::select(state, county, county2, everything())
  
  return(df_schools)
}


#' Function to process one ZIP
#'
#' @param zipcode 
#' 
#' @importFrom dplyr select bind_cols 
#'
#' @returns A data.frame of schools data for the given ZIP code, or NULL if no schools found
#' 
#' @export
#'
process_zip <- function(zipcode) {
  res <- extract_gs_schoolsdata(zipcode)
  if (is.null(res)) return(NULL)   # safety check
  
  res %>%
    dplyr::select(
      id, name, gradeLevels,
      districtId, districtName, districtCity,
      lat, lon, state,
      levelCode, schoolType, type, enrollment
    ) %>%
    dplyr::bind_cols(res$address)
}
