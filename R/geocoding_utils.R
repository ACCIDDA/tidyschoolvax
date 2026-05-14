# install.packages(c("sf", "dplyr", "tigris"))
# options(tigris_use_cache = TRUE)   # speeds up repeated calls


# 
# county_zips <- readr::read_csv(zip_county_path, show_col_types = FALSE) %>%
#   dplyr::select(zip = ZIP, fips = COUNTY, state = USPS_ZIP_PREF_STATE, ratio = TOT_RATIO)
# 
# state_zip_prefix <- county_zips %>% dplyr::select(state, zip, fips) %>%
#   mutate(state = tolower(state)) %>%
#   mutate(zip = substr(zip, 1, 2),
#          fips = substr(fips, 1, 2)) %>%
#   distinct()
# 
# # add state_zip_prefix to package as data
# usethis::use_data(state_zip_prefix, internal = TRUE, overwrite = TRUE)
# readr::write_csv(state_zip_prefix, "data/state_zip_prefix.csv")



# **2. Function: Assign County from Latitude/Longitude**
#  This uses **TIGER/Line county boundaries** (U.S. Census).



#' Get U.S. County from Latitude and Longitude
#'
#' Uses TIGER/Line county boundaries to identify the county that contains
#' each provided latitude/longitude pair. Returns county name and state FIPS.
#'
#' @param lat Numeric vector of latitudes in decimal degrees (WGS84).
#' @param lon Numeric vector of longitudes in decimal degrees (WGS84).
#'
#' @return A tibble with one row per input coordinate containing:
#' \describe{
#'   \item{latitude}{Input latitude}
#'   \item{longitude}{Input longitude}
#'   \item{county}{County name}
#'   \item{state}{State FIPS code}
#' }
#'
#' @examples
#' get_county(39.2904, -76.6122)
#'
#' @importFrom sf st_as_sf st_transform st_join st_within
#' @importFrom tibble tibble
#' @importFrom tigris counties
#' @export
get_county <- function(lat, lon) {
  
  pts <- sf::st_as_sf(
    data.frame(lat = lat, lon = lon),
    coords = c("lon", "lat"),
    crs = 4326
  )
  
  counties <- tigris::counties(cb = TRUE, year = 2023) |>
    sf::st_transform(4326)
  
  joined <- sf::st_join(pts, counties, join = sf::st_within)
  
  dplyr::tibble(
    latitude = lat,
    longitude = lon,
    county = joined$NAMELSAD,
    state = joined$STATEFP
  )
}


# **3. Function: Assign ZIP Code from Latitude/Longitude**
#  ZIP codes are not polygons for postal purposes—but the Census provides **ZCTA boundaries**, which are widely used for spatial work.



#' Get ZIP Code (ZCTA) from Latitude and Longitude
#'
#' Uses Census ZCTA polygons to identify the ZIP Code Tabulation Area (ZCTA)
#' containing each latitude/longitude point.
#'
#' @param lat Numeric vector of latitudes in decimal degrees (WGS84).
#' @param lon Numeric vector of longitudes in decimal degrees (WGS84).
#'
#' @return A tibble containing:
#' \describe{
#'   \item{latitude}{Input latitude}
#'   \item{longitude}{Input longitude}
#'   \item{zip_code}{5-digit ZCTA code}
#' }
#'
#' @examples
#' get_zip(39.2904, -76.6122)
#'
#' @importFrom sf st_as_sf st_transform st_join st_within
#' @importFrom tibble tibble
#' @importFrom tigris zctas
#' @export
get_zip <- function(lat, lon, data_year = 2020) {
  
  pts <- sf::st_as_sf(
    data.frame(lat = lat, lon = lon),
    coords = c("lon", "lat"),
    crs = 4326
  )
  
  zips <- tigris::zctas(cb = TRUE, year = data_year) |>
    sf::st_transform(4326)
  
  joined <- sf::st_join(pts, zips, join = sf::st_within)
  
  dplyr::tibble(
    latitude = lat,
    longitude = lon,
    zip_code = joined$ZCTA5CE10
  )
}





# ** Combined Function: Both County & ZIP Code**

#' Get County and ZIP Code (ZCTA) from Latitude and Longitude
#'
#' Convenience function that returns county, state FIPS, and ZCTA ZIP code
#' for each coordinate pair by performing spatial joins against both
#' county and ZCTA boundaries.
#'
#' @param lat Numeric vector of latitudes in decimal degrees (WGS84).
#' @param lon Numeric vector of longitudes in decimal degrees (WGS84).
#'
#' @return A tibble with:
#' \describe{
#'   \item{latitude}{Input latitude}
#'   \item{longitude}{Input longitude}
#'   \item{county}{County name}
#'   \item{state}{State FIPS code}
#'   \item{zip_code}{ZCTA ZIP code}
#' }
#'
#' @examples
#' get_geo_info(39.2904, -76.6122)
#'
#' @importFrom sf st_as_sf st_transform st_join st_within
#' @importFrom tibble tibble
#' @importFrom tigris counties zctas
#' @importFrom dplyr filter pull
#' @importFrom readr read_csv
#' @export
get_geo_info <- function(state_abbr = state, lat, lon, data_year = 2020) {
  
  # load zip codes to filter and speedup
  data("state_zip_prefix")
  state_fips_prefix <- state_zip_prefix %>% 
    dplyr::filter(state %in% tolower(state_abbr)) %>%
    dplyr::pull(fips) %>% unique()  
  state_zip_prefix <- state_zip_prefix %>% 
    dplyr::filter(state %in% tolower(state_abbr)) %>%
    dplyr::pull(zip) %>% unique()
    
  pts <- sf::st_as_sf(
    data.frame(lat = lat, lon = lon),
    coords = c("lon", "lat"),
    crs = 4326
  )
  
  counties <- tigris::counties(state = state_fips_prefix, cb = TRUE, year = data_year) |>
    sf::st_transform(4326)
  
  zips <- tigris::zctas(cb = TRUE, year = data_year, starts_with = state_zip_prefix) |>
    sf::st_transform(4326)
  
  county_join <- sf::st_join(pts, counties, join = sf::st_within)
  zip_join <- sf::st_join(pts, zips, join = sf::st_within)
  
  dplyr::tibble(
    latitude = lat,
    longitude = lon,
    county = county_join$NAMELSAD,
    state_abbr = county_join$STUSPS,
    zip_code = zip_join$ZCTA5
  )
}








  
