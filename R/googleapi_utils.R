# ==============================================================================
# googleapi_utils.R
# Purpose: Reusable utility functions for geocoding and school status lookup
# Includes: Manual API safety limit to prevent exceeding monthly quota
# ==============================================================================


# ------------------------------------------------------------------------------
# STANDARDIZING MISSING DATA
# ------------------------------------------------------------------------------

#' Normalize empty strings to \code{NA} across all character columns
#'
#' Trims whitespace from every character column and replaces empty strings
#' with \code{NA_character_}.
#'
#' @param df A data frame.
#'
#' @return The same data frame with empty-string character values replaced by
#'   \code{NA}.
#' @export
normalize_missing_strings <- function(df) {
  
  df %>%
    dplyr::mutate(
      dplyr::across(
        where(is.character),
        ~ dplyr::na_if(stringr::str_trim(.x), "")
      )
    )
}

# ==============================================================================
# Updated Geocoding Functions - Three-Step Workflow
# ==============================================================================

# ------------------------------------------------------------------------------
# Helper Functions
# ------------------------------------------------------------------------------

#' Check whether an address string starts with a digit
#'
#' Returns \code{TRUE} for each element of \code{addr} that is non-\code{NA}
#' and begins with a numeric digit (i.e., looks like a street address).
#'
#' @param addr Character vector of address strings.
#'
#' @return A logical vector the same length as \code{addr}.
#' @keywords internal
has_valid_address <- function(addr) {
  !is.na(addr) & stringr::str_detect(addr, "^\\d")
}

#' Validate that a data frame has the columns required for geocoding
#'
#' Stops with an informative error if \code{school_name} or \code{state} are
#' absent and issues a warning for missing optional columns.
#'
#' @param df A data frame to validate.
#'
#' @return \code{TRUE} invisibly when validation passes.
#' @keywords internal
validate_input_data <- function(df) {
  # Check for required columns
  required_cols <- c("school_name", "state")
  missing_required <- setdiff(required_cols, names(df))
  
  if (length(missing_required) > 0) {
    stop("Missing required columns: ", paste(missing_required, collapse = ", "),
         "\nRequired columns are: ", paste(required_cols, collapse = ", "))
  }
  
  # Warn about missing optional columns
  optional_cols <- c("addr_clean", "city", "zip", "county_std")
  missing_optional <- setdiff(optional_cols, names(df))
  
  if (length(missing_optional) > 0) {
    warning("Missing optional columns: ", paste(missing_optional, collapse = ", "),
            "\nThese columns help improve geocoding accuracy.")
  }
  
  # Check for empty school_name
  if (all(is.na(df$school_name))) {
    stop("All school_name values are NA. Cannot geocode without school names.")
  }
  
  return(TRUE)
}

#' Consolidate duplicate API response columns into one
#'
#' When the Google Geocoding API returns duplicate columns such as
#' \code{place_id} and \code{place_id...33}, this function coalesces all
#' columns whose name starts with \code{col_name} into a single canonical
#' column and drops the extras.
#'
#' @param df A data frame returned by \code{tidygeocoder::geocode}.
#' @param col_name Character scalar naming the canonical column (e.g.
#'   \code{"place_id"}).
#'
#' @return The data frame with duplicate columns merged.
#' @keywords internal
consolidate_api_cols <- function(df, col_name) {
  matching_cols <- grep(paste0("^", col_name), names(df), value = TRUE)
  if (length(matching_cols) <= 1) return(df)
  
  df[[col_name]] <- do.call(dplyr::coalesce,
                             lapply(matching_cols, function(cn) df[[cn]]))
  extra_cols <- setdiff(matching_cols, col_name)
  df[, !names(df) %in% extra_cols, drop = FALSE]
}

# ------------------------------------------------------------------------------
# STEP 1: Load Cache and Merge
# ------------------------------------------------------------------------------

#' Load a geocoding cache and merge it into the unique-schools table
#'
#' Reads a previously saved \code{.rds} cache file (if it exists) and fills
#' in \code{place_id}, coordinates, and address fields for any schools that
#' were already geocoded.  Schools not found in the cache are left untouched
#' so that subsequent geocoding steps can handle them.
#'
#' @param unique_schools A data frame of unique schools with at least a
#'   \code{school_name} column.
#' @param save_path Character scalar.  Full path to the \code{.rds} cache file
#'   written by \code{run_full_geocoding}.
#'
#' @return The \code{unique_schools} data frame augmented with cached geocoding
#'   columns where available.
#' @keywords internal
load_and_merge_cache <- function(unique_schools, save_path) {
  
  result <- unique_schools
  if (!("place_id"          %in% names(result))) result$place_id          <- NA_character_
  if (!("lat"               %in% names(result))) result$lat               <- NA_real_
  if (!("lon"               %in% names(result))) result$lon               <- NA_real_
  if (!("addr_clean"        %in% names(result))) result$addr_clean        <- NA_character_
  if (!("city"              %in% names(result))) result$city              <- NA_character_
  if (!("zip"               %in% names(result))) result$zip               <- NA_character_
  if (!("formatted_address" %in% names(result))) result$formatted_address <- NA_character_
  if (!("geo_source"        %in% names(result))) result$geo_source        <- NA_character_
  
  result <- result %>%
    dplyr::mutate(
      dplyr::across(dplyr::any_of(c("place_id", "addr_clean", "city",
                                    "zip", "formatted_address", "geo_source")),
                    as.character),
      dplyr::across(dplyr::any_of(c("lat", "lon")), as.numeric)
    )
  
  # ── Early exit when no cache exists ──────────────────────────────────────────
  if (!file.exists(save_path)) {
    cat("  No cache file found. Proceeding straight to geocoding.\n")
    return(result)
  }
  
  cache <- readRDS(save_path) %>%
    dplyr::distinct(school_name, .keep_all = TRUE)
  
  # --------------------------------------------------------------------------
  # Step 1a: Copy place_id (and formatted_address) for ALL schools from cache
  # --------------------------------------------------------------------------
  if ("place_id" %in% names(cache)) {
    
    # Build cache_1a with only columns that actually exist in the cache
    cache_1a <- cache %>%
      dplyr::select(school_name, place_id_cache = place_id)
    
    if ("formatted_address" %in% names(cache)) {
      cache_1a <- cache_1a %>%
        dplyr::mutate(formatted_address_cache = cache$formatted_address[
          match(school_name, cache$school_name)
        ])
      # Simpler: just select it directly
      cache_1a <- cache %>%
        dplyr::select(school_name,
                      place_id_cache          = place_id,
                      formatted_address_cache = formatted_address)
    }
    
    has_fmt_1a <- "formatted_address_cache" %in% names(cache_1a)
    
    result <- result %>%
      dplyr::left_join(cache_1a, by = "school_name") %>%
      dplyr::mutate(
        dplyr::across(dplyr::any_of(c("place_id_cache", "formatted_address_cache")),
                      as.character),
        got_place_id_from_cache = is.na(place_id) & !is.na(place_id_cache),
        place_id = dplyr::coalesce(place_id, place_id_cache),
        # Only coalesce formatted_address if the cache column was actually joined
        formatted_address = if (has_fmt_1a)
          dplyr::coalesce(formatted_address, formatted_address_cache)
        else
          formatted_address,
        geo_source = dplyr::if_else(
          got_place_id_from_cache,
          "cache",
          geo_source
        )
      ) %>%
      dplyr::select(-place_id_cache,
                    -dplyr::any_of("formatted_address_cache"),
                    -got_place_id_from_cache)
  }
  
  # --------------------------------------------------------------------------
  # Step 1b: For schools without valid addr_clean OR without lat/lon,
  # copy addr_clean, city, zip, lat, lon, formatted_address from cache
  # --------------------------------------------------------------------------
  result <- result %>%
    dplyr::mutate(
      needs_cache_data = !has_valid_address(addr_clean) | is.na(lat) | is.na(lon)
    )
  
  cache_cols_map <- c(
    addr_clean_cache        = "addr_clean",
    city_cache              = "city",
    zip_cache               = "zip",
    lat_cache               = "lat",
    lon_cache               = "lon",
    formatted_address_cache = "formatted_address"
  )
  available_cache_cols <- cache_cols_map[cache_cols_map %in% names(cache)]
  
  if (length(available_cache_cols) > 0) {
    cache_subset <- cache %>%
      dplyr::select(school_name, dplyr::any_of(available_cache_cols))
    names(cache_subset)[-1] <- names(available_cache_cols)
    
    result <- result %>%
      dplyr::left_join(cache_subset, by = "school_name") %>%
      dplyr::mutate(
        dplyr::across(dplyr::any_of(c("addr_clean_cache", "city_cache",
                                      "zip_cache", "formatted_address_cache")),
                      as.character),
        dplyr::across(dplyr::any_of(c("lat_cache", "lon_cache")),
                      as.numeric)
      )
    
    fill_pairs <- list(
      list(cache_col = "lat_cache",               orig_col = "lat"),
      list(cache_col = "lon_cache",               orig_col = "lon"),
      list(cache_col = "addr_clean_cache",        orig_col = "addr_clean"),
      list(cache_col = "formatted_address_cache", orig_col = "formatted_address")
    )
    fill_pairs <- Filter(function(p) p$cache_col %in% names(result), fill_pairs)
    
    if (length(fill_pairs) > 0) {
      got_data_flags <- lapply(fill_pairs, function(p) {
        !is.na(result[[p$cache_col]]) & is.na(result[[p$orig_col]])
      })
      result$got_data_from_cache <- result$needs_cache_data &
        Reduce(`|`, got_data_flags)
    } else {
      result$got_data_from_cache <- FALSE
    }
    
    has_addr <- "addr_clean_cache"        %in% names(result)
    has_city <- "city_cache"              %in% names(result)
    has_zip  <- "zip_cache"               %in% names(result)
    has_lat  <- "lat_cache"               %in% names(result)
    has_lon  <- "lon_cache"               %in% names(result)
    has_fmt  <- "formatted_address_cache" %in% names(result)
    
    result <- result %>%
      dplyr::mutate(
        addr_clean = if (has_addr) dplyr::if_else(
          needs_cache_data & !is.na(.data[["addr_clean_cache"]]),
          .data[["addr_clean_cache"]], addr_clean) else addr_clean,
        city = if (has_city) dplyr::if_else(
          needs_cache_data & !is.na(.data[["city_cache"]]),
          .data[["city_cache"]], city) else city,
        zip = if (has_zip) dplyr::if_else(
          needs_cache_data & !is.na(.data[["zip_cache"]]),
          .data[["zip_cache"]], zip) else zip,
        lat = if (has_lat) dplyr::if_else(
          needs_cache_data & !is.na(.data[["lat_cache"]]),
          .data[["lat_cache"]], lat) else lat,
        lon = if (has_lon) dplyr::if_else(
          needs_cache_data & !is.na(.data[["lon_cache"]]),
          .data[["lon_cache"]], lon) else lon,
        formatted_address = if (has_fmt) dplyr::if_else(
          needs_cache_data & !is.na(.data[["formatted_address_cache"]]),
          .data[["formatted_address_cache"]], formatted_address) else formatted_address,
        geo_source = dplyr::if_else(
          got_data_from_cache & is.na(geo_source),
          "cache",
          geo_source
        )
      ) %>%
      dplyr::select(-needs_cache_data, -got_data_from_cache,
                    -dplyr::any_of(names(available_cache_cols)))
  } else {
    result <- result %>% dplyr::select(-needs_cache_data)
  }
  
  return(result)
}
# ------------------------------------------------------------------------------
# STEP 2: Geocode schools WITH valid address but missing coords or place_id
# ------------------------------------------------------------------------------

#' Geocode a chunk of schools that have a valid street address
#'
#' Filters the input chunk to rows that have a street address (starts with a
#' digit) but are missing coordinates or a Place ID, calls the Google
#' Geocoding API via \code{tidygeocoder::geocode}, and returns the full chunk
#' with new values filled in.
#'
#' @param chunk_df A data frame (a chunk of the unique-schools table).
#' @param google_api_key Character scalar.  Google Geocoding API key.
#'
#' @return The \code{chunk_df} with geocoded rows updated.
#' @importFrom tidygeocoder geocode
#' @keywords internal
geocode_chunk_step2 <- function(chunk_df, google_api_key) {
  # Filter: has valid addr_clean BUT missing lat/lon OR missing place_id
  # This covers both: (a) has address but no coords, (b) has address+coords but no place_id
  to_geocode <- chunk_df %>%
    dplyr::filter(has_valid_address(addr_clean) & (is.na(lat) | is.na(lon) | is.na(place_id)))
  
  already_done <- chunk_df %>%
    dplyr::filter(!(has_valid_address(addr_clean) & (is.na(lat) | is.na(lon) | is.na(place_id))))
  
  if (nrow(to_geocode) == 0) {
    return(already_done)
  }
  
  Sys.setenv(GOOGLEGEOCODE_API_KEY = google_api_key)
  
  # Build search query: school_name, addr_clean, city, state, zip
  geocoded <- to_geocode %>%
    dplyr::mutate(
      query = stringr::str_squish(paste(
        dplyr::coalesce(school_name, ""),
        dplyr::coalesce(addr_clean, ""),
        dplyr::coalesce(city, ""),
        dplyr::coalesce(state, ""),
        dplyr::coalesce(zip, "")
      ))
    ) %>%
    tidygeocoder::geocode(
      address = query,
      method = "google",
      lat = lat_temp,
      long = lon_temp,
      full_results = TRUE
    )
  
  # Consolidate duplicate API columns (place_id...33, formatted_address...34, etc.)
  geocoded <- consolidate_api_cols(geocoded, "place_id")
  geocoded <- consolidate_api_cols(geocoded, "formatted_address")
  
  geocoded <- geocoded %>%
    dplyr::mutate(
      # Use new coords only if existing ones are missing; preserve existing coords if present
      lat               = dplyr::coalesce(lat, lat_temp),
      lon               = dplyr::coalesce(lon, lon_temp),
      # Always take new place_id and formatted_address from API (overwrite old)
      place_id          = dplyr::coalesce(place_id, NA_character_),
      formatted_address = dplyr::coalesce(formatted_address, NA_character_),
      # Mark as google only when new data was actually retrieved
      geo_source      = dplyr::if_else(
        !is.na(place_id) | !is.na(lat),
        "google",
        geo_source
      )
    ) %>%
    dplyr::select(-lat_temp, -lon_temp, -query)
  
  return(dplyr::bind_rows(already_done, geocoded))
}

# ------------------------------------------------------------------------------
# STEP 3: Geocode schools WITHOUT valid address
# ------------------------------------------------------------------------------

#' Geocode a chunk of schools that lack a valid street address
#'
#' Filters the input chunk to rows that are missing coordinates and do not
#' have a street-numbered address, builds a query from school name, county,
#' state, and ZIP, calls the Google Geocoding API via
#' \code{tidygeocoder::geocode}, and returns the full chunk with updated
#' values.  Address fields are extracted from the returned
#' \code{formatted_address} where possible.
#'
#' @param chunk_df A data frame (a chunk of the unique-schools table).
#' @param google_api_key Character scalar.  Google Geocoding API key.
#'
#' @return The \code{chunk_df} with geocoded rows updated.
#' @importFrom tidygeocoder geocode
#' @keywords internal
geocode_chunk_step3 <- function(chunk_df, google_api_key) {
  # Filter: missing lat/lon AND (no addr_clean OR addr_clean doesn't start with number)
  to_geocode <- chunk_df %>%
    dplyr::filter((is.na(lat) | is.na(lon)) & !has_valid_address(addr_clean))
  
  already_done <- chunk_df %>%
    dplyr::filter(!((is.na(lat) | is.na(lon)) & !has_valid_address(addr_clean)))
  
  if (nrow(to_geocode) == 0) {
    return(already_done)
  }
  
  Sys.setenv(GOOGLEGEOCODE_API_KEY = google_api_key)
  
  # Build search query: school_name, county_std, state, zip
  geocoded <- to_geocode %>%
    dplyr::mutate(
      query = stringr::str_squish(paste(
        dplyr::coalesce(school_name, ""),
        dplyr::coalesce(county_std, ""),
        dplyr::coalesce(state, ""),
        dplyr::coalesce(zip, "")
      ))
    ) %>%
    tidygeocoder::geocode(
      address = query,
      method = "google",
      lat = lat_temp,
      long = lon_temp,
      full_results = TRUE
    )
  
  # Consolidate duplicate API columns (place_id...33, formatted_address...34, etc.)
  geocoded <- consolidate_api_cols(geocoded, "place_id")
  geocoded <- consolidate_api_cols(geocoded, "formatted_address")
  
  geocoded <- geocoded %>%
    dplyr::mutate(
      lat               = dplyr::coalesce(lat_temp, lat),
      lon               = dplyr::coalesce(lon_temp, lon),
      place_id          = dplyr::coalesce(place_id, NA_character_),
      formatted_address = dplyr::coalesce(formatted_address, NA_character_),
      # Mark as google only when new data was actually retrieved
      geo_source      = dplyr::if_else(
        !is.na(lat) | !is.na(place_id),
        "google",
        geo_source
      )
    ) %>%
    dplyr::select(-lat_temp, -lon_temp, -query)
  
  # Apply address cleaning ONLY to Step 3 results
  geocoded <- clean_geocoded_addresses_step3(geocoded)
  
  return(dplyr::bind_rows(already_done, geocoded))
}

# ------------------------------------------------------------------------------
# Clean Step 3 Results - Extract address from formatted_address
# ------------------------------------------------------------------------------

#' Extract street address components from a Google formatted address
#'
#' For rows geocoded in Step 3 (no prior street address), attempts to parse
#' \code{addr_clean}, \code{city}, and \code{zip} out of the
#' \code{formatted_address} field returned by the API.  Only rows whose
#' \code{formatted_address} begins with a digit are updated.
#'
#' @param geocoded_df A data frame containing at least \code{formatted_address},
#'   \code{addr_clean}, \code{city}, and \code{zip} columns.
#'
#' @return The same data frame with address columns updated where possible.
#' @keywords internal
clean_geocoded_addresses_step3 <- function(geocoded_df) {
  geocoded_df %>%
    dplyr::mutate(
      # Check if formatted_address starts with a number (valid street address)
      has_numbered_address = !is.na(formatted_address) & 
        stringr::str_detect(formatted_address, "^\\d"),
      
      # Only update addr_clean, city, zip if we have a numbered address
      addr_clean = dplyr::if_else(
        has_numbered_address,
        stringr::str_trim(stringr::str_extract(formatted_address, "^[^,]+")),
        addr_clean
      ),
      
      city = dplyr::if_else(
        has_numbered_address,
        stringr::str_trim(stringr::str_match(formatted_address, "^[^,]+,\\s*([^,]+)")[,2]),
        city
      ),
      
      zip = dplyr::if_else(
        has_numbered_address,
        stringr::str_extract(formatted_address, "(\\d{5})(?=, USA)"),
        zip
      )
    ) %>%
    dplyr::select(-has_numbered_address)
}

# ------------------------------------------------------------------------------
# Process one chunk through all steps
# ------------------------------------------------------------------------------

#' Run Steps 2 and 3 geocoding on a single chunk
#'
#' Convenience wrapper that calls \code{geocode_chunk_step2} followed by
#' \code{geocode_chunk_step3} on the same data frame.
#'
#' @param chunk_df A data frame (a chunk of the unique-schools table).
#' @param google_api_key Character scalar.  Google Geocoding API key.
#'
#' @return The \code{chunk_df} with geocoded rows updated by both steps.
#' @keywords internal
process_chunk <- function(chunk_df, google_api_key) {
  # Step 2: Geocode schools with address
  chunk_df <- geocode_chunk_step2(chunk_df, google_api_key)
  
  # Step 3: Geocode schools without address
  chunk_df <- geocode_chunk_step3(chunk_df, google_api_key)
  
  return(chunk_df)
}

# ------------------------------------------------------------------------------
# MAIN ORCHESTRATION FUNCTION
# ------------------------------------------------------------------------------

#' Geocode a table of unique schools using the Google Geocoding API
#'
#' Orchestrates a three-step geocoding workflow for a table of unique schools:
#' \enumerate{
#'   \item Loads any previously saved cache and merges cached coordinates /
#'         Place IDs.
#'   \item Geocodes schools that have a street address but are missing
#'         coordinates or a Place ID.
#'   \item Geocodes schools that lack a street address entirely.
#' }
#' Results are saved to an \code{.rds} cache after each chunk so that the
#' function can be resumed after an interruption.
#'
#' @param unique_schools A data frame of unique schools.  Must contain
#'   \code{school_name} and \code{state} columns; \code{addr_clean},
#'   \code{city}, \code{zip}, and \code{county_std} are optional but improve
#'   accuracy.
#' @param geo_dir Character scalar.  Path to the directory used for cache files
#'   (\code{geocoded_cache.rds} and \code{geocoding_progress.rds}).
#' @param google_api_key Character scalar.  Google Geocoding API key (typically
#'   read from \code{Sys.getenv("GOOGLEGEO_API_KEY")}).
#'
#' @return A data frame with the same rows as \code{unique_schools} and
#'   additional columns \code{lat}, \code{lon}, \code{place_id},
#'   \code{formatted_address}, and \code{geo_source}.  The result is also
#'   written to \code{file.path(geo_dir, "geocoded_cache.rds")}.
#'
#' @export
run_full_geocoding <- function(unique_schools, geo_dir, google_api_key) {
  save_path <- file.path(geo_dir, "geocoded_cache.rds")
  progress_path <- file.path(geo_dir, "geocoding_progress.rds")
  
  # Validate input data
  validate_input_data(unique_schools)
  
  # ============================================================================
  # STEP 1: Load cache and merge
  # ============================================================================
  cat("Step 1: Loading cache and merging place_id and address data...\n")
  prepped_df <- load_and_merge_cache(unique_schools, save_path)
  
  # Count remaining work
  needs_step2_coords    <- sum( has_valid_address(prepped_df$addr_clean) & (is.na(prepped_df$lat) | is.na(prepped_df$lon)), na.rm = TRUE)
  needs_step2_place_id  <- sum( has_valid_address(prepped_df$addr_clean) & !is.na(prepped_df$lat) & !is.na(prepped_df$lon) & is.na(prepped_df$place_id), na.rm = TRUE)
  needs_step3           <- sum((is.na(prepped_df$lat) | is.na(prepped_df$lon)) & !has_valid_address(prepped_df$addr_clean), na.rm = TRUE)
  needs_step2           <- needs_step2_coords + needs_step2_place_id
  
  cat(sprintf("  - Schools needing Step 2 - have address, missing coords:   %d\n", needs_step2_coords))
  cat(sprintf("  - Schools needing Step 2 - have address+coords, no place_id: %d\n", needs_step2_place_id))
  cat(sprintf("  - Schools needing Step 3 - no address or coords:            %d\n", needs_step3))
  
  # ============================================================================
  # STEP 2 & 3: Sequential chunk processing with intermediate saves
  # ============================================================================
  if (needs_step2 > 0 || needs_step3 > 0) {
    cat("\nStep 2 & 3: Geocoding in chunks with progress saves...\n")
    
    # Split into chunks of 250
    chunks <- split(prepped_df, (seq_len(nrow(prepped_df)) - 1) %/% 250)
    n_chunks <- length(chunks)
    
    # Pre-allocate a list so each chunk result is stored by index; this avoids
    # the O(n²) memory cost of repeatedly calling bind_rows(accumulated_df, chunk).
    # A single bind_rows over all chunks is done once at the end.
    results_list <- vector("list", n_chunks)
    processed_chunks <- c()

    if (file.exists(progress_path)) {
      progress_data <- readRDS(progress_path)
      processed_chunks <- progress_data$processed_chunks
      # Backward-compatible: accept either the old "results" (single df) or the
      # new "chunk_results" (list) format saved by a previous run.
      if (!is.null(progress_data$chunk_results)) {
        results_list[processed_chunks] <- progress_data$chunk_results[processed_chunks]
      } else if (!is.null(progress_data$results)) {
        # Old progress file: the combined df of all previously processed chunks.
        # Append it as an extra list element so it is included in the final
        # bind_rows; the loop will still skip the already-processed chunk indices.
        results_list[[length(results_list) + 1L]] <- progress_data$results
      }
      cat(sprintf("Resuming from chunk %d of %d (processed: %d)\n",
                  length(processed_chunks) + 1L, n_chunks, length(processed_chunks)))
    }

    # Process remaining chunks
    for (i in seq_along(chunks)) {
      if (i %in% processed_chunks) {
        next
      }

      cat(sprintf("  Processing chunk %d of %d...", i, n_chunks))

      chunk_result <- tryCatch({
        process_chunk(chunks[[i]], google_api_key = google_api_key)
      }, error = function(e) {
        cat(sprintf(" ERROR\n"))
        cat(sprintf("    Error message: %s\n", e$message))
        cat("    Saving progress and stopping...\n")

        progress_data <- list(
          processed_chunks = processed_chunks,
          chunk_results    = results_list,
          error_chunk      = i,
          error_message    = e$message,
          timestamp        = Sys.time()
        )
        saveRDS(progress_data, progress_path)
        cat(sprintf("    Progress saved to: %s\n", progress_path))
        cat("    Run the function again to resume from this point.\n\n")

        stop(e)
      })

      results_list[[i]] <- chunk_result

      # Save intermediate progress (per-chunk list; no incremental bind_rows)
      processed_chunks <- c(processed_chunks, i)
      progress_data <- list(
        processed_chunks = processed_chunks,
        chunk_results    = results_list,
        chunks_remaining = n_chunks - i,
        timestamp        = Sys.time()
      )
      saveRDS(progress_data, progress_path)

      cat(sprintf(" Done (%d/%d chunks)\n", i, n_chunks))
    }

    # All chunks processed – combine once and clean up the progress file
    results <- dplyr::bind_rows(results_list)
    if (file.exists(progress_path)) {
      file.remove(progress_path)
      cat("Geocoding progress file cleaned up.\n")
    }
    
  } else {
    cat("\nAll schools already geocoded from cache!\n")
    results <- prepped_df
  }
  
  # ============================================================================
  # STEP 4: Save final results as cache for next run
  # ============================================================================
  cat("\nSaving final results as cache...\n")
  saveRDS(results, save_path)
  cat(sprintf("Cache saved to: %s\n", save_path))
  
  # Summary statistics
  cat("\n=== Geocoding Summary ===\n")
  cat(sprintf("Total schools:          %d\n", nrow(results)))
  cat(sprintf("Successfully geocoded:  %d\n", sum(!is.na(results$lat) & !is.na(results$lon))))
  cat(sprintf("With valid address:     %d\n", sum(has_valid_address(results$addr_clean))))
  cat(sprintf("With place_id:          %d\n", sum(!is.na(results$place_id))))
  
  if ("geo_source" %in% names(results)) {
    cat("\nBy geo_source:\n")
    source_counts <- table(results$geo_source, useNA = "ifany")
    for (i in seq_along(source_counts)) {
      cat(sprintf("  %-10s: %d\n", names(source_counts)[i], source_counts[i]))
    }
  }
  
  return(results)
}


# ------------------------------------------------------------------------------
# Get business status from Google Places API / Reasons if no status returned
# ------------------------------------------------------------------------------

#' Retrieve the business status of a school from the Google Places API
#'
#' Calls the Places Details endpoint with the given \code{place_id} and
#' returns a list with the \code{business_status} string and a
#' \code{status_reason} code explaining the outcome.
#'
#' @param place_id Character scalar.  Google Place ID.
#' @param google_api_key Character scalar.  Google Places API key.
#'
#' @return A named list with two elements:
#' \describe{
#'   \item{business_status}{Character.  One of \code{"OPERATIONAL"},
#'     \code{"CLOSED_TEMPORARILY"}, \code{"CLOSED_PERMANENTLY"}, or
#'     \code{NA_character_} when the status could not be retrieved.}
#'   \item{status_reason}{Character.  One of \code{"success"},
#'     \code{"missing_place_id"}, \code{"transient_error"}, or
#'     \code{"not_available"}.}
#' }
#' @keywords internal
get_business_status <- function(place_id, google_api_key) {
  if (is.na(place_id) || place_id == "") {
    return(list(business_status = NA_character_, status_reason = "missing_place_id"))
  }
  
  url <- paste0("https://maps.googleapis.com/maps/api/place/details/json?place_id=", 
                URLencode(place_id), "&fields=business_status&key=", google_api_key)
  
  res <- tryCatch(httr::GET(url), error = function(e) return(NULL))
  if (is.null(res) || httr::status_code(res) != 200) {
    return(list(business_status = NA_character_, status_reason = "transient_error"))
  }
  
  json <- httr::content(res, as = "parsed", simplifyVector = TRUE)
  if (!is.null(json$status) && json$status != "OK") {
    return(list(business_status = NA_character_, status_reason = "transient_error"))
  }
  
  result <- json$result
  if (is.null(result) || !"business_status" %in% names(result)) {
    return(list(business_status = NA_character_, status_reason = "not_available"))
  }
  
  list(business_status = result$business_status, status_reason = "success")
}
# ------------------------------------------------------------------------------
# Retrieve operational status (parallel)
# ------------------------------------------------------------------------------

#' Look up school operational status via Google Places, with caching
#'
#' Merges any previously saved status cache, then uses
#' \code{furrr::future_map} to call the Google Places API in parallel for all
#' schools that still need a status.  Results are saved to
#' \code{file.path(geo_dir, "status_cache.rds")} before returning.
#'
#' @param df A data frame containing at least a \code{place_id} column.
#' @param geo_dir Character scalar.  Path to the directory used for
#'   \code{status_cache.rds}.
#' @param google_api_key Character scalar.  Google Places API key (typically
#'   read from \code{Sys.getenv("GOOGLEGEO_API_KEY")}).
#'
#' @return The input data frame with additional columns \code{business_status},
#'   \code{status_reason}, and \code{business_source}.  The result is also
#'   written to \code{file.path(geo_dir, "status_cache.rds")}.
#'
#' @importFrom furrr future_map
#' @export
run_school_status_with_cache <- function(df, geo_dir, google_api_key) {
  status_path <- file.path(geo_dir, "status_cache.rds")
  df <- df %>% dplyr::mutate(orig_sort_id = dplyr::row_number())
  
  # ── Merge from cache if it exists ──────────────────────────────────────────
  if (file.exists(status_path)) {
    status_cache <- readRDS(status_path)
    
    if ("business_status" %in% names(status_cache) && 
        "place_id"        %in% names(status_cache)) {
      
      status_cache <- status_cache %>%
        dplyr::select(place_id, business_status,
                      dplyr::any_of(c("status_reason", "business_source"))) %>%
        dplyr::filter(!is.na(place_id)) %>%
        dplyr::distinct(place_id, .keep_all = TRUE)
      
      # Handle older cache files missing status_reason
      if (!"status_reason" %in% names(status_cache)) {
        status_cache$status_reason <- NA_character_
      }
      
      # Handle older cache files missing business_source
      if (!"business_source" %in% names(status_cache)) {
        status_cache$business_source <- "cache"
      } else {
        status_cache$business_source <- dplyr::coalesce(status_cache$business_source, "cache")
      }
      
      df <- df %>%
        dplyr::select(-dplyr::any_of(c("business_status", "status_reason", "business_source"))) %>%
        dplyr::left_join(status_cache, by = "place_id")
    } else {
      message("Cache file found but missing required columns — skipping cache merge.")
    }
  }
  
  # ── Ensure columns exist before any filtering ──────────────────────────────
  if (!"business_status" %in% names(df)) df$business_status <- NA_character_
  if (!"status_reason"   %in% names(df)) df$status_reason   <- NA_character_
  if (!"business_source" %in% names(df)) df$business_source <- NA_character_
  
  # ── Partition ──────────────────────────────────────────────────────────────
  already_done <- df %>% dplyr::filter(!is.na(business_status))
  no_id        <- df %>% dplyr::filter(is.na(business_status) & (is.na(place_id) | place_id == "")) %>%
    dplyr::mutate(status_reason   = "missing_place_id",
                  business_source = NA_character_)
  to_search    <- df %>% dplyr::filter(is.na(business_status) & !is.na(place_id) & place_id != "")
  
  if (nrow(to_search) > 0) {
    search_list <- furrr::future_map(
      to_search$place_id,
      ~get_business_status(.x, google_api_key),
      .progress = TRUE
    )
    results_df <- dplyr::bind_rows(lapply(search_list, tibble::as_tibble))
    to_search$business_status <- results_df$business_status
    to_search$status_reason   <- results_df$status_reason
    # Tag rows that were actually looked up — NA status means the API couldn't
    # return anything, but the attempt was still made via Google.
    to_search$business_source <- dplyr::if_else(
      results_df$status_reason == "success",
      "google",
      NA_character_
    )
  }
  
  final_df <- dplyr::bind_rows(already_done, to_search, no_id) %>%
    dplyr::arrange(orig_sort_id) %>%
    dplyr::select(-orig_sort_id)
  
  saveRDS(final_df, status_path)
  return(final_df)
}
# ------------------------------------------------------------------------------
# Re-try Retrieving operational status only for transient errors
# ------------------------------------------------------------------------------

#' Retry Google Places status lookup for rows with transient errors
#'
#' Iterates up to \code{max_attempts} times, each time re-querying the Google
#' Places API for any row whose \code{status_reason} is
#' \code{"transient_error"} and whose \code{business_status} is still
#' \code{NA}.  Stops early when no retryable rows remain.
#'
#' @param df A data frame previously processed by
#'   \code{run_school_status_with_cache}, containing \code{place_id},
#'   \code{business_status}, and \code{status_reason} columns.
#' @param google_api_key Character scalar.  Google Places API key.
#' @param max_attempts Integer scalar.  Maximum number of retry passes
#'   (default \code{5}).
#'
#' @return The \code{df} with updated \code{business_status} and
#'   \code{status_reason} values for previously-failing rows.
#' @export
retry_transient_errors <- function(df,
                                   google_api_key,
                                   max_attempts = 5) {
  
  attempt <- 1
  
  while (attempt <= max_attempts) {
    
    message("Retry attempt: ", attempt)
    
    retry_idx <- which(
      is.na(df$business_status) &
        df$status_reason == "transient_error"
    )
    
    # Stop condition
    if (length(retry_idx) == 0) {
      message("No retryable NA remaining. Stopping.")
      break
    }
    
    retry_list    <- lapply(df$place_id[retry_idx], get_business_status,
                            google_api_key = google_api_key)
    retry_results <- dplyr::bind_rows(lapply(retry_list, tibble::as_tibble))
    
    df$business_status[retry_idx] <- retry_results$business_status
    df$status_reason[retry_idx]   <- retry_results$status_reason
    
    attempt <- attempt + 1
  }
  
  df
}

# ==============================================================================
# Produce reports for manual review of schools with missing status or repeated transient errors
# ==============================================================================

#' Write a data frame to a CSV report file
#'
#' Creates \code{out_dir} if it does not exist, writes \code{df} to a CSV
#' named \code{<report_name>.csv}, and emits a message.
#'
#' @param df A data frame to write.
#' @param report_name Character scalar.  Base name for the output file
#'   (without extension).
#' @param out_dir Character scalar.  Directory path for the output file.
#'
#' @return Invisibly returns \code{NULL}.
#' @keywords internal
write_report <- function(df, report_name, out_dir) {
  
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  
  csv_path <- file.path(out_dir, paste0(report_name, ".csv"))
  
  write.csv(df, csv_path, row.names = FALSE)

  message("Saved report: ", report_name)
}

# 1. Report: Remaining missing addresses, coordinates, or status

#' Report schools with missing geocoding data or operational status
#'
#' Identifies rows that are missing a street address, coordinates, or a
#' \code{business_status} value, writes a CSV to \code{out_dir}, and returns
#' the filtered data frame for further inspection.
#'
#' @param df A data frame containing columns \code{addr_clean}, \code{lat},
#'   \code{lon}, and \code{business_status}.
#' @param out_dir Character scalar.  Directory for the output CSV
#'   (\code{missing_geocode_or_status.csv}).
#'
#' @return A data frame of rows with at least one missing geocoding or status
#'   field.
#' @export
report_missing_geo_status <- function(df, out_dir) {
  
  report <- df %>%
    dplyr::mutate(
      # Checks the cleaned columns we worked on
      missing_address = is.na(addr_clean) | addr_clean == "",
      missing_coords  = is.na(lat) | is.na(lon),
      missing_status  = is.na(business_status)
    ) %>%
    dplyr::filter(missing_address | missing_coords | missing_status) %>%
    # No select() here - keeps all variables for QC
    dplyr::arrange(missing_status, missing_coords, missing_address)
  
  write_report(
    report,
    report_name = "missing_geocode_or_status",
    out_dir = out_dir
  )
  
  return(report) # Returns to environment when assigned
}

# 2. Report schools that should have the same IDs (Proximity Check)

#' Report pairs of schools that may be duplicates based on proximity
#'
#' Computes a Haversine distance matrix for all geocoded schools and reports
#' every pair whose locations are within \code{distance_meters} of each other
#' but that have different \code{school_id} values.  Writes a CSV to
#' \code{out_dir}.
#'
#' @param df A data frame containing \code{school_id}, \code{school_name},
#'   \code{lat}, and \code{lon} columns.
#' @param out_dir Character scalar.  Directory for the output CSV
#'   (\code{potential_duplicate_schools.csv}).
#' @param distance_meters Numeric scalar.  Maximum distance in meters between
#'   two schools to flag them as potential duplicates (default \code{50}).
#'
#' @return A data frame of school pairs that are potential duplicates.
#' @importFrom geosphere distm distHaversine
#' @importFrom tidyr unpack
#' @export
report_potential_duplicate_schools <- function(df, out_dir, distance_meters = 50) {
  
  # Prepare data for distance matrix
  geo_df <- df %>%
    dplyr::filter(!is.na(lat), !is.na(lon)) %>%
    dplyr::distinct(school_id, school_name, .keep_all = TRUE)
  
  coords <- as.matrix(geo_df[, c("lon", "lat")])
  
  dist_matrix <- geosphere::distm(coords, fun = geosphere::distHaversine)
  
  # Only keep upper triangle (avoid A-B and B-A duplicates)
  idx <- which(
    dist_matrix <= distance_meters &
      dist_matrix > 0 &
      row(dist_matrix) < col(dist_matrix),
    arr.ind = TRUE
  )
  
  # Build report
  report <- tibble::tibble(
    match_1    = geo_df[idx[,1], ],
    match_2    = geo_df[idx[,2], ],
    distance_m = round(dist_matrix[idx], 1)
  ) %>%
    tidyr::unpack(cols = c(match_1, match_2), names_sep = "_") %>%
    dplyr::filter(match_1_school_id != match_2_school_id)
  
  write_report(
    report,
    report_name = "potential_duplicate_schools",
    out_dir = out_dir
  )
  
  return(report)
}

# 3. Report closed schools and last year of data available

#' Report schools identified as closed and their last year of data
#'
#' Filters to schools whose \code{business_status} is
#' \code{"closed_temporarily"} or \code{"closed_permanently"}, computes the
#' latest year each appeared in the data, writes a CSV to \code{out_dir}, and
#' returns the filtered data frame.
#'
#' @param df A data frame containing at least \code{business_status},
#'   \code{school_id}, \code{school_name}, and the column named by
#'   \code{year_col}.
#' @param out_dir Character scalar.  Directory for the output CSV
#'   (\code{closed_schools_summary.csv}).
#' @param year_col Character scalar.  Name of the column holding the numeric
#'   year (default \code{"year2"}).
#'
#' @return A data frame of closed schools with a \code{latest_year} column.
#' @export
report_closed_schools <- function(df, out_dir, year_col = "year2") {
  
  report <- df %>%
    dplyr::filter(
      business_status %in% c("closed_temporarily", "closed_permanently")
    ) %>%
    dplyr::group_by(school_id, school_name) %>%
    dplyr::mutate(
      # Correctly use the string variable for the column name
      latest_year = max(.data[[year_col]], na.rm = TRUE)
    ) %>%
    dplyr::ungroup() %>%
    # Arrange so latest closed schools are at top
    dplyr::arrange(business_status, desc(latest_year))
  
  write_report(
    report,
    report_name = "closed_schools_summary",
    out_dir = out_dir
  )
  
  return(report)
}

# 4. Report: Unique schools that have missing addresses

#' Report unique schools that are missing geocoding information
#'
#' Identifies distinct schools (by \code{school_name_std_vacc}) that are
#' missing coordinates or whose address does not start with a street number,
#' keeping only one row per \code{school_id}.  Writes a CSV to \code{out_dir}
#' and returns the filtered data frame.
#'
#' @param df A data frame containing \code{school_name_std_vacc},
#'   \code{school_id}, \code{lat}, and \code{addr_clean} columns.
#' @param out_dir Character scalar.  Directory for the output CSV
#'   (\code{unique_missing_addresses.csv}).
#'
#' @return A data frame of unique schools needing address attention.
#' @export
report_unique_missing_addresses <- function(df, out_dir) {
  
  report <- df %>%
    # Filter for schools where lat/lon is missing 
    # OR the address does not start with a number
    dplyr::filter(
      (is.na(lat) | lat == "") | 
      !grepl("^\\d", addr_clean)
    ) %>%
    # Group by Name to catch unique schools
    dplyr::distinct(school_name_std_vacc, .keep_all = TRUE) %>%
    # If there is a school_id, keep only the first instance of it
    dplyr::group_by(school_id) %>%
    dplyr::filter(is.na(school_id) | dplyr::row_number() == 1) %>%
    dplyr::ungroup() %>%
    # Sort for easier review
    dplyr::arrange(school_name_std_vacc)
  
  message(paste("Found", nrow(report), "unique schools requiring address attention."))
  
  write_report(
    report,
    report_name = "unique_missing_addresses",
    out_dir = out_dir
  )
  
  return(report)
}
# ------------------------------------------------------------------------------
# Merge geocoding results back to long file
# ------------------------------------------------------------------------------

#' Merge geocoded school data back into the long-form vaccination table
#'
#' Drops stale coordinate and address columns from \code{school_vax_long},
#' then patches in the cleaned values from \code{schools_status} in two
#' passes: first by \code{school_id}, then by \code{school_name} for rows
#' whose \code{school_id} is \code{NA}.
#'
#' @param school_vax_long A data frame of vaccination records in long format,
#'   containing at least \code{school_id} and \code{school_name} columns.
#' @param schools_status A data frame of geocoded/status results (e.g. from
#'   \code{run_school_status_with_cache}) containing \code{school_id},
#'   \code{school_name}, \code{lat}, \code{lon}, \code{addr_clean},
#'   \code{city}, \code{zip}, \code{state}, and \code{business_status}.
#'
#' @return \code{school_vax_long} with geocoding and status columns added.
#' @export
merge_geocoding_results <- function(school_vax_long, schools_status) {
  
  # 1. DROP the old/wrong coordinates from the long-form data
  # This ensures the new coordinates from schools_status actually flow in
  school_vax_long <- school_vax_long %>%
    dplyr::select(-dplyr::any_of(c("lat", "lon", "addr_clean", "city", "zip", "state")))
  
  # 2. Create the clean lookup table from schools_status
  geo_lookup <- schools_status %>%
    dplyr::select(
      school_id, school_name, lat, lon, 
      addr_clean, city, zip, state, business_status
    ) %>%
    # Ensure character types to avoid the "Logical vs Character" error
    dplyr::mutate(across(c(addr_clean, city, zip, state, business_status), as.character)) %>%
    dplyr::distinct()
  
  # 3. Preparation: Create empty target columns in school_vax_long 
  # We initialize them as NAs so rows_patch has a "hole" to fill
  target_char_cols <- c("addr_clean", "city", "zip", "state", "business_status")
  target_num_cols  <- c("lat", "lon")
  
  for(col in target_char_cols) { school_vax_long[[col]] <- NA_character_ }
  for(col in target_num_cols)  { school_vax_long[[col]] <- NA_real_ }
  
  # 4. Pass 1: Patch by school_id
  lookup_id <- geo_lookup %>% 
    dplyr::filter(!is.na(school_id)) %>% 
    dplyr::distinct(school_id, .keep_all = TRUE) %>%
    dplyr::select(-school_name)
  
  result <- school_vax_long %>%
    dplyr::rows_patch(lookup_id, by = "school_id", unmatched = "ignore")
  
  # 5. Pass 2: Patch by school_name (for rows where school_id was NA)
  lookup_name <- geo_lookup %>% 
    dplyr::filter(is.na(school_id)) %>%
    dplyr::distinct(school_name, .keep_all = TRUE) %>%
    dplyr::select(-school_id)
  
  result <- result %>%
    dplyr::rows_patch(lookup_name, by = "school_name", unmatched = "ignore")
  
  return(result)
}
