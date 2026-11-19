# Tests for the Google API cost estimate and the dry-run mode behind it.
# Nothing here sends a request: in a dry run no key is used, and the live path is
# kept from running by an empty key.

local_empty_key <- function(env = parent.frame()) {
  old <- Sys.getenv("GOOGLEGEOCODE_API_KEY", unset = NA_character_)
  Sys.setenv(GOOGLEGEOCODE_API_KEY = "")
  withr::defer(if (is.na(old)) Sys.unsetenv("GOOGLEGEOCODE_API_KEY") else
    Sys.setenv(GOOGLEGEOCODE_API_KEY = old), envir = env)
}

logged <- function(api) {
  s <- api_log_summary()
  if (!api %in% s$api) 0 else s$requests[s$api == api]
}

geo_row <- function(name, addr, lat = NA_real_, lon = NA_real_, place_id = NA_character_,
                    county = "alpha") {
  data.frame(school_name = name, addr_clean = addr, city = "town", state = "xx",
             zip = "12345", county_std = county, lat = lat, lon = lon,
             place_id = place_id, formatted_address = NA_character_,
             geo_source = NA_character_, stringsAsFactors = FALSE)
}


# ---- the flag -------------------------------------------------------------------

test_that("dry-run mode is off by default and restored after use", {
  expect_false(.api_dry_run())
  .with_api_dry_run(expect_true(.api_dry_run()))
  expect_false(.api_dry_run())
  expect_error(.with_api_dry_run(stop("boom")), "boom")
  expect_false(.api_dry_run())
})


# ---- geocoding call sites ---------------------------------------------------------

test_that("a dry-run geocode counts distinct queries and sends nothing", {
  local_empty_key()
  start_api_log(state = "xx")
  chunk <- rbind(
    geo_row("School A", "1 main st"),                                   # to geocode
    geo_row("School B", "1 main st"),                                   # same address, other name
    geo_row("School C", "2 oak ave", 40, -80, "ChIJ_real"),             # complete: untouched
    geo_row("School D", "3 elm st", 40, -80, NA_character_),            # coords but no place_id
    geo_row("School A", "1 main st")                                    # identical query to row 1
  )
  out <- .with_api_dry_run(geocode_chunk_step2(chunk, google_api_key = "unused"))

  expect_equal(nrow(out), nrow(chunk))
  expect_equal(logged("geocoding"), 3)        # A, B, D: the duplicate of A is one request
  expect_equal(api_log_summary()$failed, 0)

  by <- function(n) out[out$school_name == n, ]
  expect_equal(by("School C")$place_id, "ChIJ_real")                     # existing id kept
  expect_true(all(startsWith(by("School A")$place_id, "dryrun:")))
  # records at the same address share a synthetic id, like a real Google answer
  expect_equal(by("School A")$place_id[1], by("School B")$place_id)
  expect_false(by("School A")$place_id[1] == by("School D")$place_id)
  expect_equal(by("School D")$lat, 40)                                    # coordinates untouched
  expect_true(is.na(by("School A")$lat[1]))
  expect_equal(by("School A")$geo_source[1], "dry_run")
})

test_that("outside a dry run the geocoder is still called, and its requests logged", {
  start_api_log(state = "xx")
  sent <- NULL
  testthat::local_mocked_bindings(
    geocode = function(.tbl, ...) {
      sent <<- .tbl$query
      .tbl$lat_temp <- 41; .tbl$lon_temp <- -81
      .tbl$place_id <- "ChIJ_mock"; .tbl$formatted_address <- "1 Main St, Town, XX 12345, USA"
      .tbl
    },
    .package = "tidygeocoder"
  )
  chunk <- rbind(geo_row("School A", "1 main st"), geo_row("School B", "2 oak ave"))
  out <- geocode_chunk_step2(chunk, google_api_key = "unused")

  expect_false(.api_dry_run())
  expect_equal(sent, c("School A 1 main st town xx 12345", "School B 2 oak ave town xx 12345"))
  expect_equal(logged("geocoding"), 2)
  expect_true(all(out$place_id == "ChIJ_mock"))
  expect_true(all(out$geo_source == "google"))
  expect_false("query" %in% names(out))
})

test_that("a dry-run geocode of rows with no street address counts by name and county", {
  local_empty_key()
  start_api_log(state = "xx")
  chunk <- rbind(geo_row("School A", NA, county = "alpha"),
                 geo_row("School A", NA, county = "beta"),    # same name, other county
                 geo_row("School B", NA, county = "alpha"))
  out <- .with_api_dry_run(geocode_chunk_step3(chunk, google_api_key = "unused"))
  expect_equal(logged("geocoding"), 3)
  expect_equal(length(unique(out$place_id)), 3L)
})

test_that("nothing to geocode logs nothing", {
  local_empty_key()
  start_api_log(state = "xx")
  chunk <- geo_row("Done", "1 main st", 40, -80, "ChIJ_done")
  out <- .with_api_dry_run(process_chunk(chunk, "unused"))
  expect_equal(nrow(out), 1L)
  expect_equal(logged("geocoding"), 0)
})

test_that("run_full_geocoding() in a dry run skips cached schools and writes no cache", {
  local_empty_key()
  start_api_log(state = "xx")
  geo_dir <- tempfile("geo_"); dir.create(geo_dir)
  withr::defer(unlink(geo_dir, recursive = TRUE))
  cache <- tibble::tibble(
    school_name = c("School A", "School B"), county_std = "alpha",
    place_id = c("ChIJ_a", "ChIJ_b"), lat = 40, lon = -80,
    addr_clean = c("1 main st", "2 oak ave"), city = "town", zip = "12345",
    state = "xx", formatted_address = "x", geo_source = "google"
  )
  cache_path <- file.path(geo_dir, "geocoded_cache.rds")
  saveRDS(cache, cache_path)
  before <- tools::md5sum(cache_path)

  schools <- rbind(
    geo_row("School A", "1 main st"),   # cached
    geo_row("School B", "2 oak ave"),   # cached
    geo_row("School C", "3 elm st"),    # new
    geo_row("School D", "4 pine rd"),   # new
    geo_row("School E", NA)             # new, no address
  )
  schools <- dplyr::select(schools, -place_id, -formatted_address, -geo_source, -lat, -lon)
  out <- suppressMessages(utils::capture.output(res <- .with_api_dry_run(
    run_full_geocoding(schools, geo_dir, "unused", parallel_cache = FALSE)
  )))

  expect_equal(logged("geocoding"), 3)                       # C, D, E only
  expect_identical(unname(tools::md5sum(cache_path)), unname(before))  # cache untouched
  expect_false(file.exists(file.path(geo_dir, "geocoding_progress.rds")))
  expect_equal(nrow(res), 5L)
  expect_equal(res$place_id[res$school_name == "School A"], "ChIJ_a")    # served from cache
})


# ---- Place status call site ---------------------------------------------------------

test_that("a dry-run status lookup counts only place_ids not already cached", {
  local_empty_key()
  start_api_log(state = "xx")
  geo_dir <- tempfile("geo_"); dir.create(geo_dir)
  withr::defer(unlink(geo_dir, recursive = TRUE))
  status_path <- file.path(geo_dir, "status_cache.rds")
  saveRDS(tibble::tibble(place_id = "p1", business_status = "OPERATIONAL",
                         google_place_name = "School 1", status_reason = "success",
                         business_source = "google"), status_path)
  before <- tools::md5sum(status_path)

  df <- data.frame(place_id = c("p1", "p2", "p3", "p2", NA), school_name = letters[1:5],
                   stringsAsFactors = FALSE)
  res <- suppressMessages(.with_api_dry_run(
    run_school_status_with_cache(df, geo_dir, "unused")
  ))

  expect_equal(logged("place_details_pro"), 2)               # p2 and p3, each once
  expect_equal(nrow(res), 5L)
  expect_equal(res$business_status[res$place_id %in% "p1"], "OPERATIONAL")   # from cache
  expect_true(all(is.na(res$business_status[res$place_id %in% c("p2", "p3")])))
  expect_identical(unname(tools::md5sum(status_path)), unname(before))
})


# ---- the estimate ----------------------------------------------------------------------

test_prices <- function() {
  tibble::tibble(api = c("geocoding", "place_details_pro"),
                 google_sku = c("Geocoding API", "Places API - Place Details Pro"),
                 usd_per_1000 = c(5, 17), free_per_month = c(10000, 5000))
}

test_that("the estimate scales the dry-run counts and prices them", {
  tally <- tibble::tibble(api = c("geocoding", "place_details_pro"), requests = c(4000, 2000))
  prior <- tibble::tibble(api = character(), prior_requests = numeric())
  e <- .cost_estimate(tally, test_prices(), prior, geocoding_factor = 0.8, places_factor = 0.5)

  g <- e[e$api == "geocoding", ]; p <- e[e$api == "place_details_pro", ]
  # the dry-run count is the ceiling; expected scales it
  expect_equal(c(g$requests_dry_run, g$requests_expected, g$requests_ceiling), c(4000, 3200, 4000))
  expect_equal(c(p$requests_dry_run, p$requests_expected, p$requests_ceiling), c(2000, 1000, 2000))
  expect_equal(g$usd_list_expected, 16); expect_equal(g$usd_list_ceiling, 20)
  expect_equal(p$usd_list_expected, 17); expect_equal(p$usd_list_ceiling, 34)
  # everything is inside the free allowance with nothing used yet
  expect_equal(c(g$usd_after_free_expected, p$usd_after_free_ceiling), c(0, 0))
})

test_that("the free allowance already used this month is taken into account", {
  tally <- tibble::tibble(api = "geocoding", requests = 1000)
  prior <- tibble::tibble(api = "geocoding", prior_requests = 9500)
  e <- .cost_estimate(tally, test_prices(), prior, geocoding_factor = 1, places_factor = 0.5)
  g <- e[e$api == "geocoding", ]
  expect_equal(g$prior_requests_this_month, 9500)
  # 500 of the 1,000 fit in the remaining allowance, 500 are billed at $5 per 1,000
  expect_equal(g$usd_after_free_expected, 2.5)
  # an API with no calls is still listed, at zero
  expect_equal(e$requests_expected[e$api == "place_details_pro"], 0)
})

test_that("last month's requests do not count against this month's allowance", {
  h <- data.frame(started_at = c("2026-10-30 10:00:00 EDT", "2026-11-03 10:00:00 EST"),
                  api = "geocoding", requests = c(9000, 100), stringsAsFactors = FALSE)
  p <- .prior_month_requests(h, "2026-11")
  expect_equal(p$prior_requests, 100)
  expect_equal(nrow(.prior_month_requests(NULL, "2026-11")), 0L)
})

test_that("estimate_api_cost() rejects settings it controls and bad rates", {
  args <- list(state_id = "xx", kinder_dir = "k", greatschools_dir = "g", doe_dir = "d",
               state_school_dir = "s", state_geo_dir = "geo", state_dir = "st")
  expect_error(do.call(estimate_api_cost, c(args, list(parallel = TRUE))), "sets parallel")
  expect_error(do.call(estimate_api_cost, c(args, list(geocoding_factor = -1))), "between 0 and 1")
  expect_error(do.call(estimate_api_cost, c(args, list(places_factor = 2))), "between 0 and 1")
})

test_that("estimate_api_cost() runs the pipeline in a sandbox, in dry-run mode, and cleans up", {
  geo <- tempfile("real_geo_"); dir.create(file.path(geo, "nces"), recursive = TRUE)
  withr::defer(unlink(geo, recursive = TRUE))
  saveRDS(1, file.path(geo, "geocoded_cache.rds"))
  raw <- tempfile("real_raw_"); dir.create(file.path(raw, "nces"), recursive = TRUE)
  withr::defer(unlink(raw, recursive = TRUE))
  saveRDS(2, file.path(raw, "nces", "nces_ccd_directory_xx.rds"))
  real_before <- tools::md5sum(list.files(geo, full.names = TRUE, recursive = TRUE))
  key_before <- Sys.getenv("GOOGLEGEO_API_KEY", unset = NA_character_)
  Sys.setenv(GOOGLEGEO_API_KEY = "my-real-key"); withr::defer(
    if (is.na(key_before)) Sys.unsetenv("GOOGLEGEO_API_KEY") else Sys.setenv(GOOGLEGEO_API_KEY = key_before))

  seen <- list()
  testthat::local_mocked_bindings(
    standardize_schools = function(...) {
      a <- list(...)
      seen <<- list(args = a, dry = .api_dry_run(), key = Sys.getenv("GOOGLEGEO_API_KEY"),
                    cache_copied = file.exists(file.path(a$state_geo_dir, "geocoded_cache.rds")),
                    nces_copied = file.exists(file.path(a$nces_cache_dir, "nces_ccd_directory_xx.rds")))
      # a real run would write caches here; they must land in the sandbox copy
      saveRDS(99, file.path(a$state_geo_dir, "geocoded_cache.rds"))
      start_api_log(state = a$state_id)
      .log_api_calls("geocoding", "geocode_chunk_step2", 1000, 1000)
      .log_api_calls("place_details_pro", "run_school_status_with_cache", 400, 400)
      invisible(NULL)
    }
  )
  est <- suppressMessages(estimate_api_cost(
    state_id = "xx", kinder_dir = file.path(raw, "kindergarten_immunity"), greatschools_dir = "g",
    doe_dir = "d", state_school_dir = "s", state_geo_dir = geo,
    state_dir = file.path(tempdir(), "a", "b", "xx"),
    api_log_dir = NULL
  ))

  # what the pipeline was told
  expect_true(seen$dry)
  expect_false(identical(seen$key, "my-real-key"))
  expect_false(seen$args$parallel); expect_false(seen$args$parallel_cache)
  expect_null(seen$args$api_log_dir)
  expect_false(startsWith(seen$args$state_geo_dir, geo))         # a copy, not the real folder
  expect_false(startsWith(seen$args$temp_data_dir, geo))
  expect_true(seen$cache_copied && seen$nces_copied)
  # what the caller sees afterwards
  expect_false(.api_dry_run())
  expect_equal(Sys.getenv("GOOGLEGEO_API_KEY"), "my-real-key")
  expect_identical(tools::md5sum(list.files(geo, full.names = TRUE, recursive = TRUE)), real_before)
  expect_false(startsWith(seen$args$nces_cache_dir, raw))         # a copy of the NCES folder too
  expect_false(dir.exists(dirname(seen$args$state_geo_dir)))     # sandbox removed
  # the estimate itself
  expect_equal(est$requests_dry_run, c(1000, 400))
  expect_equal(est$requests_ceiling, c(1000, 400))
  expect_equal(est$requests_expected, c(860, 184))    # default factors 0.86 and 0.46
})

test_that("use_cache = FALSE leaves the existing caches out of the sandbox", {
  geo <- tempfile("real_geo_"); dir.create(geo)
  withr::defer(unlink(geo, recursive = TRUE))
  raw <- tempfile("real_raw_"); dir.create(file.path(raw, "nces"), recursive = TRUE)
  withr::defer(unlink(raw, recursive = TRUE))
  saveRDS(1, file.path(geo, "geocoded_cache.rds")); saveRDS(2, file.path(raw, "nces", "n.rds"))
  seen <- NULL
  testthat::local_mocked_bindings(standardize_schools = function(...) {
    a <- list(...)
    seen <<- list(cache = file.exists(file.path(a$state_geo_dir, "geocoded_cache.rds")),
                  nces = file.exists(file.path(a$nces_cache_dir, "n.rds")))
    start_api_log(state = "xx"); invisible(NULL)
  })
  e <- suppressMessages(estimate_api_cost("xx", file.path(raw, "kindergarten_immunity"),
                                          "g", "d", "s", geo, "st",
                                          use_cache = FALSE, api_log_dir = NULL))
  expect_false(seen$cache); expect_true(seen$nces)     # NCES is public data, kept
  expect_equal(sum(e$requests_expected), 0)
})
