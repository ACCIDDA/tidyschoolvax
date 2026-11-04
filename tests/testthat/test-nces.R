# Tests for NCES-based school status (R/nces_utils.R) and the pre-K-only
# reference filter. No network calls.

nces_fixture <- function() {
  tibble::tibble(
    nces_id         = c("A1", "B2", "C3", "P1"),
    nces_source     = c("nces_ccd", "nces_ccd", "nces_ccd", "nces_pss"),
    nces_name       = c("Oak Elementary", "Old Oak Elementary", "Pine Elementary", "St Mary School"),
    nces_names      = c("Oak Elementary", "Old Oak Elementary", "Pine Elementary", "St Mary School"),
    lat             = c(39.0000, 39.0005, 39.1000, 39.2000),
    lon             = c(-76.0000, -76.0005, -76.1000, -76.2000),
    last_seen       = c(2024L, 2017L, 2024L, 2024L),
    business_status = c("OPERATIONAL", "CLOSED_PERMANENTLY", "OPERATIONAL", "OPERATIONAL")
  )
}

test_that("CCD status codes map to Google-style statuses", {
  ccd <- tibble::tibble(
    year          = c(2023, 2024, 2023, 2024, 2024, 2024, 2024),
    ncessch       = c("1",  "1",  "2",  "3",  "4",  "5",  "6"),
    school_name   = c("Old Name", "New Name", "Gone", "Closed", "Inactive", "Future", "New"),
    school_status = c(1, 1, 1, 2, 6, 7, 3),
    latitude = 39, longitude = -76
  )
  s <- .summarise_ccd(ccd)
  st <- stats::setNames(s$business_status, s$nces_id)
  expect_equal(unname(st[c("1", "2", "3", "4", "6")]),
               c("OPERATIONAL", "CLOSED_PERMANENTLY", "CLOSED_PERMANENTLY",
                 "CLOSED_TEMPORARILY", "OPERATIONAL"))
  expect_true(is.na(st[["5"]]))
  expect_equal(s$nces_name[s$nces_id == "1"], "New Name")
  expect_equal(s$nces_names[s$nces_id == "1"], "New Name | Old Name")
})

test_that("PSS only confirms schools in the latest edition", {
  pss <- tibble::tibble(ppin = c("X", "Y"), school_name = c("X Academy", "Y School"),
                        latitude = 39, longitude = -76, year = c(2024L, 2022L))
  s <- .summarise_pss(pss)
  expect_equal(s$business_status, c("OPERATIONAL", NA))
  expect_null(.summarise_pss(NULL))
})

test_that("a failed PSS download stops instead of skipping to an older edition", {
  tried <- 0
  local_mocked_bindings(.download_pss_zip = function(url, dest) {
    tried <<- tried + 1
    stop("timeout")
  })
  expect_error(.download_pss_directory("MD", first_year = 2014), "timeout")
  expect_equal(tried, 1)
})

test_that(".download_pss_zip separates a missing edition from a failure", {
  local_mocked_bindings(
    GET = function(url, ...) structure(list(status_code = 503L), class = "response"),
    .package = "httr"
  )
  expect_error(.download_pss_zip("http://x", tempfile()), "HTTP 503")
  local_mocked_bindings(
    GET = function(url, ...) structure(list(status_code = 404L), class = "response"),
    .package = "httr"
  )
  expect_false(.download_pss_zip("http://x", tempfile()))
})

test_that("a failed PSS download doesn't replace the cache or get cached", {
  ccd <- tibble::tibble(year = 2024, ncessch = "1", school_name = "A", school_status = 1,
                        latitude = 39, longitude = -76)
  local_mocked_bindings(
    .download_ccd_directory = function(...) ccd,
    .download_pss_directory = function(...) stop("timeout")
  )
  dir <- withr::local_tempdir()

  # No cache: public schools for this run, nothing saved so the next run retries.
  expect_warning(out <- load_nces_reference("md", dir), "public schools only")
  expect_equal(out$nces_id, "1")
  expect_false(file.exists(file.path(dir, "nces_reference_md.rds")))

  # Stale cache: kept as it was, still stale.
  ref <- nces_fixture()
  old <- Sys.time() - 365 * 86400
  attr(ref, "downloaded_at") <- old
  attr(ref, "first_year") <- 2015
  saveRDS(ref, file.path(dir, "nces_reference_md.rds"))
  expect_warning(out <- load_nces_reference("md", dir), "using cached copy")
  expect_equal(nrow(out), 4)
  expect_equal(attr(readRDS(file.path(dir, "nces_reference_md.rds")), "downloaded_at"), old)
})

test_that("a year without coordinates doesn't change a school's status", {
  ccd <- tibble::tibble(year = c(2023, 2024, 2024), ncessch = c("1", "1", "2"),
                        school_name = c("A", "A", "No Coordinates"), school_status = 1,
                        latitude = c(39, NA, NA), longitude = c(-76, NA, NA))
  s <- .summarise_ccd(ccd)
  expect_equal(s$nces_id, "1")  # a school never located can't be matched
  expect_equal(s$business_status, "OPERATIONAL")
  expect_equal(s$last_seen, 2024L)
  expect_equal(s$lat, 39)

  pss <- tibble::tibble(ppin = "X", school_name = "X Academy", year = c(2022L, 2024L),
                        latitude = c(39, NA), longitude = c(-76, NA))
  p <- .summarise_pss(pss)
  expect_equal(p$business_status, "OPERATIONAL")
  expect_equal(p$lat, 39)
})

test_that("PSS files parse whatever the edition's column names", {
  upper <- tibble::tibble(PPIN = c("X", "Y"), PINST = c("X Academy", "Y School"),
                          PSTABB = c("MD", "VA"), LATITUDE24 = "39", LONGITUDE24 = "-76")
  lower <- tibble::tibble(ppin = c("X", "Y"), pinst = c("X Academy", "Y School"),
                          pstabb = c("MD", "VA"), latitude16 = "39", longitude16 = "-76")
  for (x in list(upper, lower)) {
    out <- .parse_pss_edition(x, "MD", 2023)
    expect_equal(out$ppin, "X")
    expect_equal(out$latitude, 39)
    expect_equal(out$year, 2024L)
  }
})

test_that("places match NCES schools by distance and name, preferring open ones", {
  places <- tibble::tibble(
    place_id = c("g1", "g1", "g2", "g3", "g4"),
    lat      = c(39.0001, 39.0001, 39.1000, 39.1000, 39.2000),
    lon      = c(-76.0001, -76.0001, -76.1000, -76.1000, -76.2000),
    name     = c("oak elem school", "Oak Elementary School",  # g1: two candidate names
                 "Totally Different Academy",                  # g2: name too different
                 "Pine Elementary",                            # g3: exact
                 "St. Mary's School")                          # g4: private
  )
  m <- match_places_to_nces(places, nces_fixture())
  expect_setequal(m$place_id, c("g1", "g3", "g4"))
  # g1 is near both Oak (open) and Old Oak (closed): the open school wins.
  expect_equal(m$nces_id[m$place_id == "g1"], "A1")
  expect_equal(m$nces_source[m$place_id == "g4"], "nces_pss")

  far <- dplyr::mutate(places, lat = lat + 0.01)  # ~1.1 km away
  expect_equal(nrow(match_places_to_nces(far, nces_fixture())), 0)
  expect_equal(nrow(match_places_to_nces(places, NULL)), 0)
})

test_that("a place shared by different schools doesn't match one of them", {
  # A county-level Google result: right beside one school, but carrying several.
  county <- tibble::tibble(place_id = "county", lat = 39.1, lon = -76.1,
                           name = c("Pine Elementary", "Unrelated Academy", "Another Christian School"))
  expect_equal(nrow(match_places_to_nces(county, nces_fixture())), 0)

  half_matching <- county[1:2, ]
  expect_equal(nrow(match_places_to_nces(half_matching, nces_fixture())), 0)

  # Variants of one school's name still match, as does a place with one odd name out.
  variants <- tibble::tibble(place_id = "one", lat = 39.1, lon = -76.1,
                             name = c("Pine Elementary", "Pine Elementary School", "Pine Elem"))
  expect_equal(match_places_to_nces(variants, nces_fixture())$nces_id, "C3")
  one_odd <- tibble::tibble(place_id = "odd", lat = 39.1, lon = -76.1,
                            name = c("Pine Elementary", "Pine Elem Sch", "123 Main Street"))
  expect_equal(match_places_to_nces(one_odd, nces_fixture())$nces_id, "C3")
  expect_equal(nrow(match_places_to_nces(one_odd, nces_fixture(), min_name_share = 0.9)), 0)
})

test_that("schools sharing a generic place go to Google rather than NCES", {
  geo_dir <- withr::local_tempdir()
  google_calls <- character()
  local_mocked_bindings(get_place_details = function(place_id, google_api_key) {
    google_calls <<- c(google_calls, place_id)
    list(business_status = NA_character_, google_place_name = "Some County",
         status_reason = "success")
  })
  df <- tibble::tibble(place_id = "county", lat = 39.1, lon = -76.1,
                       school_name = c("Pine Elementary", "Unrelated Academy", "Another Christian School"))
  out <- suppressMessages(run_school_status_with_cache(df, geo_dir, "key", nces_ref = nces_fixture(),
                                                       places_qps = 1000))
  expect_equal(google_calls, "county")
  expect_true(all(is.na(out$nces_id)))
  expect_true(all(out$google_place_name != "Pine Elementary"))
})

test_that("a former name matches but the current name is returned", {
  ref <- tibble::tibble(
    nces_id = c("A1", "B2"), nces_source = "nces_ccd",
    nces_name = c("Belair-Edison School", "Pine Elementary"),
    nces_names = c("Belair-Edison School | Brehms Lane Elementary", NA),
    lat = c(39, 39.1), lon = c(-76, -76.1), last_seen = 2024L, business_status = "OPERATIONAL"
  )
  places <- tibble::tibble(place_id = c("g1", "g2", "g3"), lat = c(39, 39, 39.1),
                           lon = c(-76, -76, -76.1),
                           name = c("Brehms Lane Elementary", "Unrelated Academy",
                                    "Pine Elementary"))
  m <- match_places_to_nces(places, ref)
  # g3's school has no name history: its current name is still used.
  expect_setequal(m$place_id, c("g1", "g3"))
  expect_equal(m$nces_id[m$place_id == "g1"], "A1")
  expect_equal(m$nces_name[m$place_id == "g1"], "Belair-Edison School")
  expect_equal(m$name_sim[m$place_id == "g1"], 1)
  expect_equal(anyDuplicated(m$place_id), 0)
})

test_that("run_school_status_with_cache takes NCES matches without calling Google", {
  geo_dir <- withr::local_tempdir()
  google_calls <- character()
  local_mocked_bindings(get_place_details = function(place_id, google_api_key) {
    google_calls <<- c(google_calls, place_id)
    list(business_status = "OPERATIONAL", google_place_name = "Google Name",
         status_reason = "success")
  })
  old_plan <- future::plan(future::sequential)
  withr::defer(future::plan(old_plan))

  df <- tibble::tibble(place_id = c("g1", "g3", "gX"),
                       lat = c(39.0001, 39.1, 38), lon = c(-76.0001, -76.1, -75),
                       school_name = c("Oak Elementary", "Pine Elementary", "Unknown Daycare"))
  out <- suppressMessages(run_school_status_with_cache(df, geo_dir, "key", nces_ref = nces_fixture()))

  expect_equal(google_calls, "gX")
  expect_equal(out$status_reason, c("nces", "nces", "success"))
  expect_equal(out$business_source, c("nces_ccd", "nces_ccd", "google"))
  expect_equal(out$nces_id, c("A1", "C3", NA))
  expect_equal(out$google_place_name[1], "Oak Elementary")

  # NCES answers are not written to the Google status cache.
  expect_equal(readRDS(file.path(geo_dir, "status_cache.rds"))$place_id, "gX")

  # Without NCES, every place goes to Google (cached gX is reused).
  google_calls <- character()
  out2 <- suppressMessages(run_school_status_with_cache(df, geo_dir, "key"))
  expect_setequal(google_calls, c("g1", "g3"))
  expect_true(all(is.na(out2$nces_id)))
})

test_that("NCES answers from an earlier call don't carry over", {
  geo_dir <- withr::local_tempdir()
  google_calls <- character()
  local_mocked_bindings(get_place_details = function(place_id, google_api_key) {
    google_calls <<- c(google_calls, place_id)
    list(business_status = "OPERATIONAL", google_place_name = "Google Name",
         status_reason = "success")
  })
  old_plan <- future::plan(future::sequential)
  withr::defer(future::plan(old_plan))

  df <- tibble::tibble(place_id = "g1", lat = 39.0001, lon = -76.0001,
                       school_name = "Oak Elementary")
  first <- suppressMessages(run_school_status_with_cache(df, geo_dir, "key", nces_ref = nces_fixture()))
  expect_equal(first$status_reason, "nces")
  expect_length(google_calls, 0)

  again <- suppressMessages(run_school_status_with_cache(first, geo_dir, "key"))
  expect_equal(google_calls, "g1")
  expect_equal(again$status_reason, "success")
  expect_equal(again$google_place_name, "Google Name")
  expect_true(is.na(again$nces_id))
})

test_that("add_google_name_variants uses its rows' coordinates and names for NCES", {
  geo_dir <- withr::local_tempdir()  # empty: no geocoding cache to fall back on
  local_mocked_bindings(get_place_details = function(place_id, google_api_key) {
    stop("should not call Google")
  })
  df <- tibble::tibble(place_id = c("g1", "g1", NA), lat = c(39.0001, 39.0001, NA),
                       lon = c(-76.0001, -76.0001, NA),
                       school_name = c("Oak Elementary", "Oak Elementary School", "No Place"))
  out <- suppressMessages(add_google_name_variants(df, geo_dir, "key", nces_ref = nces_fixture()))
  expect_equal(nrow(out), 3)
  expect_equal(out$school_name_std_google[1:2],
               rep(standardized_school_name("Oak Elementary"), 2))
  expect_true(is.na(out$school_name_std_google[3]))
})

test_that("load_nces_reference uses a fresh cache and falls back to a stale one", {
  dir <- withr::local_tempdir()
  ref <- nces_fixture()
  attr(ref, "downloaded_at") <- Sys.time()
  attr(ref, "first_year") <- 2015
  saveRDS(ref, file.path(dir, "nces_reference_md.rds"))

  local_mocked_bindings(.download_ccd_directory = function(...) stop("offline"))
  expect_equal(nrow(load_nces_reference("md", dir)), 4)

  attr(ref, "downloaded_at") <- Sys.time() - 365 * 86400
  saveRDS(ref, file.path(dir, "nces_reference_md.rds"))
  expect_warning(out <- load_nces_reference("md", dir), "using cached copy")
  expect_equal(nrow(out), 4)

  expect_warning(none <- load_nces_reference("va", dir), "Google only")
  expect_null(none)
})

test_that("first NCES year is the year before the first vaccination year", {
  expect_equal(.nces_first_year(c("2016-2017", "2015-2016", NA)), 2014L)
  expect_equal(.nces_first_year(c("ca_2014-2015", "ca_2013-2014")), 2012L)
  expect_equal(.nces_first_year(c("md_16_17", "md_15_16")), 2014L)
  expect_warning(out <- .nces_first_year(c("unknown", NA)), "Could not read a year")
  expect_equal(out, 2015L)
})

test_that("a fresh cache is re-downloaded when it doesn't reach back far enough", {
  dir <- withr::local_tempdir()
  ref <- nces_fixture()
  attr(ref, "downloaded_at") <- Sys.time()
  attr(ref, "first_year") <- 2015
  saveRDS(ref, file.path(dir, "nces_reference_md.rds"))

  requested <- NULL
  local_mocked_bindings(
    .download_ccd_directory = function(state_abbr, first_year) {
      requested <<- first_year
      stop("offline")
    }
  )
  expect_equal(nrow(load_nces_reference("md", dir, first_year = 2016)), 4)
  expect_null(requested)  # the cache starts earlier than needed, so it is used

  expect_warning(load_nces_reference("md", dir, first_year = 2014), "using cached copy")
  expect_equal(requested, 2014)

  # A cache from before first_year was recorded is not trusted to cover it.
  attr(ref, "first_year") <- NULL
  saveRDS(ref, file.path(dir, "nces_reference_md.rds"))
  expect_warning(load_nces_reference("md", dir, first_year = 2016), "using cached copy")
})

test_that("CCD and PSS downloads go back to first_year", {
  ccd_years <- integer()
  local_mocked_bindings(.fetch_ccd_year = function(year, fips) {
    ccd_years <<- c(ccd_years, year)
    tibble::tibble(year = year, ncessch = "1", school_name = "A", school_status = 1,
                   latitude = 39, longitude = -76)
  })
  .download_ccd_directory("MD", first_year = 2014)
  expect_equal(min(ccd_years), 2014)

  pss_urls <- character()
  local_mocked_bindings(.download_pss_zip = function(url, dest) {
    pss_urls <<- c(pss_urls, url)
    FALSE
  })
  expect_error(.download_pss_directory("MD", first_year = 2014), "no PSS editions")
  expect_equal(sub(".*/(pss[0-9]+)_.*", "\\1", pss_urls[length(pss_urls)]), "pss1314")

  pss_urls <- character()
  expect_error(.download_pss_directory("MD", first_year = 2015), "no PSS editions")
  expect_equal(sub(".*/(pss[0-9]+)_.*", "\\1", pss_urls[length(pss_urls)]), "pss1516")
})

test_that("pre-K-only reference schools are dropped, K-including ones kept", {
  df <- data.frame(school_name = c("Tots Daycare", "Oak PK-5", "Pine", "Unknown"),
                   level_code  = c("p", "p,e", "e", NA))
  expect_message(out <- .drop_prek_only(df, "GreatSchools"), "dropped 1 pre-K-only")
  expect_equal(out$school_name, c("Oak PK-5", "Pine", "Unknown"))
})
