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
  local_mocked_bindings(
    download.file = function(url, ...) {
      pss_urls <<- c(pss_urls, url)
      stop("offline")
    },
    .package = "utils"
  )
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
