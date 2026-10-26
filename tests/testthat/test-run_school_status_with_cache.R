# Tests for run_school_status_with_cache() (R/googleapi_utils.R). No live
# Google API calls — get_place_details() is mocked and every call recorded.

# Mock get_place_details(): `responses` maps place_id -> list of fields;
# anything not listed comes back as a transient error.
mock_place_details <- function(responses = list()) {
  calls <- character()
  fn <- function(place_id, google_api_key) {
    calls <<- c(calls, place_id)
    if (place_id %in% names(responses)) {
      utils::modifyList(
        list(business_status = NA_character_, google_place_name = NA_character_,
             status_reason = "success"),
        responses[[place_id]]
      )
    } else {
      list(business_status = NA_character_, google_place_name = NA_character_,
           status_reason = "transient_error")
    }
  }
  list(fn = fn, calls = function() calls)
}

local_status_env <- function(env = parent.frame()) {
  dir <- tempfile("status_cache_")
  dir.create(dir)
  withr::defer(unlink(dir, recursive = TRUE), envir = env)
  old_plan <- future::plan(future::sequential)
  withr::defer(future::plan(old_plan), envir = env)
  dir
}

run_status <- function(df, dir) {
  suppressMessages(run_school_status_with_cache(df, geo_dir = dir, google_api_key = "test"))
}

test_that("each unique place_id is looked up once, and not again on the next run", {
  dir  <- local_status_env()
  mock <- mock_place_details(list(
    A = list(business_status = "OPERATIONAL", google_place_name = "School A"),
    B = list(google_place_name = "123 Main St")  # success, but not a business
  ))
  local_mocked_bindings(get_place_details = mock$fn)

  df  <- data.frame(school_id = 1:4, place_id = c("A", "A", "B", NA))
  out <- run_status(df, dir)

  expect_equal(sort(mock$calls()), c("A", "B"))
  expect_equal(out$school_id, 1:4)
  expect_equal(out$business_status, c("OPERATIONAL", "OPERATIONAL", NA, NA))
  expect_equal(out$google_place_name, c("School A", "School A", "123 Main St", NA))
  expect_equal(out$status_reason, c("success", "success", "success", "missing_place_id"))

  run_status(df, dir)
  expect_equal(sort(mock$calls()), c("A", "B"))  # no new calls
})

test_that("callers with different place_id sets don't evict each other's cache", {
  dir  <- local_status_env()
  mock <- mock_place_details(list(
    A = list(business_status = "OPERATIONAL", google_place_name = "A"),
    B = list(business_status = "OPERATIONAL", google_place_name = "B"),
    C = list(business_status = "CLOSED_PERMANENTLY", google_place_name = "C")
  ))
  local_mocked_bindings(get_place_details = mock$fn)

  run_status(data.frame(place_id = c("A", "B")), dir)
  run_status(data.frame(place_id = c("B", "C")), dir)
  run_status(data.frame(place_id = c("A", "B")), dir)
  run_status(data.frame(place_id = c("B", "C")), dir)

  expect_equal(sort(mock$calls()), c("A", "B", "C"))
})

test_that("transient errors are retried and keep previously known values", {
  dir <- local_status_env()
  saveRDS(
    data.frame(place_id = c("A", "B"),
               business_status = c("OPERATIONAL", NA),
               google_place_name = c("Old A", NA),
               status_reason = c("transient_error", "not_available"),
               business_source = c("google", NA)),
    file.path(dir, "status_cache.rds")
  )
  mock <- mock_place_details()  # every call fails
  local_mocked_bindings(get_place_details = mock$fn)

  out <- run_status(data.frame(place_id = c("A", "B")), dir)

  expect_equal(mock$calls(), "A")  # "not_available" is final, not retried
  expect_equal(out$business_status, c("OPERATIONAL", NA))
  expect_equal(out$google_place_name, c("Old A", NA))
  expect_equal(out$status_reason, c("transient_error", "not_available"))
})

test_that("an old per-school cache is read, and pre-name caches are refreshed once", {
  dir <- local_status_env()
  # Old format: one row per school, repeated place_ids, no google_place_name.
  saveRDS(
    data.frame(school_id = 1:3, place_id = c("A", "A", "B"),
               business_status = c("OPERATIONAL", "OPERATIONAL", "OPERATIONAL")),
    file.path(dir, "status_cache.rds")
  )
  mock <- mock_place_details(list(
    A = list(business_status = "OPERATIONAL", google_place_name = "A"),
    B = list(business_status = "CLOSED_PERMANENTLY", google_place_name = "B")
  ))
  local_mocked_bindings(get_place_details = mock$fn)

  out <- run_status(data.frame(place_id = c("A", "B")), dir)
  expect_equal(sort(mock$calls()), c("A", "B"))
  expect_equal(out$google_place_name, c("A", "B"))

  cache <- readRDS(file.path(dir, "status_cache.rds"))
  expect_equal(sort(cache$place_id), c("A", "B"))

  run_status(data.frame(place_id = c("A", "B")), dir)
  expect_equal(sort(mock$calls()), c("A", "B"))
})
