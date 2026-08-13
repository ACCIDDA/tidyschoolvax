# Tests for clear_geocoding_cache() (R/googleapi_utils.R). No live Google API
# calls — these only exercise file presence/removal and the safety gates.

make_cache_dir <- function() {
  dir <- tempfile("geo_cache_")
  dir.create(dir)
  saveRDS(data.frame(place_id = "ChIJ1"), file.path(dir, "geocoded_cache.rds"))
  saveRDS(data.frame(place_id = "ChIJ1", business_status = "OPERATIONAL"),
          file.path(dir, "status_cache.rds"))
  saveRDS(list(processed_chunks = 1:3), file.path(dir, "geocoding_progress.rds"))
  dir
}

test_that("default what = 'status' clears only status_cache.rds", {
  dir <- make_cache_dir()
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  res <- suppressMessages(clear_geocoding_cache(dir))

  expect_false(file.exists(file.path(dir, "status_cache.rds")))
  expect_true(file.exists(file.path(dir, "geocoded_cache.rds")))
  expect_true(file.exists(file.path(dir, "geocoding_progress.rds")))
  expect_equal(basename(res$removed), "status_cache.rds")
})

test_that("clearing 'geocoded' without force = TRUE errors and removes nothing", {
  dir <- make_cache_dir()
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  expect_error(
    clear_geocoding_cache(dir, what = "geocoded"),
    "force"
  )
  expect_true(file.exists(file.path(dir, "geocoded_cache.rds")))
})

test_that("clearing 'geocoded' with force = TRUE removes it", {
  dir <- make_cache_dir()
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  res <- suppressMessages(clear_geocoding_cache(dir, what = "geocoded", force = TRUE))

  expect_false(file.exists(file.path(dir, "geocoded_cache.rds")))
  expect_equal(basename(res$removed), "geocoded_cache.rds")
})

test_that("what = 'all' with force = TRUE clears every cache file", {
  dir <- make_cache_dir()
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  res <- suppressMessages(clear_geocoding_cache(dir, what = "all", force = TRUE))

  expect_length(list.files(dir), 0L)
  expect_length(res$removed, 3L)
  expect_length(res$skipped, 0L)
})

test_that("clearing 'progress' reports how many chunks would be discarded", {
  dir <- make_cache_dir()
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  expect_message(
    clear_geocoding_cache(dir, what = "progress"),
    "3 chunk"
  )
  expect_false(file.exists(file.path(dir, "geocoding_progress.rds")))
})

test_that("already-missing cache files are reported as skipped, not an error", {
  dir <- tempfile("geo_cache_empty_")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  res <- suppressMessages(clear_geocoding_cache(dir, what = "status"))

  expect_length(res$removed, 0L)
  expect_equal(basename(res$skipped), "status_cache.rds")
})

test_that("invalid what values are rejected", {
  dir <- make_cache_dir()
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  expect_error(clear_geocoding_cache(dir, what = "bogus"))
})
