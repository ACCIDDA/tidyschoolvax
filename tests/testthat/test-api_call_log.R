# Tests for the Google API call log (R/api_call_log.R). No live API calls.

local_log_dir <- function(env = parent.frame()) {
  dir <- tempfile("api_logs_")
  withr::defer(unlink(dir, recursive = TRUE), envir = env)
  dir
}

test_prices <- function() {
  tibble::tibble(
    api            = c("geocoding", "place_details_pro"),
    google_sku     = c("Geocoding", "Place Details Pro"),
    usd_per_1000   = c(5, 17),
    free_per_month = c(10, 100)
  )
}

test_that("calls are tallied by API and caller with list-price cost", {
  start_api_log(state = "MD")
  .log_api_calls("place_details_pro", "run_school_status_with_cache", 1000, 990)
  .log_api_calls("place_details_pro", "retry_transient_errors", 10, 10)
  .log_api_calls("geocoding", "geocode_chunk_step2", 0, 0)  # ignored

  s <- api_log_summary()
  expect_equal(s$api, "place_details_pro")
  expect_equal(s$requests, 1010)
  expect_equal(s$failed, 10)
  expect_equal(s$est_cost_list_usd, 17.17)

  expect_equal(nrow(api_log_summary(by_caller = TRUE)), 2)
})

test_that("geocoding calls count unique non-blank queries", {
  start_api_log()
  geocoded <- data.frame(query = c("a", "a", "b", "", "c"),
                         lat_temp = c(1, 1, NA, NA, 2))
  .log_geocoding_calls(geocoded, "geocode_chunk_step3")

  s <- api_log_summary()
  expect_equal(s$requests, 3)
  expect_equal(s$succeeded, 2)
})

test_that("write_api_log writes a dated run file and a running history", {
  dir <- local_log_dir()

  start_api_log(state = "md", state_dir = "/x/states/md")
  .log_api_calls("place_details_pro", "run_school_status_with_cache", 80, 80)
  run1 <- suppressMessages(write_api_log(dir, prices = test_prices()))

  expect_match(basename(run1), "^google_api_calls_md_\\d{4}-\\d{2}-\\d{2}_\\d{6}\\.csv$")
  detail <- utils::read.csv(run1)
  expect_equal(detail$requests, 80)
  expect_equal(detail$state, "md")
  expect_equal(detail$status, "completed")

  # Second run crosses the 100-call free allowance: 50 of its 70 are billable.
  Sys.sleep(1)  # distinct run_id
  start_api_log(state = "ca")
  .log_api_calls("place_details_pro", "run_school_status_with_cache", 70, 70)
  suppressMessages(write_api_log(dir, status = "error", prices = test_prices()))

  history <- utils::read.csv(file.path(dir, "google_api_calls_all_runs.csv"))
  expect_equal(nrow(history), 2)
  expect_equal(history$month_to_date_requests, c(80, 150))
  expect_equal(history$est_cost_after_free_usd, c(0, 0.85))
  expect_equal(history$status, c("completed", "error"))
  expect_length(list.files(dir, "^google_api_calls_(md|ca)_"), 2)
})

test_that("a run with no API calls still writes a log", {
  dir <- local_log_dir()
  start_api_log(state = "md")
  run_file <- suppressMessages(write_api_log(dir))

  expect_equal(utils::read.csv(run_file)$requests, 0)
  expect_false(file.exists(file.path(dir, "google_api_calls_all_runs.csv")))
})

test_that("run_school_status_with_cache logs the lookups it sends", {
  geo_dir <- local_log_dir()
  dir.create(geo_dir)
  old_plan <- future::plan(future::sequential)
  withr::defer(future::plan(old_plan))
  local_mocked_bindings(get_place_details = function(place_id, google_api_key) {
    list(business_status = "OPERATIONAL", google_place_name = place_id,
         status_reason = if (place_id == "C") "transient_error" else "success")
  })

  start_api_log()
  df <- data.frame(place_id = c("A", "A", "B", "C"))
  suppressMessages(run_school_status_with_cache(df, geo_dir, "test"))
  suppressMessages(run_school_status_with_cache(df, geo_dir, "test"))  # only C retried

  s <- api_log_summary()
  expect_equal(s$requests, 4)
  expect_equal(s$failed, 2)
})
