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

  expect_match(basename(run1), "^google_api_calls_md_\\d{4}-\\d{2}-\\d{2}_\\d{6}_\\d+_api_run_.+\\.csv$")
  detail <- utils::read.csv(run1)
  expect_equal(detail$requests, 80)
  expect_equal(detail$state, "md")
  expect_equal(detail$status, "completed")

  # A same-state run started immediately has a distinct file and ID.
  run1_id <- .api_log$run$run_id
  start_api_log(state = "md")
  expect_false(identical(.api_log$run$run_id, run1_id))
  .log_api_calls("place_details_pro", "run_school_status_with_cache", 70, 70)
  run2 <- suppressMessages(write_api_log(dir, status = "error", prices = test_prices()))
  expect_false(identical(run1, run2))
  expect_equal(utils::read.csv(run1)$requests, 80)
  expect_equal(utils::read.csv(run2)$requests, 70)

  history <- utils::read.csv(file.path(dir, "google_api_calls_all_runs.csv"))
  expect_equal(nrow(history), 2)
  expect_equal(history$month_to_date_requests, c(80, 150))
  expect_equal(history$est_cost_after_free_usd, c(0, 0.85))
  expect_equal(history$status, c("completed", "error"))
  expect_length(list.files(dir, "^google_api_calls_md_"), 2)
})

test_that("concurrent runs serialize month-to-date history updates", {
  skip_on_os("windows")
  dir <- local_log_dir()

  parallel::mclapply(c("md", "ca"), function(state) {
    start_api_log(state = state)
    .log_api_calls("place_details_pro", "run_school_status_with_cache", 80, 80)
    suppressMessages(write_api_log(dir, prices = test_prices()))
  }, mc.cores = 2)

  history <- utils::read.csv(file.path(dir, "google_api_calls_all_runs.csv"))
  expect_equal(nrow(history), 2)
  expect_equal(sum(history$requests), 160)
  expect_equal(max(history$month_to_date_requests), 160)
  expect_equal(sum(history$est_cost_after_free_usd), 1.02)
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

test_that("response parsing failures are tallied by lookup and retry callers", {
  geo_dir <- local_log_dir()
  dir.create(geo_dir)
  old_plan <- future::plan(future::sequential)
  withr::defer(future::plan(old_plan))
  local_mocked_bindings(
    GET = function(...) list(),
    status_code = function(res) 200L,
    content = function(...) stop("invalid response"),
    .package = "httr"
  )

  start_api_log()
  out <- suppressMessages(run_school_status_with_cache(
    data.frame(place_id = c("A", "B")), geo_dir, "test"
  ))
  expect_equal(out$status_reason, c("transient_error", "transient_error"))

  retry_transient_errors(out, "test", max_attempts = 1)

  s <- api_log_summary()
  expect_equal(s$requests, 4)
  expect_equal(s$failed, 4)
})

test_that("Places lookups are held to places_qps", {
  calls <- character()
  local_mocked_bindings(get_place_details = function(place_id, google_api_key) {
    calls <<- c(calls, place_id)
    list(status_reason = "success")
  })
  elapsed <- system.time(out <- .get_place_details_paced(c("A", "B", "C", "D"), "key", 10))[["elapsed"]]
  expect_equal(calls, c("A", "B", "C", "D"))
  expect_length(out, 4)
  expect_gte(elapsed, 0.35)
})

test_that("get_place_details reports the HTTP status", {
  status <- 429L
  local_mocked_bindings(
    GET = function(url, ...) structure(list(status_code = status), class = "response"),
    .package = "httr"
  )
  res <- get_place_details("A", "key")
  expect_equal(res$status_reason, "transient_error")
  expect_equal(res$http_status, 429L)

  status <- 404L
  res <- get_place_details("A", "key")
  expect_equal(res$status_reason, "not_available")
  expect_equal(res$http_status, 404L)

  local_mocked_bindings(GET = function(url, ...) stop("no connection"), .package = "httr")
  res <- get_place_details("A", "key")
  expect_equal(res$status_reason, "transient_error")
  expect_true(is.na(res$http_status))
})

test_that("failed lookups are counted by cause in the message and the log", {
  geo_dir <- local_log_dir()
  dir.create(geo_dir)
  local_mocked_bindings(get_place_details = function(place_id, google_api_key) {
    failed <- list(business_status = NA_character_, google_place_name = NA_character_,
                   status_reason = "transient_error")
    switch(place_id,
           A = c(failed, http_status = 429L),
           B = c(failed, http_status = 429L),
           C = c(failed, http_status = NA_integer_),
           D = c(failed, http_status = 200L),
           list(business_status = "OPERATIONAL", google_place_name = "E",
                status_reason = "success", http_status = 200L))
  })

  start_api_log(state = "md")
  df <- data.frame(place_id = c("A", "B", "C", "D", "E"))
  expect_message(run_school_status_with_cache(df, geo_dir, "key", places_qps = 1000),
                 "4 of 5 Google lookup\\(s\\) failed \\(HTTP 429: 2; no response: 1; unreadable response: 1\\)")

  s <- api_log_summary()
  expect_equal(s$failed, 4)
  expect_equal(s$failure_codes, "HTTP 429: 2; no response: 1; unreadable response: 1")
  expect_equal(api_log_summary(by_caller = TRUE)$failure_codes, s$failure_codes)

  # Reported in the per-run file and the end-of-run block; history keeps its columns.
  log_dir <- local_log_dir()
  expect_message(path <- write_api_log(log_dir), "Place Details Pro failures: HTTP 429: 2")
  expect_equal(utils::read.csv(path)$failure_codes, s$failure_codes)
  history <- utils::read.csv(file.path(log_dir, "google_api_calls_all_runs.csv"))
  expect_false("failure_codes" %in% names(history))

  start_api_log()
  expect_true(all(is.na(api_log_summary()$failure_codes)))
})
