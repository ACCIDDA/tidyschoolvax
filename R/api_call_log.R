# api_call_log.R
# Purpose: Count every Google API request the pipeline sends and write a
#   dated per-run log with estimated costs.
#
# Requests are tallied at the call sites (geocode_chunk_step2/3 for the
# Geocoding API; run_school_status_with_cache / retry_transient_errors for
# Place Details), always in the main R process -- counters incremented inside
# future/furrr workers would be lost.


# Internal per-session state: run metadata + one row per logged batch of calls.
.api_log <- new.env(parent = emptyenv())
.api_log$run    <- NULL
.api_log$events <- NULL


#' Google Maps Platform prices used for API cost estimates
#'
#' List prices (USD per 1,000 requests) and monthly free allowances for the
#' two Google SKUs the pipeline uses.  Place status lookups request
#' \code{displayName} and \code{businessStatus}, which Google bills as
#' \emph{Place Details Pro}, not Essentials.  Prices reflect Google's
#' published rates for the first volume tier as of 2025; check them against
#' your Cloud Billing reports and pass an updated table to
#' \code{\link{write_api_log}} if they change.
#'
#' @return A tibble with columns \code{api}, \code{google_sku},
#'   \code{usd_per_1000}, and \code{free_per_month}.
#' @export
google_api_prices <- function() {
  tibble::tibble(
    api            = c("geocoding", "place_details_pro"),
    google_sku     = c("Geocoding API", "Places API - Place Details Pro"),
    usd_per_1000   = c(5, 17),
    free_per_month = c(10000, 5000)
  )
}


#' Start a new Google API call log
#'
#' Resets the in-session tally of Google API requests and records metadata
#' about the run.  Called automatically by \code{\link{standardize_schools}};
#' call it yourself before running geocoding/status functions directly.
#'
#' @param state Character. State abbreviation for the run (e.g. \code{"md"}).
#' @param state_dir Character. State directory, recorded as the run's location.
#'
#' @return Invisibly, the run metadata list.
#' @export
start_api_log <- function(state = NA_character_, state_dir = NA_character_) {
  sys <- Sys.info()
  started_at <- Sys.time()
  .api_log$run <- list(
    run_id          = paste(format(started_at, "%Y-%m-%d_%H%M%S"),
                            Sys.getpid(), basename(tempfile("api_run_")), sep = "_"),
    started_at      = format(started_at, "%Y-%m-%d %H:%M:%S %Z"),
    state           = tolower(state),
    state_dir       = state_dir,
    host            = unname(sys[["nodename"]]),
    user            = unname(sys[["user"]]),
    package_version = as.character(utils::packageVersion("tidyschoolvax"))
  )
  .api_log$events <- NULL
  invisible(.api_log$run)
}


# Internal: record a batch of `n_requests` sent to `api` by `caller`.
.log_api_calls <- function(api, caller, n_requests, n_succeeded) {
  if (n_requests == 0) return(invisible(NULL))
  .api_log$events <- dplyr::bind_rows(
    .api_log$events,
    tibble::tibble(
      logged_at   = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      api         = api,
      caller      = caller,
      requests    = as.integer(n_requests),
      succeeded   = as.integer(n_succeeded),
      failed      = as.integer(n_requests - n_succeeded)
    )
  )
  invisible(NULL)
}


# Internal: log a tidygeocoder::geocode(method = "google") call from its
# output. tidygeocoder sends one request per unique non-blank address, so
# count distinct `query` values; a request succeeded if it returned a latitude.
.log_geocoding_calls <- function(geocoded, caller) {
  sent <- !is.na(geocoded$query) & nzchar(geocoded$query)
  .log_api_calls(
    "geocoding", caller,
    n_requests  = dplyr::n_distinct(geocoded$query[sent]),
    n_succeeded = dplyr::n_distinct(geocoded$query[sent & !is.na(geocoded$lat_temp)])
  )
}


#' Summarise Google API calls logged in this session
#'
#' @param by_caller Logical. If \code{TRUE}, break counts down by the
#'   function that sent the requests as well as by API.
#' @param prices Price table; see \code{\link{google_api_prices}}.
#'
#' @return A tibble with request counts and \code{est_cost_list_usd}, the cost
#'   at list price before any free allowance.  Failed requests are included in
#'   the cost, so it is an upper bound (Google doesn't bill some errors, such
#'   as rate-limit rejections).
#' @export
api_log_summary <- function(by_caller = FALSE, prices = google_api_prices()) {
  events <- .api_log$events
  if (is.null(events) || nrow(events) == 0) {
    events <- tibble::tibble(api = character(), caller = character(),
                             requests = integer(), succeeded = integer(),
                             failed = integer())
  }
  group_cols <- if (by_caller) c("api", "caller") else "api"

  events %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) %>%
    dplyr::summarise(
      requests  = sum(requests),
      succeeded = sum(succeeded),
      failed    = sum(failed),
      .groups   = "drop"
    ) %>%
    dplyr::left_join(prices, by = "api") %>%
    dplyr::mutate(est_cost_list_usd = round(requests * usd_per_1000 / 1000, 2))
}


#' Write the Google API call log for this run
#'
#' Writes a dated CSV, \code{google_api_calls_<state>_<run_id>.csv}, where the
#' run ID includes the start time and a collision-resistant identifier,
#' with one row per API and calling function, and appends a one-row-per-API
#' summary to \code{google_api_calls_all_runs.csv} in the same directory.
#' The running history is used to estimate this run's cost after the monthly
#' free allowance, counting only runs logged in \code{log_dir}; other usage on
#' the same billing account isn't visible here, so treat that figure as a
#' lower bound and \code{est_cost_list_usd} as the upper bound.
#'
#' Both files record the run's state, state directory, host, user, start and
#' finish times, status, and package version.
#'
#' @param log_dir Directory to write the log files to (created if needed).
#' @param status Character. Run outcome recorded in the log, e.g.
#'   \code{"completed"} or \code{"error"}.
#' @param prices Price table; see \code{\link{google_api_prices}}.
#'
#' @return Invisibly, the path of the dated per-run log file.
#' @export
write_api_log <- function(log_dir, status = "completed", prices = google_api_prices()) {
  if (is.null(.api_log$run)) start_api_log()
  if (!dir.exists(log_dir)) dir.create(log_dir, recursive = TRUE)

  run  <- .api_log$run
  meta <- tibble::tibble(
    run_id          = run$run_id,
    started_at      = run$started_at,
    finished_at     = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
    status          = status,
    state           = run$state,
    state_dir       = run$state_dir,
    host            = run$host,
    user            = run$user,
    package_version = run$package_version
  )

  # Per-run detail file: one row per API x caller (or one zero-call row).
  detail <- api_log_summary(by_caller = TRUE, prices = prices)
  if (nrow(detail) == 0) {
    detail <- tibble::tibble(api = NA_character_, caller = NA_character_,
                             requests = 0L, succeeded = 0L, failed = 0L,
                             google_sku = NA_character_, usd_per_1000 = NA_real_,
                             free_per_month = NA_real_, est_cost_list_usd = 0)
  }
  state_tag <- if (is.na(run$state) || !nzchar(run$state)) "all" else run$state
  run_file  <- file.path(log_dir, sprintf("google_api_calls_%s_%s.csv", state_tag, run$run_id))
  utils::write.csv(dplyr::bind_cols(meta[rep(1, nrow(detail)), ], detail),
                   run_file, row.names = FALSE)

  # Running history: month-to-date requests per API, before and after this run.
  history_file <- file.path(log_dir, "google_api_calls_all_runs.csv")
  history <- if (file.exists(history_file)) {
    utils::read.csv(history_file, stringsAsFactors = FALSE)
  } else {
    NULL
  }
  run_month <- substr(run$started_at, 1, 7)
  prior_mtd <- if (!is.null(history) && nrow(history) > 0) {
    history %>%
      dplyr::filter(substr(started_at, 1, 7) == run_month) %>%
      dplyr::group_by(api) %>%
      dplyr::summarise(prior_requests = sum(requests), .groups = "drop")
  } else {
    tibble::tibble(api = character(), prior_requests = numeric())
  }

  run_summary <- api_log_summary(prices = prices) %>%
    dplyr::left_join(prior_mtd, by = "api") %>%
    dplyr::mutate(
      prior_requests       = dplyr::coalesce(prior_requests, 0),
      month_to_date        = prior_requests + requests,
      billable             = pmax(month_to_date - free_per_month, 0) -
                             pmax(prior_requests - free_per_month, 0),
      est_cost_after_free_usd = round(billable * usd_per_1000 / 1000, 2)
    ) %>%
    dplyr::select(api, google_sku, requests, succeeded, failed, usd_per_1000,
                  est_cost_list_usd, free_per_month,
                  month_to_date_requests = month_to_date, est_cost_after_free_usd)

  if (nrow(run_summary) > 0) {
    history_rows <- dplyr::bind_cols(meta[rep(1, nrow(run_summary)), ], run_summary)
    utils::write.table(history_rows, history_file, sep = ",", row.names = FALSE,
                       col.names = !file.exists(history_file),
                       append = file.exists(history_file))
  }

  .print_api_log_summary(run_summary, run_file)
  invisible(run_file)
}


# Internal: one readable block at the end of a run.
.print_api_log_summary <- function(run_summary, run_file) {
  msg <- "\n=== Google API calls this run ===\n"
  if (nrow(run_summary) == 0) {
    msg <- paste0(msg, "  No Google API requests were sent.\n")
  } else {
    for (i in seq_len(nrow(run_summary))) {
      r <- run_summary[i, ]
      msg <- paste0(msg, sprintf(
        "  %-32s %6d requests (%d failed)  list $%.2f  after free tier ~$%.2f  [month to date: %d / %d free]\n",
        r$google_sku, r$requests, r$failed, r$est_cost_list_usd,
        r$est_cost_after_free_usd, as.integer(r$month_to_date_requests),
        as.integer(r$free_per_month)
      ))
    }
    msg <- paste0(msg, sprintf("  Total at list price: $%.2f\n", sum(run_summary$est_cost_list_usd)))
  }
  message(paste0(msg, "  Log: ", run_file))
}
