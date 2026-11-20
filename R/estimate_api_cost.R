# ==============================================================================
# estimate_api_cost.R
# Purpose: Estimate, before a run, how many Google API requests
#   standardize_schools() will send and what they will cost.
# ==============================================================================


#' Estimate the Google API cost of a run before making it
#'
#' Runs \code{\link{standardize_schools}} in \emph{dry-run} mode: all the
#' matching and cache logic runs for real, against copies of your caches, but
#' wherever the pipeline would call Google it only counts the request (through
#' the same logger a real run uses) and carries on as if Google had answered.
#' Nothing is sent, no key is used, and nothing is written to your real caches,
#' output or log directories. Requests already in the geocoding and status caches
#' are not counted, so a re-run of a finished state is estimated at close to
#' zero.
#'
#' @section What the numbers are:
#' The dry run cannot know what Google would answer, so it assumes every geocode
#' succeeds, and it cannot link its synthetic \code{place_id}s to NCES (a real
#' run uses NCES to skip the paid lookup for schools it covers) or merge records
#' of the same place as Google does. So \code{requests_dry_run} counts every
#' school's first geocoding attempt and every place lookup, and ignores retries
#' of failed geocodes. That is more than a real run sends, because the
#' second-attempt retries it leaves out turned out to be smaller than the savings
#' it also leaves out:
#' \tabular{lrrr}{
#'   \tab dry run \tab actual run \tab ratio \cr
#'   Wisconsin, geocoding \tab 5,399 \tab 4,743 \tab 0.88 \cr
#'   Maryland, geocoding  \tab 6,166 \tab 5,189 \tab 0.84 \cr
#'   Wisconsin, place lookups \tab 2,265 \tab 1,219 \tab 0.54 \cr
#'   Maryland, place lookups  \tab 2,154 \tab   842 \tab 0.39
#' }
#' (Actual runs are from the API log, both from empty caches.) The dry-run count
#' was above the real count in both states for both APIs, so it is reported as
#' \code{requests_ceiling}. \code{requests_expected} scales it by
#' \code{geocoding_factor} and \code{places_factor}, whose defaults are the
#' average ratios above. A factor fitted on Wisconsin alone predicted Maryland's
#' geocoding to within 4% but its place lookups 38% too high, because NCES
#' covers a different share of each state's schools, so the place-lookup
#' estimate is the rougher of the two. Two states are not much evidence: a state
#' with many failed geocodes could exceed the ceiling. Re-fit the factors with
#' your own logged runs (actual divided by \code{requests_dry_run} for a
#' from-scratch run) as they accumulate.
#'
#' Costs use the list prices in \code{\link{google_api_prices}}.
#' \code{usd_after_free_*} also subtracts the monthly free allowance still
#' unused according to the running history in \code{api_log_dir}.
#'
#' @inheritParams standardize_schools
#' @param state_dir The state's real directory (see \code{\link{setup_paths}}).
#'   Only used to locate \code{api_log_dir}; the dry run writes to a temporary
#'   copy.
#' @param temp_data_dir The state's real processed-data folder. Optional: if it
#'   holds an NCES reference (\code{nces_reference_<state>.rds}, public data) the
#'   dry run uses a copy instead of downloading it again. Nothing is written to it.
#' @param use_cache If \code{TRUE} (default) the estimate is for running now,
#'   with your existing geocoding and status caches. \code{FALSE} ignores them,
#'   estimating a first run from scratch (or one after
#'   \code{\link{clear_geocoding_cache}}).
#' @param geocoding_factor,places_factor Ratio of a real run's requests to the
#'   dry run's, used for \code{requests_expected}; see the section above for
#'   where the defaults come from.
#' @param prices Price table; see \code{\link{google_api_prices}}.
#' @param api_log_dir Directory holding the running history of past runs
#'   (\code{google_api_calls_all_runs.csv}), used for the free-allowance
#'   calculation. \code{NULL} assumes none of this month's allowance is used.
#' @param verbose If \code{FALSE} (default) the dry run's progress output is
#'   suppressed.
#' @param ... Further arguments passed to \code{standardize_schools()}
#'   (\code{po_box_action}, \code{elementary_only_reference},
#'   \code{include_prek_only_reference}, \code{use_nces}, \code{places_qps}).
#'   \code{parallel}, \code{parallel_cache}, \code{api_qps} and the directory
#'   arguments are fixed by this function.
#'
#' @return Invisibly, a tibble with one row per API: \code{requests_dry_run}
#'   (what the dry run counted, also reported as \code{requests_ceiling}),
#'   \code{requests_expected}, the price and free allowance,
#'   \code{prior_requests_this_month}, and for expected and ceiling the cost at
#'   list price (\code{usd_list_*}) and after the free allowance
#'   (\code{usd_after_free_*}). A summary is also printed.
#'
#' @seealso \code{\link{write_api_log}} for the actual cost of a run.
#' @export
estimate_api_cost <- function(state_id,
                              kinder_dir,
                              greatschools_dir,
                              doe_dir,
                              state_school_dir,
                              state_geo_dir,
                              state_dir,
                              addr_source_pref = "greatschools",
                              use_cache        = TRUE,
                              geocoding_factor = 0.86,
                              places_factor    = 0.46,
                              prices           = google_api_prices(),
                              api_log_dir      = file.path(dirname(dirname(state_dir)), "checks", "tidyschoolvax", "api_logs"),
                              verbose          = FALSE,
                              temp_data_dir    = NULL,
                              ...) {

  fixed <- c("parallel", "parallel_cache", "api_qps", "api_log_dir",
             "state_dir", "state_geo_dir", "nces_cache_dir")
  clash <- intersect(names(list(...)), fixed)
  if (length(clash) > 0L) {
    stop("estimate_api_cost() sets ", paste(clash, collapse = ", "), " itself.", call. = FALSE)
  }
  valid_factor <- function(x) is.numeric(x) && length(x) == 1L && !is.na(x) && x >= 0 && x <= 1
  if (!valid_factor(geocoding_factor) || !valid_factor(places_factor)) {
    stop("geocoding_factor and places_factor must each be between 0 and 1.", call. = FALSE)
  }

  # Everything the run writes goes to a sandbox: a copy of the geocoding folder
  # (so a dry run can never touch the real caches) and scratch output folders.
  sandbox <- tempfile("api_cost_estimate_")
  dir.create(sandbox)
  on.exit(unlink(sandbox, recursive = TRUE), add = TRUE)
  sandbox_geo  <- file.path(sandbox, "geo")
  sandbox_temp <- file.path(sandbox, "temp")
  dir.create(sandbox_geo)
  dir.create(sandbox_temp)
  if (use_cache && dir.exists(state_geo_dir)) {
    keep <- list.files(state_geo_dir, full.names = TRUE)
    if (length(keep) > 0L) file.copy(keep, sandbox_geo, recursive = TRUE)
  }
  # NCES is public data, so its reference is kept even when the caches are not
  if (!is.null(temp_data_dir) && dir.exists(temp_data_dir)) {
    nces <- list.files(temp_data_dir, pattern = "^nces_reference_.*\\.rds$", full.names = TRUE)
    if (length(nces) > 0L) file.copy(nces, sandbox_temp)
  }

  # A missed call site would fail on this key instead of using (and billing) yours.
  key_before <- Sys.getenv("GOOGLEGEO_API_KEY", unset = NA_character_)
  Sys.setenv(GOOGLEGEO_API_KEY = "dry-run-no-requests-are-sent")
  on.exit({
    if (is.na(key_before)) Sys.unsetenv("GOOGLEGEO_API_KEY") else Sys.setenv(GOOGLEGEO_API_KEY = key_before)
  }, add = TRUE)

  run <- function() {
    standardize_schools(
      state_id         = state_id,
      kinder_dir       = kinder_dir,
      greatschools_dir = greatschools_dir,
      doe_dir          = doe_dir,
      state_school_dir = state_school_dir,
      temp_data_dir    = sandbox_temp,
      state_geo_dir    = sandbox_geo,
      state_dir        = file.path(sandbox, "state"),
      addr_source_pref = addr_source_pref,
      parallel         = FALSE,
      parallel_cache   = FALSE,
      api_log_dir      = NULL,
      nces_cache_dir   = sandbox_temp,
      ...
    )
  }
  if (isTRUE(verbose)) {
    .with_api_dry_run(run())
  } else {
    invisible(utils::capture.output(suppressMessages(.with_api_dry_run(run()))))
  }

  history_file <- if (is.null(api_log_dir)) NA_character_ else
    file.path(api_log_dir, "google_api_calls_all_runs.csv")
  history <- if (!is.na(history_file) && file.exists(history_file)) {
    utils::read.csv(history_file, stringsAsFactors = FALSE)
  } else {
    NULL
  }
  prior <- .prior_month_requests(history, format(Sys.Date(), "%Y-%m"))

  estimate <- .cost_estimate(api_log_summary(prices = prices), prices, prior,
                             geocoding_factor = geocoding_factor, places_factor = places_factor)
  .print_cost_estimate(estimate, state_id, use_cache, geocoding_factor, places_factor)
  invisible(estimate)
}


# Internal: turn a dry run's request counts into the expected / ceiling estimate.
# The dry-run count is the ceiling; expected scales it by a per-API factor.
#   tally  api_log_summary() from the dry run (api, requests)
#   prior  .prior_month_requests() (api, prior_requests)
.cost_estimate <- function(tally, prices, prior, geocoding_factor, places_factor) {
  counted <- stats::setNames(tally$requests, tally$api)
  dry     <- vapply(prices$api, function(a) if (a %in% names(counted)) counted[[a]] else 0,
                    numeric(1))

  est <- prices %>%
    dplyr::mutate(
      requests_dry_run = unname(dry),
      requests_expected = dplyr::case_when(
        api == "geocoding"         ~ round(requests_dry_run * geocoding_factor),
        api == "place_details_pro" ~ round(requests_dry_run * places_factor),
        TRUE                       ~ requests_dry_run
      ),
      requests_ceiling = requests_dry_run
    ) %>%
    dplyr::left_join(prior, by = "api") %>%
    dplyr::mutate(prior_requests_this_month = dplyr::coalesce(prior_requests, 0))

  after_free <- function(n) {
    billable <- pmax(est$prior_requests_this_month + n - est$free_per_month, 0) -
      pmax(est$prior_requests_this_month - est$free_per_month, 0)
    round(billable * est$usd_per_1000 / 1000, 2)
  }
  est$usd_list_expected       <- round(est$requests_expected * est$usd_per_1000 / 1000, 2)
  est$usd_list_ceiling        <- round(est$requests_ceiling  * est$usd_per_1000 / 1000, 2)
  est$usd_after_free_expected <- after_free(est$requests_expected)
  est$usd_after_free_ceiling  <- after_free(est$requests_ceiling)

  est %>%
    dplyr::select(api, google_sku, requests_dry_run, requests_expected, requests_ceiling,
                  usd_per_1000, free_per_month, prior_requests_this_month,
                  usd_list_expected, usd_list_ceiling,
                  usd_after_free_expected, usd_after_free_ceiling)
}


.print_cost_estimate <- function(est, state_id, use_cache, geocoding_factor, places_factor) {
  lines <- sprintf("\n=== Estimated Google API cost: %s (%s) ===\n", toupper(state_id),
                   if (isTRUE(use_cache)) "with existing caches" else "first run, no caches")
  for (i in seq_len(nrow(est))) {
    r <- est[i, ]
    lines <- paste0(lines, sprintf(
      "  %-32s expected %6d   ceiling %6d requests   list $%.2f - $%.2f   after free tier $%.2f - $%.2f\n",
      r$google_sku, as.integer(r$requests_expected), as.integer(r$requests_ceiling),
      r$usd_list_expected, r$usd_list_ceiling,
      r$usd_after_free_expected, r$usd_after_free_ceiling
    ))
  }
  lines <- paste0(lines, sprintf(
    "  Total   list $%.2f (expected) to $%.2f (ceiling); after free tier $%.2f to $%.2f\n",
    sum(est$usd_list_expected), sum(est$usd_list_ceiling),
    sum(est$usd_after_free_expected), sum(est$usd_after_free_ceiling)
  ))
  lines <- paste0(lines, sprintf(
    "  Expected scales the dry-run counts by %.2f (geocoding) and %.2f (place lookups), fitted to two earlier runs; the ceiling is the dry-run count. See ?estimate_api_cost.\n",
    geocoding_factor, places_factor
  ))
  message(lines)
}
