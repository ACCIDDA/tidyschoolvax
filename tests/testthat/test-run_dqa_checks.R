# ---- run_dqa_checks() --------------------------------------------------------

test_that("run_dqa_checks() returns a list with summary, results, and data_cleaned", {
  df     <- suppressMessages(clean_state_data(make_vax_df()))
  result <- suppressMessages(run_dqa_checks(df, output_dir = NULL))
  expect_type(result, "list")
  expect_true(all(c("summary", "results", "data_cleaned") %in% names(result)))
})

test_that("run_dqa_checks() summary has required columns", {
  df     <- suppressMessages(clean_state_data(make_vax_df()))
  result <- suppressMessages(run_dqa_checks(df, output_dir = NULL))
  expect_true(all(c("check", "issue_count", "action_type") %in% names(result$summary)))
})

test_that("run_dqa_checks() runs all 7 checks by default", {
  df     <- suppressMessages(clean_state_data(make_vax_df()))
  result <- suppressMessages(run_dqa_checks(df, output_dir = NULL))
  # All-checks mode produces at least 7 summary rows (coverage adds two sub-checks)
  expect_gte(nrow(result$summary), 7L)
})

test_that("run_dqa_checks() respects the 'checks' argument", {
  df     <- suppressMessages(clean_state_data(make_vax_df()))
  result <- suppressMessages(
    run_dqa_checks(df, checks = c("negatives", "duplicates"), output_dir = NULL)
  )
  expect_true(all(c("negatives", "duplicates") %in% names(result$results)))
  expect_false("extreme_outliers" %in% names(result$results))
})

test_that("run_dqa_checks() warns on invalid check names", {
  df <- suppressMessages(clean_state_data(make_vax_df()))
  expect_warning(
    suppressMessages(run_dqa_checks(df, checks = "no_such_check", output_dir = NULL)),
    "Invalid check names"
  )
})

test_that("run_dqa_checks() removes identical duplicates from data_cleaned", {
  df   <- suppressMessages(clean_state_data(make_vax_df()))
  df2  <- rbind(df, df[1L, ])   # one exact duplicate
  n_in <- nrow(df)
  result <- suppressMessages(run_dqa_checks(df2, output_dir = NULL))
  expect_equal(nrow(result$data_cleaned), n_in)
})

test_that("run_dqa_checks() saves CSV files when output_dir is specified", {
  df       <- suppressMessages(clean_state_data(make_vax_df()))
  # introduce a negative value so a file will be written
  df$current[1] <- -1L
  out_dir  <- tempfile("dqa_test_", tmpdir = tempdir())
  on.exit(unlink(out_dir, recursive = TRUE), add = TRUE)
  suppressMessages(run_dqa_checks(df, checks = "negatives",
                                  output_dir = out_dir, state = "test"))
  expect_true(dir.exists(out_dir))
  expect_true(length(list.files(out_dir)) > 0L)
})

# ---- generate_dqa_summary() --------------------------------------------------

test_that("generate_dqa_summary() requires a valid dqa_results object", {
  expect_error(generate_dqa_summary(list(x = 1), tempfile(fileext = ".csv")),
               "dqa_results must be a list with a 'summary' component")
})

test_that("generate_dqa_summary() writes a CSV file", {
  df      <- suppressMessages(clean_state_data(make_vax_df()))
  dqa_res <- suppressMessages(run_dqa_checks(df, output_dir = NULL))
  csv_path <- tempfile(fileext = ".csv")
  on.exit(unlink(csv_path), add = TRUE)
  suppressMessages(generate_dqa_summary(dqa_res, csv_path, state = "test"))
  expect_true(file.exists(csv_path))
})

test_that("generate_dqa_summary() writes an RDS file", {
  df      <- suppressMessages(clean_state_data(make_vax_df()))
  dqa_res <- suppressMessages(run_dqa_checks(df, output_dir = NULL))
  rds_path <- tempfile(fileext = ".rds")
  on.exit(unlink(rds_path), add = TRUE)
  suppressMessages(generate_dqa_summary(dqa_res, rds_path, state = "test",
                                        include_timestamp = FALSE))
  expect_true(file.exists(rds_path))
  saved <- readRDS(rds_path)
  expect_s3_class(saved, "data.frame")
})

test_that("generate_dqa_summary() adds state and timestamp columns when requested", {
  df      <- suppressMessages(clean_state_data(make_vax_df()))
  dqa_res <- suppressMessages(run_dqa_checks(df, output_dir = NULL))
  csv_path <- tempfile(fileext = ".csv")
  on.exit(unlink(csv_path), add = TRUE)
  result <- suppressMessages(
    generate_dqa_summary(dqa_res, csv_path, state = "ca", include_timestamp = TRUE)
  )
  expect_true("state" %in% names(result))
  expect_true("timestamp" %in% names(result))
  expect_true(all(result$state == "ca"))
})

test_that("generate_dqa_summary() invisibly returns the summary data frame", {
  df      <- suppressMessages(clean_state_data(make_vax_df()))
  dqa_res <- suppressMessages(run_dqa_checks(df, output_dir = NULL))
  csv_path <- tempfile(fileext = ".csv")
  on.exit(unlink(csv_path), add = TRUE)
  result <- suppressMessages(
    generate_dqa_summary(dqa_res, csv_path, include_timestamp = FALSE)
  )
  expect_s3_class(result, "data.frame")
  expect_true(all(c("check", "issue_count") %in% names(result)))
})
