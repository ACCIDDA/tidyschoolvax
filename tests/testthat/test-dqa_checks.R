# ---- check_duplicates() -------------------------------------------------------

test_that("check_duplicates() returns a list with two data-frame components", {
  df     <- suppressMessages(clean_state_data(make_vax_df()))
  result <- suppressMessages(check_duplicates(df))
  expect_type(result, "list")
  expect_named(result, c("identical_dupes", "conflicting_dupes"))
  expect_s3_class(result$identical_dupes,   "data.frame")
  expect_s3_class(result$conflicting_dupes, "data.frame")
})

test_that("check_duplicates() detects identical duplicate rows", {
  df <- suppressMessages(clean_state_data(make_vax_df()))
  df <- rbind(df, df[1L, ])  # add an exact copy of row 1
  result <- suppressMessages(check_duplicates(df))
  expect_gt(nrow(result$identical_dupes), 0L)
  expect_equal(nrow(result$conflicting_dupes), 0L)
})

test_that("check_duplicates() detects conflicting duplicates", {
  df     <- suppressMessages(clean_state_data(make_vax_df()))
  extra  <- df[1L, ]
  extra$enrollment <- extra$enrollment + 50L  # same id/year, different data
  df     <- rbind(df, extra)
  result <- suppressMessages(check_duplicates(df))
  expect_gt(nrow(result$conflicting_dupes), 0L)
})

test_that("check_duplicates() returns empty frames when no duplicates exist", {
  df     <- suppressMessages(clean_state_data(make_vax_df()))
  result <- suppressMessages(check_duplicates(df))
  expect_equal(nrow(result$identical_dupes),   0L)
  expect_equal(nrow(result$conflicting_dupes), 0L)
})

# ---- check_negative_values() -------------------------------------------------

test_that("check_negative_values() returns a data frame", {
  df     <- suppressMessages(clean_state_data(make_vax_df()))
  result <- suppressMessages(check_negative_values(df))
  expect_s3_class(result, "data.frame")
})

test_that("check_negative_values() flags rows with negative counts", {
  df            <- suppressMessages(clean_state_data(make_vax_df()))
  df$current[1] <- -1L
  result        <- suppressMessages(check_negative_values(df))
  expect_gte(nrow(result), 1L)
  expect_true(any(result$current < 0))
})

test_that("check_negative_values() returns empty frame when no negatives", {
  df     <- suppressMessages(clean_state_data(make_vax_df()))
  result <- suppressMessages(check_negative_values(df))
  expect_equal(nrow(result), 0L)
})

test_that("check_negative_values() warns when no checked columns present", {
  df <- data.frame(a = 1:3)
  expect_warning(
    check_negative_values(df, cols_to_check = "nonexistent"),
    "None of the specified columns"
  )
})

# ---- check_exceeding_enrollment_values() -------------------------------------

test_that("check_exceeding_enrollment_values() returns a data frame", {
  df     <- suppressMessages(clean_state_data(make_vax_df()))
  result <- suppressMessages(check_exceeding_enrollment_values(df))
  expect_s3_class(result, "data.frame")
})

test_that("check_exceeding_enrollment_values() flags values > enrollment", {
  df            <- suppressMessages(clean_state_data(make_vax_df()))
  df$current[1] <- df$enrollment[1] + 50L
  result        <- suppressMessages(check_exceeding_enrollment_values(df))
  expect_gte(nrow(result), 1L)
})

test_that("check_exceeding_enrollment_values() returns empty frame when clean", {
  df            <- suppressMessages(clean_state_data(make_vax_df()))
  df$current    <- pmin(df$current, df$enrollment)
  result        <- suppressMessages(check_exceeding_enrollment_values(df))
  expect_equal(nrow(result), 0L)
})

test_that("check_exceeding_enrollment_values() stops without enrollment column", {
  df <- data.frame(current = 10L)
  expect_error(check_exceeding_enrollment_values(df), "enrollment")
})

# ---- check_coverage_outliers() -----------------------------------------------

test_that("check_coverage_outliers() returns a named list", {
  df     <- suppressMessages(clean_state_data(make_vax_df()))
  result <- suppressMessages(check_coverage_outliers(df))
  expect_type(result, "list")
  expect_named(result, c("coverage_outliers", "over_coverage"))
})

test_that("check_coverage_outliers() detects >105% coverage", {
  df            <- suppressMessages(clean_state_data(make_vax_df()))
  df$current[1] <- df$enrollment[1] * 2L  # 200% coverage
  result        <- suppressMessages(check_coverage_outliers(df))
  expect_gte(nrow(result$coverage_outliers), 1L)
})

test_that("check_coverage_outliers() detects over-coverage (sum > enrollment)", {
  df            <- suppressMessages(clean_state_data(make_vax_df()))
  # Force all counts to sum well beyond enrollment
  df$current[2]    <- df$enrollment[2]
  df$delayed[2]    <- df$enrollment[2]
  df$med_exempt[2] <- df$enrollment[2]
  result           <- suppressMessages(check_coverage_outliers(df))
  expect_gte(nrow(result$over_coverage), 1L)
})

# ---- check_enrollment_deviation() --------------------------------------------

test_that("check_enrollment_deviation() returns list with flagged and school_history", {
  df     <- suppressMessages(clean_state_data(make_vax_df()))
  result <- suppressMessages(check_enrollment_deviation(df))
  expect_type(result, "list")
  expect_true(all(c("flagged", "school_history") %in% names(result)))
})

test_that("check_enrollment_deviation() flags large enrollment swings", {
  df <- suppressMessages(clean_state_data(make_vax_df()))
  # Make school 1001 appear with vastly different enrollment in one year
  df$enrollment[df$school_id == 1001L & df$year == 2015L] <- 5000L
  result <- suppressMessages(check_enrollment_deviation(df, deviation_threshold = 0.5))
  expect_gte(nrow(result$flagged), 1L)
})

test_that("check_enrollment_deviation() warns when required columns missing", {
  df <- data.frame(x = 1:3)
  expect_warning(check_enrollment_deviation(df), "school_id.*enrollment")
})

# ---- check_vaccination_deviation() -------------------------------------------

test_that("check_vaccination_deviation() returns list with flagged and school_history", {
  df     <- suppressMessages(clean_state_data(make_vax_df()))
  result <- suppressMessages(check_vaccination_deviation(df))
  expect_type(result, "list")
  expect_true(all(c("flagged", "school_history") %in% names(result)))
})

test_that("check_vaccination_deviation() flags large vaccination swings", {
  df <- suppressMessages(clean_state_data(make_vax_df()))
  df$current[df$school_id == 1001L & df$year == 2015L] <- 5000L
  result <- suppressMessages(check_vaccination_deviation(df, deviation_threshold = 0.5))
  expect_gte(nrow(result$flagged), 1L)
})

# ---- check_extreme_outliers() ------------------------------------------------

test_that("check_extreme_outliers() returns a data frame", {
  df     <- suppressMessages(clean_state_data(make_vax_df()))
  result <- suppressMessages(check_extreme_outliers(df))
  expect_s3_class(result, "data.frame")
})

test_that("check_extreme_outliers() flags 10x median enrollment", {
  df <- suppressMessages(clean_state_data(make_vax_df()))
  df$enrollment[df$school_id == 1001L & df$year == 2015L] <- 99999L
  result <- suppressMessages(check_extreme_outliers(df, multiplier = 10))
  expect_gte(nrow(result), 1L)
})

test_that("check_extreme_outliers() returns empty frame when clean", {
  df     <- suppressMessages(clean_state_data(make_vax_df()))
  result <- suppressMessages(check_extreme_outliers(df, multiplier = 100))
  expect_equal(nrow(result), 0L)
})

test_that("check_extreme_outliers() warns when required columns missing", {
  df <- data.frame(x = 1:3)
  expect_warning(check_extreme_outliers(df), "school_id.*enrollment")
})
