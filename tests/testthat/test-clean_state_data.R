test_that("clean_state_data() returns a data frame", {
  df <- make_vax_df()
  result <- suppressMessages(clean_state_data(df))
  expect_s3_class(result, "data.frame")
})

test_that("clean_state_data() keeps all required columns", {
  df <- make_vax_df()
  required <- c("year", "school_id", "school_name", "cnty_id",
                "school_county", "enrollment", "current", "delayed",
                "med_exempt", "rel_exempt")
  result <- suppressMessages(clean_state_data(df))
  expect_true(all(required %in% names(result)))
})

test_that("clean_state_data() stops when required columns are missing", {
  df <- make_vax_df()
  df$enrollment <- NULL
  expect_error(clean_state_data(df), "Missing required columns")
})

test_that("clean_state_data() removes rows with zero or NA enrollment", {
  df <- make_vax_df()
  df$enrollment[1] <- 0L
  df$enrollment[2] <- NA_integer_
  result <- suppressMessages(clean_state_data(df))
  expect_true(all(!is.na(result$enrollment) & result$enrollment > 0))
})

test_that("clean_state_data() removes rows where current == 0", {
  df <- make_vax_df()
  df$current[1] <- 0L
  result <- suppressMessages(clean_state_data(df))
  expect_true(all(result$current != 0, na.rm = TRUE))
})

test_that("clean_state_data() coerces numeric columns to integer", {
  df <- make_vax_df()
  df$year       <- as.character(df$year)
  df$enrollment <- as.numeric(df$enrollment)
  result <- suppressMessages(clean_state_data(df))
  expect_type(result$year,       "integer")
  expect_type(result$enrollment, "integer")
})

test_that("clean_state_data() drops columns not in required or optional lists", {
  df        <- make_vax_df()
  df$extra  <- "drop_me"
  result    <- suppressMessages(clean_state_data(df))
  expect_false("extra" %in% names(result))
})

test_that("clean_state_data() accepts custom required_cols", {
  df <- make_vax_df()[, c("year", "school_id", "enrollment", "current",
                           "med_exempt", "rel_exempt", "delayed",
                           "school_name", "cnty_id", "school_county")]
  result <- suppressMessages(
    clean_state_data(df, required_cols = c("year", "school_id", "enrollment",
                                           "current", "med_exempt", "rel_exempt",
                                           "delayed", "school_name", "cnty_id",
                                           "school_county"),
                     optional_cols = character(0))
  )
  expect_s3_class(result, "data.frame")
})
