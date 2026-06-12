# ---- check_expected_files() --------------------------------------------------

test_that("check_expected_files() returns TRUE when all files exist", {
  tmp <- tempfile()
  writeLines("x", tmp)
  on.exit(unlink(tmp), add = TRUE)
  result <- suppressMessages(check_expected_files(tmp))
  expect_true(result)
})

test_that("check_expected_files() stops with informative message for missing files", {
  expect_error(
    check_expected_files("/nonexistent/path/file.csv"),
    "Missing expected output files"
  )
})

test_that("check_expected_files() includes step_name in the error message", {
  expect_error(
    check_expected_files("/nonexistent/path/file.csv", step_name = "step_01"),
    "step_01"
  )
})

# ---- coerce_columns_to_numeric() ---------------------------------------------

test_that("coerce_columns_to_numeric() converts character columns to numeric", {
  df     <- data.frame(a = c("1", "2", "3"), b = c("4.5", "5.5", "6.5"),
                       stringsAsFactors = FALSE)
  result <- coerce_columns_to_numeric(df, cols = c("a", "b"))
  expect_type(result$a, "double")
  expect_type(result$b, "double")
  expect_equal(result$a, c(1, 2, 3))
})

test_that("coerce_columns_to_numeric() silently skips absent columns", {
  df     <- data.frame(a = c("1", "2"), stringsAsFactors = FALSE)
  result <- coerce_columns_to_numeric(df, cols = c("a", "missing_col"))
  expect_equal(names(result), "a")
})

# ---- extract_year() ----------------------------------------------------------

test_that("extract_year() extracts two-digit year range from filename", {
  result <- extract_year("data_2019-2020.xlsx", state = "md")
  expect_equal(result, "md_19_20")
})

test_that("extract_year() uses state prefix in output", {
  result <- extract_year("data_2021-2022.xlsx", state = "nc")
  expect_equal(result, "nc_21_22")
})

test_that("extract_year() returns fallback when no year pattern found", {
  result <- extract_year("nodates.csv", state = "ca")
  expect_match(result, "^ca_unknown_")
})

# ---- get_sd_from_ci() --------------------------------------------------------

test_that("get_sd_from_ci() returns a positive numeric value", {
  result <- get_sd_from_ci(40, 60, 100)
  expect_type(result, "double")
  expect_gt(result, 0)
})

test_that("get_sd_from_ci() returns larger SD for wider confidence intervals", {
  sd_narrow <- get_sd_from_ci(45, 55, 100)
  sd_wide   <- get_sd_from_ci(30, 70, 100)
  expect_lt(sd_narrow, sd_wide)
})

test_that("get_sd_from_ci() result scales correctly with sample size", {
  # Doubling sample size with the same CI width should give a larger SD
  sd_small <- get_sd_from_ci(40, 60, 50)
  sd_large <- get_sd_from_ci(40, 60, 200)
  expect_gt(sd_large, sd_small)
})

# ---- standardized_school_name() ----------------------------------------------

test_that("standardized_school_name() lowercases the name", {
  result <- standardized_school_name("LINCOLN ELEMENTARY")
  expect_equal(result, tolower(result))
})

test_that("standardized_school_name() expands 'elem' abbreviation", {
  result <- standardized_school_name("Lincoln Elem School")
  expect_match(result, "elementary")
})

test_that("standardized_school_name() removes 'school' keyword", {
  result <- standardized_school_name("Jefferson School")
  expect_false(grepl("school", result))
})

test_that("standardized_school_name() squishes extra whitespace", {
  result <- standardized_school_name("  Lincoln   Elementary  ")
  expect_false(grepl("  ", result))
})

# ---- standardized_county_name() ----------------------------------------------

test_that("standardized_county_name() lowercases the name", {
  result <- standardized_county_name("Wake County")
  expect_equal(result, tolower(result))
})

test_that("standardized_county_name() removes the word 'county'", {
  result <- standardized_county_name("Wake County")
  expect_false(grepl("county", result, ignore.case = TRUE))
})

test_that("standardized_county_name() removes '&' punctuation and preserves words", {
  result <- standardized_county_name("Smith & Jones")
  expect_false(grepl("&", result, fixed = TRUE))
  expect_match(result, "smith")
  expect_match(result, "jones")
})

# ---- standardize_district_name() --------------------------------------------

test_that("standardize_district_name() lowercases the name", {
  result <- standardize_district_name("Wake County School District")
  expect_equal(result, tolower(result))
})

test_that("standardize_district_name() removes 'school district'", {
  result <- standardize_district_name("Wake County School District")
  expect_false(grepl("school district", result, ignore.case = TRUE))
})

test_that("standardize_district_name() removes 'unified'", {
  result <- standardize_district_name("Los Angeles Unified School District")
  expect_false(grepl("unified", result, ignore.case = TRUE))
})

test_that("standardize_district_name() removes 'independent'", {
  result <- standardize_district_name("Austin Independent School District")
  expect_false(grepl("independent", result, ignore.case = TRUE))
})

test_that("standardize_district_name() removes 'public schools'", {
  result <- standardize_district_name("Baltimore City Public Schools")
  expect_false(grepl("public schools", result, ignore.case = TRUE))
  expect_match(result, "baltimore")
})

test_that("standardize_district_name() squishes extra whitespace", {
  result <- standardize_district_name("  Denver   School   District  ")
  expect_false(grepl("  ", result))
})

test_that("standardize_district_name() replaces '&' / '&amp;' with 'and'", {
  result <- standardize_district_name(c("Smith & Jones District", "Smith &amp; Jones District"))
  expect_false(any(grepl("&", result, fixed = TRUE)))
  expect_true(all(grepl("and", result)))
})

test_that("standardize_district_name() returns NA for NA input", {
  result <- standardize_district_name(NA_character_)
  expect_true(is.na(result))
})
