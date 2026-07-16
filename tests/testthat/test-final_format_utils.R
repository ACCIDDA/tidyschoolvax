# ---- format_select_columns() -------------------------------------------------

test_that("format_select_columns() retains district and district_std when present", {
  df <- data.frame(
    school_id   = "001",
    year        = 2023L,
    school_name = "Test School",
    county_name = "Test County",
    enrollment  = 100L,
    current     = 90L,
    med_exempt  = 2L,
    rel_exempt  = 1L,
    district     = "Test USD",
    district_std = "test",
    stringsAsFactors = FALSE
  )
  result <- suppressMessages(
    format_select_columns(
      df,
      required_cols = c("school_id", "year", "school_name", "county_name",
                        "enrollment", "current", "med_exempt", "rel_exempt"),
      optional_cols = c("school_type", "school_level", "excluded_note",
                        "addr_clean", "city", "zip", "state", "business_status",
                        "lat", "lon", "district", "district_std")
    )
  )
  expect_true("district"     %in% names(result))
  expect_true("district_std" %in% names(result))
  expect_equal(result$district,     "Test USD")
  expect_equal(result$district_std, "test")
})

test_that("format_select_columns() works when district columns are absent", {
  df <- data.frame(
    school_id   = "001",
    year        = 2023L,
    school_name = "Test School",
    county_name = "Test County",
    enrollment  = 100L,
    current     = 90L,
    med_exempt  = 2L,
    rel_exempt  = 1L,
    stringsAsFactors = FALSE
  )
  result <- suppressMessages(
    format_select_columns(
      df,
      required_cols = c("school_id", "year", "school_name", "county_name",
                        "enrollment", "current", "med_exempt", "rel_exempt"),
      optional_cols = c("school_type", "school_level", "excluded_note",
                        "addr_clean", "city", "zip", "state", "business_status",
                        "lat", "lon", "district", "district_std")
    )
  )
  expect_false("district"     %in% names(result))
  expect_false("district_std" %in% names(result))
})
