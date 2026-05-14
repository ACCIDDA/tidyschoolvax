test_that("standardize_kinder_format() returns a data.table", {
  df <- data.frame(
    sid  = 101:105,
    yr   = 2020L,
    enr  = c(100L, 120L, 90L, 110L, 80L),
    cur  = c(90L,  100L, 80L,  95L, 70L),
    med  = c(2L,    3L,  1L,   2L,  1L),
    rel  = c(5L,    8L,  4L,   6L,  4L),
    stringsAsFactors = FALSE
  )
  result <- standardize_kinder_format(
    df,
    school_id_col  = "sid",
    year_col       = "yr",
    enrollment_col = "enr",
    current_col    = "cur",
    med_exempt_col = "med",
    rel_exempt_col = "rel"
  )
  expect_true(inherits(result, "data.table"))
})

test_that("standardize_kinder_format() produces canonical base column names", {
  df <- data.frame(
    sid  = 101L,
    yr   = 2020L,
    enr  = 100L,
    cur  = 90L,
    med  = 2L,
    rel  = 5L,
    stringsAsFactors = FALSE
  )
  result <- standardize_kinder_format(
    df,
    school_id_col  = "sid",
    year_col       = "yr",
    enrollment_col = "enr",
    current_col    = "cur",
    med_exempt_col = "med",
    rel_exempt_col = "rel"
  )
  expect_true(all(c("school_id", "year", "enrollment", "current",
                    "med_exempt", "rel_exempt") %in% names(result)))
})

test_that("standardize_kinder_format() coerces numeric-like columns to integer", {
  df <- data.frame(
    sid  = "101",
    yr   = "2020",
    enr  = "100",
    cur  = "90",
    med  = "2",
    rel  = "5",
    stringsAsFactors = FALSE
  )
  result <- standardize_kinder_format(
    df,
    school_id_col  = "sid",
    year_col       = "yr",
    enrollment_col = "enr",
    current_col    = "cur",
    med_exempt_col = "med",
    rel_exempt_col = "rel"
  )
  expect_type(result$year,       "integer")
  expect_type(result$enrollment, "integer")
  expect_type(result$current,    "integer")
  expect_type(result$med_exempt, "integer")
  expect_type(result$rel_exempt, "integer")
})

test_that("standardize_kinder_format() stops when required columns are missing", {
  df <- data.frame(sid = 1L, yr = 2020L, enr = 100L, stringsAsFactors = FALSE)
  expect_error(
    standardize_kinder_format(
      df,
      school_id_col  = "sid",
      year_col       = "yr",
      enrollment_col = "enr",
      current_col    = "cur_missing",
      med_exempt_col = "med_missing",
      rel_exempt_col = "rel_missing"
    ),
    "Missing columns"
  )
})

test_that("standardize_kinder_format() adds optional columns when supplied", {
  df <- data.frame(
    sid   = 101L,
    yr    = 2020L,
    enr   = 100L,
    cur   = 90L,
    med   = 2L,
    rel   = 5L,
    name  = "Elm School",
    cnty  = "Test County",
    stringsAsFactors = FALSE
  )
  result <- standardize_kinder_format(
    df,
    school_id_col   = "sid",
    year_col        = "yr",
    enrollment_col  = "enr",
    current_col     = "cur",
    med_exempt_col  = "med",
    rel_exempt_col  = "rel",
    school_name_col = "name",
    county_name_col = "cnty"
  )
  expect_true("school_name"  %in% names(result))
  expect_true("county_name"  %in% names(result))
  expect_equal(result$school_name, "Elm School")
})

test_that("standardize_kinder_format() strips commas from numeric strings", {
  df <- data.frame(
    sid  = 1L,
    yr   = 2020L,
    enr  = "1,200",
    cur  = "1,100",
    med  = "5",
    rel  = "10",
    stringsAsFactors = FALSE
  )
  result <- standardize_kinder_format(
    df,
    school_id_col  = "sid",
    year_col       = "yr",
    enrollment_col = "enr",
    current_col    = "cur",
    med_exempt_col = "med",
    rel_exempt_col = "rel"
  )
  expect_equal(result$enrollment, 1200L)
  expect_equal(result$current,    1100L)
})
