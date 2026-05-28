test_that("fix_school_level_na() returns a data.table for data.table input without mutating it", {
  input <- data.table::data.table(
    school_name_std = c("alpha", "alpha", "beta"),
    county_std = c("county", "county", "other"),
    school_type = c("public", "public", "public"),
    school_level = c("Elementary", NA_character_, NA_character_)
  )
  original <- data.table::copy(input)

  result <- fix_school_level_na(input, n_years_data = 3L)

  expect_true(data.table::is.data.table(result))
  expect_equal(result$school_level, c("Elementary", "Elementary", NA_character_))
  expect_identical(input, original)
})

test_that("fix_school_level_na() keeps non-data.table inputs on the data.frame path", {
  inputs <- list(
    data.frame(
      school_name_std = c("alpha", "alpha"),
      county_std = c("county", "county"),
      school_type = c("public", "public"),
      school_level = c("Elementary", NA_character_),
      stringsAsFactors = FALSE
    ),
    tibble::tibble(
      school_name_std = c("alpha", "alpha"),
      county_std = c("county", "county"),
      school_type = c("public", "public"),
      school_level = c("Elementary", NA_character_)
    )
  )

  for (input in inputs) {
    result <- fix_school_level_na(input, n_years_data = 3L)

    expect_s3_class(result, "data.frame")
    expect_false(data.table::is.data.table(result))
    expect_equal(result$school_level, c("Elementary", "Elementary"))
  }
})
