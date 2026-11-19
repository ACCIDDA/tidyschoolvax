test_that("write_pipeline_table() writes exactly one file in the requested format", {
  skip_if_not_installed("arrow")
  d <- withr::local_tempdir()
  x <- data.frame(id = 1:3, zip = c("02134", "53545", "10001"), v = c(1.5, NA, 3))

  for (fmt in c("parquet", "csv", "rds")) {
    path <- write_pipeline_table(x, file.path(d, paste0("t_", fmt)), fmt)
    expect_equal(basename(path), paste0("t_", fmt, ".", fmt))
    expect_true(file.exists(path))
  }
  expect_equal(length(list.files(d, pattern = "^t_parquet")), 1L)
  expect_error(write_pipeline_table(x, file.path(d, "bad"), "xlsx"))
})

test_that("Parquet round trip keeps column types, including text ZIP codes", {
  skip_if_not_installed("arrow")
  d <- withr::local_tempdir()
  x <- data.frame(id = 1:3, zip = c("02134", "53545", "10001"), v = c(1.5, NA, 3),
                  stringsAsFactors = FALSE)
  write_pipeline_table(x, file.path(d, "t"))
  back <- read_pipeline_table(file.path(d, "t"))
  expect_identical(back$id, x$id)
  expect_identical(back$zip, x$zip)
  expect_equal(back$v, x$v)
})

test_that("read_pipeline_table() prefers Parquet, falls back to RDS, and ignores CSV by default", {
  skip_if_not_installed("arrow")
  d <- withr::local_tempdir()
  stem <- file.path(d, "t")
  expect_error(read_pipeline_table(stem), "No .parquet or .rds")

  readr::write_csv(data.frame(a = 1), paste0(stem, ".csv"))
  expect_error(read_pipeline_table(stem), "No .parquet or .rds")
  expect_equal(nrow(read_pipeline_table(stem, allow_csv = TRUE)), 1L)

  saveRDS(data.frame(a = 1:2), paste0(stem, ".rds"))
  expect_equal(nrow(read_pipeline_table(stem)), 2L)

  write_pipeline_table(data.frame(a = 1:3), stem, "parquet")
  expect_equal(nrow(read_pipeline_table(stem)), 3L)
})

test_that("read_cleaned_data() keeps ZIP codes as text and counts as integers", {
  d <- withr::local_tempdir()
  x <- data.frame(school_id = 1:2, year = 2024L, enrollment = c(50L, 60L),
                  current = c(45L, 55L), med_exempt = c(1L, NA), rel_exempt = c(2L, 3L),
                  zip = c("53545", "54301"), lat = c(42.1, 43.2), lon = c(-71.1, -89.0),
                  excluded_note = NA_character_)
  readr::write_csv(x, file.path(d, "cleaned_data.csv"))

  plain <- readr::read_csv(file.path(d, "cleaned_data.csv"), show_col_types = FALSE)
  expect_true(is.numeric(plain$zip))                       # guessed as a number

  typed <- read_cleaned_data(file.path(d, "cleaned_data.csv"))
  expect_identical(typed$zip, c("53545", "54301"))
  expect_true(is.integer(typed$school_id))
  expect_true(is.integer(typed$enrollment))
  expect_type(typed$excluded_note, "character")
})

test_that("read_cleaned_data() keeps leading zeros in ZIP codes", {
  d <- withr::local_tempdir()
  x <- data.frame(school_id = 1L, year = 2024L, zip = "02134")
  readr::write_csv(x, file.path(d, "cleaned_data.csv"))
  expect_identical(read_cleaned_data(file.path(d, "cleaned_data.csv"))$zip, "02134")
})

test_that("read_cleaned_data() reads tables that lack some of the known columns without warning", {
  d <- withr::local_tempdir()
  readr::write_csv(data.frame(school_id = 1L, exclusion_reason = "zero_enrollment"),
                   file.path(d, "excluded.csv"))
  expect_no_warning(x <- read_cleaned_data(file.path(d, "excluded.csv")))
  expect_identical(x$exclusion_reason, "zero_enrollment")
})
