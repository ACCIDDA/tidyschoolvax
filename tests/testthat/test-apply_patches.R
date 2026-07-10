# Tests for apply_patches(): the generic mechanism that applies a private table
# of manual per-record corrections to a data frame, so state-specific fixes live
# in (unpublished) data rather than hard-coded in cleaning scripts. All fixtures
# here are synthetic.

make_df <- function() {
  data.frame(
    school_id = c("A1", "A2", "A3"),
    addr = c("1 main", NA, "3 oak"),
    lat = c(35.1, NA, 35.3),
    stringsAsFactors = FALSE
  )
}

test_that("applies multi-field patches and coerces to the column type", {
  df <- make_df()
  patches <- data.frame(
    key = c("A2", "A2"),
    field = c("addr", "lat"),
    value = c("2 elm", "35.2"),
    stringsAsFactors = FALSE
  )
  out <- apply_patches(df, patches, key = "school_id")
  expect_identical(out$addr[2], "2 elm")
  expect_identical(out$lat[2], 35.2)   # coerced from character
  expect_true(is.numeric(out$lat))
  # untouched rows are unchanged
  expect_identical(out$addr[c(1, 3)], c("1 main", "3 oak"))
})

test_that("matches in the key column's own type", {
  df <- data.frame(id = c(101L, 102L), val = c(1L, 2L))
  patches <- data.frame(key = "102", field = "val", value = "9",
                        stringsAsFactors = FALSE)
  out <- apply_patches(df, patches, key = "id")
  expect_identical(out$val, c(1L, 9L))
})

test_that("the patch key column may be named after the match key", {
  df <- make_df()
  patches <- data.frame(school_id = "A1", field = "addr", value = "new",
                        stringsAsFactors = FALSE)
  out <- apply_patches(df, patches, key = "school_id")
  expect_identical(out$addr[1], "new")
})

test_that("a stale (unmatched) patch errors under strict and warns otherwise", {
  df <- make_df()
  patches <- data.frame(key = "ZZ", field = "addr", value = "x",
                        stringsAsFactors = FALSE)
  expect_error(apply_patches(df, patches, key = "school_id"), "stale patch")
  expect_warning(
    out <- apply_patches(df, patches, key = "school_id", strict = FALSE),
    "stale patch"
  )
  expect_identical(out, df)  # nothing changed
})

test_that("unknown field or missing key column is an error", {
  df <- make_df()
  expect_error(
    apply_patches(df, data.frame(key = "A1", field = "nope", value = "x"),
                  key = "school_id"),
    "not present in"
  )
  expect_error(
    apply_patches(df, data.frame(key = "A1", field = "addr", value = "x"),
                  key = "missing_col"),
    "not a column in"
  )
})

test_that("a malformed patch table is rejected", {
  df <- make_df()
  expect_error(
    apply_patches(df, data.frame(key = "A1", value = "x"), key = "school_id"),
    "missing column"
  )
})

test_that("reads and applies a .csv patch file", {
  df <- make_df()
  patches <- data.frame(key = "A2", field = "addr", value = "2 elm",
                        stringsAsFactors = FALSE)
  path <- withr::local_tempfile(fileext = ".csv")
  utils::write.csv(patches, path, row.names = FALSE)
  out <- apply_patches(df, path, key = "school_id")
  expect_identical(out$addr[2], "2 elm")
})

test_that("the bundled example patch file has the documented shape", {
  path <- system.file("extdata", "example_patches.csv", package = "tidyschoolvax")
  skip_if(identical(path, ""))
  ex <- utils::read.csv(path, stringsAsFactors = FALSE)
  expect_true(all(c("key", "field", "value") %in% names(ex)))
})
