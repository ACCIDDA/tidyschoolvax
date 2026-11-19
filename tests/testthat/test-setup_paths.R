test_that("setup_paths() returns the layout and creates the folders", {
  root <- withr::local_tempdir()
  p <- setup_paths(project_root = root, state = "xx")
  rel <- function(x) sub(paste0(root, "/"), "", x, fixed = TRUE)

  expect_equal(rel(p$state_dir),        "00_preprocessing/states/xx")
  expect_equal(rel(p$code_dir),         "00_preprocessing/states/xx/code")
  expect_equal(rel(p$raw_data_dir),     "00_preprocessing/states/xx/raw")
  expect_equal(rel(p$manual_dir),       "00_preprocessing/states/xx/raw/manual")
  expect_equal(rel(p$temp_data_dir),    "00_preprocessing/states/xx/work")
  expect_equal(rel(p$state_geo_dir),    "00_preprocessing/states/xx/geocoding")
  expect_equal(rel(p$review_dir),       "00_preprocessing/states/xx/review")
  expect_equal(rel(p$final_dir),        "00_preprocessing/states/xx/final")
  expect_equal(rel(p$general_data_dir), "00_preprocessing/shared")
  expect_equal(rel(p$vaxview_dir),      "00_preprocessing/shared/vaxview")
  expect_true(all(vapply(p, dir.exists, logical(1))))
})

test_that("cleaned tables and model inputs share the final folder", {
  p <- setup_paths(project_root = withr::local_tempdir(), state = "xx")
  expect_identical(p$clean_data_dir, p$final_dir)
  expect_identical(p$outputs_data_dir, p$final_dir)
})

test_that("VaxView is shared: two states get the same folder", {
  root <- withr::local_tempdir()
  expect_identical(setup_paths(root, "aa")$vaxview_dir, setup_paths(root, "bb")$vaxview_dir)
})

test_that("setup_paths() rejects a missing state", {
  expect_error(setup_paths(withr::local_tempdir(), state = ""), "non-empty")
})
