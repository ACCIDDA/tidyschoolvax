test_that("setup_paths() returns the layout and creates the folders", {
  root <- withr::local_tempdir()
  p <- setup_paths(project_root = root, state = "xx")
  rel <- function(x) sub(paste0(root, "/"), "", x, fixed = TRUE)

  # intermediate data
  expect_equal(rel(p$state_dir),        "data_processing/xx")
  expect_equal(rel(p$raw_data_dir),     "data_processing/xx/raw")
  expect_equal(rel(p$manual_dir),       "data_processing/xx/raw/manual")
  expect_equal(rel(p$temp_data_dir),    "data_processing/xx/processed")
  expect_equal(rel(p$state_geo_dir),    "data_processing/xx/geocoding")
  expect_equal(rel(p$general_data_dir), "data_processing/shared")
  expect_equal(rel(p$vaxview_dir),      "data_processing/shared/vaxview")
  # code, outputs, checks
  expect_equal(rel(p$code_dir),         "source/tidyschoolvax/xx")
  expect_equal(rel(p$final_dir),        "output/tidyschoolvax/xx")
  expect_equal(rel(p$review_dir),       "checks/tidyschoolvax/xx")
  expect_equal(rel(p$api_log_dir),      "checks/tidyschoolvax/api_logs")
  made <- p[setdiff(names(p), c("kinder_dir", "doe_dir", "greatschools_dir", "state_school_dir"))]
  expect_true(all(vapply(made, dir.exists, logical(1))))
})

test_that("per-source raw folders are returned but not created", {
  p <- setup_paths(project_root = withr::local_tempdir(), state = "xx")
  for (d in c("kinder_dir", "doe_dir", "greatschools_dir", "state_school_dir")) {
    expect_true(startsWith(p[[d]], p$raw_data_dir), info = d)
    expect_false(dir.exists(p[[d]]), info = d)
  }
  expect_true(dir.exists(p$raw_data_dir) && dir.exists(p$manual_dir))
})

test_that("data_processing holds intermediate data only", {
  root <- withr::local_tempdir()
  p <- setup_paths(project_root = root, state = "xx")
  inside <- function(x) startsWith(x, file.path(root, "data_processing"))
  expect_true(all(vapply(p[c("state_dir", "raw_data_dir", "manual_dir", "temp_data_dir",
                              "state_geo_dir", "general_data_dir", "vaxview_dir")], inside, logical(1))))
  expect_false(any(vapply(p[c("code_dir", "final_dir", "review_dir", "api_log_dir")], inside, logical(1))))
})

test_that("cleaned tables and model inputs share the final folder", {
  p <- setup_paths(project_root = withr::local_tempdir(), state = "xx")
  expect_identical(p$clean_data_dir, p$final_dir)
  expect_identical(p$outputs_data_dir, p$final_dir)
})

test_that("VaxView and the API logs are shared: two states get the same folders", {
  root <- withr::local_tempdir()
  a <- setup_paths(root, "aa"); b <- setup_paths(root, "bb")
  expect_identical(a$vaxview_dir, b$vaxview_dir)
  expect_identical(a$api_log_dir, b$api_log_dir)
})

test_that("setup_paths() rejects a missing state", {
  expect_error(setup_paths(withr::local_tempdir(), state = ""), "non-empty")
})

test_that("the default API-log folder of standardize_schools() is the one setup_paths() makes", {
  p <- setup_paths(withr::local_tempdir(), "xx")
  default <- eval(formals(standardize_schools)$api_log_dir, list(state_dir = p$state_dir))
  expect_identical(normalizePath(default, mustWork = FALSE), normalizePath(p$api_log_dir))
})
