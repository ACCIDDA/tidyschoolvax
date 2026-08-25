test_that("create_state_download_script() creates the file when none exists", {
  tmp <- tempfile("proj_")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  path <- suppressMessages(create_state_download_script("tx", project_root = tmp))
  expect_true(file.exists(path))
  expect_true(grepl("00_preprocessing/states/tx/01_cleaning/01_download\\.R$",
                    gsub("\\\\", "/", path)))
})

test_that("create_state_download_script() is idempotent: existing file -> message + returns path, no error", {
  tmp <- tempfile("proj_")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  first  <- suppressMessages(create_state_download_script("tx", project_root = tmp))
  writeLines("# hand-customized content", first)

  expect_message(
    second <- create_state_download_script("tx", project_root = tmp),
    "already exists"
  )
  expect_equal(second, first)
  # The existing (customized) file must be left untouched, not overwritten.
  expect_equal(readLines(first), "# hand-customized content")
})

test_that("create_state_download_script() overwrite = TRUE replaces an existing file", {
  tmp <- tempfile("proj_")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  first <- suppressMessages(create_state_download_script("tx", project_root = tmp))
  writeLines("# hand-customized content", first)

  suppressMessages(
    second <- create_state_download_script("tx", project_root = tmp, overwrite = TRUE)
  )
  expect_equal(second, first)
  expect_false(identical(readLines(first), "# hand-customized content"))
})
