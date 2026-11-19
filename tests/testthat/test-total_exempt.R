# Tests for the optional combined-exemption column (`total_exempt`), used by
# states that do not report medical and religious exemptions separately.

# ---- standardize_kinder_format() ---------------------------------------------

test_that("standardize_kinder_format() adds total_exempt only when requested", {
  df <- data.frame(
    sid = 101:102, yr = 2020L, enr = c(100L, 80L), cur = c(90L, 70L),
    med = NA_integer_, rel = NA_integer_, tot = c(4L, 6L)
  )
  args <- list(df, school_id_col = "sid", year_col = "yr", enrollment_col = "enr",
               current_col = "cur", med_exempt_col = "med", rel_exempt_col = "rel")

  with_total <- do.call(standardize_kinder_format, c(args, total_exempt_col = "tot"))
  expect_true("total_exempt" %in% names(with_total))
  expect_equal(with_total$total_exempt, c(4L, 6L))

  without_total <- do.call(standardize_kinder_format, args)
  expect_false("total_exempt" %in% names(without_total))
})

test_that("total_exempt_col is the last argument of the exported functions", {
  # Adding it must not shift the meaning of existing positional calls.
  expect_equal(tail(names(formals(standardize_kinder_format)), 1), "total_exempt_col")
  expect_equal(tail(names(formals(dqa_fix_invalid_values)), 1), "total_exempt_col")
})

# ---- dqa_fix_invalid_values() ------------------------------------------------

test_that("dqa_fix_invalid_values() blanks total_exempt larger than enrollment", {
  df <- data.frame(
    enrollment = c(100L, 100L, 100L),
    current    = c(90L, 90L, 90L),
    med_exempt = NA_integer_,
    rel_exempt = NA_integer_,
    total_exempt = c(5L, 150L, NA_integer_)
  )
  tmp <- withr::local_tempdir()
  res <- suppressMessages(dqa_fix_invalid_values(df, temp_data_dir = tmp, state = "xx"))

  expect_equal(res$data$total_exempt, c(5L, NA, NA))
  expect_equal(res$data$current, c(90L, 90L, 90L))      # current is left alone
  expect_match(res$data$excluded_note[2], "exemptions > enrollment")
  expect_true(is.na(res$data$excluded_note[1]))
})

test_that("dqa_fix_invalid_values() ignores total_exempt when the column is absent", {
  df <- data.frame(enrollment = 100L, current = 90L, med_exempt = 2L, rel_exempt = 1L)
  tmp <- withr::local_tempdir()
  res <- suppressMessages(dqa_fix_invalid_values(df, temp_data_dir = tmp, state = "xx"))
  expect_false("total_exempt" %in% names(res$data))
  expect_equal(res$data$med_exempt, 2L)
})

# ---- dqa_check_over_coverage() -----------------------------------------------

test_that("dqa_check_over_coverage() uses total_exempt when med/rel are missing", {
  df <- data.frame(
    enrollment = c(100L, 100L),
    current    = c(90L, 98L),
    med_exempt = NA_integer_,
    rel_exempt = NA_integer_,
    total_exempt = c(5L, 5L)
  )
  utils::capture.output(
    expect_warning(res <- dqa_check_over_coverage(df), "exempt exceed enrollment")
  )
  expect_equal(res$n_flagged, 1L)
})

# ---- run_final_formatting() end to end ---------------------------------------

make_final_formatting_dirs <- function(kinder) {
  root <- withr::local_tempdir(.local_envir = parent.frame())
  dirs <- file.path(root, c("temp", "clean", "vaxview", "general", "state", "out"))
  names(dirs) <- c("temp", "clean", "vaxview", "general", "state", "out")
  for (d in dirs) dir.create(d)
  saveRDS(kinder, file.path(dirs[["temp"]], "kinder_vaccination_clean_03.rds"))
  # one row per VaxView population so the append step has something to expand
  arrow::write_parquet(
    data.frame(pop = c("child", "teen", "school"), vaccine = "MMR", year = 2020,
               year_type = c("birth", "birth", "school"),
               age_range = c("35 Months", "13-17 Years", "Kindergarten"),
               n = 100, p = 0.9, x = 90, sd = 0.1, se = 0.01),
    file.path(dirs[["vaxview"]], "vax_view.parquet")
  )
  dirs
}

run_quietly <- function(dirs) {
  suppressWarnings(suppressMessages(run_final_formatting(
    state = "xx", vaccine_type_to_keep = "mmr",
    temp_data_dir = dirs[["temp"]], clean_data_dir = dirs[["clean"]],
    vaxview_dir = dirs[["vaxview"]], general_data_dir = dirs[["general"]],
    state_dir = dirs[["state"]], outputs_data_dir = dirs[["out"]]
  )))
}

test_that("run_final_formatting() keeps total_exempt-only rows and drops the rest", {
  skip_if_not_installed("arrow")
  skip_if_not_installed("withr")

  kinder <- data.frame(
    school_id    = 1:5,
    year         = 2020L,
    school_name  = paste("School", 1:5),
    county_name  = "County",
    enrollment   = 100L,
    current      = c(90L, 90L, 90L, 90L, 90L),
    med_exempt   = c(NA, NA, NA, 2L, NA),
    rel_exempt   = c(NA, NA, NA, 3L, NA),
    total_exempt = c(5L, NA, 150L, NA, NA),
    vaccine_type = "mmr",
    stringsAsFactors = FALSE
  )
  # 1: total only          -> kept
  # 2: no exemption at all -> dropped (nothing reported)
  # 3: total > enrollment  -> blanked, then dropped
  # 4: med + rel           -> kept (existing behaviour)
  # 5: nothing             -> dropped
  dirs <- make_final_formatting_dirs(kinder)
  run_quietly(dirs)

  cleaned <- readRDS(file.path(dirs[["clean"]], "cleaned_data.rds"))
  expect_equal(sort(as.integer(cleaned$school_id)), c(1L, 4L))
  expect_true("total_exempt" %in% names(cleaned))
  expect_equal(cleaned$total_exempt[cleaned$school_id == 1], 5L)
})

test_that("run_final_formatting() output is unchanged for states without total_exempt", {
  skip_if_not_installed("arrow")
  skip_if_not_installed("withr")

  kinder <- data.frame(
    school_id = 1:3, year = 2020L, school_name = paste("School", 1:3),
    county_name = "County", enrollment = 100L, current = 90L,
    med_exempt = c(2L, NA, 1L), rel_exempt = c(3L, 1L, 1L),
    vaccine_type = "mmr", stringsAsFactors = FALSE
  )
  dirs <- make_final_formatting_dirs(kinder)
  run_quietly(dirs)

  cleaned <- readRDS(file.path(dirs[["clean"]], "cleaned_data.rds"))
  expect_equal(sort(as.integer(cleaned$school_id)), c(1L, 3L))   # row 2 has NA med_exempt
  expect_false("total_exempt" %in% names(cleaned))
})
