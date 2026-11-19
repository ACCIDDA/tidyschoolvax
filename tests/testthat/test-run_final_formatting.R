# ---- run_final_formatting() ---------------------------------------------------

# Step-03-like data: 6 schools x 3 years x 2 vaccine series, with the second
# series always lower so a mix-up between them would be visible.
make_step03 <- function(types = c("mmr", "all_required")) {
  base <- expand.grid(school_id = 1:6, year = 2022:2024)
  base$school_name <- paste("School", base$school_id)
  base$county_name <- ifelse(base$school_id <= 3, "alpha", "beta")
  base$enrollment  <- 60L + base$school_id * 5L
  base$med_exempt  <- 1L
  base$rel_exempt  <- 2L
  base$state       <- "xx"
  base$lat         <- 40 + base$school_id / 10
  base$lon         <- -80 - base$school_id / 10
  rows <- lapply(seq_along(types), function(i) {
    d <- base
    d$vaccine_type <- types[[i]]
    d$current <- d$enrollment - 4L - 10L * (i - 1L)
    d
  })
  do.call(rbind, rows)
}

make_vaxview <- function() {
  data.frame(
    vaccine   = "MMR",
    age_range = c("24 Months", "13-17 Years", "Kindergarten"),
    pop       = c("child", "teen", "school"),
    year      = 2023L,
    n         = 500L,
    p         = 0.9,
    x         = 450,
    sd        = 0.01,
    se        = 0.01,
    stringsAsFactors = FALSE
  )
}

# Build a fresh set of directories and write the inputs; returns their paths.
setup_final_format <- function(step03) {
  skip_if_not_installed("arrow")
  root <- tempfile("final_format_")
  d <- list(
    temp = file.path(root, "temp"), clean = file.path(root, "clean"),
    out = file.path(root, "out"), state = file.path(root, "state"),
    vaxview = file.path(root, "vaxview"), general = file.path(root, "general")
  )
  for (x in d) dir.create(x, recursive = TRUE)
  saveRDS(step03, file.path(d$temp, "kinder_vaccination_clean_03.rds"))
  arrow::write_parquet(make_vaxview(), file.path(d$vaxview, "vax_view.parquet"))
  d
}

run_ff <- function(d, vaccine_type_to_keep, ...) {
  suppressWarnings(suppressMessages(
    run_final_formatting(
      state = "xx", vaccine_type_to_keep = vaccine_type_to_keep,
      temp_data_dir = d$temp, clean_data_dir = d$clean, vaxview_dir = d$vaxview,
      general_data_dir = d$general, state_dir = d$state, outputs_data_dir = d$out,
      ...
    )
  ))
}

# VaxView expansion: child -> 2 obs_populations rows, teen -> 6, school -> 1, but
# one obs row each, so the 3 VaxView rows are appended to obs as 3 rows.
n_vaxview_obs <- 3L
n_vaxview_pops <- 2L + 6L + 1L

test_that("a single vaccine type keeps the previous layout and behaviour", {
  d <- setup_final_format(make_step03())
  res <- run_ff(d, "mmr")

  expect_named(res, c("kinder_dat", "cleaned_data", "obs", "locations", "obs_populations",
                      "excluded_rows", "years_by_school"))
  expect_true(file.exists(file.path(d$clean, "cleaned_data.csv")))
  expect_true(file.exists(file.path(d$out, "obs.parquet")))
  # no per-type subfolders and no stacked file
  expect_length(list.dirs(d$clean, recursive = FALSE), 0L)
  expect_false(file.exists(file.path(d$clean, "cleaned_data_all_types.csv")))

  cleaned <- read_cleaned_data(file.path(d$clean, "cleaned_data.csv"))
  expect_equal(nrow(cleaned), 18L)
  # VaxView is appended for a single type, as before
  expect_equal(nrow(as.data.frame(arrow::read_parquet(file.path(d$out, "obs.parquet")))), 18L + n_vaxview_obs)
  # ...and expanded child x2, teen x6, school x1 (the fixture's VaxView table has
  # a column called `n`, which must not leak into the expansion)
  expect_equal(nrow(as.data.frame(arrow::read_parquet(file.path(d$out, "obs_populations.parquet")))), 18L + n_vaxview_pops)
})

test_that("several vaccine types run separately into subfolders", {
  d <- setup_final_format(make_step03())
  res <- run_ff(d, c("mmr", "all_required"))

  expect_named(res, c("by_type", "cleaned_data_all_types", "excluded_rows_all_types",
                      "years_by_school"))
  expect_named(res$by_type, c("mmr", "all_required"))
  for (vt in c("mmr", "all_required")) {
    expect_true(file.exists(file.path(d$clean, vt, "cleaned_data.csv")))
    expect_true(file.exists(file.path(d$out, vt, "obs.parquet")))
    expect_true(file.exists(file.path(d$out, vt, "obs_populations.parquet")))
    expect_true(file.exists(file.path(d$out, vt, "linkage_key.parquet")))
  }
  # nothing is written to the top-level folders by the per-type runs
  expect_false(file.exists(file.path(d$clean, "cleaned_data.csv")))
  expect_false(file.exists(file.path(d$out, "obs.parquet")))
})

test_that("the stacked file keeps every series, labelled, with nothing collapsed", {
  d <- setup_final_format(make_step03())
  res <- run_ff(d, c("mmr", "all_required"))

  all_types <- read_cleaned_data(file.path(d$clean, "cleaned_data_all_types.csv"))
  expect_true(file.exists(file.path(d$clean, "cleaned_data_all_types.csv")))
  expect_identical(names(all_types)[1], "vaccine_type")
  expect_equal(nrow(all_types), 36L)
  expect_equal(table(all_types$vaccine_type)[["mmr"]], 18L)
  expect_equal(table(all_types$vaccine_type)[["all_required"]], 18L)
  expect_equal(as.data.frame(all_types), as.data.frame(res$cleaned_data_all_types),
               ignore_attr = TRUE)

  # the series stayed separate: same school-year, different counts
  s1 <- all_types[all_types$school_id == 1 & all_types$year == 2022, ]
  expect_equal(s1$current[s1$vaccine_type == "mmr"], 61L)          # 65 - 4
  expect_equal(s1$current[s1$vaccine_type == "all_required"], 51L) # 65 - 14
})

test_that("VaxView is added only to MMR series by default with several types", {
  d <- setup_final_format(make_step03())
  run_ff(d, c("mmr", "all_required"))

  expect_equal(nrow(as.data.frame(arrow::read_parquet(file.path(d$out, "mmr", "obs.parquet")))),          18L + n_vaxview_obs)
  expect_equal(nrow(as.data.frame(arrow::read_parquet(file.path(d$out, "all_required", "obs.parquet")))), 18L)
  expect_equal(nrow(as.data.frame(arrow::read_parquet(file.path(d$out, "mmr", "obs_populations.parquet")))), 18L + n_vaxview_pops)
  expect_equal(nrow(as.data.frame(arrow::read_parquet(file.path(d$out, "all_required", "obs_populations.parquet")))), 18L)
  # and the linkage key has no VaxView rows for the non-MMR series
  lk <- as.data.frame(arrow::read_parquet(file.path(d$out, "all_required", "linkage_key.parquet")))
  expect_equal(sum(is.na(lk$school_id)), 0L)
})

test_that("include_vaxview overrides the default in both directions", {
  d <- setup_final_format(make_step03())
  run_ff(d, c("mmr", "all_required"), include_vaxview = TRUE)
  expect_equal(nrow(as.data.frame(arrow::read_parquet(file.path(d$out, "all_required", "obs.parquet")))), 18L + n_vaxview_obs)

  d2 <- setup_final_format(make_step03())
  run_ff(d2, "mmr", include_vaxview = FALSE)
  expect_equal(nrow(as.data.frame(arrow::read_parquet(file.path(d2$out, "obs.parquet")))), 18L)
  expect_equal(nrow(as.data.frame(arrow::read_parquet(file.path(d2$out, "obs_populations.parquet")))), 18L)

  expect_error(run_ff(d2, "mmr", include_vaxview = NA), "include_vaxview")
})

test_that("NULL processes every vaccine type present", {
  d <- setup_final_format(make_step03(c("mmr", "dtap", "polio")))
  res <- run_ff(d, NULL)
  expect_setequal(names(res$by_type), c("mmr", "dtap", "polio"))
  expect_equal(nrow(res$cleaned_data_all_types), 18L * 3L)
  for (vt in c("mmr", "dtap", "polio")) {
    expect_true(file.exists(file.path(d$clean, vt, "cleaned_data.csv")))
  }
})

test_that("vaccine type matching is case-insensitive and de-duplicated", {
  d <- setup_final_format(make_step03())
  res <- run_ff(d, c("MMR", "mmr", "All_Required"))
  expect_named(res$by_type, c("mmr", "all_required"))
})

test_that("unknown vaccine types are skipped with a warning, or error if none match", {
  d <- setup_final_format(make_step03())
  expect_warning(
    suppressMessages(run_final_formatting(
      "xx", c("mmr", "tdap"), d$temp, d$clean, d$vaxview, d$general, d$state, d$out
    )),
    "not in the data and skipped: tdap"
  )
  expect_error(run_ff(d, c("tdap", "hepb")), "None of the requested vaccine types")
})

test_that("several types need a vaccine_type column", {
  step03 <- make_step03()
  step03$vaccine_type <- NULL
  d <- setup_final_format(step03)
  expect_error(run_ff(d, NULL), "vaccine_type")
  expect_error(run_ff(d, c("mmr", "dtap")), "vaccine_type")
})

test_that(".vaccine_type_dirnames() makes safe, unique folder names", {
  expect_equal(.vaccine_type_dirnames(c("MMR", "All Required", "dtap/5")),
               c("mmr", "all_required", "dtap_5"))
  expect_equal(.vaccine_type_dirnames(c("a b", "a-b")), c("a_b", "a_b_1"))
  expect_equal(.vaccine_type_dirnames("--"), "type")
})


# ---- excluded rows --------------------------------------------------------------

# One row per exclusion reason, all school-years that otherwise look fine.
make_problem_rows <- function(type = "mmr") {
  base <- data.frame(
    school_id = 7:12, year = 2022L,
    school_name = paste("Problem", 7:12), county_name = "alpha",
    enrollment = 70L, current = 60L, med_exempt = 1L, rel_exempt = 2L,
    state = "xx", lat = 41, lon = -81, vaccine_type = type, stringsAsFactors = FALSE
  )
  base$enrollment[1] <- NA                  # 7: missing enrollment
  base$enrollment[2] <- 0L                  # 8: zero enrollment
  base$current[3]    <- 99L                 # 9: vaccinated count above enrollment
  base$med_exempt[4] <- 80L                 # 10: exemption above enrollment
  base$current[5]    <- NA                  # 11: missing count
  base$current[6]    <- 0L                  # 12: zero vaccinated
  base
}

test_that("every row left out of cleaned_data is saved with its reason", {
  d <- setup_final_format(rbind(make_step03(), make_problem_rows()))
  res <- run_ff(d, c("mmr", "all_required"))

  all_ex <- as.data.frame(readr::read_csv(file.path(d$clean, "cleaned_data_excluded_rows_all_types.csv"), show_col_types = FALSE))
  ex <- all_ex[all_ex$vaccine_type == "mmr", ]
  expect_equal(nrow(ex), 6L)
  expect_identical(names(ex)[1:4], c("vaccine_type", "exclusion_reason",
                                     "exclusion_description", "exclusion_detail"))
  expect_equal(
    setNames(ex$exclusion_reason, ex$school_id),
    c("7" = "missing_enrollment", "8" = "zero_enrollment", "9" = "invalid_count",
      "10" = "invalid_count", "11" = "missing_count", "12" = "zero_vaccinated")
  )
  expect_false(anyNA(ex$exclusion_description))
  # the original values are kept, not the blanked ones
  expect_equal(ex$current[ex$school_id == 9], 99L)
  expect_equal(ex$med_exempt[ex$school_id == 10], 80L)
  # the detail names which count was blanked
  expect_equal(ex$exclusion_detail[ex$school_id == 9],  "blanked as impossible: current")
  # an impossible exemption blanks both exemption counts
  expect_equal(ex$exclusion_detail[ex$school_id == 10], "blanked as impossible: med_exempt; rel_exempt")
  expect_true(is.na(ex$exclusion_detail[ex$school_id == 11]))
  expect_false(".row_id" %in% names(ex))
})

test_that("states with only total_exempt keep those rows and label exclusions correctly", {
  step03 <- make_step03("mmr")
  step03$med_exempt <- NA_integer_
  step03$rel_exempt <- NA_integer_
  step03$total_exempt <- 3L
  extra <- make_problem_rows()[c(3, 5), ]          # school ids 9 and 11
  extra$med_exempt <- NA_integer_
  extra$rel_exempt <- NA_integer_
  extra$total_exempt <- c(150L, NA)                 # 9: total above enrollment; 11: nothing reported
  extra$current <- 60L
  d <- setup_final_format(dplyr::bind_rows(step03, extra))
  res <- run_ff(d, "mmr")

  expect_equal(nrow(res$kinder_dat), nrow(step03))        # every total-only row kept
  expect_true("total_exempt" %in% names(res$kinder_dat))
  ex <- res$excluded_rows
  expect_equal(
    setNames(ex$exclusion_reason, ex$school_id),
    c("9" = "invalid_count", "11" = "missing_count")
  )
  expect_equal(ex$exclusion_detail[ex$school_id == 9], "blanked as impossible: total_exempt")
})

test_that("kept rows plus excluded rows account for every input row", {
  d <- setup_final_format(rbind(make_step03(), make_problem_rows()))
  res <- run_ff(d, c("mmr", "all_required"))
  for (vt in c("mmr", "all_required")) {
    kept <- read_cleaned_data(file.path(d$clean, vt, "cleaned_data.csv"))
    ex   <- as.data.frame(readr::read_csv(file.path(d$clean, "cleaned_data_excluded_rows_all_types.csv"), show_col_types = FALSE))
    ex   <- ex[ex$vaccine_type == vt, ]
    n_in <- sum(readRDS(file.path(d$temp, "kinder_vaccination_clean_03.rds"))$vaccine_type == vt)
    expect_equal(nrow(kept) + nrow(ex), n_in, info = vt)
  }
})

test_that("a series with nothing to exclude gets an empty excluded-rows file", {
  d <- setup_final_format(make_step03())
  res <- run_ff(d, "mmr")
  expect_equal(nrow(res$excluded_rows), 0L)
  expect_true(all(c("exclusion_reason", "exclusion_description") %in% names(res$excluded_rows)))
  expect_true(file.exists(file.path(d$clean, "cleaned_data_excluded_rows.csv")))
})

test_that("excluded rows are stacked across vaccine types", {
  prob <- rbind(make_problem_rows("mmr"), make_problem_rows("all_required"))
  d <- setup_final_format(rbind(make_step03(), prob))
  res <- run_ff(d, c("mmr", "all_required"))

  all_ex <- as.data.frame(readr::read_csv(file.path(d$clean, "cleaned_data_excluded_rows_all_types.csv"), show_col_types = FALSE))
  expect_true(file.exists(file.path(d$clean, "cleaned_data_excluded_rows_all_types.csv")))
  expect_equal(nrow(all_ex), 12L)
  expect_equal(as.vector(table(all_ex$vaccine_type)[c("mmr", "all_required")]), c(6L, 6L))
  # the CSV is for reading, so compare values, not column types
  expect_equal(names(all_ex), names(res$excluded_rows_all_types))
  expect_equal(all_ex$exclusion_reason, res$excluded_rows_all_types$exclusion_reason)
})

test_that("the existing DQA exports do not gain the internal row id", {
  d <- setup_final_format(rbind(make_step03(), make_problem_rows()))
  run_ff(d, "mmr")
  for (f in c("xx_zero_or_missing_enrollment_removed.csv", "xx_invalid_values_fixed.csv")) {
    expect_true(file.exists(file.path(d$temp, f)), info = f)
    expect_false(".row_id" %in% names(utils::read.csv(file.path(d$temp, f))), info = f)
  }
})


# ---- summarise_years_by_school() --------------------------------------------------

test_that("summarise_years_by_school() counts rows per school and year", {
  d <- data.frame(school_id = c(1, 1, 1, 2, 2), year = c(2021, 2022, 2023, 2021, 2023),
                  school_name = c("a", "a", "a", "b", "b"), county_name = "c")
  y <- summarise_years_by_school(d)
  expect_equal(nrow(y), 2L)
  expect_equal(y[["2021"]], c(1, 1)); expect_equal(y[["2022"]], c(1, 0)); expect_equal(y[["2023"]], c(1, 1))
  expect_equal(y$n_years, c(3, 2)); expect_equal(y$n_rows, c(3, 2))
  expect_identical(names(y), c("school_id", "school_name", "county_name", "vaccine_type",
                               "2021", "2022", "2023", "n_years", "n_rows",
                               "has_duplicate_years", "has_missing_years", "years_missing"))
})

test_that("summarise_years_by_school() flags duplicate and missing years", {
  d <- data.frame(school_id = c(1, 1, 1, 2, 2), year = c(2021, 2021, 2022, 2021, 2023))
  y <- summarise_years_by_school(d)
  y1 <- y[y$school_id == 1, ]; y2 <- y[y$school_id == 2, ]
  expect_true(y1$has_duplicate_years);  expect_false(y2$has_duplicate_years)
  expect_equal(y1[["2021"]], 2)
  # school 1 has no 2023 row, school 2 has no 2022 row
  expect_true(y1$has_missing_years); expect_equal(y1$years_missing, "2023")
  expect_true(y2$has_missing_years); expect_equal(y2$years_missing, "2022")
})

test_that("summarise_years_by_school() has no missing years when every year is present", {
  y <- summarise_years_by_school(data.frame(school_id = c(1, 1, 2, 2), year = c(2021, 2022, 2021, 2022)))
  expect_false(any(y$has_missing_years)); expect_true(all(is.na(y$years_missing)))
  expect_false(any(y$has_duplicate_years))
})

test_that("available years are judged per vaccine type", {
  d <- data.frame(school_id = c(1, 1, 1, 2, 2), year = c(2022, 2023, 2023, 2023, 2023),
                  vaccine_type = c("all", "all", "mmr", "mmr", "mmr"))
  y <- summarise_years_by_school(d)
  expect_equal(nrow(y), 3L)  # school 1 x all, school 1 x mmr, school 2 x mmr
  # mmr only exists in 2023, so no school is missing a year there
  expect_false(any(y$has_missing_years[y$vaccine_type == "mmr"]))
  # the "all" series only has school 1 -> not missing either
  expect_false(any(y$has_missing_years[y$vaccine_type == "all"]))
  expect_true(y$has_duplicate_years[y$vaccine_type == "mmr" & y$school_id == 2])
})

test_that("summarise_years_by_school() takes descriptors from each school's latest row", {
  d <- data.frame(school_id = c(1, 1), year = c(2021, 2023), school_name = c("old", "new"),
                  city = c("x", "y"))
  y <- summarise_years_by_school(d)
  expect_equal(y$school_name, "new"); expect_equal(y$city, "y")
})

test_that("summarise_years_by_school() labels the series and drops rows without a school_id", {
  d <- data.frame(school_id = c(1, NA), year = c(2021, 2021))
  expect_message(y <- summarise_years_by_school(d, vaccine_type = "mmr"), "missing school_id")
  expect_equal(nrow(y), 1L); expect_equal(y$vaccine_type, "mmr")
  expect_error(summarise_years_by_school(data.frame(school_id = 1)), "year")
})

test_that("years-by-school is written by run_final_formatting()", {
  d <- setup_final_format(make_step03())
  res <- run_ff(d, c("mmr", "all_required"))
  y <- as.data.frame(readr::read_csv(file.path(d$clean, "cleaned_data_years_by_school.csv"), show_col_types = FALSE))
  expect_true(file.exists(file.path(d$clean, "cleaned_data_years_by_school.csv")))
  expect_equal(nrow(y), 12L)  # 6 schools x 2 series
  expect_equal(sum(y$n_rows), 36L)
  # the CSV is for reading, so compare values, not column types
  expect_equal(names(y), names(res$years_by_school))
  expect_equal(y$n_rows, res$years_by_school$n_rows)
  expect_false(file.exists(file.path(d$clean, "mmr", "cleaned_data_years_by_school.csv")))
})


test_that("each table is written once, in the format the policy assigns it", {
  d <- setup_final_format(make_step03())
  run_ff(d, "mmr")

  # final, human-readable tables: CSV only
  for (f in c("cleaned_data", "cleaned_data_excluded_rows", "cleaned_data_years_by_school")) {
    expect_true(file.exists(file.path(d$clean, paste0(f, ".csv"))), info = f)
    expect_false(file.exists(file.path(d$clean, paste0(f, ".rds"))), info = f)
  }
  # model inputs: Parquet only
  for (f in c("obs", "locations", "obs_populations", "linkage_key")) {
    expect_true(file.exists(file.path(d$out, paste0(f, ".parquet"))), info = f)
    expect_false(file.exists(file.path(d$out, paste0(f, ".csv"))), info = f)
    expect_false(file.exists(file.path(d$out, paste0(f, ".rds"))), info = f)
  }
  # no leftover step copies
  expect_false(any(grepl("^kinder_vaccination_clean\\.|^dqa_report", list.files(d$temp))))
})

test_that("final_format and model_input_format switch the formats", {
  d <- setup_final_format(make_step03())
  run_ff(d, "mmr", final_format = "parquet", model_input_format = "csv")

  expect_true(file.exists(file.path(d$clean, "cleaned_data.parquet")))
  expect_false(file.exists(file.path(d$clean, "cleaned_data.csv")))
  expect_true(file.exists(file.path(d$out, "obs.csv")))
  expect_false(file.exists(file.path(d$out, "obs.parquet")))
  expect_error(run_ff(d, "mmr", final_format = "xlsx"), "should be one of")
})

test_that("a step-03 file written as Parquet is read as well as an RDS one", {
  step03 <- make_step03()
  d <- setup_final_format(step03)
  unlink(file.path(d$temp, "kinder_vaccination_clean_03.rds"))
  write_pipeline_table(step03, file.path(d$temp, "kinder_vaccination_clean_03"), "parquet")
  run_ff(d, "mmr")
  expect_true(file.exists(file.path(d$clean, "cleaned_data.csv")))
})

test_that("stacked all-types tables follow final_format", {
  d <- setup_final_format(make_step03())
  run_ff(d, c("mmr", "all_required"))
  expect_true(file.exists(file.path(d$clean, "cleaned_data_all_types.csv")))
  expect_false(file.exists(file.path(d$clean, "cleaned_data_all_types.rds")))
  expect_true(file.exists(file.path(d$clean, "cleaned_data_excluded_rows_all_types.csv")))
})


test_that("review_dir collects every data-quality flag file in one place", {
  d <- setup_final_format(rbind(make_step03(), make_problem_rows()))
  review <- file.path(dirname(d$out), "review")
  run_ff(d, "mmr", review_dir = review)
  flags <- list.files(review, pattern = "\\.csv$")
  expect_true(length(flags) > 0)
  # the flags that used to go to temp_data_dir and state_dir are not left behind
  expect_equal(length(list.files(d$state, pattern = "\\.csv$")), 0L)
  expect_false(any(grepl("_check\\.csv$|_removed\\.csv$|_fixed\\.csv$", list.files(d$temp))))
})

test_that("with several types the flags go to review_dir/<type>/", {
  d <- setup_final_format(make_step03())
  review <- file.path(dirname(d$out), "review")
  run_ff(d, c("mmr", "all_required"), review_dir = review)
  expect_true(dir.exists(file.path(review, "mmr")))
  expect_true(dir.exists(file.path(review, "all_required")))
})

test_that("series_subfolder gives a single series the same layout as several", {
  d <- setup_final_format(make_step03())
  res <- run_ff(d, "mmr", series_subfolder = TRUE)
  expect_true(file.exists(file.path(d$clean, "mmr", "cleaned_data.csv")))
  expect_true(file.exists(file.path(d$out, "mmr", "obs.parquet")))
  expect_false(file.exists(file.path(d$clean, "cleaned_data.csv")))
  # one type: nothing to stack, but excluded rows and years are still written once
  expect_false(file.exists(file.path(d$clean, "cleaned_data_all_types.csv")))
  expect_true(file.exists(file.path(d$clean, "cleaned_data_excluded_rows_all_types.csv")))
  expect_true(file.exists(file.path(d$clean, "cleaned_data_years_by_school.csv")))
  expect_named(res, c("by_type", "cleaned_data_all_types", "excluded_rows_all_types", "years_by_school"))
})

test_that("series_subfolder works for a state whose data has no vaccine_type column", {
  step03 <- make_step03()
  step03 <- step03[step03$vaccine_type == "mmr", setdiff(names(step03), "vaccine_type")]
  d <- setup_final_format(step03)
  run_ff(d, "mmr", series_subfolder = TRUE)
  expect_true(file.exists(file.path(d$clean, "mmr", "cleaned_data.csv")))
})

test_that("several types do not write per-series excluded-rows or years files", {
  d <- setup_final_format(make_step03())
  run_ff(d, c("mmr", "all_required"))
  expect_false(file.exists(file.path(d$clean, "mmr", "cleaned_data_excluded_rows.csv")))
  expect_false(file.exists(file.path(d$clean, "mmr", "cleaned_data_years_by_school.csv")))
  expect_true(file.exists(file.path(d$clean, "cleaned_data_excluded_rows_all_types.csv")))
  expect_true(file.exists(file.path(d$clean, "cleaned_data_years_by_school.csv")))
})

test_that("with review_dir no empty per-series temp or state folders are created", {
  d <- setup_final_format(make_step03())
  review <- file.path(dirname(d$out), "review")
  run_ff(d, c("mmr", "all_required"), review_dir = review)
  expect_false(dir.exists(file.path(d$temp, "mmr")))
  expect_false(dir.exists(file.path(d$state, "mmr")))
})
