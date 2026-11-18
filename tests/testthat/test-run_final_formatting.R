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

  expect_named(res, c("kinder_dat", "obs", "locations", "obs_populations"))
  expect_true(file.exists(file.path(d$clean, "cleaned_data.rds")))
  expect_true(file.exists(file.path(d$out, "obs.rds")))
  # no per-type subfolders and no stacked file
  expect_length(list.dirs(d$clean, recursive = FALSE), 0L)
  expect_false(file.exists(file.path(d$clean, "cleaned_data_all_types.rds")))

  cleaned <- readRDS(file.path(d$clean, "cleaned_data.rds"))
  expect_equal(nrow(cleaned), 18L)
  # VaxView is appended for a single type, as before
  expect_equal(nrow(readRDS(file.path(d$out, "obs.rds"))), 18L + n_vaxview_obs)
  # ...and expanded child x2, teen x6, school x1 (the fixture's VaxView table has
  # a column called `n`, which must not leak into the expansion)
  expect_equal(nrow(readRDS(file.path(d$out, "obs_populations.rds"))), 18L + n_vaxview_pops)
})

test_that("several vaccine types run separately into subfolders", {
  d <- setup_final_format(make_step03())
  res <- run_ff(d, c("mmr", "all_required"))

  expect_named(res, c("by_type", "cleaned_data_all_types"))
  expect_named(res$by_type, c("mmr", "all_required"))
  for (vt in c("mmr", "all_required")) {
    expect_true(file.exists(file.path(d$clean, vt, "cleaned_data.rds")))
    expect_true(file.exists(file.path(d$out, vt, "obs.rds")))
    expect_true(file.exists(file.path(d$out, vt, "obs_populations.rds")))
    expect_true(file.exists(file.path(d$out, vt, "linkage_key.rds")))
  }
  # nothing is written to the top-level folders by the per-type runs
  expect_false(file.exists(file.path(d$clean, "cleaned_data.rds")))
  expect_false(file.exists(file.path(d$out, "obs.rds")))
})

test_that("the stacked file keeps every series, labelled, with nothing collapsed", {
  d <- setup_final_format(make_step03())
  res <- run_ff(d, c("mmr", "all_required"))

  all_types <- readRDS(file.path(d$clean, "cleaned_data_all_types.rds"))
  expect_true(file.exists(file.path(d$clean, "cleaned_data_all_types.csv")))
  expect_identical(names(all_types)[1], "vaccine_type")
  expect_equal(nrow(all_types), 36L)
  expect_equal(table(all_types$vaccine_type)[["mmr"]], 18L)
  expect_equal(table(all_types$vaccine_type)[["all_required"]], 18L)
  expect_identical(all_types, res$cleaned_data_all_types)

  # the series stayed separate: same school-year, different counts
  s1 <- all_types[all_types$school_id == 1 & all_types$year == 2022, ]
  expect_equal(s1$current[s1$vaccine_type == "mmr"], 61L)          # 65 - 4
  expect_equal(s1$current[s1$vaccine_type == "all_required"], 51L) # 65 - 14
})

test_that("VaxView is added only to MMR series by default with several types", {
  d <- setup_final_format(make_step03())
  run_ff(d, c("mmr", "all_required"))

  expect_equal(nrow(readRDS(file.path(d$out, "mmr", "obs.rds"))),          18L + n_vaxview_obs)
  expect_equal(nrow(readRDS(file.path(d$out, "all_required", "obs.rds"))), 18L)
  expect_equal(nrow(readRDS(file.path(d$out, "mmr", "obs_populations.rds"))), 18L + n_vaxview_pops)
  expect_equal(nrow(readRDS(file.path(d$out, "all_required", "obs_populations.rds"))), 18L)
  # and the linkage key has no VaxView rows for the non-MMR series
  lk <- readRDS(file.path(d$out, "all_required", "linkage_key.rds"))
  expect_equal(sum(is.na(lk$school_id)), 0L)
})

test_that("include_vaxview overrides the default in both directions", {
  d <- setup_final_format(make_step03())
  run_ff(d, c("mmr", "all_required"), include_vaxview = TRUE)
  expect_equal(nrow(readRDS(file.path(d$out, "all_required", "obs.rds"))), 18L + n_vaxview_obs)

  d2 <- setup_final_format(make_step03())
  run_ff(d2, "mmr", include_vaxview = FALSE)
  expect_equal(nrow(readRDS(file.path(d2$out, "obs.rds"))), 18L)
  expect_equal(nrow(readRDS(file.path(d2$out, "obs_populations.rds"))), 18L)

  expect_error(run_ff(d2, "mmr", include_vaxview = NA), "include_vaxview")
})

test_that("NULL processes every vaccine type present", {
  d <- setup_final_format(make_step03(c("mmr", "dtap", "polio")))
  res <- run_ff(d, NULL)
  expect_setequal(names(res$by_type), c("mmr", "dtap", "polio"))
  expect_equal(nrow(res$cleaned_data_all_types), 18L * 3L)
  for (vt in c("mmr", "dtap", "polio")) {
    expect_true(file.exists(file.path(d$clean, vt, "cleaned_data.rds")))
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
