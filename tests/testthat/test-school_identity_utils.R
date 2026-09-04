# Tests for the Google-identity-based rename resolution added on top of the
# matching cascade: run_matching_cascade() (matching_schools_utils.R),
# .run_exact_join_pass() (matching_schools_utils.R), and
# augment_with_google_name_variant() / log_school_renames()
# (school_identity_utils.R).
#
# These exercise only the pure data-transformation logic — no live Google API
# calls are made anywhere in this file (resolve_school_place_ids() and
# add_google_name_variants(), which do call out to run_full_geocoding() /
# run_school_status_with_cache(), are intentionally not unit-tested here;
# place_id / google name columns are supplied directly on synthetic data,
# the same way other tests in this suite mock inputs rather than dependencies).

make_school_df <- function(names, counties, source_id_col = "data1_id", ...) {
  n <- length(names)
  df <- data.frame(
    school_name     = names,
    school_name_std = tidyschoolvax:::standardized_school_name(names),
    county          = counties,
    county_std      = tolower(counties),
    school_level    = rep("elementary", n),
    school_type     = rep("public", n),
    stringsAsFactors = FALSE
  )
  df[[source_id_col]] <- seq_len(n)
  extra <- list(...)
  for (nm in names(extra)) df[[nm]] <- extra[[nm]]
  df
}

# ---- .run_exact_join_pass ----------------------------------------------------

test_that(".run_exact_join_pass links rows sharing a place_id", {
  d1 <- make_school_df("Old Name Elementary", "Alpha County", "data1_id",
                       place_id = "ChIJ111")
  d2 <- make_school_df("New Name Elementary", "Alpha County", "data2_id",
                       place_id = "ChIJ111")

  res <- tidyschoolvax:::.run_exact_join_pass(d1, d2, join_col = "place_id")

  expect_equal(nrow(res$matched), 1L)
  expect_equal(res$matched$match_score, 1)
  expect_equal(res$matched$data1_id, 1L)
  expect_equal(res$matched$data2_id, 1L)
  expect_equal(res$matched$school_name_std_data1, "old name elementary")
  expect_equal(res$matched$school_name_std_data2, "new name elementary")
  expect_equal(nrow(res$unmatched_dat1), 0L)
})

test_that(".run_exact_join_pass leaves rows with no matching place_id unmatched", {
  d1 <- make_school_df("Some School", "Alpha County", "data1_id", place_id = "ChIJ111")
  d2 <- make_school_df("Other School", "Alpha County", "data2_id", place_id = "ChIJ222")

  res <- tidyschoolvax:::.run_exact_join_pass(d1, d2, join_col = "place_id")

  expect_equal(nrow(res$matched), 0L)
  expect_equal(nrow(res$unmatched_dat1), 1L)
})

test_that(".run_exact_join_pass is a no-op when join_col is absent from either side", {
  d1 <- make_school_df("Some School", "Alpha County", "data1_id")
  d2 <- make_school_df("Some School", "Alpha County", "data2_id")

  res <- tidyschoolvax:::.run_exact_join_pass(d1, d2, join_col = "place_id")

  expect_equal(nrow(res$matched), 0L)
  expect_equal(nrow(res$unmatched_dat1), 1L)
})

test_that(".run_exact_join_pass ignores NA/empty place_id values", {
  d1 <- make_school_df(c("A School", "B School"), c("Alpha County", "Alpha County"),
                       "data1_id", place_id = c(NA_character_, ""))
  d2 <- make_school_df("A School", "Alpha County", "data2_id", place_id = "ChIJ111")

  res <- tidyschoolvax:::.run_exact_join_pass(d1, d2, join_col = "place_id")

  expect_equal(nrow(res$matched), 0L)
  expect_equal(nrow(res$unmatched_dat1), 2L)
})

# ---- run_matching_cascade: parity with a plain fuzzy pass --------------------

test_that("run_matching_cascade with a single fuzzy pass matches match_schools_names directly", {
  d1 <- make_school_df("Lincoln Elementary", "Alpha County", "data1_id")
  d2 <- make_school_df(c("Lincoln Elementary", "Jefferson Middle"),
                       c("Alpha County", "Alpha County"), "data2_id")

  direct <- match_schools_names(d1, d2)
  cascade <- run_matching_cascade(
    d1, d2,
    passes = list(list(label = "county", match_cols1 = "county_std", match_cols2 = "county_std",
                       threshold_jw = 0.6, threshold_jw_min = 0.6, exact_jw = 0.05))
  )

  expect_equal(nrow(cascade$matched), nrow(direct$matched))
  expect_equal(cascade$matched$data1_id, direct$matched$data1_id)
  expect_equal(cascade$matched$data2_id, direct$matched$data2_id)
  expect_equal(cascade$matched$match_method, "county")
})

test_that("run_matching_cascade only sends still-unmatched rows to later passes", {
  # Two data1 rows: one matches cleanly on an exact-name pass, the other only
  # matches on a looser fallback pass. The cascade should resolve both without
  # the second pass re-matching (or re-attempting) the first row.
  d1 <- rbind(
    make_school_df("Lincoln Elementary", "Alpha County", "data1_id"),
    make_school_df("Weshington Primry",  "Alpha County", "data1_id")
  )
  d1$data1_id <- seq_len(nrow(d1))
  d2 <- rbind(
    make_school_df("Lincoln Elementary", "Alpha County", "data2_id"),
    make_school_df("Washington Primary", "Alpha County", "data2_id")
  )
  d2$data2_id <- seq_len(nrow(d2))

  passes <- list(
    list(label = "tight",  match_cols1 = "county_std", match_cols2 = "county_std",
        threshold_jw = 0.05, threshold_jw_min = 0.05, exact_jw = 0.02),
    list(label = "loose",  match_cols1 = "county_std", match_cols2 = "county_std",
        threshold_jw = 0.6,  threshold_jw_min = 0.6,  exact_jw = 0.05)
  )
  res <- run_matching_cascade(d1, d2, passes = passes)

  expect_equal(nrow(res$matched), 2L)
  expect_equal(sort(res$matched$match_method), c("loose", "tight"))
  expect_equal(nrow(res$unmatched_dat1), 0L)
  # Each data1 row should appear in the matched output exactly once.
  expect_equal(sort(res$matched$data1_id), c(1L, 2L))
})

test_that("run_matching_cascade skips a pass when its condition is FALSE", {
  d1 <- make_school_df("Lincoln Elementary", "Alpha County", "data1_id")
  d2 <- make_school_df("Lincoln Elementary", "Alpha County", "data2_id")

  passes <- list(
    list(label = "never", match_cols1 = "county_std", match_cols2 = "county_std",
        threshold_jw = 0.6, threshold_jw_min = 0.6, exact_jw = 0.05,
        condition = function(d1, d2) FALSE),
    list(label = "county", match_cols1 = "county_std", match_cols2 = "county_std",
        threshold_jw = 0.6, threshold_jw_min = 0.6, exact_jw = 0.05)
  )
  res <- run_matching_cascade(d1, d2, passes = passes)

  expect_equal(nrow(res$matched), 1L)
  expect_equal(res$matched$match_method, "county")
})

# ---- run_matching_cascade: the actual rename-bridging behavior ---------------

test_that("a place_id Pass 0 links a renamed school that fuzzy matching alone would miss", {
  # Same physical school, completely different name string between the two
  # sides (as happens across a rename) — too different for any reasonable
  # Jaro-Winkler threshold, but they share a Google place_id.
  d1 <- make_school_df("Robert E Lee Elementary", "Alpha County", "data1_id",
                       place_id = "ChIJ999")
  d2 <- make_school_df("Northside Elementary",    "Alpha County", "data2_id",
                       place_id = "ChIJ999")

  fuzzy_only <- match_schools_names(d1, d2)
  expect_equal(nrow(fuzzy_only$matched), 0L)   # confirms names are indeed too different

  passes <- list(
    list(label = "place_id", type = "exact_join", join_col = "place_id"),
    list(label = "county", match_cols1 = "county_std", match_cols2 = "county_std",
        threshold_jw = 0.6, threshold_jw_min = 0.6, exact_jw = 0.05)
  )
  cascade <- run_matching_cascade(d1, d2, passes = passes)

  expect_equal(nrow(cascade$matched), 1L)
  expect_equal(cascade$matched$match_method, "place_id")
  expect_equal(nrow(cascade$unmatched_dat1), 0L)
})

# ---- augment_with_google_name_variant -----------------------------------------

test_that("augment_with_google_name_variant adds a synonym row only when names differ", {
  ref <- data.frame(
    data2_id                = 1:2,
    school_name_std         = c("old name elementary", "same name elementary"),
    school_name_std_google  = c("new name elementary",  "same name elementary"),
    stringsAsFactors        = FALSE
  )

  augmented <- augment_with_google_name_variant(ref)

  expect_equal(nrow(augmented), 3L)   # one extra row for the school that changed
  expect_true("new name elementary" %in% augmented$school_name_std)
  expect_true("old name elementary" %in% augmented$school_name_std)
  # The synonym row keeps the same data2_id as its original — whichever name
  # variant wins a fuzzy match, it must resolve to the same reference school.
  expect_equal(
    augmented$data2_id[augmented$school_name_std == "new name elementary"],
    1L
  )
})

test_that("augment_with_google_name_variant is a no-op without a google-name column", {
  ref <- data.frame(data2_id = 1L, school_name_std = "some school", stringsAsFactors = FALSE)
  expect_identical(augment_with_google_name_variant(ref), ref)
})

test_that("a Google-name synonym row lets a kinder record match the reference by its new name", {
  # The kinder record already uses the NEW name (as it would post-rename), but
  # the reference source hasn't been re-matched by name — it only carries the
  # OLD name plus a resolved Google current name. Augmenting the reference
  # table should let this match without any place_id on either side.
  kinder <- make_school_df("New Name Elementary", "Alpha County", "data1_id")
  ref    <- make_school_df("Old Name Elementary",  "Alpha County", "data2_id")
  ref$school_name_std_google <- "new name elementary"

  unaugmented <- match_schools_names(kinder, ref)
  expect_equal(nrow(unaugmented$matched), 0L)

  ref_aug <- augment_with_google_name_variant(ref)
  augmented_result <- match_schools_names(kinder, ref_aug)

  expect_equal(nrow(augmented_result$matched), 1L)
  expect_equal(augmented_result$matched$data2_id, 1L)
})

# ---- log_school_renames --------------------------------------------------------

test_that("log_school_renames filters to mismatched-name pairs and writes a CSV", {
  matched_df <- data.frame(
    school_name_std_data1 = c("old name elementary", "same school"),
    school_name_std_data2 = c("new name elementary", "same school"),
    match_method           = c("place_id", "county"),
    match_score             = c(1, 0.9),
    county_std               = c("alpha county", "alpha county"),
    stringsAsFactors = FALSE
  )

  out_dir <- tempfile("rename_report_", tmpdir = tempdir())
  dir.create(out_dir)
  on.exit(unlink(out_dir, recursive = TRUE), add = TRUE)

  res <- suppressMessages(log_school_renames(matched_df, out_dir = out_dir))

  expect_equal(nrow(res), 1L)
  expect_equal(res$school_name_std_data1, "old name elementary")
  expect_true(file.exists(file.path(out_dir, "school_renames_detected.csv")))
})

test_that("log_school_renames returns an empty result and writes nothing when no renames found", {
  matched_df <- data.frame(
    school_name_std_data1 = "same school",
    school_name_std_data2 = "same school",
    match_method           = "county",
    stringsAsFactors = FALSE
  )

  out_dir <- tempfile("rename_report_empty_", tmpdir = tempdir())
  dir.create(out_dir)
  on.exit(unlink(out_dir, recursive = TRUE), add = TRUE)

  res <- suppressMessages(log_school_renames(matched_df, out_dir = out_dir))

  expect_equal(nrow(res), 0L)
  expect_false(file.exists(file.path(out_dir, "school_renames_detected.csv")))
})

test_that("log_school_renames handles a matched_df missing the required columns gracefully", {
  res <- log_school_renames(data.frame(x = 1), out_dir = tempdir())
  expect_equal(nrow(res), 0L)
})
