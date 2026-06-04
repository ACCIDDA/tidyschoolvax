# Tests for match_schools_batch_cpp() (C++ core) and the updated
# match_schools_names() wrapper.

# ---- match_schools_batch_cpp: basic contract --------------------------------

test_that("match_schools_batch_cpp returns the right structure", {
  res <- tidyschoolvax:::match_schools_batch_cpp(
    names1           = c("lincoln elementary", "washington high"),
    groups1          = c("alpha county", "beta county"),
    names2           = c("lincoln elementary", "jefferson middle", "washington high school"),
    groups2          = c("alpha county", "alpha county", "beta county"),
    threshold_jw_min = 0.6,
    threshold_jw     = 0.6,
    exact_jw         = 0.05
  )

  expect_named(res, c("status", "candidates_idx", "candidates_jw",
                       "candidates_soundex", "candidates_cosine",
                       "candidates_osa", "candidates_qgram", "candidates_jaccard"))
  expect_length(res$status, 2L)
  expect_length(res$candidates_idx, 2L)
})

test_that("exact match produces status 2 and correct candidate index", {
  res <- tidyschoolvax:::match_schools_batch_cpp(
    names1           = "lincoln elementary",
    groups1          = "alpha county",
    names2           = c("lincoln elementary", "jefferson middle"),
    groups2          = c("alpha county", "alpha county"),
    threshold_jw_min = 0.6,
    threshold_jw     = 0.6,
    exact_jw         = 0.05
  )

  expect_equal(res$status[1], 2L)
  expect_equal(res$candidates_idx[[1]], 1L)          # row 1 of data2
  expect_lt(res$candidates_jw[[1]], 0.05)
})

test_that("unmatched when all JW above threshold_jw_min", {
  res <- tidyschoolvax:::match_schools_batch_cpp(
    names1           = "zzz",
    groups1          = "alpha county",
    names2           = c("lincoln elementary", "jefferson middle"),
    groups2          = c("alpha county", "alpha county"),
    threshold_jw_min = 0.6,
    threshold_jw     = 0.6,
    exact_jw         = 0.05
  )

  expect_equal(res$status[1], 1L)
})

test_that("status 0 when no data2 rows share the group key", {
  res <- tidyschoolvax:::match_schools_batch_cpp(
    names1           = "lincoln elementary",
    groups1          = "unknown county",
    names2           = "lincoln elementary",
    groups2          = "alpha county",
    threshold_jw_min = 0.6,
    threshold_jw     = 0.6,
    exact_jw         = 0.05
  )

  expect_equal(res$status[1], 0L)
})

test_that("non-exact candidates return status 3 with up to 10 rows", {
  # Build a data2 with 15 names that are JW-close but not exact
  names2  <- paste0("washington high school ", 1:15)
  groups2 <- rep("beta county", 15)

  res <- tidyschoolvax:::match_schools_batch_cpp(
    names1           = "washington highschool",
    groups1          = "beta county",
    names2           = names2,
    groups2          = groups2,
    threshold_jw_min = 0.6,
    threshold_jw     = 0.6,
    exact_jw         = 0.05
  )

  expect_equal(res$status[1], 3L)
  expect_lte(length(res$candidates_idx[[1]]), 10L)
})

test_that("status-3 candidates include all six pre-computed metrics", {
  names2  <- paste0("washington high school ", 1:5)
  groups2 <- rep("beta county", 5)

  res <- tidyschoolvax:::match_schools_batch_cpp(
    names1           = "washington highschool",
    groups1          = "beta county",
    names2           = names2,
    groups2          = groups2,
    threshold_jw_min = 0.6,
    threshold_jw     = 0.6,
    exact_jw         = 0.05
  )

  expect_equal(res$status[1], 3L)
  n_cand <- length(res$candidates_idx[[1]])

  # All six metric vectors must be present and have the same length
  expect_length(res$candidates_jw[[1]],      n_cand)
  expect_length(res$candidates_soundex[[1]], n_cand)
  expect_length(res$candidates_cosine[[1]],  n_cand)
  expect_length(res$candidates_osa[[1]],     n_cand)
  expect_length(res$candidates_qgram[[1]],   n_cand)
  expect_length(res$candidates_jaccard[[1]], n_cand)

  # Values should be non-negative
  expect_true(all(res$candidates_osa[[1]]     >= 0L))
  expect_true(all(res$candidates_qgram[[1]]   >= 0L))
  expect_true(all(res$candidates_jaccard[[1]] >= 0))
})

test_that("match_schools_names produces correct result for a status-3 pair", {
  # "lincoln elem" vs "lincoln elementary" — close but not exact (status 3)
  d1 <- data.frame(
    school_name       = "lincoln elem",
    school_name_std   = tolower(gsub("[[:punct:] ]+", "", "lincoln elem")),
    county            = "alpha county",
    county_std        = "alpha county",
    school_level      = "elementary",
    school_type       = "public",
    stringsAsFactors  = FALSE
  )
  d1$data1_id <- 1L

  d2 <- data.frame(
    school_name       = c("lincoln elementary", "jefferson middle"),
    school_name_std   = tolower(gsub("[[:punct:] ]+", "", c("lincoln elementary", "jefferson middle"))),
    county            = c("alpha county", "alpha county"),
    county_std        = c("alpha county", "alpha county"),
    school_level      = rep("elementary", 2L),
    school_type       = rep("public", 2L),
    stringsAsFactors  = FALSE
  )
  d2$data2_id <- seq_len(nrow(d2))
  # Ensure the standardized-name columns match production behavior, including
  # whitespace/punctuation handling.
  d1$school_name_std <- tidyschoolvax:::standardized_school_name(d1$school_name)
  d2$school_name_std <- tidyschoolvax:::standardized_school_name(d2$school_name)

  res <- match_schools_names(d1, d2)

  # The near-match should be found and linked to the correct data2 row
  expect_equal(nrow(res$matched), 1L)
  expect_equal(nrow(res$unmatched_dat1), 0L)
  expect_true(grepl("lincoln", res$matched$school_name_std_data2[1]))
})

test_that("NA name in data1 is treated as unmatched", {
  res <- tidyschoolvax:::match_schools_batch_cpp(
    names1           = NA_character_,
    groups1          = "alpha county",
    names2           = "lincoln elementary",
    groups2          = "alpha county",
    threshold_jw_min = 0.6,
    threshold_jw     = 0.6,
    exact_jw         = 0.05
  )

  expect_equal(res$status[1], 1L)
})

test_that("NA group in data1 is treated as unmatched", {
  res <- tidyschoolvax:::match_schools_batch_cpp(
    names1           = "lincoln elementary",
    groups1          = NA_character_,
    names2           = "lincoln elementary",
    groups2          = "alpha county",
    threshold_jw_min = 0.6,
    threshold_jw     = 0.6,
    exact_jw         = 0.05
  )

  expect_equal(res$status[1], 1L)
})

test_that("multiple data1 rows with different groups are handled independently", {
  res <- tidyschoolvax:::match_schools_batch_cpp(
    names1           = c("lincoln elementary", "no match school"),
    groups1          = c("alpha county", "alpha county"),
    names2           = c("lincoln elementary", "jefferson middle"),
    groups2          = c("alpha county", "alpha county"),
    threshold_jw_min = 0.6,
    threshold_jw     = 0.6,
    exact_jw         = 0.05
  )

  expect_equal(res$status[1], 2L)   # exact match
  expect_equal(res$status[2], 1L)   # unmatched
})

test_that("candidates_idx values are valid 1-based indices into names2", {
  names2 <- c("lincoln elementary", "lincoln middle", "jefferson primary")
  res <- tidyschoolvax:::match_schools_batch_cpp(
    names1           = "lincoln elem",
    groups1          = "alpha county",
    names2           = names2,
    groups2          = rep("alpha county", 3),
    threshold_jw_min = 0.6,
    threshold_jw     = 0.6,
    exact_jw         = 0.05
  )

  if (res$status[1] %in% c(2L, 3L)) {
    idx <- res$candidates_idx[[1]]
    expect_true(all(idx >= 1L & idx <= length(names2)))
  }
})

test_that("soundex_cosine_thresh parameter affects candidate filtering", {
  # Use two strings that share a soundex code (S530) but whose JW is above
  # threshold_jw so the cosine leg is the only way they pass the filter.
  # "smith school" (s530) vs "smyth scool" (s530): JW ~0.1 which is below
  # threshold_jw=0.6, so both strings would typically pass via JW anyway.
  # Instead test with names where JW > threshold but soundex matches:
  # "saintjohnprimary" vs "stjohnschool": similar soundex, high JW
  res_strict <- tidyschoolvax:::match_schools_batch_cpp(
    names1           = "roosveltprimaryschool",
    groups1          = "alpha county",
    names2           = c("ruzveltelementary"),   # different spelling, same phonetic root
    groups2          = "alpha county",
    threshold_jw_min = 0.6,
    threshold_jw     = 0.05,    # very tight JW — only cosine+soundex leg can pass
    exact_jw         = 0.01,
    soundex_cosine_thresh = 0.0  # cosine must be 0 — impossible, so will fail
  )

  res_lenient <- tidyschoolvax:::match_schools_batch_cpp(
    names1           = "roosveltprimaryschool",
    groups1          = "alpha county",
    names2           = c("ruzveltelementary"),
    groups2          = "alpha county",
    threshold_jw_min = 0.6,
    threshold_jw     = 0.05,    # same tight JW
    exact_jw         = 0.01,
    soundex_cosine_thresh = 1.0  # cosine always passes when soundex matches
  )

  # With cosine_thresh=0 the cosine leg never fires (impossible cosine=0),
  # so only the tight JW leg is active → likely unmatched.
  # With cosine_thresh=1.0 the cosine leg admits the candidate when soundex=0.
  # The two calls must produce different outcomes.
  expect_false(identical(res_strict$status[1], res_lenient$status[1]))
})

# ---- match_schools_names: end-to-end ----------------------------------------

make_school_df <- function(names, counties, source_id_col = "data1_id") {
  n <- length(names)
  df <- data.frame(
    school_name     = names,
    school_name_std = tolower(gsub("[[:punct:] ]+", "", names)),
    county          = counties,
    county_std      = tolower(counties),
    school_level    = rep("elementary", n),
    school_type     = rep("public", n),
    stringsAsFactors = FALSE
  )
  df[[source_id_col]] <- seq_len(n)
  df
}

test_that("match_schools_names returns required list elements", {
  d1 <- make_school_df(c("Lincoln Elementary"), c("Alpha County"), "data1_id")
  d2 <- make_school_df(c("Lincoln Elementary", "Jefferson Middle"),
                       c("Alpha County",       "Alpha County"), "data2_id")

  res <- match_schools_names(d1, d2)

  expect_named(res, c("matched", "unmatched_dat1", "unmatched_dat2",
                       "match_options", "match_summary"))
})

test_that("match_schools_names finds an exact match", {
  d1 <- make_school_df("Lincoln Elementary", "Alpha County", "data1_id")
  d2 <- make_school_df(c("Lincoln Elementary", "Jefferson Middle"),
                       c("Alpha County",       "Alpha County"), "data2_id")

  res <- match_schools_names(d1, d2)

  expect_equal(nrow(res$matched),        1L)
  expect_equal(nrow(res$unmatched_dat1), 0L)
  expect_equal(res$matched$match_category[1], "Exact Match")
})

test_that("match_schools_names places no-county-match in unmatched_dat1", {
  d1 <- make_school_df("Lincoln Elementary", "Unknown County", "data1_id")
  d2 <- make_school_df("Lincoln Elementary", "Alpha County",   "data2_id")

  res <- match_schools_names(d1, d2)

  expect_equal(nrow(res$matched),        0L)
  expect_equal(nrow(res$unmatched_dat1), 1L)
})

test_that("match_schools_names respects custom match_cols", {
  d1 <- make_school_df("Lincoln Elementary", "Alpha County", "data1_id")
  d1$state <- "CA"
  d2 <- make_school_df(c("Lincoln Elementary", "Lincoln Elem"),
                       c("Alpha County",       "Alpha County"), "data2_id")
  d2$state <- c("CA", "NY")

  res <- match_schools_names(d1, d2,
                              match_cols1 = c("county_std", "state"),
                              match_cols2 = c("county_std", "state"))

  # Only the CA row should be a candidate; NY row excluded by group key
  expect_equal(nrow(res$matched), 1L)
  expect_equal(res$matched$state[1], "CA")
})

# ---- parallel = TRUE: sequential-plan fallback warning ----------------------

test_that("parallel = TRUE with sequential plan emits a warning and still matches", {
  d1 <- make_school_df("Lincoln Elementary", "Alpha County", "data1_id")
  d2 <- make_school_df(c("Lincoln Elementary", "Jefferson Middle"),
                       c("Alpha County",       "Alpha County"), "data2_id")

  # Ensure we are on the default sequential plan
  future::plan(future::sequential)

  expect_warning(
    res <- match_schools_names(d1, d2, parallel = TRUE),
    regexp = "non-sequential.*future.*plan|sequential.*future.*plan",
    ignore.case = TRUE
  )

  # Despite the fallback, results must be correct
  expect_equal(nrow(res$matched),        1L)
  expect_equal(nrow(res$unmatched_dat1), 0L)
  expect_equal(res$matched$match_category[1], "Exact Match")
})
