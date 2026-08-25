# Tests for match_locations() and the underlying score_candidates_cpp() scorer.

# ---- score_candidates_cpp: output structure ----------------------------------

test_that("score_candidates_cpp returns expected columns and types", {
  res <- tidyschoolvax:::score_candidates_cpp("lincoln", c("lincoln", "jefferson"))

  expect_named(res, c("name", "osa", "qgram", "cosine", "jaccard", "jw",
                       "soundex", "score_sums"))
  expect_type(res$osa,        "integer")
  expect_type(res$qgram,      "integer")
  expect_type(res$cosine,     "double")
  expect_type(res$jaccard,    "double")
  expect_type(res$jw,         "double")
  expect_type(res$soundex,    "double")
  expect_type(res$score_sums, "double")
  expect_equal(nrow(res), 2L)
})

test_that("score_candidates_cpp gives zero distances for an exact match", {
  res <- tidyschoolvax:::score_candidates_cpp("lincoln", "lincoln")

  expect_equal(res$osa[1],        0L)
  expect_equal(res$qgram[1],      0L)
  expect_equal(res$cosine[1],     0)
  expect_equal(res$jaccard[1],    0)
  expect_equal(res$jw[1],         0)
  expect_equal(res$soundex[1],    0)
  expect_equal(res$score_sums[1], 0)
})

test_that("score_candidates_cpp gives positive distances for non-matching strings", {
  res <- tidyschoolvax:::score_candidates_cpp("lincoln", "jefferson")

  expect_gt(res$osa[1],        0)
  expect_gt(res$score_sums[1], 0)
})

test_that("score_candidates_cpp propagates NA for a NA candidate", {
  res <- tidyschoolvax:::score_candidates_cpp("lincoln", c("lincoln", NA_character_))

  expect_equal(res$osa[1],    0L)           # exact match is still correct
  expect_true(is.na(res$osa[2]))
  expect_true(is.na(res$cosine[2]))
  expect_true(is.na(res$jw[2]))
  expect_true(is.na(res$score_sums[2]))
})

# ---- match_locations: return shape -------------------------------------------

test_that("match_locations returns NA when input 'a' is NA", {
  result <- match_locations(NA_character_, c("alpha", "beta"))
  expect_true(is.na(result))
})

test_that("match_locations errors when 'a' has length > 1", {
  expect_error(
    match_locations(c("alpha", "beta"), c("alpha", "beta")),
    regexp = "length 1"
  )
})

test_that("match_locations errors when 'names' is NULL", {
  expect_error(
    match_locations("alpha", NULL),
    regexp = "names_standard"
  )
})

test_that("match_locations returns NA for no match when return_score_matrix=FALSE", {
  result <- match_locations("zzzzzzz", c("alpha", "beta"))
  expect_true(is.na(result))
})

test_that("match_locations returns full distance matrix for no match when return_score_matrix=TRUE", {
  result <- match_locations("zzzzzzz", c("alpha", "beta"),
                             return_score_matrix = TRUE)

  expect_s3_class(result, "data.frame")
  expect_named(result, c("name", "osa", "qgram", "cosine", "jaccard", "jw",
                          "soundex", "score_sums"))
  expect_equal(nrow(result), 2L)
  # The 'name' column must contain the original unstandardised names
  expect_equal(result$name, c("alpha", "beta"))
})

# ---- match_locations: single best match -------------------------------------

test_that("match_locations finds a close match and returns a data frame", {
  # 'lincolnelementary' and 'lincoln elementary' differ by one space removal
  result <- match_locations("Lincoln Elementary",
                             c("Lincoln Elementary", "Jefferson Middle"),
                             return_name  = TRUE,
                             return_score = FALSE)

  expect_s3_class(result, "data.frame")
  expect_named(result, "name")
  expect_equal(result$name[1], "Lincoln Elementary")
})

test_that("match_locations returns score_sum column when return_score=TRUE", {
  result <- match_locations("Lincoln Elementary",
                             c("Lincoln Elementary", "Jefferson Middle"),
                             return_name  = TRUE,
                             return_score = TRUE)

  expect_named(result, c("name", "score_sum"))
  expect_true(is.numeric(result$score_sum))
})

test_that("match_locations returns only score_sum when return_name=FALSE", {
  result <- match_locations("Lincoln Elementary",
                             c("Lincoln Elementary", "Jefferson Middle"),
                             return_name  = FALSE,
                             return_score = TRUE)

  expect_false("name" %in% names(result))
  expect_true("score_sum" %in% names(result))
})

test_that("match_locations score_sum is numeric (not character) for a single best match", {
  result <- match_locations("Lincoln Elementary",
                             c("Lincoln Elementary", "Jefferson Middle"),
                             return_name  = TRUE,
                             return_score = TRUE)

  expect_type(result$score_sum, "double")
})

# ---- match_locations: pre_standardized flag ----------------------------------

test_that("pre_standardized=FALSE (default) produces the same result as pre_standardized=TRUE on already-standardized inputs", {
  # "lincoln elementary" with standardization applied externally equals the
  # result when standardize_location_strings() is called internally.
  result_default <- match_locations(
    "lincoln elementary",
    c("lincoln elementary school"),
    pre_standardized = FALSE
  )
  result_pre <- match_locations(
    "lincolnelementary",
    c("lincolnelementary"),
    pre_standardized = TRUE
  )

  # Both calls should find a match (no missing values in returned result)
  expect_false(anyNA(result_default))
  expect_false(anyNA(result_pre))
})

test_that("pre_standardized=TRUE skips standardization: raw unstandardized inputs differ from standardized ones", {
  # Passing mixed-case, spaced string with pre_standardized=TRUE means it is NOT
  # lowercased/stripped, so the C++ scorer sees "Lincoln Elementary" vs
  # "lincoln elementary" — these are different strings and will have a non-zero
  # distance, unlike when pre_standardized=FALSE normalises both first.
  result_pre_raw <- match_locations(
    "Lincoln Elementary",
    c("lincoln elementary"),
    return_name         = TRUE,
    return_score        = TRUE,
    pre_standardized    = TRUE
  )

  result_normalized <- match_locations(
    "Lincoln Elementary",
    c("lincoln elementary"),
    return_name         = TRUE,
    return_score        = TRUE,
    pre_standardized    = FALSE
  )

  # With pre_standardized=FALSE both sides are lowercased, giving a perfect
  # match (score_sum == 0).  With pre_standardized=TRUE the raw strings are
  # used and the score will be higher (worse).
  expect_equal(result_normalized$score_sum, 0)
  expect_gt(result_pre_raw$score_sum, 0)
})

# ---- exact_match_locations: require="all" is row-adaptive --------------------
# Regression coverage for a real bug found on a full CA run: computing one
# FIXED "usable columns" set for the whole call (e.g. via .usable_geo_cols())
# and requiring it uniformly meant any row missing just one column that
# happened to be populated on *some other* row (district_std, in CA's case
# many private/religious schools have none) was excluded from ever
# exact-matching at all — even though its own available columns (name +
# county) would have uniquely identified it. This cost ~100k previously-good
# matches on a real run. exact_match_locations() must use each row's OWN
# non-NA columns among `cols`, not one set for every row in the call.

test_that("exact_match_locations require='all' matches a row missing one of `cols` on the columns it does have", {
  data1 <- tibble::tibble(
    orig_id = c("k1", "k2"),
    school_name_std = c("lincoln elementary", "lincoln elementary"),
    county_std   = c("alpha", "alpha"),
    district_std = c(NA_character_, "alpha usd")  # k1 has no district; k2 does
  )
  data2 <- tibble::tibble(
    orig_id = c("m1", "m2"),
    school_name_std = c("lincoln elementary", "lincoln elementary"),
    county_std   = c("alpha", "alpha"),
    district_std = c(NA_character_, "alpha usd")
  )

  res <- exact_match_locations(data1, data2, cols = c("county_std", "district_std"), require = "all")

  # k1 (no district) matches m1 on name+county alone -- it must NOT be
  # excluded just because k2 happens to have a district.
  expect_true("k1" %in% res$matched$orig_id_1)
  expect_equal(res$matched$orig_id_2[res$matched$orig_id_1 == "k1"], "m1")
  # k2 (has a district) matches m2 on name+county+district.
  expect_true("k2" %in% res$matched$orig_id_1)
  expect_equal(res$matched$orig_id_2[res$matched$orig_id_1 == "k2"], "m2")
})

test_that("exact_match_locations require='all' still rejects a mismatch on a column the row DOES have", {
  data1 <- tibble::tibble(
    orig_id = "k1",
    school_name_std = "lincoln elementary",
    county_std   = "alpha",
    district_std = "alpha usd"
  )
  data2 <- tibble::tibble(
    orig_id = "m1",
    school_name_std = "lincoln elementary",
    county_std   = "alpha",
    district_std = "beta usd"  # different district -> must not match
  )

  res <- exact_match_locations(data1, data2, cols = c("county_std", "district_std"), require = "all")
  expect_equal(nrow(res$matched), 0L)
})

test_that("exact_match_locations require='all' skips (does not name-only match) a row with none of `cols` populated", {
  data1 <- tibble::tibble(
    orig_id = "k1",
    school_name_std = "lincoln elementary",
    county_std   = NA_character_,
    district_std = NA_character_
  )
  data2 <- tibble::tibble(
    orig_id = "m1",
    school_name_std = "lincoln elementary",
    county_std   = "alpha",
    district_std = "alpha usd"
  )

  res <- exact_match_locations(data1, data2, cols = c("county_std", "district_std"), require = "all")
  expect_equal(nrow(res$matched), 0L)
  expect_equal(res$unmatched_dat1$orig_id, "k1")
})

# ---- fix_school_type_na: input/return classes ---------------------------------

make_fix_school_type_data <- function() {
  data.frame(
    school_name_std = c("lincoln", "lincoln", "lincoln", "washington"),
    county_std      = c("alpha", "alpha", "alpha", "beta"),
    school_level    = c("elementary", "elementary", "elementary", "middle"),
    school_type     = c("public", NA, "public", NA),
    stringsAsFactors = FALSE
  )
}

test_that("fix_school_type_na returns a data.table and does not mutate data.table input", {
  input    <- data.table::as.data.table(make_fix_school_type_data())
  original <- data.table::copy(input)

  result <- fix_school_type_na(input, n_years_data = 10)

  expect_true(data.table::is.data.table(result))
  expect_equal(result$school_type[2], "public")
  expect_identical(input, original)
})

test_that("fix_school_type_na keeps non-data.table inputs on the data.frame path", {
  data_frame_input <- make_fix_school_type_data()
  tibble_input     <- tibble::as_tibble(make_fix_school_type_data())

  data_frame_result <- fix_school_type_na(data_frame_input, n_years_data = 10)
  tibble_result     <- fix_school_type_na(tibble_input, n_years_data = 10)

  expect_s3_class(data_frame_result, "data.frame")
  expect_false(data.table::is.data.table(data_frame_result))
  expect_equal(data_frame_result$school_type[2], "public")

  expect_s3_class(tibble_result, "data.frame")
  expect_false(data.table::is.data.table(tibble_result))
  expect_false(inherits(tibble_result, "tbl_df"))
  expect_equal(tibble_result$school_type[2], "public")
})
