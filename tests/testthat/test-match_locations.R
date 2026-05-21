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
