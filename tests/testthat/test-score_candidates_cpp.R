# Tests for score_candidates_cpp() (C++ multi-metric string-distance scorer)

# ---- output structure -------------------------------------------------------

test_that("score_candidates_cpp returns a data.frame with expected columns", {
  res <- tidyschoolvax:::score_candidates_cpp("school", c("school", "skool"))

  expect_s3_class(res, "data.frame")
  expect_named(res, c("name", "osa", "qgram", "cosine", "jaccard",
                       "jw", "soundex", "score_sums"))
  expect_equal(nrow(res), 2L)
})

test_that("score_candidates_cpp column types are correct", {
  res <- tidyschoolvax:::score_candidates_cpp("abc", c("abc", "xyz"))

  expect_type(res$name,       "character")
  expect_type(res$osa,        "integer")
  expect_type(res$qgram,      "integer")
  expect_type(res$cosine,     "double")
  expect_type(res$jaccard,    "double")
  expect_type(res$jw,         "double")
  expect_type(res$soundex,    "double")
  expect_type(res$score_sums, "double")
})

test_that("score_candidates_cpp returns one row per candidate", {
  candidates <- c("alpha", "beta", "gamma", "delta")
  res <- tidyschoolvax:::score_candidates_cpp("alpha", candidates)

  expect_equal(nrow(res), length(candidates))
  expect_equal(res$name, candidates)
})

# ---- identical strings → all distances zero ---------------------------------

test_that("identical query and candidate yield all-zero distances", {
  res <- tidyschoolvax:::score_candidates_cpp("lincoln", "lincoln")

  expect_equal(res$osa[1],        0L)
  expect_equal(res$qgram[1],      0L)
  expect_equal(res$cosine[1],     0.0)
  expect_equal(res$jaccard[1],    0.0)
  expect_equal(res$jw[1],         0.0)
  expect_equal(res$soundex[1],    0.0)
  expect_equal(res$score_sums[1], 0.0)
})

# ---- known distance values --------------------------------------------------

test_that("OSA distance is correct for a single substitution", {
  # "cat" -> "bat" requires one substitution
  res <- tidyschoolvax:::score_candidates_cpp("cat", "bat")
  expect_equal(res$osa[1], 1L)
})

test_that("qgram distance is correct for a single substitution", {
  # "cat" vs "bat": character 'c' replaced by 'b', two unigrams differ → 2
  res <- tidyschoolvax:::score_candidates_cpp("cat", "bat")
  expect_equal(res$qgram[1], 2L)
})

test_that("completely different strings have maximum distances", {
  # "abc" vs "xyz" share no characters
  res <- tidyschoolvax:::score_candidates_cpp("abc", "xyz")

  expect_equal(res$osa[1],     3L)  # three substitutions
  expect_equal(res$qgram[1],   6L)  # 3 chars each, none shared
  expect_equal(res$cosine[1],  1.0) # no shared unigrams → maximum cosine distance
  expect_equal(res$jaccard[1], 1.0) # no shared unigrams → maximum jaccard distance
})

# ---- score_sums consistency -------------------------------------------------

test_that("score_sums equals the sum of the six individual metrics", {
  candidates <- c("school", "skool", "scool", "skhool")
  res <- tidyschoolvax:::score_candidates_cpp("school", candidates)

  expected_sums <- res$osa + res$qgram + res$cosine +
                   res$jaccard + res$jw + res$soundex
  expect_equal(res$score_sums, expected_sums, tolerance = 1e-10)
})

# ---- NA handling ------------------------------------------------------------

test_that("NA candidate produces NA for all metric columns", {
  res <- tidyschoolvax:::score_candidates_cpp("school", NA_character_)

  expect_true(is.na(res$osa[1]))
  expect_true(is.na(res$qgram[1]))
  expect_true(is.na(res$cosine[1]))
  expect_true(is.na(res$jaccard[1]))
  expect_true(is.na(res$jw[1]))
  expect_true(is.na(res$soundex[1]))
  expect_true(is.na(res$score_sums[1]))
})

test_that("NA candidate among valid ones only affects the NA row", {
  res <- tidyschoolvax:::score_candidates_cpp("school",
                                               c("school", NA_character_, "skool"))

  # Row 1: identical, all zeros
  expect_equal(res$osa[1], 0L)
  # Row 2: NA
  expect_true(is.na(res$osa[2]))
  expect_true(is.na(res$score_sums[2]))
  # Row 3: valid, non-NA
  expect_false(is.na(res$osa[3]))
  expect_false(is.na(res$score_sums[3]))
})

# ---- metric ranges ----------------------------------------------------------

test_that("continuous metrics are in [0, 1] for non-NA rows", {
  candidates <- c("lincoln elementary", "jefferson middle", "washington high",
                  "adams school", "")
  res <- tidyschoolvax:::score_candidates_cpp("lincoln", candidates)
  valid <- !is.na(res$cosine)

  expect_true(all(res$cosine[valid]  >= 0 & res$cosine[valid]  <= 1))
  expect_true(all(res$jaccard[valid] >= 0 & res$jaccard[valid] <= 1))
  expect_true(all(res$jw[valid]      >= 0 & res$jw[valid]      <= 1))
  expect_true(all(res$soundex[valid] %in% c(0, 1)))
})

test_that("integer metrics are non-negative for non-NA rows", {
  candidates <- c("lincoln elementary", "jefferson middle", "washington high")
  res <- tidyschoolvax:::score_candidates_cpp("lincoln", candidates)

  expect_true(all(res$osa   >= 0L))
  expect_true(all(res$qgram >= 0L))
})
