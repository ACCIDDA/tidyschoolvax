# ---- review_unmatched_schools() -----------------------------------------------

# One kinder row per school-year. school_id = NA marks "matched no reference school".
kinder_row <- function(name, std, county, years, lat = 40, lon = -80, school_id = NA_integer_) {
  data.frame(
    school_id = school_id, school_name = name, school_name_std_vacc = std,
    county_std = county, year_source = paste0(years, "-", years + 1),
    lat = lat, lon = lon, stringsAsFactors = FALSE
  )
}

# ~1 km north of (40, -80)
near <- 40.009

make_unmatched <- function() {
  rbind(
    kinder_row("oak el",        "oak",         "alpha", 2018:2025),  # DOE has it (name mismatch)
    kinder_row("pine el",       "pine",        "alpha", 2018:2019),  # closed NCES school nearby
    kinder_row("bethany luth",  "bethany luth","beta",  2018:2020),  # renamed: NCES former name
    kinder_row("ktec",          "ktec",        "alpha", 2018:2025),  # possible match only
    kinder_row("zzz academy",   "zzz academy", "alpha", 2018:2019),  # nothing similar, stopped
    kinder_row("qqq school",    "qqq",         "alpha", 2018:2025),  # nothing similar, reporting
    kinder_row("far el",        "far",         "alpha", 2018:2025),  # NCES name match, too far
    kinder_row("matched el",    "matched",     "alpha", 2018:2025, school_id = 1L)  # linked: ignored
  )
}

make_doe_ref <- function() {
  data.frame(
    school_name     = c("oak elementary", "oak elementary", "ktec schools of innovation", "unrelated"),
    school_name_std = c("oak",            "oak",            "ktec schools of innovation", "unrelated"),
    county_std      = c("alpha",          "gamma",          "alpha",                      "alpha"),
    city            = c("oakville",       "elsewhere",      "kenosha",                    "x"),
    current_status  = c("open",           "open",           "open",                       "open"),
    stringsAsFactors = FALSE
  )
}

make_nces <- function() {
  data.frame(
    nces_id = c("1", "2", "3"),
    nces_name  = c("pine elementary", "kenosha lutheran academy", "far elementary"),
    nces_names = c("pine elementary", "kenosha lutheran academy | bethany luth", "far elementary"),
    lat = c(near, near, 40.2),            # "far elementary" is ~22 km away
    lon = c(-80, -80, -80),
    business_status = c("CLOSED_PERMANENTLY", "OPERATIONAL", "OPERATIONAL"),
    last_seen = c(2019L, 2025L, 2025L),
    stringsAsFactors = FALSE
  )
}

run_review <- function(..., nces = make_nces()) {
  suppressMessages(review_unmatched_schools(
    make_unmatched(), reference_sources = list(doe = make_doe_ref()), nces_ref = nces, ...
  ))
}

explain <- function(r, name) r$likely_explanation[r$school_name == name]

test_that("only schools without a school_id are listed, one row each", {
  r <- run_review()
  expect_equal(nrow(r), 7L)
  expect_false("matched el" %in% r$school_name)
  expect_equal(anyDuplicated(r[c("school_name", "county_std")]), 0L)
})

test_that("each school gets the matching explanation", {
  r <- run_review()
  expect_match(explain(r, "oak el"),       "open school in a reference source")
  expect_match(explain(r, "pine el"),      "closed school: matches a closed NCES school nearby")
  expect_match(explain(r, "bethany luth"), "renamed school")
  expect_match(explain(r, "ktec"),         "possible name match")
  expect_match(explain(r, "zzz academy"),  "stopped reporting.*probably closed")
  expect_match(explain(r, "qqq school"),   "still reporting")
})

test_that("candidates come from the right place", {
  r <- run_review()
  oak <- r[r$school_name == "oak el", ]
  # same-county DOE school, not the identical name in another county
  expect_equal(oak$doe_name, "oak elementary"); expect_equal(oak$doe_city, "oakville")
  expect_equal(oak$doe_sim, 1)
  pine <- r[r$school_name == "pine el", ]
  expect_equal(pine$nces_name, "pine elementary"); expect_equal(pine$nces_status, "CLOSED_PERMANENTLY")
  expect_true(pine$nces_dist_m < 2000)
  # the former name that matched is reported next to the current name
  beth <- r[r$school_name == "bethany luth", ]
  expect_equal(beth$nces_name, "kenosha lutheran academy")
  expect_equal(beth$nces_matched_name, "bethany luth")
  expect_true(is.na(pine$nces_matched_name))
})

test_that("an NCES name match that is far away is not treated as confident", {
  r <- run_review()
  far <- r[r$school_name == "far el", ]
  expect_equal(far$nces_name, "far elementary")
  expect_true(far$nces_dist_m > 5000)
  expect_false(grepl("closed school|renamed|open school in a reference", far$likely_explanation))
  expect_match(far$likely_explanation, "possible name match")
  # with a wider confidence radius it is
  r2 <- run_review(strong_dist_m = 50000)
  expect_match(explain(r2, "far el"), "open school in a reference source")
})

test_that("similarity thresholds are adjustable", {
  r <- run_review(possible_sim = 0.99, strong_sim = 0.995)
  expect_match(explain(r, "ktec"), "no similar reference school")
})

test_that("reporting and year columns describe each school's data", {
  r <- run_review()
  z <- r[r$school_name == "zzz academy", ]
  expect_equal(c(z$first_year, z$last_year, z$n_years), c(2018, 2019, 2))
  expect_equal(z$reporting, "last reported 2019")
  expect_equal(r$reporting[r$school_name == "qqq school"], "still reporting in the latest year")
})

test_that("blank decision columns are provided and rows are grouped by explanation", {
  r <- run_review()
  expect_true(all(c("reviewer_decision", "matched_reference_name", "reviewer_notes") %in% names(r)))
  expect_true(all(is.na(r$reviewer_decision)))
  expect_identical(r$likely_explanation, sort(r$likely_explanation))
  expect_identical(names(r)[1:5], c("school_name", "county_std", "reporting",
                                    "likely_explanation", "best_sim"))
})

test_that("it works without NCES, and with several reference sources", {
  r <- suppressMessages(review_unmatched_schools(
    make_unmatched(),
    reference_sources = list(doe = make_doe_ref(), gs = transform(make_doe_ref(), lat = 40, lon = -80))
  ))
  expect_false(any(grepl("^nces_", names(r))))
  expect_true(all(c("doe_sim", "gs_sim", "gs_dist_m") %in% names(r)))
  expect_match(explain(r, "pine el"), "no similar reference school")  # needs NCES to be called closed
})

test_that("a reference source with missing columns is skipped with a warning", {
  expect_warning(
    r <- suppressMessages(review_unmatched_schools(
      make_unmatched(), reference_sources = list(doe = make_doe_ref(), bad = data.frame(x = 1))
    )),
    "skipping source 'bad'"
  )
  expect_true("doe_sim" %in% names(r)); expect_false("bad_sim" %in% names(r))
})

test_that("nothing to review gives a zero-row table and a message", {
  df <- make_unmatched(); df$school_id <- 1L
  expect_message(r <- review_unmatched_schools(df, list(doe = make_doe_ref())), "every school matched")
  expect_equal(nrow(r), 0L)
})

test_that("bad input is rejected", {
  expect_error(review_unmatched_schools(data.frame(school_id = NA)), "missing column")
  expect_error(review_unmatched_schools(make_unmatched(), reference_sources = list(make_doe_ref())),
               "named list")
})

test_that("schools without coordinates still get reference candidates but no NCES ones", {
  df <- make_unmatched(); df$lat <- NA_real_; df$lon <- NA_real_
  r <- suppressMessages(review_unmatched_schools(df, list(doe = make_doe_ref()), nces_ref = make_nces()))
  expect_equal(r$doe_name[r$school_name == "oak el"], "oak elementary")
  expect_true(all(is.na(r$nces_name)))
})
