# End-to-end smoke test of .run_school_matching_pipeline() — everything
# standardize_schools() does after cleaning (Parts 2-7 of the geocode-first
# redesign). No live Google API calls: build_location_key_table() doesn't
# carry a pre-existing place_id column through (geocoding happens *after*
# it, by design in the new pipeline), so the only reliable way to avoid a
# live API call is the same mechanism the real pipeline itself uses to
# avoid one — a pre-populated on-disk geocoding cache
# (geocoded_cache.rds, read by load_and_merge_cache()/run_full_geocoding()
# before ever considering an API call). This test writes that cache file
# directly, keyed by every school_name/county_std combination each stage
# will query with (GreatSchools/DOE query their own raw name; kinder
# queries its already-lowercased school_name_std).
#
# Scenario built into the fixtures:
#   - "Jefferson Middle": identical name + address in GS and DOE -> resolves
#     via the exact-match step alone, before either ever reaches geocoding.
#     No kinder row targets it in this fixture, so it's a pure "does the
#     GS<->DOE exact-match step still work" control, unrelated to the
#     master_full/master_elem split below.
#   - "Lincoln Elementary": identical name + county + district in GS and
#     kinder -> kinder's own exact-match step (require = "all" on whichever
#     geo columns are populated) resolves it directly, no geocoding needed.
#   - "Robert E Lee Elementary" (GS) / "Northside Elementary" (DOE and
#     kinder): different names -> exact match correctly misses, but all
#     three share place_id "ChIJ_lee" (pre-seeded) -> place_id-matched into
#     ONE school. This is the rename case the whole redesign exists for:
#     the master ends up keyed on GS's name, and kinder's differently-named
#     record still resolves to the same school_id via place_id alone.
#   - "Mary Moss Academy": GS-only, level_code "m" (middle) -- deliberately
#     NOT elementary-eligible, yet kinder reports a K-eligible record under
#     the identical name/county/district. Regression case for a real MD
#     bug: exact-match and place_id-match used to run against an
#     elementary-only candidate pool, so a real school GreatSchools/DOE
#     happens to classify as non-elementary (a combined-grade or
#     alternative-ed program, say) could never resolve even on an exact
#     name+location match. Must now resolve via kinder's own exact-match
#     step despite the "m" level_code.
#   - "Mystery School" (kinder only): pre-seeded with a place_id nothing
#     else shares, and a name/county nothing exact- or fuzzy-matches ->
#     must land in unmatched_schools_final.csv.
#   - "Woodmore" (GS, county "alpha"): kinder reports it under "alpha" for
#     two years and, for one year only, under "beta" -- with "alpha"'s own
#     kinder years never covering that one year. Regression case for a
#     real MD bug: a county typo blocks exact-match, and geocoding a bare
#     name+wrong-county query often resolves to SOME real-looking but
#     unrelated place (pre-seeded as place_id "ChIJ_wrong_beta", sharing
#     nothing with master) rather than failing outright — so
#     place_id-match can't bridge it either. Must resolve via the
#     county-typo correction: detect the complementary year gap, correct
#     "beta" to "alpha", and exact-match successfully on retry.

make_gs <- function() {
  tibble::tibble(
    data1_id        = 1:5,
    school_name     = c("Jefferson Middle", "Robert E Lee Elementary", "Lincoln Elementary",
                        "Mary Moss Academy", "Woodmore Elementary"),
    school_name_std = c("jefferson middle", "robert e lee elementary", "lincoln elementary",
                        "mary moss academy", "woodmore"),
    county_std       = c("alpha", "alpha", "alpha", "alpha", "alpha"),
    county            = c("Alpha", "Alpha", "Alpha", "Alpha", "Alpha"),
    city               = c("Springfield", "Springfield", "Springfield", "Springfield", "Springfield"),
    state               = c("md", "md", "md", "md", "md"),
    zip                  = c("21201", "21202", "21203", "21204", "21205"),
    addr_clean            = c("200 oak ave", "100 main st", "300 elm st", "400 clay st", "500 wood ave"),
    school_type            = c("public", "public", "public", "public", "public"),
    school_level             = c("middle", "elementary", "elementary", "middle", "elementary"),
    level_code                 = c("m", "e", "e", "m", "e"),
    district_std                = c("alpha usd", "alpha usd", "alpha usd", "alpha usd", "alpha usd"),
    lat = c(39.30, 39.29, 39.28, 39.27, 39.26), lon = c(-76.62, -76.61, -76.60, -76.59, -76.58)
  )
}

make_doe <- function() {
  tibble::tibble(
    data2_id        = 1:2,
    school_name     = c("Jefferson Middle", "Northside Elementary"),
    school_name_std = c("jefferson middle", "northside elementary"),
    county_std       = c("alpha", "alpha"),
    county            = c("Alpha", "Alpha"),
    city               = c("Springfield", "Springfield"),
    addr_clean          = c("200 oak ave", "999 unrelated rd"),
    school_type          = c("public", "public"),
    school_level           = c("middle", "elementary"),
    level_code               = c("m", "e"),
    district_std              = c("alpha usd", "alpha usd")
  )
}

make_kinder <- function() {
  tibble::tibble(
    year_source        = c("md_20_21", "md_20_21", "md_21_22", "md_21_22",
                           "md_18_19", "md_19_20", "md_21_22"),
    school_name          = c("lincoln elementary", "northside elementary", "mystery school",
                             "mary moss academy", "woodmore", "woodmore", "woodmore"),
    school_name_std      = c("lincoln elementary", "northside elementary", "mystery school",
                             "mary moss academy", "woodmore", "woodmore", "woodmore"),
    county                = c(rep("alpha", 4), "alpha", "alpha", "beta"),
    county_std             = c(rep("alpha", 4), "alpha", "alpha", "beta"),
    school_type             = rep("public", 7),
    school_level             = c("elementary", "elementary", "elementary", NA_character_,
                                 "elementary", "elementary", "elementary"),
    level_code                 = c("e", "e", "e", NA_character_, "e", "e", "e"),
    district_std                = c(rep("alpha usd", 4), "alpha usd", "alpha usd", NA_character_),
    total_k_students = c(60, 95, 40, 20, 30, 32, 12),
    count_vacc          = c(55, 88, 35, 18, 28, 29, 10)
  )
}

# Pre-populate the on-disk geocoding cache with every (school_name,
# county_std) combination each pipeline stage will query with, so
# run_full_geocoding() finds nothing left to do and never calls the API.
# GreatSchools/DOE query their own raw name; kinder queries its already-
# lowercased school_name_std at the matching stage. Part 7's final
# geocoding/status pass re-groups by school_name_std_vacc (kinder's own
# name, not the master's canonical one) even for rows that matched via
# kinder's exact-match step and thus never geocoded themselves — so
# "lincoln elementary" (lowercase) needs its own cache entry too, distinct
# from GS's own "Lincoln Elementary" (title case) query in Part 3.
write_geocode_cache <- function(geo_dir) {
  cache <- tibble::tibble(
    school_name = c("Jefferson Middle", "Robert E Lee Elementary", "Lincoln Elementary",
                    "Northside Elementary", "northside elementary", "mystery school",
                    "lincoln elementary", "Mary Moss Academy", "mary moss academy",
                    "Woodmore Elementary", "woodmore", "woodmore"),
    county_std   = c(rep("alpha", 9), "alpha", "alpha", "beta"),
    place_id       = c("ChIJ_jeff", "ChIJ_lee", "ChIJ_lincoln",
                       "ChIJ_lee", "ChIJ_lee", "ChIJ_mystery_unmatched",
                       "ChIJ_lincoln", "ChIJ_marymoss", "ChIJ_marymoss",
                       "ChIJ_woodmore", "ChIJ_woodmore", "ChIJ_wrong_beta"),
    lat             = 39.29, lon = -76.61
  )
  saveRDS(cache, file.path(geo_dir, "geocoded_cache.rds"))
}

test_that(".run_school_matching_pipeline() resolves exact and place_id matches; genuinely unmatched schools go to a report", {
  tmp <- tempfile("pipeline_smoke_"); dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  geo_dir    <- file.path(tmp, "geo");    dir.create(geo_dir)
  review_dir <- file.path(tmp, "review"); dir.create(review_dir)
  temp_dir   <- file.path(tmp, "temp");   dir.create(temp_dir)
  write_geocode_cache(geo_dir)

  result <- suppressWarnings(suppressMessages(
    .run_school_matching_pipeline(
      greatschools_dat = make_gs(),
      doe_dat          = make_doe(),
      kinder_dat       = make_kinder(),
      kinder_has_addr  = FALSE,
      other_dat        = NULL,
      state_id         = "md",
      state_geo_dir    = geo_dir,
      temp_data_dir    = temp_dir,
      state_dir        = dirname(review_dir),
      review_dir       = review_dir,
      google_api_key   = "unused-in-this-test",
      addr_source_pref = "greatschools"
    )
  ))

  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
  expect_true(file.exists(file.path(temp_dir, "kinder_vaccination_clean_02.csv")))

  by_name <- function(x) result[result$school_name_std_vacc == x, , drop = FALSE]

  # Clean exact match (kinder's own exact-match step, no geocoding needed):
  # "lincoln elementary" shares name/county/district with GS's row and
  # resolves directly.
  lincoln <- by_name("lincoln elementary")
  expect_equal(nrow(lincoln), 1L)
  expect_false(is.na(lincoln$school_id))
  expect_equal(lincoln$school_name_std, "lincoln elementary")

  # Rename bridged via place_id: kinder's "northside elementary" record
  # shares place_id ChIJ_lee with GS's "Robert E Lee Elementary" (the name
  # that actually survives into the master) and must resolve to that
  # school_id even though the name strings share nothing in common.
  north <- by_name("northside elementary")
  expect_equal(nrow(north), 1L)
  expect_false(is.na(north$school_id))
  expect_equal(north$school_name_std, "robert e lee elementary")

  # Genuinely unmatched kinder record: unique place_id, no exact/fuzzy
  # candidate anywhere in the master -> unmatched report.
  expect_true(file.exists(file.path(review_dir, "unmatched_schools_final.csv")))
  unmatched <- read.csv(file.path(review_dir, "unmatched_schools_final.csv"))
  expect_true(any(grepl("mystery school", unmatched$school_name_std, fixed = TRUE)))
  mystery <- by_name("mystery school")
  expect_equal(nrow(mystery), 1L)
  expect_true(is.na(mystery$school_id))

  # Exact match against a non-elementary (level_code "m") master row: must
  # still resolve, since exact/place_id matching is deterministic and
  # doesn't get safer by excluding real schools GreatSchools/DOE classify
  # as non-elementary. Also confirms it never fell through to the
  # unmatched report.
  mary_moss <- by_name("mary moss academy")
  expect_equal(nrow(mary_moss), 1L)
  expect_false(is.na(mary_moss$school_id))
  expect_equal(mary_moss$school_name_std, "mary moss academy")
  expect_false(any(grepl("mary moss academy", unmatched$school_name_std, fixed = TRUE)))

  # County-typo correction: kinder reports "woodmore" under "beta" for
  # exactly the one year "alpha"'s own records don't cover, and that
  # record's geocode resolves to a real-looking but unrelated place
  # (place_id-match can't bridge it). Must resolve to the SAME school_id
  # as the "alpha" years via the county correction, and the corrected
  # county must be visible in the final output too, not just the match.
  woodmore <- by_name("woodmore")
  expect_equal(nrow(woodmore), 3L)
  expect_false(any(is.na(woodmore$school_id)))
  expect_equal(length(unique(woodmore$school_id)), 1L)
  expect_true(all(woodmore$county_std == "alpha"))
  expect_false(any(unmatched$school_name_std == "woodmore"))

  # The combined place_id crosswalk was written, and DOE's "Jefferson Middle"
  # (which exact-matched GS's row on name+address) never shows up in it —
  # exact matches resolve before geocoding/collapsing ever runs.
  expect_true(file.exists(file.path(review_dir, "place_id_crosswalk.csv")))
  crosswalk <- read.csv(file.path(review_dir, "place_id_crosswalk.csv"))
  expect_false(any(crosswalk$source == "doe" & crosswalk$orig_name_std == "jefferson middle"))
})

# ---- geocode_original_then_std(): retry-selection pure logic (no API) --------

test_that(".rows_needing_retry() flags only rows missing a place_id", {
  expect_equal(tidyschoolvax:::.rows_needing_retry(c("a", NA, "", "b")), c(2L, 3L))
  expect_equal(tidyschoolvax:::.rows_needing_retry(c("a", "b")), integer(0))
})

test_that(".merge_geocode_retry() derives geocode_name_used correctly", {
  attempt1 <- data.frame(place_id = c("ChIJ_a", NA, NA), stringsAsFactors = FALSE)
  attempt2 <- data.frame(place_id = c("ChIJ_b", NA), stringsAsFactors = FALSE)
  retry_idx <- c(2L, 3L)

  merged <- tidyschoolvax:::.merge_geocode_retry(attempt1, attempt2, retry_idx)

  expect_equal(merged$place_id, c("ChIJ_a", "ChIJ_b", NA))
  expect_equal(merged$geocode_name_used, c("original", "standardized", "failed"))
})

test_that(".merge_geocode_retry() treats a malformed attempt2 as a failed retry instead of erroring", {
  attempt1 <- data.frame(place_id = c("ChIJ_a", NA), stringsAsFactors = FALSE)
  retry_idx <- 2L

  # Missing place_id column entirely (e.g. a live API response for a
  # pathological, empty-query row).
  attempt2_no_col <- data.frame(lat = NA_real_)
  merged1 <- tidyschoolvax:::.merge_geocode_retry(attempt1, attempt2_no_col, retry_idx)
  expect_equal(merged1$place_id, c("ChIJ_a", NA))
  expect_equal(merged1$geocode_name_used, c("original", "failed"))

  # Row-count mismatch against retry_idx.
  attempt2_wrong_n <- data.frame(place_id = c("ChIJ_x", "ChIJ_y"), stringsAsFactors = FALSE)
  merged2 <- tidyschoolvax:::.merge_geocode_retry(attempt1, attempt2_wrong_n, retry_idx)
  expect_equal(merged2$place_id, c("ChIJ_a", NA))
  expect_equal(merged2$geocode_name_used, c("original", "failed"))
})

# ---- .attach_source_metadata(): level_code is unioned across a collapsed group ----
# Regression coverage for a real bug found on a real CA run: GreatSchools
# sometimes lists the same physical campus twice under different names for
# different grade bands (e.g. a preschool-only listing and a separate TK-8
# listing), both resolving to the same Google place_id via
# collapse_reference_by_place_id(). "First packed id wins" for level_code
# meant the collapsed row could inherit the NARROWER listing's level_code
# (e.g. "p" alone), silently failing the downstream elementary filter
# (grepl("e", level_code)) for a school that genuinely does serve
# elementary grades -- exactly what happened to a real "Our Savior
# Lutheran" campus in Alameda County.

test_that(".attach_source_metadata() unions level_code across every packed orig_id, not just the first", {
  # orig_id "1; 2" mimics a collapsed pair: source row 1 is preschool-only
  # (level_code "p"), source row 2 is TK-8 (level_code "p,e,m"). First-only
  # would keep "p"; the fix must reflect coverage from BOTH.
  lkt <- tibble::tibble(
    orig_id = c("1; 2", "3"),
    school_name_std = c("our savior luth ministries", "unrelated school")
  )
  source_df <- tibble::tibble(
    data1_id   = c(1, 2, 3),
    level_code = c("p", "p,e,m", "e"),
    school_type = c("private", "private", "public")
  )

  out <- tidyschoolvax:::.attach_source_metadata(
    lkt, source_df, id_col = "data1_id", extra_cols = c("level_code", "school_type")
  )

  collapsed_row <- out[out$orig_id == "1; 2", ]
  expect_true(grepl("e", collapsed_row$level_code, fixed = TRUE))
  expect_equal(sort(strsplit(collapsed_row$level_code, ",")[[1]]), c("e", "m", "p"))

  # A row with a single (uncollapsed) orig_id behaves exactly as before.
  single_row <- out[out$orig_id == "3", ]
  expect_equal(single_row$level_code, "e")
})

# ---- .kinder_geocode_name(): geocode on the fuller pre-standardization name ----
# Regression coverage for a real bug found on a real CA run: kinder's
# geocode query used school_name_std (already stripped of level words by
# add_school_level(), e.g. "laurel elementary" -> "laurel") for BOTH the
# original and standardized-name-retry attempts. A bare "laurel" + county
# query is ambiguous enough that Google resolved it to the wrong, generic
# place instead of the actual "Laurel Elementary" school, blocking the
# place_id-match that should have bridged it to GreatSchools/DOE's own
# (address-based, correct) geocode of the same school.

test_that(".kinder_geocode_name() prefers the packed original name over the stripped std name", {
  expect_equal(
    tidyschoolvax:::.kinder_geocode_name("laurel elementary", "laurel"),
    "laurel elementary"
  )
  # Multiple packed original spellings -> first (alphabetically, for
  # determinism, matching how school_names_orig itself is packed).
  expect_equal(
    tidyschoolvax:::.kinder_geocode_name("laurel elem; laurel elementary", "laurel"),
    "laurel elem"
  )
})

test_that(".kinder_geocode_name() falls back to school_name_std when school_names_orig is missing or blank", {
  expect_equal(tidyschoolvax:::.kinder_geocode_name(NA_character_, "laurel"), "laurel")
  expect_equal(tidyschoolvax:::.kinder_geocode_name("", "laurel"), "laurel")
})

test_that("build_kinder_unique_schools() packs school_names_orig from the pre-standardization name", {
  kinder_dat <- tibble::tibble(
    vacc_data_id    = 1:3,
    year_source     = c("ca_20_21", "ca_20_21", "ca_21_22"),
    school_name_std = c("laurel", "laurel", "laurel"),
    school_name     = c("laurel elementary", "laurel elem", "laurel elementary"),
    county_std      = rep("contra costa", 3),
    school_type     = rep("public", 3),
    school_level    = rep("elementary", 3),
    level_code      = rep("e", 3)
  )

  out <- tidyschoolvax:::build_kinder_unique_schools(kinder_dat, n_years_data = 2)

  expect_true("school_names_orig" %in% names(out))
  expect_equal(nrow(out), 1L)
  expect_equal(out$school_names_orig, "laurel elem; laurel elementary")
})

# ---- .merge_kinder_type_duplicates_if_unlisted() ------------------------------
# Regression coverage for a real MD case: "Chesterton Academy of Annapolis"
# was reported "public" in 2021's kinder data and "private" in 2023's --
# build_kinder_unique_schools() keys on school_type, so this became two
# permanently-separate, permanently-unmatchable groups. Neither
# greatschools_dat nor doe_dat has ANY record of the school at all (a small
# private school GreatSchools hasn't indexed), so there's no authoritative
# source to arbitrate -- merge and assume "private" (DOE's roster IS a
# state's public-school list; absent from it means not public).

make_type_dup_kinder <- function(type1 = "public", type2 = "private") {
  tibble::tibble(
    vacc_data_id    = 1:2,
    year_source     = c("md_20_21", "md_22_23"),
    school_name_std = "chesterton academy of annapolis",
    school_name     = "chesterton academy of annapolis",
    county_std      = "anne arundel",
    school_type     = c(type1, type2),
    school_level    = c(NA_character_, NA_character_),
    level_code      = c(NA_character_, NA_character_)
  )
}

test_that(".merge_kinder_type_duplicates_if_unlisted() merges type-duplicate groups absent from both sources", {
  ku <- tidyschoolvax:::build_kinder_unique_schools(make_type_dup_kinder(), n_years_data = 2)
  expect_equal(nrow(ku), 2L)  # confirms the split actually happens pre-fix

  gs  <- tibble::tibble(school_name_std = c("some other school"))
  doe <- tibble::tibble(school_name_std = c("yet another school"))

  out <- tidyschoolvax:::.merge_kinder_type_duplicates_if_unlisted(ku, gs, doe)

  expect_equal(nrow(out), 1L)
  expect_equal(out$school_type, "private")
  expect_equal(out$n_records, 2L)
  expect_setequal(strsplit(out$vacc_data_ids, "; ")[[1]], c("1", "2"))
  expect_setequal(strsplit(out$year_sources, "; ")[[1]], c("md_20_21", "md_22_23"))
})

test_that(".merge_kinder_type_duplicates_if_unlisted() leaves groups alone when the school IS listed in a source", {
  ku <- tidyschoolvax:::build_kinder_unique_schools(make_type_dup_kinder(), n_years_data = 2)

  gs  <- tibble::tibble(school_name_std = c("chesterton academy of annapolis"))
  doe <- tibble::tibble(school_name_std = c("yet another school"))

  out <- tidyschoolvax:::.merge_kinder_type_duplicates_if_unlisted(ku, gs, doe)

  # Found in greatschools_dat -> real matching should get a shot at it
  # normally, so the split is left untouched rather than papered over.
  expect_equal(nrow(out), 2L)
})

test_that(".merge_kinder_type_duplicates_if_unlisted() leaves single-type groups and empty input alone", {
  ku_single <- tidyschoolvax:::build_kinder_unique_schools(make_type_dup_kinder("public", "public"), n_years_data = 2)
  gs  <- tibble::tibble(school_name_std = character(0))
  doe <- tibble::tibble(school_name_std = character(0))

  out <- tidyschoolvax:::.merge_kinder_type_duplicates_if_unlisted(ku_single, gs, doe)
  expect_equal(nrow(out), 1L)
  expect_equal(out$school_type, "public")  # untouched, not forced to "private"

  empty <- ku_single[0, ]
  out_empty <- tidyschoolvax:::.merge_kinder_type_duplicates_if_unlisted(empty, gs, doe)
  expect_equal(nrow(out_empty), 0L)
})

# ---- .detect_kinder_county_outliers() ------------------------------------
# Regression coverage for a real MD case: "Woodmore Elementary School" is
# reported under Prince George's County for every year except 2022, and
# under Anne Arundel County for exactly 2022 -- Prince George's own records
# are missing 2022 entirely (not duplicately available there), the
# fingerprint of a one-year county typo rather than a genuinely different
# second school.

make_woodmore_kinder <- function() {
  tibble::tibble(
    orig_id       = as.character(1:3),
    school_name_std = "woodmore",
    county_std      = c("prince georges", "prince georges", "anne arundel"),
    year_sources    = c("2015; 2016; 2017; 2018; 2019; 2020; 2021", "2023", "2022"),
    n_records       = c(70, 10, 5)
  )
}

test_that(".detect_kinder_county_outliers() flags a county reported only for a year the real county is missing", {
  out <- tidyschoolvax:::.detect_kinder_county_outliers(make_woodmore_kinder())

  expect_equal(nrow(out), 1L)
  expect_equal(out$orig_id, "3")
  expect_equal(out$county_std, "anne arundel")
  expect_equal(out$corrected_county_std, "prince georges")
})

test_that(".detect_kinder_county_outliers() does NOT flag a county whose year is already duplicately available", {
  ku <- make_woodmore_kinder()
  # Anne Arundel's row now covers 2021, a year Prince George's ALREADY has
  # -- overlap means it's no longer a clean complementary gap.
  ku$year_sources[3] <- "2021"

  out <- tidyschoolvax:::.detect_kinder_county_outliers(ku)
  expect_equal(nrow(out), 0L)
})

test_that(".detect_kinder_county_outliers() ignores single-county schools and handles empty/malformed input", {
  single_county <- make_woodmore_kinder()[1:2, ]
  expect_equal(nrow(tidyschoolvax:::.detect_kinder_county_outliers(single_county)), 0L)

  empty <- make_woodmore_kinder()[0, ]
  expect_equal(nrow(tidyschoolvax:::.detect_kinder_county_outliers(empty)), 0L)

  missing_cols <- tibble::tibble(orig_id = "1", school_name_std = "x")
  expect_equal(nrow(tidyschoolvax:::.detect_kinder_county_outliers(missing_cols)), 0L)
})
