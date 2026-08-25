# Tests for Tier 1 within-source deduplication: collapse_reference_by_place_id()
# (GreatSchools/DOE) and .collapse_kinder_by_place_id() (kindergarten), plus
# the .reaggregate_kinder_by() helper extracted to support them. No live
# Google API calls — place_id values are supplied directly on synthetic data.

# ---- collapse_reference_by_place_id() (GreatSchools / DOE) -------------------

test_that("collapse_reference_by_place_id collapses rows sharing a place_id, keeping the first", {
  df <- tibble::tibble(
    data1_id        = 1:3,
    school_name     = c("Lincoln Elem", "Lincoln Elementary School", "Jefferson Middle"),
    school_name_std = c("lincoln elem", "lincoln elementary school", "jefferson middle"),
    place_id        = c("ChIJ_A", "ChIJ_A", "ChIJ_B")
  )

  res <- collapse_reference_by_place_id(
    df, id_col = "data1_id", name_col = "school_name",
    name_std_col = "school_name_std", source_label = "greatschools"
  )

  expect_equal(nrow(res$data), 2L)
  expect_equal(res$data$data1_id, c(1L, 3L))  # canonical = first row of the group

  expect_equal(nrow(res$crosswalk), 3L)
  expect_setequal(
    names(res$crosswalk),
    c("source", "orig_id", "orig_name", "orig_name_std", "place_id",
      "new_id", "new_name", "new_name_std", "n_collapsed", "collapsed")
  )
  cw <- res$crosswalk
  expect_equal(cw$new_id[cw$orig_id == "2"], "1")
  expect_true(cw$collapsed[cw$orig_id == "2"])
  expect_false(cw$collapsed[cw$orig_id == "1"])
  expect_false(cw$collapsed[cw$orig_id == "3"])
  expect_equal(cw$n_collapsed[cw$orig_id == "1"], 2L)
  expect_equal(cw$n_collapsed[cw$orig_id == "3"], 1L)
  expect_true(all(cw$source == "greatschools"))
})

test_that("collapse_reference_by_place_id passes rows through unchanged when place_id is NA", {
  df <- tibble::tibble(
    data2_id        = 1:2,
    school_name     = c("A School", "B School"),
    school_name_std = c("a school", "b school"),
    place_id        = NA_character_
  )
  res <- collapse_reference_by_place_id(df, "data2_id", "school_name", "school_name_std", "doe")

  expect_equal(nrow(res$data), 2L)
  expect_true(all(!res$crosswalk$collapsed))
  expect_true(all(res$crosswalk$n_collapsed == 1L))
  expect_equal(res$crosswalk$new_id, res$crosswalk$orig_id)
})

test_that("collapse_reference_by_place_id handles a missing place_id column", {
  df <- tibble::tibble(data1_id = 1L, school_name = "X", school_name_std = "x")
  res <- collapse_reference_by_place_id(df, "data1_id", "school_name", "school_name_std", "greatschools")

  expect_equal(nrow(res$data), 1L)
  expect_true(is.na(res$crosswalk$place_id))
  expect_false(res$crosswalk$collapsed)
})

test_that("collapse_reference_by_place_id handles empty input", {
  df <- tibble::tibble(data1_id = integer(0), school_name = character(0),
                       school_name_std = character(0), place_id = character(0))
  res <- collapse_reference_by_place_id(df, "data1_id", "school_name", "school_name_std", "greatschools")

  expect_equal(nrow(res$data), 0L)
  expect_equal(nrow(res$crosswalk), 0L)
})

# ---- collapse_reference_by_place_id(): grade-level compatibility -------------
# Regression coverage for a real bug found on a real CA run: two GreatSchools
# listings at the SAME address (same place_id) can be genuinely different
# institutions -- "Antioch YMCA Child Care-Laurel" (level_code "p", a
# childcare program) and "Laurel Elementary School" (level_code "e", the
# actual K-5 school) share one building. Collapsing them purely on place_id
# equality wrongly merged a PK-only program and the K-5 school into "one
# school." When source_df/source_id_col are supplied, two same-place_id rows
# only collapse if their level_code sets overlap.

test_that(".level_code_subgroups() splits disjoint level_code sets, merges overlapping ones", {
  # {p} and {e} share nothing -> two separate sub-groups.
  expect_equal(tidyschoolvax:::.level_code_subgroups(c("p", "e")), c(1L, 2L))

  # {p} and {p,e,m} share "p" -> one sub-group (Our Savior Lutheran case).
  expect_equal(tidyschoolvax:::.level_code_subgroups(c("p", "p,e,m")), c(1L, 1L))

  # Transitive bridge: {p} <-> {p,e} <-> {e,m} are pairwise-or-transitively
  # compatible even though {p} and {e,m} alone share nothing.
  expect_equal(tidyschoolvax:::.level_code_subgroups(c("p", "p,e", "e,m")), c(1L, 1L, 1L))

  # An unknown/missing level_code is compatible with everything.
  expect_equal(tidyschoolvax:::.level_code_subgroups(c("p", NA_character_)), c(1L, 1L))
  expect_equal(tidyschoolvax:::.level_code_subgroups(c(NA_character_, NA_character_)), c(1L, 1L))

  # Single-row / empty input: trivially one group each, no error.
  expect_equal(tidyschoolvax:::.level_code_subgroups("e"), 1L)
  expect_equal(tidyschoolvax:::.level_code_subgroups(character(0)), integer(0))
})

test_that(".union_level_code() unions across packed ids and skips missing ones", {
  lookup <- c(`1` = "p", `2` = "p,e,m", `3` = "e", `4` = NA_character_)

  expect_equal(tidyschoolvax:::.union_level_code("1", lookup), "p")
  expect_equal(tidyschoolvax:::.union_level_code("1; 2", lookup), "e,m,p")
  expect_equal(tidyschoolvax:::.union_level_code("4", lookup), NA_character_)
  # An id not present in the lookup at all contributes nothing.
  expect_equal(tidyschoolvax:::.union_level_code("1; 999", lookup), "p")
})

# ---- .union_level_code_pairs(): cross-source absorption --------------------
# Regression coverage for a real bug found on a real MD run: GreatSchools
# lists only a preschool Head Start program ("Patuxent Elementary Head
# Start", level_code "p") at an address; DOE separately lists "Patuxent
# Elementary" (grades 1-5) at the IDENTICAL address under a different name.
# The names never exact-match, so DOE's row only ever reaches master via
# place_id -- at which point it was being discarded (master already "has"
# this school via GS) without ever contributing its own level_code, leaving
# master permanently preschool-only at that address.

test_that(".union_level_code_pairs() combines two level_code vectors elementwise", {
  expect_equal(
    tidyschoolvax:::.union_level_code_pairs(c("p", "e"), c("e", "p,m")),
    c("e,p", "e,m,p")
  )
  # NA on either side just contributes nothing.
  expect_equal(tidyschoolvax:::.union_level_code_pairs("p", NA_character_), "p")
  expect_equal(tidyschoolvax:::.union_level_code_pairs(NA_character_, "e"), "e")
  expect_equal(tidyschoolvax:::.union_level_code_pairs(NA_character_, NA_character_), NA_character_)
})

test_that(".match_source_against_master() unions an absorbed source row's level_code into master", {
  master_keys <- tibble::tibble(
    orig_id = "1", source = "greatschools",
    school_name = "Patuxent Elementary Head Start", school_name_std = "patuxent elementary head start",
    county_std = "calvert", district_std = NA_character_,
    addr_clean = "35 appeal ln", city = "lusby", zip = "20657", state = "md",
    place_id = "ChIJ_patuxent", geocode_name_used = "original", level_code = "p"
  )
  source_keys <- tibble::tibble(
    orig_id = "10", source = "doe",
    school_name = "Patuxent Elementary", school_name_std = "patuxent elementary",
    county_std = "calvert", district_std = NA_character_,
    addr_clean = "35 appeal ln", city = "lusby", zip = "20657", state = "md"
  )
  source_df <- tibble::tibble(
    data2_id = "10", school_name = "Patuxent Elementary", level_code = "e", school_type = "public"
  )
  # Pre-seed the cache so geocode_original_then_std() never calls a live API.
  tmp <- tempfile("union_level_"); dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  cache <- tibble::tibble(
    school_name = "Patuxent Elementary", county_std = "calvert",
    place_id = "ChIJ_patuxent", lat = 38.35, lon = -76.45
  )
  saveRDS(cache, file.path(tmp, "geocoded_cache.rds"))

  res <- suppressWarnings(suppressMessages(tidyschoolvax:::.match_source_against_master(
    source_keys = source_keys, master_keys = master_keys, source_label = "doe",
    source_df = source_df, source_id_col = "data2_id",
    extra_cols = c("school_type", "level_code"),
    state_id = "md", state_geo_dir = tmp, google_api_key = "unused",
    exact_cols = c("city", "addr_clean"), exact_require = "any"
  )))

  # DOE's row was absorbed (not appended as a new master row)...
  expect_equal(nrow(res$master_keys), 1L)
  # ...but master's level_code now reflects BOTH sources' coverage.
  expect_equal(res$master_keys$level_code, "e,p")
})

test_that("collapse_reference_by_place_id does NOT collapse listings with disjoint level_code when source_df is supplied", {
  df <- tibble::tibble(
    data1_id        = 1:2,
    school_name     = c("Antioch YMCA Child Care-Laurel", "Laurel Elementary School"),
    school_name_std = c("antioch ymca child care laurel", "laurel"),
    place_id        = c("ChIJ_laurel", "ChIJ_laurel")
  )
  source_df <- tibble::tibble(data1_id = 1:2, level_code = c("p", "e"))

  res <- collapse_reference_by_place_id(
    df, id_col = "data1_id", name_col = "school_name",
    name_std_col = "school_name_std", source_label = "greatschools",
    source_df = source_df, source_id_col = "data1_id"
  )

  # Both rows survive as their own schools -- neither collapsed away.
  expect_equal(nrow(res$data), 2L)
  expect_true(all(!res$crosswalk$collapsed))
})

test_that("collapse_reference_by_place_id STILL collapses listings with overlapping level_code when source_df is supplied", {
  df <- tibble::tibble(
    data1_id        = 1:2,
    school_name     = c("Our Savior Luth Ministries", "Our Savior Lutheran"),
    school_name_std = c("our savior luth ministries", "our savior lutheran"),
    place_id        = c("ChIJ_savior", "ChIJ_savior")
  )
  source_df <- tibble::tibble(data1_id = 1:2, level_code = c("p", "p,e,m"))

  res <- collapse_reference_by_place_id(
    df, id_col = "data1_id", name_col = "school_name",
    name_std_col = "school_name_std", source_label = "greatschools",
    source_df = source_df, source_id_col = "data1_id"
  )

  expect_equal(nrow(res$data), 1L)
  expect_true(res$crosswalk$collapsed[res$crosswalk$orig_id == "2"])
})

test_that("collapse_reference_by_place_id ignores level_code entirely when source_df is omitted (default, unchanged behavior)", {
  df <- tibble::tibble(
    data1_id        = 1:2,
    school_name     = c("Antioch YMCA Child Care-Laurel", "Laurel Elementary School"),
    school_name_std = c("antioch ymca child care laurel", "laurel"),
    place_id        = c("ChIJ_laurel", "ChIJ_laurel")
  )

  res <- collapse_reference_by_place_id(
    df, id_col = "data1_id", name_col = "school_name",
    name_std_col = "school_name_std", source_label = "greatschools"
  )

  expect_equal(nrow(res$data), 1L)
})

# ---- .collapse_kinder_by_place_id() -------------------------------------------

test_that(".collapse_kinder_by_place_id merges groups and keeps the most-recent-year name", {
  dt_unique <- data.frame(
    school_name_std = c("old name elementary", "new name elementary", "jefferson middle"),
    county_std      = c("alpha", "alpha", "alpha"),
    school_type     = c("public", "public", "public"),
    school_level    = c("elementary", "elementary", "middle"),
    level_code      = c("e", "e", "m"),
    n_records       = c(2L, 3L, 1L),
    year_sources    = c("ca_18_19; ca_19_20", "ca_22_23; ca_23_24", "ca_23_24"),
    vacc_data_ids   = c("1; 2", "3; 4; 5", "6"),
    vacc_school_id  = 1:3,
    place_id        = c("ChIJ_X", "ChIJ_X", NA_character_),
    stringsAsFactors = FALSE
  )

  res <- tidyschoolvax:::.collapse_kinder_by_place_id(dt_unique)

  # The two place_id = ChIJ_X rows collapse to one; jefferson middle (no
  # place_id) passes through untouched.
  expect_equal(nrow(res$data), 2L)

  merged <- res$data[res$data$school_name_std == "new name elementary", ]
  expect_equal(nrow(merged), 1L)
  expect_equal(merged$n_records, 5L)
  expect_equal(merged$year_sources, "ca_18_19; ca_19_20; ca_22_23; ca_23_24")
  expect_setequal(strsplit(merged$vacc_data_ids, "; ")[[1]], c("1", "2", "3", "4", "5"))
  expect_false("old name elementary" %in% res$data$school_name_std)

  cw <- res$crosswalk
  expect_equal(nrow(cw), 3L)
  expect_setequal(
    names(cw),
    c("source", "orig_id", "orig_name", "orig_name_std", "place_id",
      "new_id", "new_name", "new_name_std", "n_collapsed", "collapsed")
  )
  expect_true(all(cw$source == "kinder"))

  old_row <- cw[cw$orig_name_std == "old name elementary", ]
  expect_true(old_row$collapsed)
  expect_equal(old_row$new_name_std, "new name elementary")
  expect_equal(cw$n_collapsed[cw$orig_name_std == "new name elementary"], 2L)
  expect_equal(cw$n_collapsed[cw$orig_name_std == "jefferson middle"], 1L)
})

test_that(".collapse_kinder_by_place_id is a no-op without a place_id column", {
  dt_unique <- data.frame(
    school_name_std = "a school", county_std = "alpha", school_type = "public",
    school_level = "elementary", level_code = "e", n_records = 1L,
    year_sources = "ca_23_24", vacc_data_ids = "1", vacc_school_id = 1L,
    stringsAsFactors = FALSE
  )
  res <- tidyschoolvax:::.collapse_kinder_by_place_id(dt_unique)

  expect_identical(res$data, dt_unique)
  expect_equal(nrow(res$crosswalk), 0L)
})

test_that(".collapse_kinder_by_place_id handles empty input", {
  res <- tidyschoolvax:::.collapse_kinder_by_place_id(data.frame())
  expect_equal(nrow(res$data), 0L)
  expect_equal(nrow(res$crosswalk), 0L)
})

# ---- .reaggregate_kinder_by() (extracted helper) ------------------------------

test_that(".reaggregate_kinder_by merges packed columns and reassigns vacc_school_id", {
  dt <- data.table::data.table(
    school_name_std   = c("a school", "a school"),
    county_std        = c("alpha", "alpha"),
    n_records         = c(1L, 2L),
    year_sources      = c("ca_18_19", "ca_19_20"),
    vacc_data_ids     = c("1", "2; 3"),
    school_names_orig = c("A School", "a school")
  )
  res <- tidyschoolvax:::.reaggregate_kinder_by(dt, key_cols = c("school_name_std", "county_std"))

  expect_equal(nrow(res), 1L)
  expect_equal(res$n_records, 3L)
  expect_equal(res$year_sources, "ca_18_19; ca_19_20")
  expect_equal(res$vacc_school_id, 1L)
  expect_equal(res$school_names_orig, "A School; a school")
})
