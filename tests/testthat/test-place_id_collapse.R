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
    school_name_std = c("a school", "a school"),
    county_std      = c("alpha", "alpha"),
    n_records       = c(1L, 2L),
    year_sources    = c("ca_18_19", "ca_19_20"),
    vacc_data_ids   = c("1", "2; 3")
  )
  res <- tidyschoolvax:::.reaggregate_kinder_by(dt, key_cols = c("school_name_std", "county_std"))

  expect_equal(nrow(res), 1L)
  expect_equal(res$n_records, 3L)
  expect_equal(res$year_sources, "ca_18_19; ca_19_20")
  expect_equal(res$vacc_school_id, 1L)
})
