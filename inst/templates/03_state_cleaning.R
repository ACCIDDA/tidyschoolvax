# 03_state_cleaning.R — Generic State Cleaning Template
#
# Purpose:
#   Apply state-specific data transformations to the kindergarten vaccination
#   data that has been standardized in 02_standardize_school.R.
#   This script covers:
#     1. State-specific column adjustments (scale conversions, imputation, etc.)
#     2. Manual school-name / data-entry corrections
#     3. Address and geocoding fixes
#     4. Standardize column names and assign school IDs for DQA
#     5. Export
#
#   Input:  temp_data_dir/kinder_vaccination_clean_02.rds
#   Output: temp_data_dir/kinder_vaccination_clean_03.rds
#           temp_data_dir/kinder_vaccination_clean_03.csv
#
#   USAGE:
#     This file is sourced by the orchestration script after
#     02_standardize_school.R has run.  The variables `state`, `temp_data_dir`,
#     and other path variables are expected to exist in the calling environment.
#
#   HOW TO ADAPT FOR A NEW STATE:
#     1. Copy this file to
#        00_preprocessing/states/<XX>/01_cleaning/03_state_cleaning.R
#        (or run tidyschoolvax::create_state_cleaning_script("<XX>"))
#     2. Search for every "# CUSTOMIZE:" comment and follow the instruction.
#     3. Delete option blocks that do not apply to your state.
#     4. Keep the "# GENERIC:" blocks as-is unless your data require changes.
#
#   LEGEND:
#     # GENERIC:    — uses a package function; usually no edits needed.
#     # CUSTOMIZE:  — state-specific; must be adapted before running.
# =============================================================================


# =============================================================================
# PART 1. LOAD DATA
# =============================================================================

# GENERIC — load the output of 02_standardize_school.R:
kinder_dat <- readRDS(file.path(temp_data_dir, "kinder_vaccination_clean_02.rds"))


# =============================================================================
# PART 2. STATE-SPECIFIC COLUMN ADJUSTMENTS
# =============================================================================

# -----------------------------------------------------------------------------
# 2a. Scale / imputation fixes
# -----------------------------------------------------------------------------
# CUSTOMIZE: Insert any state-specific transformations here.  Common examples:
#
#   * Convert vaccine_coverage from 0–100 scale to 0–1:
#     kinder_dat <- kinder_dat |>
#       dplyr::mutate(vaccine_coverage = dplyr::if_else(
#         vaccine_coverage > 1, vaccine_coverage / 100, vaccine_coverage
#       ))
#
#   * Back-calculate a missing count from enrollment * vaccine_coverage:
#     kinder_dat <- kinder_dat |>
#       dplyr::mutate(count = dplyr::if_else(
#         is.na(count) & !is.na(vaccine_coverage),
#         round(enrollment * vaccine_coverage),
#         count
#       ))
#
#   * Compute a derived denominator when the state separates surveyed students
#     from the total enrollment (see MD's 03_state_cleaning.R for a full example).
#
# (Remove this comment block once your customization is in place.)


# -----------------------------------------------------------------------------
# 2b. Identify and remove implausible values
# -----------------------------------------------------------------------------
# CUSTOMIZE: Set the names of the count columns (non-negative integers) and
# the percent columns (values in [0, 1]) for THIS state.

percent_cols <- c("vaccine_coverage")            # CUSTOMIZE for this state
count_cols   <- c("enrollment", "count")         # CUSTOMIZE for this state


# GENERIC — distributional overview before cleaning:
kinder_dat |>
  dplyr::select(dplyr::all_of(c(count_cols, percent_cols))) |>
  tidyr::pivot_longer(dplyr::everything(),
                      names_to  = "column",
                      values_to = "value") |>
  dplyr::group_by(column) |>
  dplyr::summarise(
    n          = dplyr::n(),
    n_missing  = sum(is.na(value)),
    n_negative = sum(value < 0, na.rm = TRUE),
    n_zero     = sum(value == 0, na.rm = TRUE),
    min        = min(value, na.rm = TRUE),
    p25        = stats::quantile(value, 0.25, na.rm = TRUE),
    median     = stats::median(value, na.rm = TRUE),
    mean       = round(mean(value, na.rm = TRUE), 1),
    p75        = stats::quantile(value, 0.75, na.rm = TRUE),
    max        = max(value, na.rm = TRUE),
    .groups    = "drop"
  ) |>
  print(width = Inf)


# GENERIC — pre-cleaning summary of invalid values:
pre_clean_summary <- dplyr::bind_rows(
  # Count columns: flag negatives
  kinder_dat |>
    dplyr::select(dplyr::all_of(count_cols)) |>
    tidyr::pivot_longer(dplyr::everything(),
                        names_to  = "column",
                        values_to = "value") |>
    dplyr::group_by(column) |>
    dplyr::summarise(
      col_type    = "count",
      n_missing   = sum(is.na(value)),
      n_negative  = sum(value < 0, na.rm = TRUE),
      n_out_range = NA_integer_,
      .groups     = "drop"
    ),
  # Percent columns: flag values outside [0, 1]
  kinder_dat |>
    dplyr::select(dplyr::all_of(percent_cols)) |>
    tidyr::pivot_longer(dplyr::everything(),
                        names_to  = "column",
                        values_to = "value") |>
    dplyr::group_by(column) |>
    dplyr::summarise(
      col_type    = "percent",
      n_missing   = sum(is.na(value)),
      n_negative  = sum(value < 0, na.rm = TRUE),
      n_out_range = sum(value < 0 | value > 1, na.rm = TRUE),
      .groups     = "drop"
    )
)
print(pre_clean_summary, width = Inf)


# GENERIC — set negatives / out-of-range values to NA:
kinder_dat <- kinder_dat |>
  dplyr::mutate(
    dplyr::across(dplyr::all_of(count_cols),
                  ~ dplyr::if_else(. < 0, NA_real_, .)),
    dplyr::across(dplyr::all_of(percent_cols),
                  ~ dplyr::if_else(. < 0 | . > 1, NA_real_, .))
  )


# GENERIC — post-cleaning summary and comparison:
post_clean_summary <- dplyr::bind_rows(
  kinder_dat |>
    dplyr::select(dplyr::all_of(count_cols)) |>
    tidyr::pivot_longer(dplyr::everything(),
                        names_to  = "column",
                        values_to = "value") |>
    dplyr::group_by(column) |>
    dplyr::summarise(
      col_type    = "count",
      n_missing   = sum(is.na(value)),
      n_negative  = sum(value < 0, na.rm = TRUE),
      n_out_range = NA_integer_,
      .groups     = "drop"
    ),
  kinder_dat |>
    dplyr::select(dplyr::all_of(percent_cols)) |>
    tidyr::pivot_longer(dplyr::everything(),
                        names_to  = "column",
                        values_to = "value") |>
    dplyr::group_by(column) |>
    dplyr::summarise(
      col_type    = "percent",
      n_missing   = sum(is.na(value)),
      n_negative  = sum(value < 0, na.rm = TRUE),
      n_out_range = sum(value < 0 | value > 1, na.rm = TRUE),
      .groups     = "drop"
    )
)
print(post_clean_summary, width = Inf)


# GENERIC — compare pre vs post to confirm no unexpected NAs were introduced:
dplyr::left_join(
  pre_clean_summary |>
    dplyr::rename(missing_before   = n_missing,
                  negative_before  = n_negative,
                  out_range_before = n_out_range),
  post_clean_summary |>
    dplyr::rename(missing_after   = n_missing,
                  negative_after  = n_negative,
                  out_range_after = n_out_range),
  by = c("column", "col_type")
) |>
  dplyr::mutate(new_nas_created = missing_after - missing_before) |>
  print(width = Inf)


# =============================================================================
# PART 3. MANUAL DATA-ENTRY CORRECTIONS
# =============================================================================
# CUSTOMIZE: Fix any known data-entry errors discovered during review.
# Common patterns:
#
#   * Correct a misplaced count value (e.g. count placed in wrong column):
#     kinder_dat$count_up_to_date[
#       kinder_dat$county_std       == "<county>" &
#       kinder_dat$year_source      == "<year>"   &
#       kinder_dat$school_name_orig == "<school>"
#     ] <- <correct_value>
#
#   * Fix a clearly implausible enrollment figure (obvious typo):
#     kinder_dat$total_enrollment[
#       kinder_dat$county_std       == "<county>" &
#       kinder_dat$year_source      == "<year>"   &
#       kinder_dat$school_name_orig == "<school>"
#     ] <- <correct_value>
#
#   See NC's 03_state_cleaning.R for a worked example using both $ assignment
#   and data.table syntax (data.table::setDT() / := operator).
#
# (Remove this comment block once your customization is in place.)


# =============================================================================
# PART 4. ADDRESS AND GEOCODING FIXES
# =============================================================================
# CUSTOMIZE: Correct or drop schools with missing or invalid addresses/geocodes.
# NOTE: This section runs before standardize_kinder_format(), so address columns
#       still have their pre-standardization names (e.g. school_name_orig).
#
# --- Option A: Drop rows missing lat/lon or with non-street addresses ---------
# Use when the preferred strategy is to exclude unlocatable schools.
#
# kinder_dat <- kinder_dat |>
#   dplyr::filter(
#     !is.na(lat) & !is.na(lon),
#     grepl("^[0-9]", addr_clean)       # keeps only rows where address starts with a number
#   )
#
# --- Option B: Patch known corrections for schools that failed geocoding ------
# Do NOT hard-code the corrections into this (committed) script. Keep them in a
# PRIVATE patch file -- these are real, identifiable corrections and must never
# be committed (the package .gitignore ignores *_patches.csv / *_patches.rds /
# patches/). Apply them with apply_patches(), which reads the patch table and
# sets the named fields on the matching rows.
#
# The patch file is long-form: one correction per row, with columns
#   key,field,value
# where `key` identifies the record. Prefer a STABLE id over a mutable label
# (school_name_orig is brittle -- it changes when the upstream name changes).
# See inst/extdata/example_patches.csv for a (synthetic) example of the format.
#
# kinder_dat <- apply_patches(
#   kinder_dat,
#   patches = "<path to your private *_patches.csv>",  # kept out of git
#   key     = "school_name_orig"  # CUSTOMIZE: prefer a stable school id if you have one
# )
#
# `key` may be a vector for a COMPOSITE key when no single stable id exists --
# e.g. key = c("county_std", "year", "school_name_orig"); the patch file then
# carries one column per key part plus `field` and `value`.
#
# GENERIC — ensure addr_clean is consistently lower-case after any edits:
# kinder_dat <- kinder_dat |>
#   dplyr::mutate(addr_clean = tolower(addr_clean))
#
# GENERIC — report remaining missing geocodes after fixes:
# sum(is.na(kinder_dat$lat) | is.na(kinder_dat$lon))


# =============================================================================
# 04_final_formatting.R. `med_exempt_col` and `rel_exempt_col` are required
# arguments to `standardize_kinder_format()`. If your state does not provide
# one or both exemption columns, this template creates placeholder NA columns
# below so the required mappings can still be passed safely.

# GENERIC — map state-specific column names to the standard set expected by
# 04_final_formatting.R.
# GENERIC — ensure required exemption columns exist even when the state does
# not report them.
if (!"med_exempt" %in% names(kinder_dat)) {
  kinder_dat$med_exempt <- NA_real_
}
if (!"rel_exempt" %in% names(kinder_dat)) {
  kinder_dat$rel_exempt <- NA_real_
}

#
# CUSTOMIZE: Update each *_col argument to match THIS state's actual column
# names.  med_exempt_col and rel_exempt_col are required by
# standardize_kinder_format().  If the state does not provide exemption counts,
# create placeholder NA columns before calling this function:
#   kinder_dat$med_exempt <- NA_integer_
  med_exempt_col      = "med_exempt",        # CUSTOMIZE if column name differs
  rel_exempt_col      = "rel_exempt",        # CUSTOMIZE if column name differs
kinder_dat <- standardize_kinder_format(
  df                  = kinder_dat,
  school_id_col       = "school_id",        # CUSTOMIZE if column name differs
  year_col            = "school_year",       # CUSTOMIZE if column name differs
  enrollment_col      = "enrollment",        # CUSTOMIZE: e.g. "new_denom" for MD
  current_col         = "count",             # CUSTOMIZE: e.g. "count_vacc" for MD
  med_exempt_col      = "med_exempt",        # CUSTOMIZE: column of medical exemption counts
  rel_exempt_col      = "rel_exempt",        # CUSTOMIZE: column of religious exemption counts
  school_name_col     = "school_name_orig",
  county_name_col     = "county_std",
  vaccine_type_col    = "category",          # CUSTOMIZE if column name differs
  school_type_col     = "school_type",
  school_level_col    = "school_level",
  level_code_col      = "level_code",
  addr_clean_col      = "addr_clean",
  city_col            = "city",
  zip_col             = "zip",
  state_col           = "state",
  business_status_col = "business_status",
  lat_col             = "lat",
  lon_col             = "lon"
)

dplyr::glimpse(kinder_dat)


# GENERIC — propagate an existing school_id to all rows sharing the same
# addr_clean + city + county_name (fills NAs without overwriting existing IDs).
# NOTE: standardize_kinder_format() outputs county as `county_name`.
kinder_dat <- kinder_dat |>
  dplyr::group_by(addr_clean, county_name, city) |>
  dplyr::mutate(
    group_id  = if (all(is.na(school_id))) NA_integer_
                else dplyr::first(na.omit(school_id)),
    school_id = dplyr::if_else(is.na(school_id), group_id, school_id)
  ) |>
  dplyr::select(-group_id) |>
  dplyr::ungroup()


# GENERIC — generate new integer IDs for addr_clean + city + county_name combos
# that are still NA after the propagation step above:
max_id <- if (all(is.na(kinder_dat$school_id))) 0L
          else max(kinder_dat$school_id, na.rm = TRUE)

new_ids_map <- kinder_dat |>
  dplyr::filter(is.na(school_id)) |>
  dplyr::distinct(addr_clean, city, county_name) |>
  dplyr::mutate(new_generated_id = max_id + dplyr::row_number())

kinder_dat <- kinder_dat |>
  dplyr::left_join(new_ids_map, by = c("addr_clean", "city", "county_name")) |>
  dplyr::mutate(school_id = dplyr::coalesce(school_id, new_generated_id)) |>
  dplyr::select(-new_generated_id)


# GENERIC — enforce identical business_status across all rows sharing a school_id:
kinder_dat <- kinder_dat |>
  dplyr::group_by(school_id) |>
  tidyr::fill(business_status, .direction = "downup") |>
  dplyr::ungroup()


# GENERIC — sanity check: print number of distinct school IDs:
dplyr::n_distinct(kinder_dat$school_id)


# =============================================================================
# PART 6. EXPORT
# =============================================================================

# GENERIC — write outputs consumed by 04_final_formatting.R:
readr::write_csv(kinder_dat,
                 file.path(temp_data_dir, "kinder_vaccination_clean_03.csv"))

saveRDS(kinder_dat,
        file.path(temp_data_dir, "kinder_vaccination_clean_03.rds"))
