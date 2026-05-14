# 01_download.R — Generic State Download Template
#
# Purpose:
#   Download and save state-specific data required for the preprocessing
#   pipeline.  Covers three data sources:
#     1. Kindergarten school-level vaccination data (state health department)
#     2. State school reference data (DOE / DOA, public + private)
#     3. Optional third school data source (e.g. a state EDDIE-style report)
#
#   Outputs (consumed by subsequent pipeline steps):
#     kinder_dir/kinder_dat.csv  &  .rds  — kindergarten vaccination records
#     doe_dir/doe_dat.csv        &  .rds  — DOE/state school reference data
#     state_school_dir/third_dat.csv & .rds  — optional third school source
#
#   NOTE: GreatSchools.org and CDC VaxView downloads are handled by the
#   package function: tidyschoolvax::download_agnostic_data()
#
#   USAGE:
#     This file is sourced by vignettes/preprocessing-orchestration.R after
#     packages are loaded and directory paths are defined.  The variables
#     `state`, `kinder_dir`, `doe_dir`, and `state_school_dir`
#     are expected to exist in the calling environment (set via setup_paths()).
#
#   HOW TO ADAPT FOR A NEW STATE:
#     1. Copy this file to 00_preprocessing/states/<XX>/01_cleaning/01_download.R
#     2. Search for every "# CUSTOMIZE:" comment and follow the instruction.
#     3. Delete or keep the option blocks that apply to your state.
#     4. Move any manual school-name or address corrections to
#        03_state_cleaning.R so this file stays concise.
#
#   LEGEND:
#     # GENERIC:    — uses a package function; usually no edits needed.
#     # CUSTOMIZE:  — state-specific; must be adapted before running.
# =============================================================================


# =============================================================================
# PART 1. KINDERGARTEN VACCINATION DATA
# =============================================================================


# -----------------------------------------------------------------------------
# 1a. Obtain raw data
# -----------------------------------------------------------------------------
# CUSTOMIZE: Choose ONE of the three options below that matches how your state
# distributes kindergarten vaccination data, then delete the others.


# --- Option A: Download Excel files from a state health-department website ---
# Use when annual .xlsx files are listed on a public web page.

# state_url  <- "https://<state-health-dept-url>"   # CUSTOMIZE: URL
# base_url   <- "https://<state-health-dept-base>"  # CUSTOMIZE: base URL for relative hrefs
# page_html  <- rvest::read_html(state_url)
# all_links  <- page_html |>
#   rvest::html_elements("a") |>
#   rvest::html_attr("href")
# xlsx_links <- all_links[grepl("\\.xlsx$", all_links, ignore.case = TRUE)]
# xlsx_links <- ifelse(
#   grepl("^http", xlsx_links),
#   xlsx_links,
#   paste0(base_url, xlsx_links)
# )
# download_xlsx_files(xlsx_links, file.path(kinder_dir, "raw_downloaded"))


# --- Option B: Pre-placed file shared directly by the state health dept ------
# Use when the state shares data via email, SFTP, or a secure portal.
# Place the raw file(s) in kinder_dir before running this script.

# kinder_raw <- read.csv(file.path(kinder_dir, "kinder_raw.csv"))
# OR
# kinder_raw <- readxl::read_excel(file.path(kinder_dir, "kinder_raw.xlsx"))


# --- Option C: ZIP archive from a public data portal -------------------------
# Use when data are distributed as a single zip containing multiple files.

# zip_url  <- "https://<download-url>/data.zip"            # CUSTOMIZE: URL
# zip_path <- file.path(kinder_dir, "raw_downloaded", "data.zip")
# download.file(zip_url, destfile = zip_path, mode = "wb")
# unzip(zip_path, exdir = file.path(kinder_dir, "raw_downloaded"))


# -----------------------------------------------------------------------------
# 1b. Read raw file(s) into R
# -----------------------------------------------------------------------------

# GENERIC — multi-year Excel files (one .xlsx per school year, e.g. Option A/C):
# files <- fs::dir_ls(
#   file.path(kinder_dir, "raw_downloaded"),
#   regexp = "\\.xlsx$",
#   type   = "file"
# )
# files <- files[!grepl("~\\$", fs::path_file(files))]          # drop temp files
# files <- files[stringr::str_detect(
#   fs::path_file(files), "20[0-9]{2}-20[0-9]{2}"               # keep YYYY-YYYY
# )]
# if (length(files) == 0)
#   stop("No valid school-year Excel files found in ",
#        file.path(kinder_dir, "raw_downloaded"))
#
# kinder_list <- purrr::map(files, read_file)                    # GENERIC
# names(kinder_list) <- purrr::map_chr(
#   files, extract_year, state = state                           # GENERIC
# )
#
# # Save individual raw CSVs for auditing
# for (nm in names(kinder_list)) {
#   write.csv(kinder_list[[nm]],
#             file      = file.path(kinder_dir, paste0(nm, "_raw.csv")),
#             row.names = FALSE)
# }


# GENERIC — single pre-placed CSV (Option B): `kinder_raw` is already loaded.
# Apply janitor::clean_names() to normalize column names before proceeding.
# kinder_raw <- kinder_raw |> janitor::clean_names()


# -----------------------------------------------------------------------------
# 1c. Standardize columns across years (multi-year Excel list only)
# -----------------------------------------------------------------------------
# CUSTOMIZE: uncomment the lines whose fixes apply to your state.
#   fix_o0_problem      — replace letter 'O'/'o' with zero in numeric cols
#   clean_redactions    — convert redaction symbols (e.g. "**") to NA
#   remove_redacted_rows — drop rows that were redacted due to small enrolment

# GENERIC:
# kinder_list <- purrr::map(kinder_list, fix_o0_problem)
# kinder_list <- purrr::map(kinder_list, clean_redactions)
# kinder_list <- purrr::map(kinder_list, remove_redacted_rows)

# GENERIC — align columns across all years (adds NA for years missing a col):
# all_columns <- unique(unlist(lapply(kinder_list, colnames)))
# kinder_list <- purrr::map(kinder_list, harmonize_columns, all_cols = all_columns)


# -----------------------------------------------------------------------------
# 1d. Reshape to long format
# -----------------------------------------------------------------------------
# GENERIC — for multi-year Excel list:
# cleaned_long_list <- purrr::map(kinder_list, clean_vaccination_data)

# CUSTOMIZE: if additional count/percent columns are still character after
# clean_vaccination_data, list them here.
# cols_to_numeric <- c("total_k_students", "total_k_students_with_records")
# GENERIC:
# cleaned_long_list <- lapply(
#   cleaned_long_list, coerce_columns_to_numeric, cols = cols_to_numeric
# )

# CUSTOMIZE — for a single pre-placed file (Option B): reshape from wide to
# long manually, using dplyr::bind_rows() to stack the overall and per-vaccine
# rows, then standardize column names to the expected set:
#   year_source, school_name, school_type, county, vaccine_type,
#   total_enrollment, count_up_to_date, count_med_exempt, count_rel_exempt,
#   count_not_up_to_date
# See 00_preprocessing/states/nc/01_cleaning/01_download.R for an example.


# -----------------------------------------------------------------------------
# 1e. Merge annual files and finalize column names
# -----------------------------------------------------------------------------

# GENERIC — multi-year Excel list:
# kinder_dat <- dplyr::bind_rows(cleaned_long_list, .id = "year_source")

# CUSTOMIZE: rename raw column names to the standardized names expected by
# downstream scripts.  Remove the lines for columns your state does not have.
# kinder_dat <- kinder_dat |>
#   dplyr::rename(
#     county           = <raw_county_column>,         # CUSTOMIZE
#     school_type      = <raw_school_type_column>,    # CUSTOMIZE
#     vaccine_coverage = value                        # keep if using clean_vaccination_data
#   )

# GENERIC:
# kinder_dat <- clean_lowercase(kinder_dat)
# kinder_dat <- standardize_kinder_classes(kinder_dat)
# kinder_dat <- clean_school_types(kinder_dat, "school_type")

# CUSTOMIZE: state-specific deduplication, manual school-name corrections, and
# address fixes belong in 03_state_cleaning.R, not here.  Keep this file
# focused on loading, reshaping, and basic standardization.

# CUSTOMIZE: fill derived columns your state may be missing (example from MD):
# kinder_dat <- kinder_dat |>
#   dplyr::mutate(
#     total_k_students_surveyed = ifelse(
#       is.na(total_k_students_surveyed),
#       total_k_students_with_records + total_k_students_without_records,
#       total_k_students_surveyed
#     )
#   )


# -----------------------------------------------------------------------------
# 1f. Export
# -----------------------------------------------------------------------------

# GENERIC:
# write.csv(kinder_dat,
#           file      = file.path(kinder_dir, "kinder_dat.csv"),
#           row.names = FALSE)
# saveRDS(kinder_dat, file.path(kinder_dir, "kinder_dat.rds"))


# =============================================================================
# PART 2. DEPARTMENT OF EDUCATION / STATE SCHOOL REFERENCE DATA
# =============================================================================
# Place raw DOE/DOA files in doe_dir before running. Expected file names:
#   <state>_public.csv   — public schools
#   <state>_charter.csv  — charter schools (if provided separately from public)
#   us_private.csv       — national private-school file (filtered to state)
#
# CUSTOMIZE: column names differ across states.  Use dplyr::rename() to map
# each source file's columns to the standard set:
#   school_name, street, city, county, zip, school_type, grades
#
# Sources to consider for each state:
#   Public/Charter: state GIS portal, NCES CCD, state DOE open-data API
#   Private:        NCES Private School Universe Survey (us_private.csv)


# --- Public schools ----------------------------------------------------------

# doe_public <- readr::read_csv(file.path(doe_dir, paste0(state, "_public.csv"))) |>
#   janitor::clean_names() |>
#   dplyr::rename(
#     school_name = <raw_name_col>,    # CUSTOMIZE
#     street      = <raw_address_col>  # CUSTOMIZE
#   ) |>
#   dplyr::mutate(
#     zip         = stringr::str_extract(as.character(zip), "^\\d{5}"),
#     school_type = "public"
#   )


# --- Charter schools (omit block if charters are included in the public file) -

# doe_charter <- readr::read_csv(file.path(doe_dir, paste0(state, "_charter.csv"))) |>
#   janitor::clean_names() |>
#   dplyr::rename(
#     school_name = <raw_name_col>,    # CUSTOMIZE
#     street      = <raw_address_col>  # CUSTOMIZE
#   ) |>
#   dplyr::mutate(
#     zip         = stringr::str_extract(as.character(zip), "^\\d{5}"),
#     school_type = "charter"
#   )


# --- Private schools (national NCES file, filtered to state) -----------------

# doe_private <- readr::read_csv(file.path(doe_dir, "us_private.csv")) |>
#   janitor::clean_names() |>
#   dplyr::rename(
#     school_name = <raw_name_col>,    # CUSTOMIZE
#     county      = <raw_county_col>   # CUSTOMIZE
#   ) |>
#   dplyr::mutate(
#     zip         = stringr::str_extract(as.character(zip), "^\\d{5}"),
#     school_type = "private"
#   ) |>
#   dplyr::filter(.data$state == .env$state)


# --- Combine and export ------------------------------------------------------

# GENERIC:
# doe_dat <- dplyr::bind_rows(doe_public, doe_charter, doe_private)

# CUSTOMIZE: if your state's DOE file already contains grade-range data, add:
# doe_dat <- standardize_grades(doe_dat,
#                               bgn_col = "<begin_grade_col>",   # CUSTOMIZE
#                               end_col = "<end_grade_col>",     # CUSTOMIZE
#                               out_col = "grades")

# GENERIC:
# readr::write_csv(doe_dat, file.path(doe_dir, "doe_dat.csv"))
# saveRDS(doe_dat, file.path(doe_dir, "doe_dat.rds"))


# =============================================================================
# PART 3. OPTIONAL THIRD SCHOOL DATA SOURCE
# =============================================================================
# Some states publish an additional authoritative school list beyond the DOE
# files above (e.g. NC EDDIE active-schools report, state licensing databases).
# If your state has such a source, place the raw file inside state_school_dir
# with "raw" anywhere in the filename (e.g. "eddie_raw.csv").
# If no third source is needed, leave this entire section commented out.
#
# Required output columns: school_name, street, city, county, state, zip,
#                           school_type, grades
# The cleaned output must be saved as other_dat.csv (the filename read by
# setup_other_sourcedata() inside tidyschoolvax::standardize_schools()).

# GENERIC — file detection and reading:
# raw_file_path <- find_raw_state_file(state_school_dir)
#
# if (!is.null(raw_file_path)) {
#
#   third_raw <- read_raw_state_file(raw_file_path)
#
#   # GENERIC — column selection and standardization:
#   other_dat <- standardize_state_school_data(
#     df  = third_raw,
#     col_map = list(                        # CUSTOMIZE: map to this state's cols
#       school_name = "<raw_name_col>",
#       street      = "<raw_address_col>",
#       city        = "<raw_city_col>",
#       state       = "<raw_state_col>",
#       zip         = "<raw_zip_col>",
#       school_type = "<raw_type_col>",
#       grades      = "<raw_grades_col>"
#     ),
#     school_type_map = list(                # CUSTOMIZE: map raw labels to standard
#       charter = "<CharterLabel>",
#       public  = c("<PublicLabel>", "<FederalLabel>")
#       # any value not listed defaults to "private"
#     )
#   )
#
#   # GENERIC — add county from ZIP when county is absent from the raw file:
#   other_dat <- add_county_from_zip(
#     df         = other_dat,
#     state_abbr = toupper(state)
#     # zip_county_path omitted: uses bundled zip_county package data by default
#   )
#
#   # GENERIC — export as other_dat.csv (required by setup_other_sourcedata()):
#   readr::write_csv(other_dat, file.path(state_school_dir, "other_dat.csv"))
#
#   message("Other school dataset cleaned and saved to: ", state_school_dir)
#
# } else {
#
#   other_dat <- NULL
#   message("No raw other dataset file found in: ", state_school_dir,
#           " -- skipping third dataset.")
# }
