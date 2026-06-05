# 00_orchestration.R
# Purpose: Script to process the whole workflow, from data download to geocoding. 


# 0. Preparation

# Load Packages ----------------------------------------------------------------
# For development (working directly in the repo), use:
#   devtools::load_all()
# For a fully installed package, use:
#   library(tidyschoolvax)
library(tidyschoolvax)

# Specify inputs----------------------------------------------------------------
project_root <- path.expand(getwd()) 
#project_root <- here()
setwd(project_root)
state <- "ca"  # <-- Specify state
state_id <- "ca"  # <-- Specify state for geocoding search string.
vaccine_type_to_keep <- "mmr"   # <-- specify what vaccine type you want
current_state <- toupper(state)
state_name <- state.name[match(current_state, state.abb)]  # Full state name (e.g., "North Carolina")
google_api_key <- "YOUR_GOOGLE_API_KEY"  # <-- Specify Google API key
Sys.setenv(GOOGLEGEO_API_KEY = google_api_key)
# Address source preference: which data source's address to use as the primary
# address in the output.
# Options:
#   "greatschools" (default) - use GreatSchools address
#   "doe"                    - use Department of Education address
#   "other"                  - use additional dataset address (if available for the state)
#   "kinder"                 - use kindergarten data address where available
# Set to "kinder" if the state's kindergarten vaccination data includes reliable
# school addresses that should take priority over other sources.
addr_source_pref <- "kinder"

# Paths and directories
paths <- tidyschoolvax::setup_paths(project_root = project_root, state = state)
list2env(paths, envir = environment())



# --- New state onboarding (run once, then comment out) -----------------------
# When adding a new state for the first time, uncomment the lines below.
# They scaffold the state-specific download and cleaning scripts from the
# package templates and immediately open them in your editor so you can fill
# in the CUSTOMIZE sections before running the rest of the pipeline.
#
# `project_root` is passed explicitly so the files are always written relative
# to the same root used by the rest of this script.  It defaults to getwd()
# inside the function if omitted. The namespaced calls below assume the
# tidyschoolvax package is installed (or loaded via devtools::load_all()).
#
# download_script <- tidyschoolvax::create_state_download_script(state, project_root = project_root)
# file.edit(download_script)
#
# cleaning_script <- tidyschoolvax::create_state_cleaning_script(state, project_root = project_root)
# file.edit(cleaning_script)



# 1. Download vaccination data and standardize column names --------------------
# Purpose
## Download kindergarten school-level vax data, state school data (DOE, DOA, etc.),
## greatschools.org data for standardized school names, and VaxView data

# 1a. State-agnostic downloads (GreatSchools.org, CDC VaxView) -----------------
cat("Step 1a: Download state-agnostic data (GreatSchools, VaxView)...\n")
tidyschoolvax::download_agnostic_data(
  state            = state,
  state_name       = state_name,
  greatschools_dir = greatschools_dir,
  vaxview_dir      = vaxview_dir
)

# 1b. State-specific downloads (kindergarten vax data, DOE/DOA school data) ---
cat("Step 1b: Download state-specific data...\n")
source(file.path(state_dir, "01_cleaning/01_download.R"))

# Outputs
## Merged annual kindergarten school-level vax data (kinder.csv & kinder.rds)
## greatschools.org data for state (greatschools.csv & greatschools.rds)
## State data on all schools (DOE, DOA, etc.) (doe.csv & doe.rds)
## Vaxview data

# Check for expected outputs
check_expected_files(
  paths = c(
    file.path(kinder_dir, "kinder_dat.csv"),
    file.path(kinder_dir, "kinder_dat.rds"),
    file.path(greatschools_dir, "greatschools_dat.csv"),
    file.path(greatschools_dir, "greatschools_dat.rds"),
    file.path(doe_dir, "doe_dat.csv"),
    file.path(doe_dir, "doe_dat.rds"),
    file.path(vaxview_dir, "ChildVaxView.csv"),
    file.path(vaxview_dir, "SchoolVaxView.csv"),
    file.path(vaxview_dir, "TeenVaxView.csv"),
    file.path(vaxview_dir, "vax_view.parquet")
  ),
  step_name = "01_download"
)



# 2. Standardize school name and assign unique IDs / Geocode and school status search -------------------

## Purpose
## Standardize school names by matching across data sources (DOE, greatschools, kinder)
## Assign unique school and county IDs
## Use google API to geocode schools and look for school status 

# Set up parallel workers for fuzzy school-name matching.
# Adjust `workers` to the number of cores you want to dedicate; a common
# default is all available cores minus one.
future::plan(future::multisession, workers = max(1L, parallel::detectCores() - 1L, na.rm = TRUE))

cat("Step 2: Standardize school names, assign unique school and county IDs\n")
school_vax_joined <- tidyschoolvax::standardize_schools(
  state_id         = state_id,
  kinder_dir       = kinder_dir,
  greatschools_dir = greatschools_dir,
  doe_dir          = doe_dir,
  state_school_dir = state_school_dir,
  temp_data_dir    = temp_data_dir,
  state_geo_dir    = state_geo_dir,
  state_dir        = state_dir,
  addr_source_pref = addr_source_pref,
  parallel         = TRUE,
  parallel_cache   = TRUE,
  api_qps          = 50
)

# Restore the default sequential plan after the parallel step.
future::plan(future::sequential)

# Output
## Data with addresses, school level, county, standardized names, lat, lon, operational status
## County key 
## School key

# Check for expected outputs
check_expected_files(
  paths = c(
    file.path(temp_data_dir, "kinder_vaccination_clean_02.csv"),
    file.path(temp_data_dir, "kinder_vaccination_clean_02.rds"),
    file.path(temp_data_dir, "school_key.csv"),
    file.path(temp_data_dir, "county_key.csv")
    
  ),
  step_name = "02_standardize_school"
)

# 3. State Specific Fixes -------------------------------------------------------------

cat("Step 3: State-specific data cleaning...\n")
source(file.path(state_dir, "01_cleaning/03_state_cleaning.R"))

# Output:
## State specific cleaned data (intermediate file) to be used for Step 4

# Check for expected outputs
check_expected_files(
  paths = c(
    file.path(temp_data_dir, "kinder_vaccination_clean_03.rds"),
    file.path(temp_data_dir, "kinder_vaccination_clean_03.csv")
    
  ),
  step_name = "03_state_cleaning"
)


# 4. Final Formatting and DQA Checks --------------------------------------------------------

cat("Step 4: Final formatting and DQA checks...\n")
tidyschoolvax::run_final_formatting(
  state                = state,
  vaccine_type_to_keep = vaccine_type_to_keep,
  temp_data_dir        = temp_data_dir,
  clean_data_dir       = clean_data_dir,
  vaxview_dir          = vaxview_dir,
  general_data_dir     = general_data_dir,
  state_dir            = state_dir,
  outputs_data_dir     = outputs_data_dir
)

# Output:
## Cleaned vaccination data with DQA checks applied, ready for geocoding
## DQA .csv files with issue cases in temp directory
# Check for expected outputs
check_expected_files(
  paths = c(
    file.path(clean_data_dir, "cleaned_data.rds"),
    file.path(clean_data_dir, "cleaned_data.csv")
  ),
  step_name = "04_final_formatting"
)












