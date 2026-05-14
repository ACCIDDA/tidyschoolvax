# Shared test fixtures for tidyschoolvax unit tests

#' Build a minimal synthetic vaccination data frame
#'
#' Returns a data frame with 4 years x 5 schools (20 rows) that satisfies
#' the default column requirements of clean_state_data().
make_vax_df <- function(n_schools = 5, years = 2015:2018, seed = 42) {
  set.seed(seed)
  n <- n_schools * length(years)
  df <- data.frame(
    year        = rep(years, each = n_schools),
    school_id   = rep(1001L:(1000L + n_schools), times = length(years)),
    school_name = rep(paste("Test School", seq_len(n_schools)), times = length(years)),
    cnty_id     = rep(1L:n_schools %% 3L + 1L, times = length(years)),
    school_county = rep(paste("County", seq_len(n_schools) %% 3 + 1), times = length(years)),
    school_type = sample(c("public", "private"), n, replace = TRUE),
    enrollment  = sample(80L:200L, n, replace = TRUE),
    med_exempt  = sample(0L:3L,   n, replace = TRUE),
    rel_exempt  = sample(0L:5L,   n, replace = TRUE),
    delayed     = sample(0L:8L,   n, replace = TRUE),
    stringsAsFactors = FALSE
  )
  df$current <- pmax(1L,
    df$enrollment - df$med_exempt - df$rel_exempt - df$delayed)
  df
}
